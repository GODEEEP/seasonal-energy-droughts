library(tidyverse)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000
)

# import::from(sf, read_sf, st_transform, st_as_sf, st_intersects)
# import::from(jsonlite, read_json)
# import::from(data.table, melt.data.table, as.data.table, setnames, merge.data.table, setkey)
# import::from(ggthemes, colorblind_pal)

# source("lib.R")

min_sites_per_ba <- 5
start_year <- 1980

# metadata
message("Reading metadata")
wind_config <- read_csv("/Volumes/data/tgw-gen-historical/wind/eia_wind_configs.csv")
solar_config <- read_csv("/Volumes/data/tgw-gen-historical/solar/eia_solar_configs.csv")


# generation data
solar_list <- "/Volumes/data/tgw-gen-historical/solar/historical" |>
  list.files("*.csv", full.names = TRUE) |>
  map(read_csv, .progress = TRUE)
wind_list <- "/Volumes/data/tgw-gen-historical/wind/historical" |>
  list.files("*.csv", full.names = TRUE) |>
  map(read_csv, .progress = TRUE)

# add back in the last day of the year in leap days, just duplicate the day before
solar_years <- sapply(solar_list, function(x) {
  year(x$datetime[1])
})
for (y in solar_years) {
  yeari <- which(solar_years == y)
  if (leap_year(y)) {
    last_day <- solar_list[[yeari]] |>
      tail(24) |>
      mutate(datetime = datetime + hours(24))
    solar_list[[yeari]] <- bind_rows(solar_list[[yeari]], last_day)
  }
}
wind_years <- sapply(solar_list, function(x) {
  year(x$datetime[1])
})
for (y in wind_years) {
  yeari <- which(wind_years == y)
  if (leap_year(y)) {
    last_day <- wind_list[[yeari]] |>
      tail(24) |>
      mutate(datetime = datetime + hours(24))
    wind_list[[yeari]] <- bind_rows(wind_list[[yeari]], last_day)
  }
}


solar_wide <- solar_list |>
  map(function(x) {
    x |>
      mutate(month = month(datetime), year = year(datetime)) |>
      group_by(year, month) |>
      summarise(across(-datetime, ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  }, .progress = TRUE) |>
  bind_rows()
# temp fix for names, delete if the header data gets recomputed
# names(solar_wide)[2:ncol(solar_wide)] = paste0(names(solar_wide)[-1],'_',0:(ncol(solar_wide)-2))
wind_wide <- wind_list |>
  map(function(x) {
    x |>
      mutate(month = month(datetime), year = year(datetime)) |>
      group_by(year, month) |>
      summarise(across(-datetime, ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  }, .progress = TRUE) |>
  bind_rows()


rm(solar_list, wind_list)

message("Formatting data")
# filter out bas with less then the min number of both wind and solar plants
# also filter to WECC
solar_count <- solar_config |>
  # filter(nerc_region == 'WECC') |> # & ba != 'SWPP') |>
  group_by(ba) |>
  summarise(solar_num_sites = length(unique(plant_code)))
wind_count <- wind_config |>
  # filter(nerc_region == 'WECC') |># & ba != 'SWPP') |>
  group_by(ba) |>
  summarise(wind_num_sites = length(unique(plant_code)))
plant_counts <- solar_count |>
  full_join(wind_count, by = "ba")

# add east and west split of large BAs
# bas = c(plant_counts |> filter(!(ba %in% c('MISO','PJM','WACM'))) |> pull(ba),
#         'MISO-W', 'MISO-E', 'PJM-W', 'PJM-E')
bas <- plant_counts$ba

chunk_size <- 100

solar_wind_load_ba_ave_list <- list()
for (bai in bas) {
  message("\t", bai)

  # solar
  solar_config_ba <- solar_config |> filter(ba == bai)
  no_solar <- ifelse(nrow(solar_config_ba) > 0, FALSE, TRUE)

  if (!no_solar) {
    solar_wide_ba <- solar_wide |> select(year, month, solar_config_ba |> pull(plant_code_unique))

    n_solar_plants <- nrow(solar_config_ba)
    n_solar_chunks <- ceiling(n_solar_plants / chunk_size)

    # split the data into chunks and compute the ba sum of each chunk
    # otherwise my laptop runs out of memory -_-
    solar_ba_ave_list <- list()
    for (chunki in 1:n_solar_chunks) {
      start <- (chunki - 1) * chunk_size + 2
      end <- min(chunki * chunk_size, n_solar_plants)

      solar_ba <- solar_wide_ba |>
        select(year, month, start:end + 1) |>
        pivot_longer(-c(year, month), names_to = "plant_code_unique") |>
        inner_join(solar_config_ba, by = "plant_code_unique") |>
        mutate(hours_in_month = ifelse(leap_year(year) & month == 2, 29 * 24,
          days_in_month(month) * 24
        )) |>
        mutate(gen_mwh = system_capacity * value * hours_in_month)

      solar_ba_ave_list[[chunki]] <-
        solar_ba |>
        group_by(year, month, ba) |>
        summarise(
          solar_gen_mwh = sum(gen_mwh),
          hours_in_month = hours_in_month[1],
          solar_capacity = sum(system_capacity), .groups = "drop"
        )
    }

    # add together all the chunks and compute the ba capacity factor
    solar_ba_ave <- solar_ba_ave_list |>
      bind_rows() |>
      group_by(year, month, ba) |>
      summarise(
        ba = ba[1],
        solar_gen_mwh = sum(solar_gen_mwh),
        solar_capacity = sum(solar_capacity),
        hours_in_month = hours_in_month[1],
        .groups = "drop"
      ) |>
      mutate(solar_cf = solar_gen_mwh / solar_capacity / hours_in_month) |>
      inner_join(solar_count, by = "ba")
  }

  # wind
  wind_config_ba <- wind_config |> filter(ba == bai)
  no_wind <- ifelse(nrow(wind_config_ba) > 0, FALSE, TRUE)

  if (!no_wind) {
    wind_wide_ba <- wind_wide |> select(year, month, wind_config_ba |> pull(plant_code_unique))

    n_wind_plants <- nrow(wind_config_ba)
    n_wind_chunks <- ceiling(n_wind_plants / chunk_size)

    wind_ba_ave_list <- list()
    for (chunki in 1:n_wind_chunks) {
      start <- (chunki - 1) * chunk_size + 2
      end <- min(chunki * chunk_size, n_wind_plants)

      wind_ba <- wind_wide_ba |>
        select(year, month, start:end + 1) |>
        pivot_longer(-c(year, month), names_to = "plant_code_unique") |>
        inner_join(wind_config_ba, by = "plant_code_unique") |>
        mutate(hours_in_month = ifelse(leap_year(year) & month == 2, 29 * 24,
          days_in_month(month) * 24
        )) |>
        mutate(gen_mwh = system_capacity * value * hours_in_month)


      wind_ba_ave_list[[chunki]] <- wind_ba |>
        group_by(year, month, ba) |>
        summarise(
          wind_gen_mwh = sum(gen_mwh),
          hours_in_month = hours_in_month[1],
          wind_capacity = sum(system_capacity), .groups = "drop"
        )
    }

    wind_ba_ave <- wind_ba_ave_list |>
      bind_rows() |>
      group_by(year, month, ba) |>
      summarise(
        ba = ba[1],
        wind_gen_mwh = sum(wind_gen_mwh),
        wind_capacity = sum(wind_capacity),
        hours_in_month = hours_in_month[1],
        .groups = "drop"
      ) |>
      mutate(wind_cf = wind_gen_mwh / wind_capacity / hours_in_month) |>
      inner_join(wind_count, by = "ba")
  }

  # each BA has either wind, solar or both
  solar_wind_load_ba_ave_list[[bai]] <-
    if (no_wind) {
      solar_ba_ave
    } else if (no_solar) {
      wind_ba_ave
    } else {
      solar_ba_ave |>
        inner_join(wind_ba_ave, by = join_by(year, month, ba, hours_in_month))
    }
}
solar_wind_load_ba_ave <- bind_rows(solar_wind_load_ba_ave_list)

solar_wind_load_ba_ave |> write_csv("data/ba_solar_wind_load_monthly_1980_2019.csv")
