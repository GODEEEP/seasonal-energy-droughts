library(tidyverse)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

source("lib.R")

timezone <- "US/Pacific"

drought_path <- "data/droughts/"

periods <- c(
  "monthly" = 1
)
lower_thresh <- c(
  "monthly" = 0.4
)
upper_thresh <- 1 - lower_thresh

hydro <- read_csv("data/godeeep-hydro-monthly.csv") |>
  rename(hydro_mwh = power_predicted_mwh) |>
  mutate(
    hydro_mwh = ifelse(hydro_mwh < 0, 0, hydro_mwh),
    hydro_mwh = ifelse(hydro_mwh > nameplate * n_hours, nameplate * n_hours, hydro_mwh),
    month = month(datetime),
    year = year(datetime)
  ) |>
  group_by(ba, year, month) |>
  summarise(
    hydro_gen_mwh = sum(hydro_mwh),
    hydro_capacity = sum(nameplate),
    hours_in_month = n_hours[1],
    hydro_num_sites = length(unique(eia_id)),
    .groups = "drop"
  ) |>
  mutate(hydro_cf = hydro_gen_mwh / hydro_capacity / hours_in_month)
wind_solar <- read_csv("data/ba_solar_wind_load_monthly_1980_2019.csv")

hydro_bas <- hydro |>
  pull(ba) |>
  sort() |>
  unique()
ws_bas <- wind_solar |>
  pull(ba) |>
  sort() |>
  unique()

bas_to_include <- data.frame(ba = hydro_bas, hydro = T) |>
  full_join(data.frame(ba = ws_bas, ws = T), by = join_by(ba)) |>
  na.omit() |>
  pull(ba)

hydro_wind_solar <- hydro |>
  inner_join(wind_solar, by = join_by(ba, year, month, hours_in_month)) |>
  na.omit() |>
  mutate(
    datetime_utc = ISOdate(year, month, 1, 0, 0),
    datetime_local = datetime_utc
  )

i <- 1
#
period <- periodi <- periods[i]
period_name <- names(periods)[i]
lt <- lower_thresh[i]
ut <- upper_thresh[i]
message("\n", period_name)


ba_gen_all <- hydro_wind_solar %>%
  # normalize the laod data each year to account for increasing load signal
  group_by(ba) |>
  mutate(
    # standardized values
    solar_s = sdei(solar_gen_mwh),
    wind_s = sdei(wind_gen_mwh),
    hydro_s = sdei(hydro_gen_mwh),
    # quantiles
    solar_q = quantile(solar_gen_mwh, lt),
    wind_q = quantile(wind_gen_mwh, lt),
    hydro_q = quantile(hydro_gen_mwh, lt)
  ) |>
  ungroup()

n_years <- ba_gen_all$year |>
  unique() |>
  length()


####################################
####################################
####################################
# Droughts
####################################
####################################
####################################
# message("Droughts")

for (bai in unique(ba_gen_all$ba)) {
  #
  message("\t", bai)

  ba_gen <- ba_gen_all |> filter(ba == bai)
  # wind and solar
  # -1.28 corresponds to 10th percentile
  wsh_droughts_all <- ba_gen |> energy_drought(wind_s < qnorm(lt) & solar_s < qnorm(lt) & hydro_s < qnorm(lt))
  wsh_droughts <- energy_drought_filter(wsh_droughts_all)

  # hydro and wind
  wh_droughts_all <- ba_gen |> energy_drought(wind_s < qnorm(lt) & hydro_s < qnorm(lt))
  wh_droughts <- energy_drought_filter(wh_droughts_all)

  # hydro and solar
  sh_droughts_all <- ba_gen |> energy_drought(solar_s < qnorm(lt) & hydro_s < qnorm(lt))
  sh_droughts <- energy_drought_filter(sh_droughts_all)

  # wind and solar
  ws_droughts_all <- ba_gen |> energy_drought(solar_s < qnorm(lt) & wind_s < qnorm(lt))
  ws_droughts <- energy_drought_filter(ws_droughts_all)

  # wind
  # -1.28 corresponds to 10th percentile
  # wind_droughts_all = ba_gen |> energy_drought(wind_gen_mwh < wind_q10)
  wind_droughts_all <- ba_gen |> energy_drought(wind_s < qnorm(lt))
  wind_droughts <- energy_drought_filter(wind_droughts_all)

  # solar
  # -1.28 corresponds to 10th percentile
  # solar_droughts_all = ba_gen |> energy_drought(solar_gen_mwh < solar_q10)
  solar_droughts_all <- ba_gen |> energy_drought(solar_s < qnorm(lt))
  solar_droughts <- energy_drought_filter(solar_droughts_all)

  # hydro
  # -1.28 corresponds to 10th percentile
  # solar_droughts_all = ba_gen |> energy_drought(solar_gen_mwh < solar_q10)
  hydro_droughts_all <- ba_gen |> energy_drought(hydro_s < qnorm(lt))
  hydro_droughts <- energy_drought_filter(hydro_droughts_all)


  # write_csv converts the local time to UTC when it writes out
  write_ba_drought <- function(x, drought_type, period_name, bai) {
    if (nrow(x) > 1) {
      x |>
        rename(datetime_utc = datetime_local) |>
        write_csv(sprintf(
          "%s/%s_p%s_droughts_%s.csv", drought_path, drought_type, lt * 100, bai
        ), progress = F)
    }
  }

  # wsh_droughts_all |>
  #   select(-datetime_utc) |>
  #   write_ba_drought("wsh_all", period_name, bai)

  wsh_droughts |> write_ba_drought("wsh", period_name, bai)
  wh_droughts |> write_ba_drought("wh", period_name, bai)
  sh_droughts |> write_ba_drought("sh", period_name, bai)
  ws_droughts |> write_ba_drought("ws", period_name, bai)

  wind_droughts |> write_ba_drought("wind", period_name, bai)
  solar_droughts |> write_ba_drought("solar", period_name, bai)
  hydro_droughts |> write_ba_drought("hydro", period_name, bai)
  #
}
# stop()

# read the BA files and combine
read_combine_ba_droughts <- function(drought_type) {
  combo_fn <- sprintf("%s/%s_p%s_droughts.csv", drought_path, drought_type, lt * 100)
  ba_fns <- drought_path |>
    list.files(sprintf("^%s_p%s_droughts_*", drought_type, lt * 100), full.names = TRUE) %>%
    grep(combo_fn, ., value = TRUE, invert = TRUE)
  ba_fns |>
    map(function(x) {
      read_csv(x)
    }) |>
    bind_rows() |>
    mutate(across(where(is.numeric), \(x) round(x, 4))) |>
    select(-timezone) |>
    write_csv(combo_fn, progress = F)
  unlink(ba_fns)
}
read_combine_ba_droughts("wsh")
read_combine_ba_droughts("wh")
read_combine_ba_droughts("sh")
read_combine_ba_droughts("ws")
read_combine_ba_droughts("wind")
read_combine_ba_droughts("solar")
read_combine_ba_droughts("hydro")

# rm(ba_gen_all)
# rm(list = ls() %>% grep("_all", ., value = T))
# rm(list = ls() %>% grep("_droughts", ., value = T))
# gc()
# sapply(ls(), function(x) format(object.size(get(x)), unit='Mb')) %>% sort
