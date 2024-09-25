library(tidyverse)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

# event 1 - 11, 12
# event 2 - 11
# event 3 - 2
# event 4 - 12
# event 5 - 12

# pcm_input <- "data/GODEEEP_35"
pcm_input <- "data/ADS_2032"

output_dir <- "data/gridview_inputs_wecc_seasonal_drought_events"

wind_solar_plants <- read_csv("data/HourlyResource_General.csv", skip = 2) |>
  left_join(read_csv("data/GeneratorList.csv", skip = 2), by = join_by(GeneratorKey)) |>
  rename(name = CommitmentShapeName) |>
  distinct(name, ba = `Area Name`, type = Type, capacity = `Capacity(MW)`) |>
  arrange(name, ba) |>
  distinct(name, type, .keep_all = T) |>
  mutate(name = gsub(".DAT", "", name)) |>
  full_join(read_csv("data/gridview_plants_no_ba.csv"), by = join_by(name, type)) |>
  mutate(ba = ifelse(is.na(ba), ba_manual, ba)) |>
  select(-ba_manual)
hydro_plants <- read_csv("data/GeneratorList.csv", skip = 2) |>
  filter(SubType == "Hydro") |>
  select(`Generator Name` = Name, capacity = `PSSEMaxCap(MW)`, ba = `Area Name`)

read_gridview_file <- function(fn) {
  col_names <- scan(fn, what = "c", nlines = 1, quiet = T, sep = ",")
  col_names[col_names == ""] <- make.names(col_names[col_names == ""])
  col_names <- gsub(".DAT: 2030", "", col_names)
  col_names <- gsub(".dat: 2030", "", col_names)
  col_names <- gsub(".DAT", "", col_names)
  col_names <- gsub(".dat", "", col_names)
  read_csv(fn, skip = 1, n_max = 8790, name_repair = "universal_quiet") |>
    rename_all(~col_names) |>
    pivot_longer(-Index)
}

convert_ciso <- function(x) {
  x |> mutate(
    ba_orig = ba,
    ba = case_when(
      ba %in% c("CIPV", "CIPB", "CISC", "CISD") ~ "CISO",
      .default = ba
    )
  )
}

rooftop_solar <- "%s/rooftop_solar.csv" |>
  sprintf(pcm_input) |>
  read_gridview_file() |>
  mutate(ba = gsub("-", "_", name)) |>
  mutate(ba = strsplit(ba, "_") |> sapply("[", 3)) |>
  convert_ciso()

# generation is already unitized
solar <- "%s/solar.csv" |>
  sprintf(pcm_input) |>
  read_gridview_file() |>
  left_join(wind_solar_plants |> filter(type == "Solar"), by = "name") |>
  convert_ciso()

# generation appears to be out of 10000
wind <- "%s/wind.csv" |>
  sprintf(pcm_input) |>
  read_gridview_file() |>
  filter(name != "X") |>
  left_join(wind_solar_plants |> filter(type == "Wind"), by = "name") |>
  mutate(value = value / 10000) |>
  convert_ciso()

# hydro is monthly MWh
month_id <- 1:12 |> `names<-`(month.abb)
hydro <- "%s/HydroMonthlyVarSchedule.csv" |>
  sprintf(pcm_input) |>
  read_csv(skip = 1) |>
  left_join(hydro_plants, by = join_by(`Generator Name`))
hydro_schedule <- hydro |>
  filter(DataTypeName == "MonthlyEnergy") |>
  pivot_longer(all_of(month.abb), names_to = "month_name") |>
  mutate(
    month = month_id[month_name],
    hours_in_month = (days_in_month(1:12) * 24)[month_name]
  ) |>
  convert_ciso()

worst_events <- read_csv("data/worst_events.csv")
droughts <- read_csv("data/droughts/wsh_p40_droughts.csv") |> inner_join(worst_events)

hours8760 <- tibble(datetime = seq.POSIXt(
  as.POSIXct("2001-01-01 00:00:00", tz = "UTC"),
  as.POSIXct("2001-12-31 23:00:00", tz = "UTC"),
  by = "hour"
)) |>
  mutate(month = month(datetime), Index = 1:8760)

for (i in 1:5) {
  #
  message("Adjusting event ", i)
  drought_events_ba <- droughts |> filter(worst_index == i)

  wind_list <- solar_list <- rooftop_solar_list <- hydro_list <- list()
  for (bai in drought_events_ba$ba) {
    #
    ba_drought <- drought_events_ba |>
      filter(ba == bai) %>%
      replicate(.$drought_duration, ., simplify = F) |>
      bind_rows() |>
      mutate(
        datetime_utc = datetime_utc[1] + months(1:drought_duration[1]),
        year = year(datetime_utc),
        month = month(datetime_utc)
      ) |>
      filter(year == year[1]) |>
      select(
        month,
        wind_drought_cf = wind_cf,
        solar_drought_cf = solar_cf,
        hydro_drought_cf = hydro_cf
      )

    # adjust the hourly wind generation to match the monthly average during drought months
    wind_ba <- wind |>
      filter(ba == bai) |>
      left_join(hours8760, by = "Index") |>
      mutate(month = month(datetime))
    wind_ba_adjusted <- wind_ba |>
      left_join(
        ba_drought |> select(month, wind_drought_cf),
        by = "month"
      ) |>
      left_join(
        wind_ba |> group_by(ba, month) |> summarise(wind_cf_monthly = mean(value)),
        by = join_by(ba, month)
      ) |>
      mutate(
        value = ifelse(is.na(wind_drought_cf),
          value, ifelse(wind_drought_cf < wind_cf_monthly,
            wind_drought_cf / wind_cf_monthly * value, value
          )
        ),
        reduction = wind_drought_cf / wind_cf_monthly
      )
    wind_list[[bai]] <- wind_ba_adjusted #|> select(Index, name, value)

    # adjust the hourly solar generation to match the monthly average during drought months
    solar_ba <- solar |>
      filter(ba == bai) |>
      left_join(hours8760, by = "Index") |>
      mutate(month = month(datetime)) #|>
    solar_ba_adjusted <- solar_ba |>
      left_join(
        ba_drought |> select(month, solar_drought_cf),
        by = "month"
      ) |>
      left_join(
        solar_ba |> group_by(ba, month) |> summarise(solar_cf_monthly = mean(value)),
        by = join_by(ba, month)
      ) |>
      mutate(
        value = ifelse(is.na(solar_drought_cf),
          value, ifelse(solar_drought_cf < solar_cf_monthly,
            solar_drought_cf / solar_cf_monthly * value, value
          )
        ),
        reduction = solar_drought_cf / solar_cf_monthly
      )
    solar_list[[bai]] <- solar_ba_adjusted #|> select(Index, name, value)

    # adjust the hourly rooftop solar generation to match the monthly average during drought months
    rooftop_solar_ba <- rooftop_solar |>
      filter(ba == bai) |>
      left_join(hours8760, by = "Index") |>
      mutate(month = month(datetime)) #|>
    rooftop_solar_ba_adjusted <- rooftop_solar_ba |>
      left_join(
        ba_drought |> select(month, solar_drought_cf),
        by = "month"
      ) |>
      left_join(
        rooftop_solar_ba |> group_by(ba, month) |> summarise(rooftop_solar_cf_monthly = mean(value)),
        by = join_by(ba, month)
      ) |>
      # scale the hourly generation
      mutate(
        value = ifelse(is.na(solar_drought_cf),
          value, ifelse(solar_drought_cf < rooftop_solar_cf_monthly,
            solar_drought_cf / rooftop_solar_cf_monthly * value, value
          )
        ),
        reduction = solar_drought_cf / rooftop_solar_cf_monthly
      )
    rooftop_solar_list[[bai]] <- rooftop_solar_ba_adjusted #|> select(Index, name, value)

    # adjust the monthly hydro generation to match the monthly average during drought months
    hydro_ba <- hydro_schedule |>
      filter(ba == bai) |>
      mutate(
        hydro_cf = value / capacity / hours_in_month,
        hydro_cf = ifelse(hydro_cf > 1, 1, hydro_cf),
        hydro_cf = ifelse(hydro_cf < 0, 0, hydro_cf)
      )
    hydro_ba_adjusted <- hydro_ba |>
      left_join(
        ba_drought |> select(month, hydro_drought_cf),
        by = "month"
      ) |>
      mutate(
        value = ifelse(is.na(hydro_drought_cf),
          value, ifelse(hydro_drought_cf < hydro_cf,
            hydro_drought_cf / hydro_cf * value, value
          )
        ),
        reduction = hydro_drought_cf / hydro_cf
      )
    hydro_list[[bai]] <- hydro_ba_adjusted #|> select(-hydro_cf, -hydro_drought_cf)
  }
  # stop()
  message("Writing GridView files")

  # combine the adjusted and non-adjusted data
  wind_adjusted <- bind_rows(wind_list) |>
    bind_rows(wind |> filter(!(ba %in% drought_events_ba$ba))) |>
    select(Index, name, value)
  solar_adjusted <- bind_rows(solar_list) |>
    bind_rows(solar |> filter(!(ba %in% drought_events_ba$ba))) |>
    select(Index, name, value)
  rooftop_solar_adjusted <- bind_rows(rooftop_solar_list) |>
    bind_rows(rooftop_solar |> filter(!(ba %in% drought_events_ba$ba))) |>
    select(Index, name, value)
  hydro_adjusted <- bind_rows(hydro_list) |>
    bind_rows(hydro_schedule |> filter(!(ba %in% drought_events_ba$ba))) |>
    select(`Generator Name`, DataTypeName, DatatypeID, Year, PatternName, month_name, value)

  # combine the reduction factors to check
  wind_reduction <- bind_rows(wind_list) |>
    bind_rows(wind |> filter(!(ba %in% drought_events_ba$ba))) |>
    distinct(ba, month, reduction) |>
    na.omit() |>
    mutate(resource = "wind", event = i)
  solar_reduction <- bind_rows(solar_list) |>
    bind_rows(solar |> filter(!(ba %in% drought_events_ba$ba))) |>
    distinct(ba, month, reduction) |>
    na.omit() |>
    mutate(resource = "solar", event = i)
  rooftop_solar_reduction <- bind_rows(rooftop_solar_list) |>
    bind_rows(rooftop_solar |> filter(!(ba %in% drought_events_ba$ba))) |>
    distinct(ba, month, reduction) |>
    na.omit() |>
    mutate(resource = "rooftop", event = i)
  hydro_reduction <- bind_rows(hydro_list) |>
    bind_rows(hydro_schedule |> filter(!(ba %in% drought_events_ba$ba))) |>
    distinct(ba, month, reduction) |>
    na.omit() |>
    mutate(resource = "hydro", event = i)

  reduction <- bind_rows(wind_reduction, solar_reduction, rooftop_solar_reduction, hydro_reduction)


  event_dir <- file.path(output_dir, paste0("event", i))
  dir.create(event_dir, showWarnings = F)


  write_gridview_hourly_input <- function(gen, fn) {
    f <- file(fn, "w")
    wide <- gen |> pivot_wider(id_cols = Index)
    names(wide)[2:ncol(wide)] <- paste0(names(wide)[-1], ".dat: 2030")
    # column names header
    names(wide) |>
      paste(collapse = ",") |>
      paste0("\n") |>
      cat(file = f)
    # second header row
    paste0("Year,", paste0(rep(2030, ncol(wide) - 1), collapse = ",")) |>
      paste0("\n") |>
      cat(file = f)

    mean_row <- wide |> apply(2, mean, na.rm = T)
    sum_row <- wide |> apply(2, sum, na.rm = T)
    max_row <- wide |> apply(2, max, na.rm = T)
    min_row <- wide |> apply(2, min, na.rm = T)
    n_cols <- length(mean_row)

    # generation data
    write.table(wide,
      col.names = FALSE, row.names = FALSE, na = "",
      quote = FALSE, sep = ",", append = TRUE, file = f
    )
    # write.table messes with the file connection so reopen it, appending
    close(f)
    f <- file(fn, "a")
    # summary rows
    cat("AVG", mean_row[2:n_cols], sep = ",", file = f)
    cat("\n", file = f)
    cat("SUM", sum_row[2:n_cols], sep = ",", file = f)
    cat("\n", file = f)
    cat("MAX", max_row[2:n_cols], sep = ",", file = f)
    cat("\n", file = f)
    cat("MIN", min_row[2:n_cols], sep = ",", file = f)
    cat("\n", file = f)
    close(f)
  }

  wind_fn <- file.path(event_dir, "wind.csv")
  write_gridview_hourly_input(wind_adjusted |> mutate(value = value * 10000), wind_fn)

  solar_fn <- file.path(event_dir, "solar.csv")
  write_gridview_hourly_input(solar_adjusted |> mutate(value = round(value, 3)), solar_fn)

  rooftop_fn <- file.path(event_dir, "roof_solar.csv")
  write_gridview_hourly_input(rooftop_solar_adjusted |> mutate(value = round(value, 3)), rooftop_fn)

  # hydro format is different
  hydro_file <- file(file.path(event_dir, "HydroMonthlyVarSchedule.csv"), "w")
  cat('"HYDROMONTHLYVAR"\n', file = hydro_file)
  hydro_adjusted |>
    pivot_wider(id_cols = everything(), names_from = month_name) |>
    bind_rows(hydro |> filter(DataTypeName != "MonthlyEnergy")) |>
    arrange(`Generator Name`, DataTypeName) |>
    select(-c(capacity, ba)) |>
    relocate(PatternName, .after = last_col()) |>
    mutate(PatternName = ifelse(is.na(PatternName), "", PatternName)) |>
    write.csv(hydro_file, row.names = F)
  close(hydro_file)
}
