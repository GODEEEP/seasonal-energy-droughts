# Function library
# Cameron Bracken, April 2024
# cameron.bracken@pnnl.gov

#' Title
#'
#' @param time
#' @param unique_id
#' @param pad
#' @param run_diff
#'
#' @return
#' @export
#'
#' @examples
run_length <- function(time, unique_id = FALSE, pad = 3, run_diff = 1) {
  # browser()
  lt <- length(time)
  if (lt == 0) {
    return(numeric(0))
  }
  if (unique_id) {
    l <- character(lt)
    run_length_count <- list()
  } else {
    l <- numeric(lt)
  }

  month_seq <- seq.POSIXt(min(time), max(time), by = "month")

  i <- 1
  rl <- 1
  start_run <- 1
  end_run <- 1
  run_len <- 1

  while (i <= lt) {
    i <- i + 1
    if (i <= lt) {
      d <- which(month_seq == time[i]) - which(month_seq == time[i - 1])
      # message(time[i], " ", d)
    } else {
      # last iteration we've gon past the end of the timeseries, there is no time t
      # set diff to 2 to trigger an end of run
      d <- 2
    }
    if (d == run_diff) {
      # still in a run
      end_run <- i
      run_len <- run_len + 1
    } else {
      # run ended, or we hit the end
      if (unique_id) {
        rlc <- as.character(run_len)
        if (is.null(run_length_count[[rlc]])) {
          run_length_count[[rlc]] <- 1
        } else {
          run_length_count[[rlc]] <- run_length_count[[rlc]] + 1
        }
        l[start_run:end_run] <- sprintf(paste0("%02d_%0", pad, "d"), run_len, run_length_count[[rlc]])
      } else {
        l[start_run:end_run] <- run_len
      }
      run_len <- 1
      start_run <- i
      end_run <- i
      run_len <- 1
    }
  }
  if (any(l == 0)) browser()
  l
}

energy_drought <- function(gen, criteria) {
  gen |>
    filter({{ criteria }}) |>
    group_by(ba) |>
    mutate(
      drought_duration = run_length(datetime_utc, run_diff = periodi),
      drought_id = run_length(datetime_utc, unique_id = TRUE, run_diff = periodi),
      severity_mwh = ((solar_q - solar_gen_mwh) + (wind_q - wind_gen_mwh) + (hydro_q - hydro_gen_mwh))
      # standardize
      # severity = (severity_mwh)/sd(severity_mwh),
    )
}

energy_drought_filter <- function(ed) {
  ed |>
    group_by(ba, drought_id) |>
    summarise(
      datetime_local = datetime_local[1],
      timezone = timezone[1],
      drought_duration = drought_duration[1],
      # run_length_days = drought_length_days[1],
      # standardized severity metric
      severity_ws = (mean(abs(solar_s)) + mean(abs(wind_s))) / 2,
      severity_hws = (mean(abs(solar_s)) + mean(abs(wind_s)) + mean(abs(hydro_s))) / 3,
      # add the severity or mw for each timestep over the drought
      severity_mwh = sum(severity_mwh),
      wind_gen_mwh = sum(wind_gen_mwh),
      solar_gen_mwh = sum(solar_gen_mwh),
      hydro_gen_mwh = sum(hydro_gen_mwh),
      year = year[1],
      month = month[1],
      # average capacity factor over the drought duration
      wind_cf = mean(wind_cf),
      solar_cf = mean(solar_cf),
      hydro_cf = mean(hydro_cf),
      solar_s = mean(solar_s),
      wind_s = mean(wind_s),
      hydro_s = mean(hydro_s),
      .groups = "drop"
    ) |>
    # filter single timestep events that only occur at night
    filter(!is.na(severity_ws) & !is.na(severity_hws))
}

#' Title
#'
#' @param hours
#' @param n
#'
#' @return
#' @export
#'
#' @examples
nhour_periods <- function(hours, n) {
  # generate unique periods for aggregation based on a
  # sequence of hours from hourly data. n is the number of hours
  # in each aggregation period. The data is assumed to start at the
  # beginning of a day.
  nh <- length(hours)
  np <- floor(nh / n)
  periods <- rep(1:np, each = n)
  # in case the periods dont divide the input evenly
  if (nh %% n != 0) periods <- c(periods, rep(np + 1, nh %% n))
  periods
}


#' Title
#'
#' @param names
#'
#' @return
#' @export
#'
#' @examples
dedup_names <- function(names) {
  # stolen from an old version of pandas
  # https://stackoverflow.com/questions/24685012/
  # pandas-dataframe-renaming-multiple-identically-named-columns

  counts <- list()

  for (i in 1:length(names)) {
    col <- names[i]
    cur_count <- counts[[col]]

    if (is.null(cur_count)) cur_count <- 0

    if (cur_count > 0) {
      names[i] <- sprintf("%s_%d", col, cur_count)
    }

    counts[[col]] <- cur_count + 1
  }

  return(names)
}


#' Starndardized Drought Energy Index
#'
#' @param x vector of values to compute the index for, they need not be continuous in time
#'
#' @return index value for each data point
#' @export
#'
#' @examples
sdei <- function(x) {
  n <- length(x)
  e <- ecdf(x)
  p <- (1 + e(x) * n) / (n + 2)
  if (all(x == 0)) {
    # hack for energy droughts, if all values are zero then the plotting position
    # returns a value slightly less than 1. This breaks the continuity of the energy
    # droughts over night. So in this case set the index to a large negative value
    # to allow the drought to continue over night.
    rep(-5, n)
  } else {
    qnorm(p, 0, 1)
  }
}


read_csvs_to_list <- function(dir, pattern, ...) {
  fns <- list.files(dir, pattern, full.names = TRUE, ...)
  data_list <- list()
  for (fn in fns) data_list[[fn]] <- read_csv(fn, show = FALSE, progress = FALSE)
  data_list
}

fill_leap_years <- function(data_list) {
  years <- names(data_list) |>
    basename() |>
    tools::file_path_sans_ext() |>
    strsplit("_") |>
    sapply("[", 4) |>
    as.numeric()
  for (y in years) {
    yeari <- which(years == y)
    if (leap_year(y)) {
      last_day <- data_list[[yeari]] |>
        tail(24) |>
        mutate(datetime = datetime + hours(24))
      data_list[[yeari]] <- bind_rows(data_list[[yeari]], last_day)
    }
  }
  data_list
}

fill_leap_year <- function(x, y) {
  last_day <- x |>
    tail(24) |>
    mutate(datetime = datetime + hours(24))
  bind_rows(x, last_day)
}

read_year_csv <- function(year, dir, pattern, ...) {
  fn <- list.files(dir, pattern, full.names = TRUE, ...) %>% grep(paste0("_", year), ., value = T)
  data <- read_csv(fn, show = FALSE, progress = FALSE)
  data
}

read_load_year <- function(year) {
  read_year_csv(year, c("data/tell/historic", "data/tell/rcp85hotter_ssp3"), "TELL_Bal*", recursive = TRUE) |>
    rename(
      datetime_utc = Time_UTC,
      ba = BA_Code,
      load_mwh = Scaled_TELL_BA_Load_MWh
    ) |>
    group_by(ba) |>
    mutate(
      max_load = max(load_mwh),
      load_cf = load_mwh / max_load
    ) |>
    select(datetime_utc, ba, load_cf, max_load)
}

read_wind_solar_year <- function(year, dir, pattern) {
  # browser()
  read_year_csv(year, dir, pattern) |>
    fill_leap_year(year) |>
    # convert to hour beginning
    rename(datetime_utc = datetime) |>
    mutate(datetime_utc = datetime_utc - hours(1))
}

read_experiment_scenario_year <- function(year, tech, experiment, scenario, data_dir) {
  read_wind_solar_year(year, sprintf("%s/%s/%s", data_dir, experiment, scenario), sprintf("%s*", tech))
}

read_baseline_year <- function(year, tech, experiment, data_dir) {
  read_wind_solar_year(year, sprintf("%s/%s", data_dir, experiment), sprintf("%s*", tech))
}



colorblind_ramp <- function(n) {
  colorRampPalette(colorblind_pal()(8)[-1])(n)
}

infra_year <- function(x) {
  x |>
    pull(plant_code) %>%
    substr(1, ifelse(nchar(.) <= 9, 2, 4)) %>%
    as.numeric() %>%
    ifelse(. < 100, . + 2000, .)
}
