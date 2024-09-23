library(gridExtra)
library(tidyverse)
library(ggpubr)
library(sf)
import::from(ggthemes, colorblind_pal)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

droughts <- read_csv("data/droughts/wsh_p40_droughts.csv")
wh_droughts <- read_csv("data/droughts/wh_p40_droughts.csv")
sh_droughts <- read_csv("data/droughts/sh_p40_droughts.csv")
ws_droughts <- read_csv("data/droughts/ws_p40_droughts.csv")
wind_droughts <- read_csv("data/droughts/wind_p40_droughts.csv")
solar_droughts <- read_csv("data/droughts/solar_p40_droughts.csv")
hydro_droughts <- read_csv("data/droughts/hydro_p40_droughts.csv")

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

hydro_wind_solar <- hydro |>
  inner_join(wind_solar, by = join_by(ba, year, month, hours_in_month)) |>
  na.omit() |>
  mutate(
    datetime_utc = ISOdate(year, month, 1, 0, 0),
    datetime_local = datetime_utc
  )

expand_droughts <- function(x) {
  x |>
    mutate(id = 1:nrow(x)) |>
    group_split(id) |>
    map(function(x) {
      replicate(x$drought_duration, x, simplify = F) |>
        bind_rows() |>
        mutate(
          datetime_utc = datetime_utc[1] + months(0:(drought_duration[1] - 1)),
          year = year(datetime_utc),
          month = month(datetime_utc)
        ) #|>
      # filter(year == year[1]) |>
    }) |>
    bind_rows()
}

droughts_all <- bind_rows(
  expand_droughts(droughts) |> mutate(drought_type = "wsh"),
  expand_droughts(wh_droughts) |> mutate(drought_type = "wh"),
  expand_droughts(sh_droughts) |> mutate(drought_type = "sh"),
  expand_droughts(ws_droughts) |> mutate(drought_type = "ws"),
  expand_droughts(wind_droughts) |> mutate(drought_type = "w"),
  expand_droughts(solar_droughts) |> mutate(drought_type = "s")
) |>
  mutate(drought_type = factor(drought_type, levels = c("w", "s", "ws", "sh", "wh", "wsh")))


# drought duration seasonality
p_freq_dur_all <- droughts_all |> ggplot() +
  geom_histogram(aes(x = month, y = after_stat(density), fill = factor(drought_duration)), bins = 12) +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~drought_type) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  scale_fill_manual("Compound Drought Duration (months)", values = rev(colorblind_pal()(8))) +
  labs(x = "Month", y = "Number of VRE Drought Events")
p_freq_dur_all


p_freq_dur <- droughts_all |>
  filter(drought_type == "wsh") |>
  ggplot() +
  geom_histogram(aes(x = month, y = after_stat(density), fill = factor(drought_duration)), bins = 12) +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  scale_fill_manual("Compound Drought Duration (months)", values = colorblind_pal()(8)[-1][1:5]) +
  labs(x = "Month", y = "Number of VRE Drought Events")
p_freq_dur

# seasonality_conus <- hydro_wind_solar |>
#   group_by(month) |>
#   summarise(
#     hydro_q10 = quantile(hydro_cf, .1), solar_q10 = quantile(solar_cf, .1), wind_q10 = quantile(wind_cf, .1),
#     hydro_q90 = quantile(hydro_cf, .9), solar_q90 = quantile(solar_cf, .9), wind_q90 = quantile(wind_cf, .9),
#     hydro_cf = mean(hydro_cf), solar_cf = mean(solar_cf), wind_cf = mean(wind_cf)
#   ) |>
#   pivot_longer(-c(month), names_sep = "_", names_to = c("variable", "name")) |>
#   pivot_wider(id_cols = c(month, variable))

# # seasonality overall
# seasonality_conus |> ggplot() +
#   geom_ribbon(aes(month, ymin = q10, ymax = q90, y = cf, fill = variable), alpha = .25) +
#   geom_line(aes(month, y = cf, color = variable), linewidth = 1.5) +
#   scale_fill_manual("", values = colorblind_pal()(8)[c(6, 7, 4)]) +
#   scale_color_manual("", values = colorblind_pal()(8)[c(6, 7, 4)]) +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank(),
#     legend.position = "top"
#   ) +
#   scale_x_continuous(breaks = 1:12) +
#   # scale_y_continuous(limits = c(0, 1)) +
#   labs(x = "Month", y = "Monthly generation capacity factor")

# seasonality
seasonality <- hydro_wind_solar |>
  group_by(month, ba) |>
  summarise(
    hydro_q10 = quantile(hydro_cf, .1), solar_q10 = quantile(solar_cf, .1), wind_q10 = quantile(wind_cf, .1),
    hydro_q90 = quantile(hydro_cf, .9), solar_q90 = quantile(solar_cf, .9), wind_q90 = quantile(wind_cf, .9),
    hydro_cf = mean(hydro_cf), solar_cf = mean(solar_cf), wind_cf = mean(wind_cf)
  ) |>
  pivot_longer(-c(month, ba), names_sep = "_", names_to = c("variable", "name")) |>
  pivot_wider(id_cols = c(month, ba, variable))

# seasonality by ba
p_seasonality <- seasonality |> ggplot() +
  geom_ribbon(aes(month, ymin = q10, ymax = q90, y = cf, fill = variable), alpha = .6) +
  geom_line(aes(month, y = cf, color = variable)) +
  facet_wrap(~ba, ncol = 6) +
  scale_fill_manual("", values = colorblind_pal()(8)[c(6, 7, 4)]) +
  scale_color_manual("", values = colorblind_pal()(8)[c(6, 7, 4)]) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Month", y = "Monthly generation capacity factor")
p_seasonality

# arrangeGrob(p_freq_dur, p_seasonality, ncol = 1, heights = c(.3, .7)) |>
# ggsave(filename = "plots/freq_dur_season.pdf", width = 10, height = 10)


# frequency map
ba_centroids <- read_csv("data/ba-centroids.csv", show = F, progress = F) |>
  # filter(ba %in% bas_to_include) |>
  # adjust positions for plot clarity
  mutate(
    lon = case_when(
      ba == "CISO" ~ lon - 3,
      ba == "SRP" ~ lon + 1,
      ba == "AZPS" ~ lon - 2,
      ba == "PACW" ~ lon - 2,
      ba == "AVA" ~ lon,
      ba == "WALC" ~ lon + 1,
      ba == "BPAT" ~ lon + 1,
      ba == "PSEI" ~ lon - 1,
      .default = lon
    ),
    lat = case_when(
      ba == "CISO" ~ lat + 3,
      ba == "TEPC" ~ lat - 1,
      ba == "PSCO" ~ lat - 1.5,
      ba == "WALC" ~ lat + 3,
      ba == "PACW" ~ lat - 1,
      ba == "AVA" ~ lat + 1,
      ba == "PSEI" ~ lat + 1,
      .default = lat
    )
  )

states_sf <- st_read("/Volumes/data/shapefiles/cb_2018_us_state_5m/cb_2018_us_state_5m.shp") #|>
# filter(NAME %in% c(
#   "California", "Washington", "Oregon", "Idaho", "Nevada", "Montana",
#   "New Mexico", "Arizona", "Utah", "Colorado", "Wyoming"
# ))
freq <- droughts |>
  group_by(ba) |>
  summarise(n = n() / length(1982:2019)) |>
  left_join(ba_centroids, by = "ba")

p_map_freq <-
  ggplot() +
  # ggplot(ba_sf)+
  # ggplot(ba_sf) +
  geom_sf(color = grey(.7), linewidth = .3, fill = "white", data = states_sf) +
  geom_point(aes(lon, lat, fill = n, size = n), data = freq, shape = 21) +
  theme_void() +
  # geom_sf(aes(fill = NAME), alpha = 0.5) +
  # geom_sf_label(aes(label = short_name)) +
  # scale_fill_manual("BA", values = colorblind_pal()(6)[-1]) +
  scale_size_continuous("Average events per year", breaks = seq(0, 1.6, by = .4), range = c(1, 10)) +
  scale_fill_viridis_c("Average events per year", breaks = seq(0, 1.6, by = .4), option = "G") +
  guides(
    fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1),
    size = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)
  ) +
  coord_sf(xlim = c(-124.2, -69.5), ylim = c(25, 48.5)) +
  # theme_bw() +
  theme(legend.position = "top") +
  labs(x = "", y = "")
p_map_freq


# arrangeGrob(ggarrange(p_freq_dur, p_map_freq, nrow = 1, labels = c("A", "B")),
#   p_seasonality,
#   nrow = , heights = c(.4, .6),
# )

p_combo <- (p_seasonality / (p_freq_dur + p_map_freq)) +
  plot_layout(ncol = 1, heights = c(.65, .35)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
p_combo
ggsave(p_combo, filename = "plots/freq_dur_season.pdf", width = 10, height = 10)
