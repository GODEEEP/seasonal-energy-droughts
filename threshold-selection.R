library(tidyverse)
import::from(ggthemes, colorblind_pal)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

droughts <- seq(10, 50, by = 10) |>
  map(function(p) {
    read_csv("data/droughts/wsh_p%s_droughts.csv" |> sprintf(p)) |>
      mutate(threshold = paste0("p", p))
  }) |>
  bind_rows()

drought_count <- droughts |>
  group_by(ba, threshold) |>
  summarise(n = length(drought_id), .groups = "drop")

p_num_droughts <- drought_count |>
  ggplot() +
  geom_bar(aes(ba, n, fill = threshold),
    color = "grey", linewidth = .3,
    stat = "identity", position = position_dodge2(width = 0.9, preserve = "single")
  ) +
  labs(y = "Total events [1080-2019]") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top"
  ) +
  # scale_fill_viridis_d(option = "G")
  scale_fill_manual(values = colorblind_pal()(6)[-1])
p_num_droughts
ggsave("plots/num_droughts.pdf", width = 5, height = 3)

drought_count |>
  group_by(threshold) |>
  summarise(n_bas = length(unique(ba)), n_events = sum(n))
