library(tidyverse)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

hydro <- read_csv("data/godeeep-hydro-monthly.csv")
