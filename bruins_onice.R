library(tidyverse)
library(gt)
library(paletteer)

options(scipen = 9999)

# Fill in these variable names
working_directory <- "/Users/tuckerboynton/Desktop"
filename <- "EH_std_sk_stats_5v5_regular_adj_2021-01-16.csv"


# Read in relevant data
setwd(working_directory)
data <- read.csv(filename)
players <- data %>%
  select(Player, TOI, xGF.60, xGA.60, xG..60) %>%
  # filter(TOI > 100) %>%
  mutate(TOI = round(TOI, 3))

# Set theme for gt table
my_theme <- function(data) {
  tab_options(
    data = data,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 10,
    column_labels.font.size = 10,
    table.font.size = 10,
    heading.align = "left",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3)
  )
}

# Create table
onice <- players %>%
  arrange(desc(xG..60)) %>%
  gt() %>%
  tab_header(
    title = "Title",
    subtitle = "Subtitle"
  ) %>%
  my_theme() %>%
  tab_source_note(md("**Table:** @Tucker_TnL | **Data:** @Evolving-Hockey")) %>%
  cols_label(xG..60 = "Diff",
             xGF.60 = "xGF/60",
             xGA.60 = "xGA/60") %>%
  cols_align(align = "center", columns = c(2:5)) %>%
  data_color(
    columns = 3, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = 4, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = 5, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  )

onice