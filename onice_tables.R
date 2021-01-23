library(tidyverse)
library(gt)
library(paletteer)

options(scipen = 9999)

# Fill in these variable names
working_directory <- "/Users/tuckerboynton/Desktop/EH Postgames"
filename <- "5.csv"
filename_lagged <- "2.csv"

# Read in relevant data
setwd(working_directory)
data <- read.csv(filename)

lagged_data <- read.csv(filename_lagged)
lag <- lagged_data %>%
  mutate(xG_lag = xG..60) %>%
  select(Player, xG_lag)

data <- merge(data, lag, all = TRUE)

players <- data %>%
  select(Player, TOI, xGF.60, xGA.60, xG..60, xG_lag) %>%
  # filter(TOI > 100) %>%
  mutate(TOI = round(TOI, 3),
         pct_chg = ifelse(!is.na(xG_lag), round(((xG..60 - xG_lag) / abs(xG..60)), 2), "--")) %>%
  select(Player, TOI, xGF.60, xGA.60, xG..60, pct_chg)

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
  filter(TOI >= 25) %>%
  arrange(desc(xG..60)) %>%
  gt() %>%
  tab_header(
    title = "Bruins Skaters 5v5 On-Ice",
    subtitle = "2020/21 Season | Min. 25 TOI"
  ) %>%
  my_theme() %>%
  tab_source_note(md("**Table:** @Tucker_TnL | **Data:** @EvolvingHockey")) %>%
  cols_label(xG..60 = "Diff",
             xGF.60 = "xGF/60",
             xGA.60 = "xGA/60",
             pct_chg = "% Change") %>%
  cols_align(align = "center", columns = c(2:6)) %>%
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
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = vars(pct_chg),
      rows = pct_chg <= 0
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "green")
    ),
    locations = cells_body(
      columns = vars(pct_chg),
      rows = pct_chg > 0
    )
  ) %>%
  tab_footnote(
    footnote = "Over last 3 games",
    locations = cells_column_labels(
      columns = 6
    )
  )

onice