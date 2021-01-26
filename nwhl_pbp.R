library(tidyverse)
library(ggplot2)
library(zoo)
library(paletteer)
library(gt)
library(ggtext)

setwd("/Users/tuckerboynton/Desktop")
data1 <- read.csv("NWHLPBP2021-0124.csv")
data2 <- read.csv("NWHLPBP2021-0123.csv")

data3 <- rbind(data1, data2)

data4 <- data3 %>%
  mutate(five_min = case_when(
    game_seconds <= 300 ~ 2.5,
    game_seconds <= 600 & game_seconds > 300 ~ 7.5,
    game_seconds <= 900 & game_seconds > 600 ~ 12.5,
    game_seconds <= 1200 & game_seconds > 900 ~ 17.5,
    game_seconds <= 1500 & game_seconds > 1200 ~ 22.5,
    game_seconds <= 1800 & game_seconds > 1500 ~ 27.5,
    game_seconds <= 2100 & game_seconds > 1800 ~ 32.5,
    game_seconds <= 2400 & game_seconds > 2100 ~ 37.5,
    game_seconds <= 2700 & game_seconds > 2400 ~ 42.5,
    game_seconds <= 3000 & game_seconds > 2700 ~ 47.5,
    game_seconds <= 3300 & game_seconds > 3000 ~ 52.5,
    game_seconds > 3600 ~ 57.5)
    ) %>%
  group_by(event_team, five_min) %>%
  filter(event_type == "game_shot" | event_type == "goal", !is.na(five_min)) %>%
  summarize(
    shots = round((n() / 2) * 3, 1)
  )

data5 <- spread(data4, event_team, shots)

my_theme <- function(data) {
  tab_options(
    data = data,
    heading.title.font.size = 22,
    heading.subtitle.font.size = 16,
    column_labels.font.size = 20,
    table.font.size = 20,
    heading.align = "left",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    source_notes.font.size = 16,
    table_body.border.bottom.color = "black",
    table_body.border.top.color = "black"
  )
}

nwhl <- data5 %>%
  gt() %>%
  cols_label(
    period = "Period"
  ) %>%
  tab_header(
    title = md("**NWHL Shooting Pressure, 2021**"),
    subtitle = "Shots For per 60 | Through 2 G"
  ) %>%
  my_theme() %>%
  tab_source_note(md("**Table:** @Tucker_TnL | **Data:** @alyssastweeting")) %>%
  cols_align(align = "center", columns = c(2:7)) %>%
  data_color(
    columns = 2:7, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = c(15, 70)
    )
  ) %>%
  fmt_number(columns = 2:7, decimals = 1)

nwhl


data4 %>%
  ggplot(aes(x = five_min, y = shots)) +
  geom_smooth(aes(group = event_team, colour = factor(event_team)), se = FALSE, size = 2) +
  labs(x = "Minutes Into Game",
       y = "Shots For per 60",
       caption = "Figure: @Tucker_TnL",
       subtitle = "Data: @alyssastweeting",
       colour = "Team",
       title = "NWHL Shot Pressure, 2021") +
  theme(
    legend.position = "left",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))