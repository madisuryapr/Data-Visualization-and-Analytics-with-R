# Consumer Confidence Index Trend for Selected Indonesian Cities
# This file demonstrate how to create Data Visualization for selected
# Indonesian cities' Consumer Confidence Index (CCI) across 9 cities,
# encapsulating 3 Indonesia's regions.

# Load Packages
library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)
library(patchwork)
library(gghighlight)
library(ggplot2)
library(grid)

# Read data
dataframe <- read.csv("Data/Indonesia Consumer Confidence Index.csv")
dataframe

# Perform dataframe cleaning
dataframe1 <- dataframe %>%
  mutate(date = lubridate::my(Time)) %>%
  select(-Time) %>%
  pivot_longer(
    !date,
    names_to = "city",
    values_to = "value"
  ) %>%
  na.omit() %>%
  mutate(city = ifelse(city == "Bandar.Lampung", "Bandar Lampung", city),
         value = round(value, 2)) %>%
  arrange(city, date)

dataframe1

# Determine theme, font family and size, as well as caption text that provides additional information
text_font <- "Helvetica"
theme_set(theme_minimal(base_family = text_font, base_size = 10))
bg_color <- "#F4F4F4"
text_color <- "#01162B"
showtext_auto(enable = TRUE)

caption_text <- str_glue("**Created by:** Muhammad Adisurya Pratama<br>", "**Inspired by:** Gilbert Fontana<br>", "**Data:** Bank Indonesia, 2025<br>", "**LinkedIn**: Muhammad Adisurya Pratama | **GitHub:** madisuryapr<br>")

# Create the main chart
plot1 <- dataframe1 %>%
  ggplot() +
  geom_hline(yintercept = 100, linetype =  "solid",
             linewidth = 0.25) +
  geom_point(
    data = dataframe1 %>%
        group_by(city) %>%
        slice_max(date),
        aes(
          x = date, y = value,
          color = city  
        ),
        shape = 16
  ) +
  geom_line(
    aes(
        x = date, y = value,
        color = city
    )
  ) +
  gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(
        color = alpha("grey85", 1)
    )
  ) +
  geom_text(
    data = dataframe1 %>%
        group_by(city) %>%
        slice_max(date),
        aes(
            x = date, y = value,
            color = city, label = round(value)
        ),
        hjust = -0.5, vjust = 0.5, size = 3, 
        family = text_font, fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
    "Jakarta" = "#6C0102",
    "Bandung" = "#E22227",
    "Semarang" = "#FF1A66",
    "Medan" = "#354E53",
    "Palembang" = "#5E7680",
    "Bandar Lampung" = "#0D7680",
    "Banjarmasin" = "#0F5499",
    "Pontianak" = "#3D547F",
    "Samarinda" = "#1A8CFF"
    )
  ) +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(
    breaks = c(60, 80, 100, 120, 140, 160),
    labels = c("", "", "100", "", "", "")
  ) +
  facet_wrap(
    ~ factor(
        city,
        levels = c('Jakarta', 'Bandung', 'Semarang', 'Medan', 'Palembang', 'Bandar Lampung', 'Banjarmasin', 'Pontianak', 'Samarinda')
    )
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = text_color, size = 11),
    strip.text.x = element_text(size = 11, family = text_font, face = "bold"),
    plot.caption.position = "plot",
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.background = element_rect(color = bg_color, fill = bg_color),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none"
  )

plot1

# Create Title and Subtitle for the data visualization
# Title block
title_block <- tibble(
  x = 0, y = 0,
  label = "**Consumer Confidence for Selected Indonesian Cities**"
)

title_plot <- ggplot(title_block, aes(x, y)) +
  geom_textbox(
    aes(label = label),
    width = unit(1, "npc"),  # full plot width
    box.color = bg_color, fill = bg_color,
    size = 12, lineheight = 1,
    halign = 0.5,  # centered text inside the box
    valign = 0.5
  ) +
  theme_void() +
  theme(
    plot.margin = margin(10, 20, 0, 20),
    plot.background = element_rect(fill = bg_color, color = bg_color)
  )

# Subtitle block
subtitle_block <- tibble(
  x = 0, y = 0,
  label = "This chart reports consumer confidence indicator for 9 selected Indonesian cities, encompassing 3 different regions: Jawa, Sumatera, and Kalimantan. Consumer Confidence Index (CCI), published monthly by Bank Indonesia, reflects consumers' perspective on current economic conditions and expectations for the future trajectory. A CCI above 100 = optimism, below 100 = pessimism. Recently, 6 of 9 cities show a downward trend."
)

subtitle_plot <- ggplot(subtitle_block, aes(x, y)) +
  geom_textbox(
    aes(label = label),
    width = unit(1, "npc"),
    box.color = bg_color, fill = bg_color,
    family = text_font, size = 4, lineheight = 1,
    halign = 0,  # left-align paragraph style
    valign = 0
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0, 40, 10, 40),
    plot.background = element_rect(fill = bg_color, color = bg_color)
  )

# Combine title, subtitle, and main chart
finalPlot <- ((title_plot + subtitle_plot) / plot1) +
  plot_layout(heights = c(0.25, 0.75)) +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_markdown(
        family = text_font, hjust = 0, size = 9, color = text_color,
        margin = margin(10, 0, 0, 0)
      ),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

finalPlot

# Save the Plot
ggsave(
    filename = "Indonesia Consumer Confidence.jpeg",
    plot = finalPlot,
    units = "px",
    width = 1600,
    height = 1080,
    dpi = 150,
    bg = "transparent"
)
