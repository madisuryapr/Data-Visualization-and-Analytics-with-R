# Indonesia Headline Inflation
# This code objective is to visualize Indonesia's headline inflation trend
# over a decade of data, starting from January 2015 until the most recent one (June 2025)

# Load packages
library(ggplot2)
library(tidyverse)
library(readxl)

# Import and view data
idn_inf <- read_excel("Data/Indonesia Headline Inflation.xlsx", sheet = "data")
idn_inf

# Assess variable class
class(idn_inf$month_date)         # data class for month_date
class(idn_inf$headline_inflation) # data class for headline_inflation

# Convert month_date data type into Date data
idn_inf <- idn_inf %>%
  mutate(month_date = as.Date(month_date))
class(idn_inf$month_date) # re-assess data type

# Visualize time series chart for headline inflation with ggplot2
vis_inf <- ggplot(data = idn_inf,
                  mapping = aes(x = month_date,
                                y = headline_inflation)) +
  geom_line(color = "#052659", linewidth = 1.25) +
  geom_hline(yintercept = 0, color = "#2C2F38", linewidth = 0.5, alpha = 0.70) +
  geom_vline(xintercept = as.Date("2020-03-31"), color = "#971C1E", linewidth = 0.5,
             linetype = 2, alpha = 0.70) +
  labs(
    title = "Indonesia Experienced its Deflation for the First Time Since 25 Years ago in Q1-2025",
    subtitle = "Inflation of Consumer Price Index (percent, year-over-year)",
    x = "",
    y = "",
    caption = "Data Source: Bank Indonesia\nCreated by: Muhammad Adisurya Pratama"
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_labels = "%b-%y",
    breaks = unique(c(seq.Date(from = as.Date("2015-01-01"),
                               to = as.Date("2025-07-01"),
                               by = "1 year"),
                      as.Date("2025-07-01"))),
    limit = c(as.Date("2015-01-01"), as.Date("2025-07-01"))
  )+
  scale_y_continuous(
    expand = c(0, 0), limits = c(-0.5, 8),
    breaks = seq(0, 8, by = 2)
  ) +
  annotate(geom = "point", x = as.Date("2022-09-30"), y = 5.95,
           size = 4.5, shape = 21, fill = "#052659") +
  annotate(geom = "text", x = as.Date("2022-09-30"), y = 6.50,
           label = "Highest inflation since \nThe beginning of COVID-19 \nPandemic at 5.95% (yoy)",
           family = "Helvetica",
           color = "#000000",
           size = 4) +
  annotate(geom = "point", x = as.Date("2025-02-28"), y = -0.1,
           size = 4.5, shape = 21, fill = "#971C1E") +
  annotate(geom = "text", x = as.Date("2024-03-31"), y = 0.5,
           label = "Indonesia experienced deflation \nin February 2025,\nreaching -0.09% (yoy)",
           family = "Helvetica",
           color = "#000000",
           size = 4) +
  annotate(geom = "text", x = as.Date("2020-10-31"), y = 7.5,
           label = "COVID-19 Pandemic \nEmerged in Indonesia",
           family = "Helvetica",
           color = "#000000",
           size = 4) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Times New Roman", face = "bold",
                              color = "#052659", size = 22),
    plot.subtitle = element_text(family = "Helvetica", color = "#000000",
                                 size = 14),
    plot.caption = element_text(family = "Helvetica", color = "#000000",
                                size = 12, hjust = 0),
    plot.caption.position = "plot",
    axis.text.x = element_text(family = "Helvetica", color = "#000000",
                               size = 12, angle = -90, vjust = 0.5),
    axis.text.y = element_text(family = "Helvetica", color = "#000000",
                               size = 12),
    axis.line = element_line(color = "#9BA8AB", linewidth = 1.25),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks = element_line(color = "#2C2F38")
  )

# View Indonesia's Headline Inflation time series chart
vis_inf

# Save Indonesia Headline Inflation chart
ggsave(
  "Indonesia Headline Inflation.jpeg",
  plot = vis_inf,
  width = 1600,
  height = 900,
  units = "px",
  dpi = 110
)
