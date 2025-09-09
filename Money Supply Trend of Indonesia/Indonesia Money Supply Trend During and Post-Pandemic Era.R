# Indonesia Money Supply Trend During and Post-Pandemic Era
# This R File objective is to perform data visualization for
# Monthly Indonesia's money supply dataset, covering January 2020 until June 2025 period of time.

# Lead necessary packages
library(readxl)    # for importing excel data
library(tidyverse) # for visualizing and data manipulation
library(ggtext)    # packages to conduct additonal text within visualization
library(ggrepel)   # Show text to visualization without any legend
library(zoo)       # Utilized as time series date manipulation
library(scales)    # Converting chart's scale

# Read initial data
money_supply <- read_excel(path = "Data/Indonesia Money Supply.xlsx", sheet = "data")
money_supply # view initial data

# Perform data transformation, this includes:
# 1. Converting data from wide to long format
# 2. Converting values to date
# 3. Creating Variable name and color
moneysupp_long <- money_supply %>%
  pivot_longer(
    cols = '43861':'45838',
    names_to = "Date",
    values_to = "value_billion_idr"
  )

moneysupp_long # view long-formatted data

# Check data classification
class(moneysupp_long$Variable)
class(moneysupp_long$Date)
class(moneysupp_long$value_billion_idr)

# Convert the number string into date format, create monthly time-series date, as well as create Category for Money Supply name, alongside its corresponding color.
moneysupp_long <- moneysupp_long %>%
  mutate(
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    month_date = as.yearmon(Date),
    category_name = case_when(
      Variable == "narrow_money_m1" ~ "Narrow Money (M1)",
      Variable == "broad_money_m2" ~ "Broad Money (M2)"
    ),
    category_color = case_when(
      category_name == "Narrow Money (M1)" ~ "#990F3D",
      category_name == "Broad Money (M2)" ~ "#0D4680"
    )
  )

moneysupp_long # re-check long-formatted data

# Add several features, including:
# difference from previous month
# difference from previous year on the same month
# month-to-month (mtm) growth
# year-over-year (yoy) growth
moneysupp_long <- moneysupp_long %>%
  group_by(category_name) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    diff_mtm = value_billion_idr - lag(value_billion_idr, 1),
    diff_yoy = value_billion_idr - lag(value_billion_idr, 12),
    growth_mtm = round((value_billion_idr / lag(value_billion_idr, 1) - 1) *100, 2),
    growth_yoy = round((value_billion_idr / lag(value_billion_idr, 12) - 1) * 100, 2)
  ) %>%
  ungroup()

moneysupp_long # check dataframe whether the operation has done properly

# Create dataframe for the most recent data
most_recent <- moneysupp_long %>%
  filter(Date == "2025-06-30")

# Perform data visualization process for Indonesia's Money Supply, both value and yoy growth
# Static Version of Indonesia Money Supply Visualization
id_moneysup_val <- ggplot(
  data = moneysupp_long,
  mapping = aes(
    x = month_date,
    y = value_billion_idr,
    color = category_name,
    fill = category_name
  )
) +
  geom_area(alpha = 0.2, linewidth = 1, position = "identity") +
  geom_segment(
    aes(
      x = min(moneysupp_long$month_date),
      xend = max(moneysupp_long$month_date),
      y = 0, 
      yend = 0
    ),
    color = "#333333",
    linetype = "solid",
    linewidth = 0.3
  ) +
  scale_fill_manual(values = unique(moneysupp_long$category_color)) +
  scale_color_manual(values = unique(moneysupp_long$category_color)) +
  labs(
    title = "Indonesia's Money Supply Continues the Upward Trajectory",
    subtitle = "This figure reports Indonesia's money supply trend during and post-pandemic era, covering Jan-2020 until Jun-2025 period of time. <br>It includes both <span style = 'color:#990F3D;'><b>Narrow Money Supply (M1)</b></span> and <span style = 'color:#0D4680'><b>Broad Money Supply (M2)</b></span> in which reflect the liquidity of Indonesian economy.</br>",
    caption = "<b>Data Source:</b> Bank Indonesia<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Billion IDR</b>",
    color = "Category", fill = "Category"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = scales::label_comma(),
    limits = c(0, 12000000),
    breaks = seq(0, 12000000, by = 2000000)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 12000000)
  ) +
  scale_x_yearmon(
    expand = expansion(mult = c(0, 0.03)),
    limits = c(as.yearmon("2020-01-01"), as.yearmon("2025-06-01")),
    breaks = seq(
      from = min(moneysupp_long$month_date),
      to = max(moneysupp_long$month_date),
      by = 0.5
    ),
    format = "%b-%y"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F6F6F6"),
    plot.background = element_rect(fill = "#F6F6F6"),
    plot.title = element_text(family = "Helvetica", face = "bold", 
                 color = "#333333", size = 18),
    plot.subtitle = ggtext::element_markdown(family = "Arial", color = "#333333", 
                    size = 11),
    plot.caption = ggtext::element_markdown(family = "Arial", color = "#333333", 
                   size = 10),
    plot.caption.position = "plot",
    legend.background = element_rect(fill = "#F6F6F6", color = NA),
    legend.key = element_rect(fill = "#F6F6F6", color = NA),
    legend.title = element_text(
      family = "Arial",
      face = "bold",
      color = "#333333",
      size = 10
    ),
    legend.text = element_text(
      family = "Arial",
      color = "#333333",
      size = 10
    ),
    axis.text.x = element_text(family = "Arial", color = "#333333", size = 10),
    axis.text.y = element_text(family = "Arial", color = "#333333", size = 10),
    axis.title.y = ggtext::element_markdown(family = "Arial", color = "#333333", size = 10),
    axis.title.x = ggtext::element_markdown(family = "Arial", color = "#333333", size = 10),
    axis.line.y = element_line(color = "#333333", linewidth = 0.30),
    axis.line.x = element_blank(),
    axis.ticks.length = unit(0.20, "cm"),
    axis.ticks = element_line(color = "#333333")
  )

id_moneysup_val

# Create Visualization for both M1 and M2 yoy growth
id_moneysup_yoy <- ggplot(
  data = moneysupp_long,
  mapping = aes(
    x = month_date,
    y = growth_yoy,
    color = category_name
  )
) + 
  geom_line(linewidth = 1.25) +
  geom_segment(
    aes(
      x = as.yearmon("2021-01-01"),
      xend = as.yearmon("2025-06-01"),
      y = 0, yend = 0
    ),
    color = "#333333",
    linetype = "solid",
    linewidth = 0.30
  ) +
  geom_point(
    aes(color = category_name),
    data = most_recent,
    size = 3.5, shape = 19
  ) +
  geom_text_repel(
    data = most_recent,
    aes(
      color = category_name, 
      label = category_name
    ),
    family = "Helvetica",
    fontface = "bold",
    direction = "y",
    size = 3.5,
    hjust = -0.50,
    vjust = 1,
    segment.size = 0.70,
    segment.alpha = 0.60,
    segment.linetype = "dotted",
    box.padding = 0.40,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
    scale_color_manual(values = unique(moneysupp_long$category_color)) +
  labs(
    title = "Money Supply Annualized Growth Reached Its Peak During Pandemic",
    subtitle = "This figure reports Indonesia's money supply year-over-year (yoy) growth trend during and post-pandemic era, covering Jan-2021 until Jun-2025 period of time. <br>It includes both <span style = 'color:#990F3D;'><b>Narrow Money Supply (M1)</b></span> and <span style = 'color:#0D4680'><b>Broad Money Supply (M2)</b></span> in which reflect the liquidity of Indonesian economy.</br>",
    caption = "<b>Data Source:</b> Bank Indonesia<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Growth (% yoy)</b>"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 22),
    breaks = seq(0, 24, by = 4)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 24)
  ) +
  scale_x_yearmon(
    expand = expansion(mult = c(0, 0.20)),
    limits = c(as.yearmon("2021-01-01"), as.yearmon("2025-06-01")),
    breaks = seq(
      from = as.yearmon("2021-01-01"),
      to = as.yearmon("2025-06-01"),
      by = 0.5
    ),
    format = "%b-%y"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F6F6F6"),
    plot.background = element_rect(fill = "#F6F6F6"),
    plot.title = element_text(family = "Helvetica", face = "bold", 
                 color = "#333333", size = 18),
    plot.subtitle = ggtext::element_markdown(family = "Arial", color = "#333333", 
                    size = 10),
    plot.caption = ggtext::element_markdown(family = "Arial", color = "#333333", 
                   size = 10),
    plot.caption.position = "plot",
    legend.position = "none",
    axis.text.x = element_text(family = "Arial", color = "#333333", size = 10),
    axis.text.y = element_text(family = "Arial", color = "#333333", size = 10),
    axis.title.y = ggtext::element_markdown(family = "Arial", color = "#333333", size = 10),
    axis.title.x = ggtext::element_markdown(family = "Arial", color = "#333333", size = 10),
    axis.line.y = element_line(color = "#333333", linewidth = 0.30),
    axis.line.x = element_blank(),
    axis.ticks.length = unit(0.20, "cm"),
    axis.ticks = element_line(color = "#333333")
  )

id_moneysup_yoy

# Save Data Visualization into JPEG Format
# Money Supply in Billion IDR
ggsave(
  "Indonesia Money Supply Trend.jpeg",
  plot = id_moneysup_val,
  width = 1600,
  height = 1024,
  units = "px",
  dpi = 150
)

# Money Supply Annualized (yoy) growth
ggsave(
  "Indonesia Money Supply Annualized Growth.jpeg",
  plot = id_moneysup_yoy,
  width = 1600,
  height = 1024,
  units = "px",
  dpi = 150
)
