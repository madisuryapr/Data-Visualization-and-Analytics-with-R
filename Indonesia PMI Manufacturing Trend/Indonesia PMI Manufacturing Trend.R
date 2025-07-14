# PMI Manufacturing and Services Trend of Indonesia
# In this File, I visualize Indonesia's Purchasing Managers' Index (PMI)
# Manufacturing and Services Sector over the period January 2023 until June 2025

# Load Library
library(readxl)  # for reading excel-formatted file
library(ggplot2) # for performing data visualization
library(dplyr)   # for modifying data type
library(ggtext)  # for providing title/subtitle with different colors text
library(zoo)     # converting irregular time series date into monthly terms

# Import Data File
idn_pmi_manufacturing <- read_excel("Data/Indonesia PMI Manufacturing.xlsx")
idn_pmi_manufacturing # view data

# Check data classicifaction
class(idn_pmi_manufacturing$Date)
class(idn_pmi_manufacturing$pmi_manufacturing)

# Convert Date column class from POSIXct into Date format
idn_pmi_manufacturing <- idn_pmi_manufacturing %>%
  mutate(Date = as.Date(Date))

# Re-check data classification
class(idn_pmi_manufacturing$Date)
class(idn_pmi_manufacturing$pmi_manufacturing)

# According to S&P Global, Purchasing Managers'Index (PMI)
# has threshold at 50, suggesting that higher/lower value than
# determined threshold indicates expansion/contraction of an industrial sector

# Create New column that indicates expansion or contraction
idn_pmi_manufacturing <- idn_pmi_manufacturing %>%
  mutate(
    trend_zone = case_when(
      pmi_manufacturing > 50.0 ~ "expansion",
      pmi_manufacturing < 50.0 ~ "contraction",
      pmi_manufacturing == 50 ~ "neutral/stagnant",
      is.na(pmi_manufacturing) ~ "unidentified"
    ),
    trend_zone_color = case_when(
      trend_zone == "expansion" ~ "#0D4680",
      trend_zone == "contraction" ~ "#971C1E",
      trend_zone == "neutral/stagnant" ~ "0D4680",
      trend_zone == "unidentified" ~ "#000000"
    ),
    pmi_manufacturing = as.numeric(pmi_manufacturing)
  )

# View dataframe after adding trend_zone column
idn_pmi_manufacturing

# Perform Data Visualization with ggplot2
pmi_vis <- ggplot(
  data = idn_pmi_manufacturing,
  mapping = aes (
    x = as.yearmon(Date),
    y = pmi_manufacturing - 50
  )
) +
  geom_col(
    fill = idn_pmi_manufacturing$trend_zone_color
  ) +
  geom_hline(
    yintercept = 0, color = "#000000",
    linetype = "solid", linewidth = 0.35
  ) +
  geom_text(
    aes(
      label = pmi_manufacturing,
      y = pmi_manufacturing - 50,
      vjust = ifelse(pmi_manufacturing - 50 >= 0, 1.25, -0.25)
    ),
    hjust = 0.5,
    family = "Inter",
    fontface = "bold",
    size = 3.5,
    color = "white",
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(45, 48, 50, 52, 55) - 50,
    labels = c(45, 48, 50, 52, 55)
  ) +
  coord_cartesian(
    ylim = c(45 - 50, 55 - 50)
  ) +
  scale_x_yearmon(
    expand = expansion(mult = c(0, 0.03)),
    limits = c(as.yearmon("2023-05-31"), as.yearmon("2025-07-01")),
    breaks = seq(from = min(as.yearmon(idn_pmi_manufacturing$Date)), 
                 to = max(as.yearmon(idn_pmi_manufacturing$Date)), by = 0.25),
    format = '%b-%y'
  ) +
  labs(
    title = "What has Happened to Indonesia's Manufacturing Sector?",
    subtitle = "This visualization reports Indonesia's Purchasing Managers' Index (PMI) Manufacturing, encompassing 2 years period of time.<br><span style = 'color:#800D33;'><b>Indonesia has experienced two periods of consecutive contractions</b></span> due to slowing global and domestic demand, as well as global trade uncertainty.</br>",
    caption = "<b>Data Source:</b> S&P Global | Obtained from FastBull.com<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr | Tableau Public: madisuryapr</br>",
    x = "",
    y = ""
  ) +
  annotate(
    geom = "text", x = as.yearmon("2024-09-30"), y = -2,
    label = "Indonesia's manufacturing sector experienced\ncontraction from July to November 2024\n due to slowing global and domestic demand.",
    family = "Helvetica",
    color = "#000000",
    size = 3
  ) +
  annotate(
    geom = "text", x = as.yearmon("2025-04-30"), y = -4,
    label = "Consecutive contraction of manufacturing sector\nonce again occurred due to global trade uncertainty.",
    family = "Helvetica",
    color = "#000000",
    size = 3
  ) +
  theme(
    plot.background = element_rect(fill = "#FFFCFA"),
    panel.background = element_rect(fill = "#FFFCFA"),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Libre Baskerville", face = "bold",
                              color = "#0D4860", size = 16),
    plot.subtitle = ggtext::element_markdown(family = "Helvetica", color = "#000000", size = 10),
    plot.caption = ggtext::element_markdown(family = "Helvetica", color = "#000000", size = 9),
    axis.text.x = element_text(family = "Helvetica", color = "#000000",
                               size = 10, vjust = 0.5),
    axis.text.y = element_text(family = "Helvetica", color = "#000000",
                               size = 10),
    axis.line = element_line(color = "#000000", linewidth = 0.35),
    axis.ticks.length = unit(0.20, "cm"),
    axis.ticks = element_line(color = "#000000")
  )

# View Data Visualization
pmi_vis

# Save Data Visualization into repository
ggsave(
  "Indonesia PMI Manufacturing Trend Over the Last 2 Years.jpeg",
  plot = pmi_vis,
  width = 1700,
  height = 1040,
  units = "px",
  dpi = 160
)
