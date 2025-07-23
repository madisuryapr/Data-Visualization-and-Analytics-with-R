# Central Banks Policy Rate Trend for Several Countries
# This R file objective is to perform data visualization for
# monthly Central Banks Policy Rates for several countries, including Indonesia

# Utilized data within this code is as of 20th July, 2025

# Load necessary packages
library(readxl)    # for reading Excel (.xlsx) file
library(tidyverse) # for performing data manipulation and visualization
library(ggtext)    # packages to conduct additional text within visualization
library(ggrepel)   # packages for repelling overlapped text labels
library(zoo)       # Manipulating time series date

# Read Initial data
policyrate <- read_excel(path = "Data/Central Banks Policy Rate Trend Data.xlsx",
              sheet = "data")
policyrate # view intial data

# transform data from wide to long format and re-arrange based on country
policyrate_long <- policyrate %>% pivot_longer(
  cols = Indonesia:Japan,
  names_to = "country",
  values_to = "rate_value"
) %>% arrange(country)

policyrate_long # view long-format data

# Check data classification
class(policyrate_long$Date)       # Check data classification for Date column
class(policyrate_long$country)    # Check data classification for country column
class(policyrate_long$rate_value) # Check data classification for rate_value column

# Convert Date column format from POSIXct into Date format
# and create monthly data form with zoo
policyrate_long <- policyrate_long %>%
  mutate(Date = as.Date(Date),
         month_date = as.yearmon(Date),
        country = as.factor(country))

class(policyrate_long$Date) # re-check data classification for Date column

# Perform descriptive statistics for policyrate_long data
descriptive_statistics <- policyrate_long %>%
  group_by(country) %>% #grouping data by country
  summarise(
    rate_mean = mean(rate_value, na.rm = TRUE), # policy rate mean
    min_rate  = min(rate_value, na.rm = TRUE),  # minimum value of policy rate
    max_rate  = max(rate_value, na.rm = TRUE),  # maximum value of policy rate
    sd_rate   = sd(rate_value, na.rm = TRUE)    # standard deviation of policy rate
  )

descriptive_statistics # view table

# Extract the most recent data for bullet point
rate_most_recent <- policyrate_long %>%
  filter(Date == "2025-07-31")

rate_most_recent # view all most recent data

# create colors vector for each country
colors <- c(
  "#E3C39D", # China
  "#FF1A66", # European Union
  "#E6D9CE", # India
  "#E22227", # Indonesia
  "#1AECFF", # Japan
  "#C6D4EC", # Thailand
  "#F4FEFE", # United Kingdom
  "#1A8CFF"  # United States
)

# Perform data visualization process
polrate_vis <- ggplot(
  data = policyrate_long,
  mapping = aes(
    x = month_date,
    y = rate_value,
    color = country
  ) # Mapping visualization of interest rate
) + 
  geom_line(linewidth = 1.5) + # Perform line chart visualization
  geom_segment( # Creating zero line
    aes(
      x = min(policyrate_long$month_date),
      xend = max(policyrate_long$month_date),
      y = 0, yend = 0
    ),
    color = "#B3C3D1",
    linetype = "solid",
    linewidth = 0.25,
    alpha = 0.80
  ) +
  geom_segment( # creating x axis line without breaking through maximum date of data
    aes(
      x = min(policyrate_long$month_date),
      xend = max(policyrate_long$month_date),
      y = -0.25,
      yend = -0.25
    ),
    color = "#FFFFFF",
    linewidth = 0.30
  ) +
  geom_point(aes(color = country),
             data = rate_most_recent,
            size = 3.5, shape = 19) + # Generating point for the most recent data
  geom_text_repel( # Generating labels for showing countries
    data = rate_most_recent,
    aes(color = country, label = country),
    family = "Helvetica",
    fontface = "bold",
    direction = "y",
    size = 5,
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
  labs( # Configuring visualization's main information
    title = "Are We Entering The End of Higher-for-Longer Era?",
    subtitle = "This figure reports Central Banks interest rate for several countries, encompassing Jan-2023 until Jul-2025 period of time.<br> At the start of 2025, some Central Banks have taken <span style = 'color:#FFEBD9;'><b>Monetary Easing Stance by reducing interest rate, including Indonesia.</b></span></br>",
    caption = "<b>Data Source:</b> Bank Indonesia | Trading Economics (as of 20th July, 2025)<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
    x = "",
    y = "<b>Rate (%)</b>"
  ) +
  scale_color_manual(values = colors) + # Applying manual color configuration
  scale_y_continuous( # Adjusting y axis scale
    expand = c(0, 0),
    limits = c(-0.25, 7.75),
    breaks = seq(-0.25, 7.75, by = 2)
  ) +
  coord_cartesian( # Setting addition y axis values for more robust representation
    clip = "off",
    ylim = c(-0.25, 7.75)
  ) +
scale_x_yearmon( # Employing zoo packages' function to create monthly data
    expand = expansion(mult = c(0, 0.05)),
    limits = c(as.yearmon("2023-01-01"), as.yearmon("2025-12-01")),
    breaks = seq(
      from = min(policyrate_long$month_date),
      to = max(policyrate_long$month_date),
      by = 0.25
    ),
    format = "%b-%y"
  ) +
  theme( # Configuring visualization's theme inspired by Financial Times dark background chart.
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#212631"),
    plot.background = element_rect(fill = "#212631"),
    plot.title = element_text(family = "Helvetica", face = "bold", 
                 color = "#FFEBD9", size = 18),
    plot.subtitle = ggtext::element_markdown(family = "Arial", color = "#FFFFFF", 
                    size = 12),
    plot.caption = ggtext::element_markdown(family = "Arial", color = "#FFFFFF", 
                   size = 10),
    plot.caption.position = "plot",
    axis.text.x = element_text(family = "Arial", color = "#FFFFFF", size = 10),
    axis.text.y = element_text(family = "Arial", color = "#FFFFFF", size = 10),
    axis.title.y = ggtext::element_markdown(family = "Arial", color = "#FFFFFF", size = 10),
    axis.line.y = element_line(color = "#FFFFFF", linewidth = 0.30),
    axis.line.x = element_blank(),
    axis.ticks.length = unit(0.20, "cm"),
    axis.ticks = element_line(color = "#FFFFFF"),
    legend.position = "none"                                        
  )

# View Policy Rate Data Visualization
polrate_vis

# Save Data Visualization into JPEG Format
ggsave(
  "Central Banks Policy Rate Trend for Several Countries.jpeg",
  plot = polrate_vis,
  width = 1700,
  height = 1080,
  units = "px",
  dpi = 150
)
