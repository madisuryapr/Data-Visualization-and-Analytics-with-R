# Indonesia Digital Payment Systems' Transactions Volume Trend Over Time
# This code presents how to create time series chart by utilizing R Packages
# Where it examies the trend of Indonesia's Digital Payment Systems landscape for Transactions Volume

# Load Library
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggtext)

# Assign file path
file_path <- "Data/Indonesia Digital Payment Systems.xlsx"

# Extract all sheets within file path for each digital payment system
debit_cards <- read_excel(path = file_path, sheet = "debit_cards")
credit_cards <- read_excel(path = file_path, sheet = "credit_cards")
e_money <- read_excel(path = file_path, sheet = "electro_money")
sknbi <- read_excel(path = file_path, sheet = "SKNBI")
bi_rtgs <- read_excel(path = file_path, sheet = "BI-RTGS")

# Create a name for each digital payment system dataframe
debit_cards <- debit_cards %>% mutate(system_name = "Debit Cards")
credit_cards <- credit_cards %>% mutate(system_name = "Credit Cards")
e_money <- e_money %>% mutate(system_name = "Electronic Money")
sknbi <- sknbi %>% mutate(system_name = "BI Clearing System")
bi_rtgs <- bi_rtgs %>% mutate(system_name = "BI-RTGS System")

# Append all digital payment systems data into single dataframe
digital_pays <- bind_rows(debit_cards, credit_cards, e_money, sknbi, bi_rtgs)
digital_pays # view the latest dataframe

# Inspect All varuables classification
class(digital_pays$Date)
class(digital_pays$transactions_value)
class(digital_pays$transactions_volume)
class(digital_pays$system_name)

# Convert Date from POSIXct into Date class
digital_pays <- digital_pays %>%
  mutate(Date = as.Date(Date))
class(digital_pays$Date) # re-inspect Date class

# Calculate year-over-year (yoy) growth for each digital payment system
digital_pays <- digital_pays %>%
  arrange(system_name, Date) %>%
  group_by(system_name) %>%
  mutate(
    yoy_transactions_value = if_else(
      !is.na(lag(transactions_value, 12)) & lag(transactions_value, 12) > 0,
      (transactions_value / lag(transactions_value, 12) - 1) * 100,
      NA_real_
    ),
    yoy_transactions_volume = if_else(
      !is.na(lag(transactions_volume, 12)) & lag(transactions_volume, 12) > 0,
      (transactions_volume / lag(transactions_volume, 12) - 1) * 100,
      NA_real_
    )
  ) %>%
ungroup()

# Perform Additional feature creation, including:
# 1. Round the yoy growth results into 2 decimal numbers only
# 2. Transform transactions volume data into million of transactions format
# 3. Create a column in which determine each Digital payment Systems' color.
digital_pays <- digital_pays %>%
  mutate(
    yoy_transactions_value = round(yoy_transactions_value, 2),
    yoy_transactions_volume = round(yoy_transactions_volume, 2),
    million_tvolume = round((transactions_volume / 1000), 2),
    system_color = case_when(
      system_name == "Debit Cards" ~ "#720F32",
      system_name == "Credit Cards" ~ "#114665",
      system_name == "Electronic Money" ~ "#9DACCC",
      system_name == "BI Clearing System" ~ "#003E91",
      system_name == "BI-RTGS System" ~ "#177EE6"
    )
  )

# Create a function to assign visualization theme
vis_theme <- function(base_size = 12,
                      base_family = "Arial",
                      base_color = "#333333") {
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F6F6F6"),
    plot.background = element_rect(fill = "#F6F6F6"),
    plot.title = ggtext::element_markdown(
      family = "Times New Roman", face = "bold",
      color = base_color, size = base_size + 2
    ),
    plot.subtitle = ggtext::element_markdown(
      family = base_family, color = base_color,
      size = base_size - 3
    ),
    plot.caption = ggtext::element_markdown(
      family = base_family, color = base_color,
      size = base_size - 3
    ),
    legend.background = element_rect(fill = "#F6F6F6", color = NA),
    legend.key = element_rect(fill = "#F6F6F6", color = NA),
    legend.title = element_text(
      family = base_family, face = "bold",
      color = base_color, size = base_size - 2
    ),
    legend.text = element_text(
      family = base_family, color = base_color,
      size = base_size - 2
    ),
    axis.text.x = element_text(
      family = base_family, color = base_color,
      size = base_size - 2
    ),
    axis.text.y = element_text(
      family = base_family, color = base_color,
      size = base_size - 2
    ),
    axis.title.y = ggtext::element_markdown(
      family = base_family, color = base_color,
      size = base_size - 2, angle = 90
    ),
    axis.title.x = ggtext::element_markdown(
      family = base_family, color = base_color,
      size = base_size - 2
    ),
    axis.line.y = element_line(color = base_color, linewidth = 0.30),
    axis.line.x = element_blank(),
    axis.ticks.length = unit(0.20, "cm"),
    axis.ticks = element_line(color = base_color)
  )
}

# Perform Data Visualization process
## 1. Debit Cards Transactions Volume
debit_volume <- digital_pays %>%
  filter(system_name == "Debit Cards") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = million_tvolume
    )
  ) +
  geom_area(
    alpha = 0.6, linewidth = 0.5,
    position = "identity",
    color = "#990F3D",
    fill = unique(
      digital_pays$system_color[digital_pays$system_name == "Debit Cards"]
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "Indonesia Debit Cards Transactions Volume <span style = 'color:#990F3D;'><b>Shows Upward Trend Over Time</b></span>",
    subtitle = "Debit Cards transactions volume over the period of Jan-2009 until Nov-2025.",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment System and Financial Market Infrastructure Statistic<br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Millions of Transactions</b>"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 700, by = 100)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 700)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.03)),
    limits = as.Date(c(min(digital_pays$Date), max(digital_pays$Date))),
    breaks = seq(
      from = min(digital_pays$Date),
      to = max(digital_pays$Date),
      by = "1 year"
    ),
    date_labels = "%b-%y"
  ) +
  vis_theme()

debit_volume

## 2. Credit Cards Transactions Volume
credit_volume <- digital_pays %>%
  filter(system_name == "Credit Cards") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = million_tvolume
    )
  ) +
  geom_area(
    alpha = 0.6, linewidth = 0.5,
    color = "#114665",
    position = "identity",
    fill = unique(
      digital_pays$system_color[digital_pays$system_name == "Credit Cards"]
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "Credit Cards Transactions Volume in Indonesia <span style = 'color:#114665;'><b>Declined at The Beginning of COVID-19 Pandemic</b></span>",
    subtitle = "Transactions Volume of Credit Cards from Jan-2009 to Nov-2025.",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment System and Financial Market Infrastructure Statistic<br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Millions of Transactions</b>"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 50, by = 5)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 50)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.03)),
    limits = as.Date(c(min(digital_pays$Date), max(digital_pays$Date))),
    breaks = seq(
      from = min(digital_pays$Date),
      to = max(digital_pays$Date),
      by = "1 year"
    ),
    date_labels = "%b-%y"
  ) +
  vis_theme()

credit_volume

## 3. Electronic Money Transactions Volume
emoney_volume <- digital_pays %>%
  filter(system_name == "Electronic Money") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = million_tvolume
    )
  ) +
  geom_area(
    alpha = 0.6, linewidth = 0.5,
    color = "#9DACCC",
    position = "identity",
    fill = unique(
      digital_pays$system_color[digital_pays$system_name == "Electronic Money"]
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "Electronic Money Transactions Volume in Indonesia <span style = 'color:#9DACCC;'><b>Surges Significantly After the Eruption of COVID-19 Pandemic</b></span>",
    subtitle = "Electronic Money transactions volume for Jan-2009 until Nov-2025.",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistics<br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Millions of Transactions</b>"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 3200, by = 320)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 3200)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.03)),
    limits = as.Date(
      c(min(digital_pays$Date), max(digital_pays$Date))
    ),
    breaks = seq(
      from = min(digital_pays$Date),
      to = max(digital_pays$Date),
      by = "1 year"
    ),
    date_labels = "%b-%y"
  ) +
  vis_theme()

emoney_volume

## 4. SKNBI Clearing System
sknbi_volume <- digital_pays %>%
  filter(system_name == "BI Clearing System") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = million_tvolume
    )
  ) +
  geom_area(
    alpha = 0.6, linewidth = 0.5,
    color = "#003E91",
    position = "identity",
    fill = unique(
      digital_pays$system_color[digital_pays$system_name == "BI Clearing System"]
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "Sistem Kliring Nasional Bank Indonesia (SKNBI) System Transactions Volume Performance <span style = 'color:#003E91;'><b>Remain Stable Over Time</b></span>",
    subtitle = "SKNBI Clearing System transactions volume over the period of Jan-2009 until Nov-2025.",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic<br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
   x = "<b>Date</b>", y = "<b>Millions of Transactions</b>"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 20, by = 4)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 20)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.03)),
    limits = as.Date(
      c(min(digital_pays$Date), max(digital_pays$Date))
    ),
    breaks = seq(
      from = min(digital_pays$Date),
      to = max(digital_pays$Date),
      by = "1 year"
    ),
    date_labels = "%b-%y" 
  ) +
  vis_theme()

sknbi_volume

## 5. BI-RTGS System
birtgs_volume <- digital_pays %>%
  filter(system_name == "BI-RTGS System") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = transactions_volume
    )
  ) +
  geom_area(
    alpha = 0.6, linewidth = 0.5,
    color = "#177EE6",
    position = "identity",
    fill = unique(
      digital_pays$system_color[digital_pays$system_name == "BI-RTGS System"]
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "BI-RTGS Transactions Volume <span style = 'color:#177EE6;'><b>Relatively Stable Amidst COVID-19 Pandemic</b></span>",
    subtitle = "Bank Indonesia Real-Time Gross Settlement (BI-RTGS) system transactions volume over the period of Jan-2009 until Nov-2025.",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic<br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
   x = "<b>Date</b>", y = "<b>Thousands of Transactions</b>"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1800, by = 300)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 1800)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.03)),
    limits = as.Date(c(min(digital_pays$Date), max(digital_pays$Date))),
    breaks = seq(
      from = min(digital_pays$Date),
      to = max(digital_pays$Date),
      by = "1 year"
    ),
    date_labels = "%b-%y"
  ) +
  vis_theme()

birtgs_volume

# Create a function to save each visualization
save_plot <- function(
  plot_object, file_name,
  width_px = 1600, height_px = 1020,
  plot_unit = "px", dpi = 150
) {
  ggsave(
    filename = file_name,
    plot = plot_object,
    width = width_px,
    height = height_px,
    units = plot_unit,
    dpi = dpi
  )
}

# Perform simulation for save_plot() function.
save_plot(
  plot_object = birtgs_volume,
  file_name = "BI-RTGS System Transactions Volume.jpeg"
)
