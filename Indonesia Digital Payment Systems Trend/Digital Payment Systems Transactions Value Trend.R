# Indonesia Digital Payment Systems' Transactions Value Trend Over Time
# This code presents how to create time series chart by utilizing R Packages
# Where it examies the trend of Indonesia's Digital Payment Systems landscape for Transactions Value

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
digital_pays # view the data

# Inspect All variables classification
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
      !is.na(lag(transactions_value, 12)) & lag(transactions_value, 12) >0,
      (transactions_value / lag(transactions_value, 12) - 1) * 100,
      NA_real_
    ),
    yoy_transactions_volume = if_else(
      !is.na(lag(transactions_volume, 12)) & lag(transactions_volume, 12) >0,
      (transactions_volume / lag(transactions_volume, 12) - 1) * 100,
      NA_real_
    )
  ) %>%
ungroup()

# Perform Additional feature creation, including:
# 1. Round the yoy growth results into 2 decimal numbers only
# 2. Transform transactions value data into Trillion Rupiah (IDR) format
# 3. Create a column in which determine each Digital Payment Systems' color.
digital_pays <- digital_pays %>%
  mutate(
    yoy_transactions_value = round(yoy_transactions_value, 2),
    yoy_transactions_volume = round(yoy_transactions_volume, 2),
    trillion_tvalue = round((transactions_value / 1000), 2),
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
        color = base_color, size = base_size + 2),
      plot.subtitle = ggtext::element_markdown(
        family = base_family, color = base_color,
        size = base_size - 2
      ),
      plot.caption = ggtext::element_markdown(
        family = base_family, color = base_color,
        size = base_size - 3
      ),
      legend.background = element_rect(fill = "#F6F6F6", color = NA),
      legend.key = element_rect(fill = "#F6F6F6", color = NA),
      legend.title = element_text(
        family = base_family, face = "bold",
        color = base_color, size = base_size -2
      ),
      legend.text = element_text(
        family = base_family, color = base_color,
        size = base_size - 2
      ),
      axis.text.x = element_text(
        family = base_family, color = base_color,
      size = base_size - 2),
      axis.text.y = element_text(
        family = base_family, color = base_color,
        size = base_size - 2,
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
## 1. Debit Cards Transactions Value
debit_value <- digital_pays %>%
  filter(system_name == "Debit Cards") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = trillion_tvalue
    )
  ) +
  geom_area(alpha = 0.6, linewidth = 0.5, 
            position = "identity",
            color = "#990F3D",
            fill = unique(digital_pays$system_color[digital_pays$system_name == "Debit Cards"])) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "Indonesia Debit Cards Transactions Value <span style = 'color:#990F3D;'><b>Continues Showing Upward Trend</b></span>",
    subtitle = "This figure reports Indonesia's Debit Cards transactions value trend over the period of Jan-2009 until Nov-2025.<br>It reflects the total value of transactions using debit cards as the media of digital payment system.</br>",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic <br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Trillion IDR</b>",
    color = "Category", fill = "Category"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 800, by = 100)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0,800)
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

debit_value

## 2. Credit Cards Transactions Value
credit_value <- digital_pays %>%
  filter(system_name == "Credit Cards") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = trillion_tvalue
    )
  ) +
  geom_area(
    alpha = 0.6, linewidth = 0.5,
    color = "#114665",
    position = "identity",
    fill = unique(digital_pays$system_color[digital_pays$system_name == "Credit Cards"])
  ) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.30
  ) +
  labs(
    title = "Credit Cards Transactions Value in Indonesia <span style = 'color:#114665;'><b>Experienced Significant Decline at The Beginning of COVID-19 Pandemic</b></span>",
    subtitle = "This figure represents Credit Cards Transactions Value for Indonesia, encompassing Jan-2009 until Nov-2025 period of time.<br>It examines the total value of transactions by employing credit cards as digital payment system.</br>",
    caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Trillion IDR</b>",
    color = "Category", fill = "Category"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 45, by = 5)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0,45)
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

credit_value

# 3. Electronic Moeny Transactions Value
emoney_value <- digital_pays %>%
  filter(system_name == "Electronic Money") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = trillion_tvalue
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
    title = "Indonesia Electronic Money Transactions Value Exhibits <span style = 'color:#9DACCC;'><b>Significant Upward Trajectory</b></span>",
    subtitle = "This figure reports Electronic Money Transactions Value for Indonesia, covering Jan-2009 until Nov-2025 time period.<br>It encompasses the total value of transactions by utilizing electronic money as digital payment system in all forms.</br>",
   caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
   x = "<b>Date</b>", y = "<b>Trillion IDR</b>",
   color = "Category", fill = "Category"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 360, by = 60)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 360)
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

emoney_value

# 4. SKNBI Clearing System
sknbi_value <- digital_pays %>%
  filter(system_name == "BI Clearing System") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = trillion_tvalue
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
    title = "Sistem Kliring Nasional Bank Indonesia (SKNBI) System Transactions Value <span style = 'color:#003E91;'><b>Performs Gradual Increase Over Time</b></span>",
    subtitle = "This figure shows SKNBI system transactions over the period of Jan-2009 until Nov-2025.<br>It represent the total value of transactions by employing Clearing system for underpinning interbank transactions.</br>",
   caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
   x = "<b>Date</b>", y = "<b>Trillion IDR</b>",
   color = "Category", fill = "Category"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 550, by = 50)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 550)
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

sknbi_value

# 5. BI-RTGS System
birtgs_value <- digital_pays %>%
  filter(system_name == "BI-RTGS System") %>%
  ggplot(
    mapping = aes(
      x = Date,
      y = trillion_tvalue
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
    title = "BI-RTGS System Transactions Value <span style = 'color:#177EE6;'><b>Shows Upward Trajectory in Long-Term</b></span>",
    subtitle = "This figure shows Bank Indonesia Real-Time Gross Settlement (BI-RTGS) system transactions value over the period of Jan-2009 until Nov-2025.<br>It represent the total value of transactions by deploying RTGS System to support the Execution of Monetary Operation.</br>",
   caption = "<b>Data Source:</b> Bank Indonesia - Payment Systems and Financial Market Infrastructure Statistic<br>LinkedIn: Muhammad Adisurya Pratama | GitHub: madisuryapr</br>",
   x = "<b>Date</b>", y = "<b>Trillion IDR</b>",
   color = "Category", fill = "Category"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 25000, by = 2500)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 25000)
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

birtgs_value

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
  plot_object = birtgs_value,
  file_name = "BI-RTGS System Transactions Value.jpeg")
