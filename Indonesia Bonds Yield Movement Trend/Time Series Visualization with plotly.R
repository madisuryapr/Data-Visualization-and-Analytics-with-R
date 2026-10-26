## INDONESIA BONDS YIELD MOVEMENT TREND
# This code presents how to create time series chart
# by employing high-frequency daily data of Indonesia's Government Bonds Yield

# Load Library
library(ggplot2)
library(ggtext)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(readxl)
library(systemfonts)

# Assign folder path
folder_path = "Clean Data"

# Get A list of all datasets
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$",
full.names = TRUE)

# read all datasets
for (file in file_list) {
  var_name <- tools::file_path_sans_ext(basename(file))
  assign(
    var_name,
    read_excel(file, sheet = "Sheet1", na = "N/A")
  )
}

# re-assign all dataframe names for convenience
id_bonds1yr <- `Bonds Yield 1-Year`
id_bonds5yr <- `Bonds Yield 5-Year`
id_bonds10yr <- `Bonds Yield 10-Year`
id_bonds30yr <- `Bonds Yield 30-Year`

# remove all prior data
rm(`Bonds Yield 1-Year`, `Bonds Yield 5-Year`,
    `Bonds Yield 10-Year`, `Bonds Yield 30-Year`)

# Check data structure
str(id_bonds1yr)
str(id_bonds5yr)
str(id_bonds10yr)
str(id_bonds30yr)

# Convert the POSIXct Date format into as.Date
dataframes <- c(
  "id_bonds1yr", "id_bonds5yr",
  "id_bonds10yr", "id_bonds30yr"
)

for(df in dataframes) {
  data <- get(df)
  
  data <- data %>%
    mutate(
      day_date = as.Date(day_date)
    )
  assign(df, data)
  message(
    "Successfully processed: ", df
  )
}

# Re-check all dataframe structures
str(id_bonds1yr)
str(id_bonds5yr)
str(id_bonds10yr)
str(id_bonds30yr)

# Create a function to assign visualization theme
visual_theme <- function(
  base_size = 12,
  base_family = "Arial",
  base_color = "#333333"
) {
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F6F6F6"),
    plot.background = element_rect(fill = "#F6F6F6"),
    plot.title = element_markdown(
      family = "Helvetica", face = "bold",
      color = base_color, size = base_size + 2
    ),
    plot.subtitle = element_markdown(
      family = base_family, color = base_color,
      size = base_size - 3
    ),
    plot.caption = element_markdown(
      family = base_family, color = base_color,
      size = base_size - 5
    ),
    legend.background = element_rect(fill = "#F6F6F6", color = NA),
    legend.key = element_rect(fill = "#F6F6F6", color = NA),
    legend.title = element_text(
      family = base_family, face = "bold",
      color = base_color, size = base_size - 3
    ),
    legend.text = element_text(
      family = base_family, color = base_color,
      size = base_size - 2
    ),
    axis.text.x = element_text(
      family = base_family, color = base_color,
      size = base_size - 5, angle = 90
    ),
    axis.text.y = element_text(
      family = base_family, color = base_color,
      size = base_size - 5
    ),
    axis.title.y = element_markdown(
      family = base_family, color = base_color,
      size = base_size - 3, angle = 90
    ),
    axis.title.x = element_markdown(
      family = base_family, color = base_color,
      size = base_size - 3
    ),
    axis.line.y = element_line(color = base_color, linewidth = 0.30),
    axis.line.x = element_line(color = base_color, linewidth = 0.30),
    axis.ticks.length = unit(0.20, "cm"),
    axis.ticks = element_line(color = base_color)
  )
}

## Perform Data Visualization Process
# Step 1: Create Underlying Time Series Visualization
vis_bonds1yr <- id_bonds1yr %>%
 ggplot(
    mapping = aes(
      x = day_date,
      y = yield,
      text = paste0(
        "<b>Date:</b> ", format(day_date, "%d %b %Y"), "<br>",
        "<b>Yield:</b> ", sprintf("%.2f%%", yield)
      )
    )
  ) +
  geom_line(
    aes(
      group = 1
    ),
    color = "#4E607C",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = 0, color = "#2C2F38",
    linewidth = 0.5, alpha = 0.75
  ) +
  labs(
    title    = "Indonesian Government 1-Year Bonds Yield <span style = 'color:#990F3D;'><b>Surges Amidst Q2-2026 Global Uncertainty</b></span>",
    subtitle = "Indonesia's Government 1-Year Bonds Yield trend over the period of January 2020 until September 2026.",
    caption  = "<b>Data Source:</b> Investing.com <br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Percent (%)</b>"  
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(2, 9, by = 1)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(2, 9)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.02)),
    limits = as.Date(
      c(
        min(id_bonds1yr$day_date), 
        as.Date("2026-09-30")
      )
    ),
    breaks = unique(
      c(
        seq.Date(
          from = as.Date("2020-01-02"),
          to = as.Date("2026-09-30"),
          by = "3 months"
        ),
        as.Date("2026-09-30")
      )
    ),
    date_labels = "%b-%y"
  ) +
  visual_theme()

vis_bonds1yr

# Step 2: Make above chart interactive with ggplotly() function
vis_bonds1yr_interactive <- ggplotly(
  vis_bonds1yr,
  tooltip = "text"
) %>%
  layout(
    # Title and subtitle
    title = list(
      text = paste0(
        "<b>Indonesian Government 1-Year Bonds Yield</b> ",
        "<span style='color:#990F3D;'><b>",
        "Surges Amidst Q2-2026 Global Uncertainty",
        "</b></span>",
        "<br><span style='font-size:12px; color:#333333;'>",
        "Indonesia's Government 1-Year Bonds Yield trend ",
        "over the period of January 2020 until September 2026.",
        "</span>"
      ),
      x = 0,
      xanchor = "left"
    ),
    # Interactive date axis and range slider
    xaxis = list(
      rangeslider = list(
        visible = TRUE,
        thickness = 0.10
      )
    ),
    hovermode = "x unified", # Unified hover information
    # Modifying the Hover label of ggplotly chart
    hoverlabel = list(
      font = list(
        family = "Helvetica, Arial, sans-serif",
        size = 12,
        color = "#333333"
      ),
      backgroundcolor = "#FFFFFF",
      bordercolor = "#C0C0C0"
    ),
    annotations = list(
      list(
        xref = "paper",
        yref = "paper",
        x = 0,
        y = -0.32,
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        align = "left",
        text = paste0(
          "<b>Data Source:</b> Investing.com",
          "<br><b>LinkedIn:</b> Muhammad Adisurya Pratama",
          " | <b>GitHub:</b> madisuryapr"
        ),
        font = list(
          family = "Arial",
          size = 11,
          color = "#333333"
        )
      )
    ),
    # Leave room for the caption beneath the chart
    margin = list(
      l = 65,
      r = 30,
      t = 110,
      b = 130
    )
  )

# View Interactive chart
vis_bonds1yr_interactive

# Create Save function for underlying visualization of interactive chart
save_plot <- function(
  plot_object, file_name,
  width_px = 1920, height_px = 1200,
  plot_unit = "px", dpi = 175
) {
  ggsave(
    filename = file_name,
    plot_object,
    width = width_px,
    height = height_px,
    units = plot_unit,
    dpi = dpi
  )

  message(
      "Successfully saved the Plot as: ",
      file_name
  )
}

# Create Save function for interactive plotly chart
save_widget <- function(
  widget_object, file_name
) {
  saveWidget(
    widget = widget_object,
    file = file_name,
    selfcontained = TRUE
  )

  message(
    "Successfully saved the Widget as: ",
    file_name
  )
  
}

# Save the result of ggplot2 visualization to test the effectiveness of function
save_plot(
  plot_object = vis_bonds1yr,
  file_name = "Result_ggplot2/Indonesia 1-Year Government Bonds Yield.jpeg"
)

# Save the result of ggplotly visualization to test the effectiveness of function
save_widget(
  widget_object = vis_bonds1yr_interactive,
  file_name = "Result_plotly/Indonesia 1-Year Government Bonds Yield.html"
)


## Continue the process with other bonds yield datasets
# Indonesian Government 5-Year Bonds Yield
vis_bonds5yr <- id_bonds5yr %>%
  ggplot(
    mapping = aes(
      x = day_date,
      y = yield,
      text = paste0(
        "<b>Date:</b> ", format(day_date, "%d %b %Y"), "<br>",
        "<b>Yield:</b> ", sprintf("%.2f%%", yield)
      )
    )
  ) +
  geom_line(
    aes(
      group = 1
    ),
    color = "#720F32",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = 0, color = "#2C2F38",
    linewidth = 0.5, alpha = 0.75
  ) +
  labs(
    title    = "Indonesian Government 5-Year Bonds Yield <span style = 'color:#990F3D;'><b>Promotes Increasing Trend Since The Beginning of Q4-25</b></span>",
    subtitle = "Indonesia's government 5-Year Bonds Yield trend over the period of January 2020 until September 2026.",
    caption  = "<b>Data Source:</b> Investing.com <br><b>LinkedIn:</b> Muhammad Adisurya Pratama | <b>GitHub:</b> madisuryapr</br>",
    x = "<b>Date</b>", y = "<b>Percent (%)</b>"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(4, 9, by = 1)
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(4, 9)
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.02)),
    limits = as.Date(
      c(
      min(id_bonds5yr$day_date),
      as.Date("2026-09-30")
      )
    ),
    breaks = unique(
      c(
        seq.Date(
          from = as.Date("2020-01-02"),
          to = as.Date("2026-09-30"),
          by = "3 months"
        ),
        as.Date("2026-09-30")
      )
    ),
    date_labels = "%b-%y"
  ) +
  visual_theme()

vis_bonds5yr

# Make 5-Year Bonds Yield Visualization interactive with ggplotly() function
vis_bonds5yr_interactive <- ggplotly(
  vis_bonds5yr,
  tooltip = "text"
) %>%
  layout(
    title = list(
      text = paste0(
        "<b>Indonesia Government 5-Year Bonds Yield</b> ",
        "<span style ='color:#990F3D;'><b>",
        "Promotes Increasing Trend Since The Beginning of Q4-25",
        "</b></span>",
        "<br><span style ='font-size:12px; color:#333333;'>",
        "Indonesia's Government 5-Year Bonds Yield trend ",
        "over the period of January 2020 until September 2026.",
        "</span>"
      ),
      x = 0,
      xanchor = "left"
    ),
    xaxis = list(
      rangeslider = list(
        visible = TRUE,
        thickness = 0.10
      )
    ),
    hovermode = "x unified",
    hoverlabel = list(
      font = list(
        family = "Helvetica, Arial, sans-serif",
        size = 12,
        color = "#333333"
      ),
      backgroundcolor = "#FFFFFF",
      bordercolor = "#C0C0C0"
    ),
    annotations = list(
      list(
        xref = "paper",
        yref = "paper",
        x = 0,
        y = -0.32,
        xanchor = "left",
        yanchor = "top",
        align = "left",
        text = paste0(
          "<b>Data Source:</b> Investing.com",
          "<br><b>LinkedIn:</b> Muhammad Adisurya Pratama",
          " | <b>GitHub:</b> madisuryapr"          
        ),
        font = list(
          family = "Arial",
          size = 11,
          color = "#333333"
        )
      )
    ),
    margin = list(
      l = 65,
      r = 30,
      t = 110,
      b = 130
    )
  )

# View Interactive chart
vis_bonds5yr_interactive

# Save both ggplot2 and ggplotly visualization results
save_plot(
  plot_object = vis_bonds5yr,
  file_name = "Result_ggplot2/Indonesia 5-Year Government Bonds Yield.jpeg"
)

save_widget(
  widget_object = vis_bonds5yr_interactive,
  file_name = "Result_plotly/Indonesia 5-Year Government Bonds Yield.html"
)
