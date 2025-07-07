# Top 10 Indonesian Banks based on Assets
# In this code, I demonstrate my coding skill with R programming to visualize
# 10 Indonesia banks with the highest total assets, alongside their corresponding yoy growth
# total assets and yoy growth are in Trillion IDR and percentage points, respectively

# Load library
library(ggplot2) # data visualization packages
library(writexl) # convert dataframe into excel format
library(ggimage) # insert image to ggplot chart

# create data
bank_id <- c("BMRI", "BBRI", "BBCA", "BBNI", "BBTN",
             "BRIS", "BNGA", "NISP", "BNLI", "BDMN")
bank_name <- c("Bank Mandiri", "Bank Rakyat Indonesia", "Bank Central Asia",
               "Bank Negara Indonesia", "Bank Tabungan Negara", "Bank Syariah Indonesia",
               "Bank CIMB Niaga", "Bank OCBC NISP", "Bank Permata", "Bank Danamon")
total_assets <- c(2463.66, 2098.23, 1533.76, 1146.58, 469.61,
                  400.88, 370.99, 293.10, 264.28, 250.80)
yoy_growth <- c(13.8, 5.4, 6.2, 7.4, 3.4, 12.0, 11.4, 16.1, 4.5, 10.7)
image_dir <- c("Bank_Logo/Bank Mandiri.png", "Bank_Logo/Bank Rakyat Indonesia.png",
               "Bank_Logo/Bank Central Asia.png", "Bank_Logo/Bank Negara Indonesia.png",
               "Bank_Logo/Bank Tabungan Negara.png", "Bank_Logo/Bank Syariah Indonesia.png",
               "Bank_Logo/Bank CIMB Niaga.png", "Bank_Logo/Bank OCBC NISP.png", "Bank_Logo/Bank Permata.png",
               "Bank_Logo/Bank Danamon.png")

# Convert total assets vector into integer
total_assets <- as.integer(total_assets)

# create data frame
banks_assets <- data.frame(bank_id, bank_name, total_assets, yoy_growth, image_dir)
banks_assets # view dataframe

# write dataframe into excel format
write_xlsx(x = banks_assets, path = "Data/Top 10 Indonesian Banks Total Assets.xlsx", col_names = TRUE)

# inspect data classification for each column
class(banks_assets$bank_id)
class(banks_assets$bank_name)
class(banks_assets$total_assets)
class(banks_assets$yoy_growth)
class(banks_assets$image_dir)

# Perform data visualization process
bank_vis <- ggplot(data = banks_assets,
                   mapping = aes(x = total_assets,
                                 y = reorder(bank_name, total_assets))
) +
  geom_col(fill = "#0D4680") +
  geom_image(aes(
    x = total_assets + 180, image = image_dir), 
    size = 0.11, asp = 1.6
  ) +
  geom_text(
    aes(label = paste0(total_assets, " | " , yoy_growth, "%")),
    hjust = 1,
    colour = "#FFFFFF",
    family = "Arial",
    fontface = "bold",
    size = 3,
    position = position_dodge(0.9)
) +
  labs(
    title = "Two State-Owned Banks Become Top 3 Banks with The Largest Total Assets in Indonesia",
    subtitle = "Top 10 Banks based on total assets for Q1-2025 (growth in percent year-over-year)",
    x = "Total Assets (Trillion IDR)",
    y = "",
    caption = "Data Source: CNBC Indonesia\nLinkedIn: Muhammad Adisurya Pratama"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limit = c(0, 3000),
    breaks = seq(0, 3000, by = 500)
  ) +
  theme(
    plot.background = element_rect(fill = "#FFF1E5"),
    panel.background = element_rect(fill = "#FFF1E5"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#000000", linewidth = 0.5, 
                          linetype = 3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "Georgia", face = "bold",
                              color = "#6C0102", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "Arial", color = "#000000",
                                 size = 11),
    plot.caption = element_text(family = "Arial", color = "#000000",
                                size = 10, hjust = 0),
    plot.caption.position = "plot",
    axis.text.x = element_text(family = "Arial", color = "#000000",
                               size = 10, hjust = 0.5),
    axis.text.y = element_text(family = "Arial", color = "#000000",
                               size = 10),
    axis.ticks.length = unit(0.20, "cm"),
    axis.line.y = element_line(color = "#000000", linewidth = 0.5),
    axis.ticks = element_line(color = "#000000")
  )

# View visualized data
bank_vis

# Save visualized data into repository
ggsave(
  filename = "Top 10 Indonesian Banks Q1-2025 from Total Assets.jpeg",
  plot = bank_vis,
  width = 1700,
  height = 1080,
  units = "px",
  dpi = 150
)
