library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggrepel)

# Load the data
data <- fromJSON("houston_temp.json")$daily |> 
  as.data.frame() |> 
  mutate(
    date = as.Date(time),
    year = year(date),
    month_num = month(date),
    day = day(date),
    temp = temperature_2m_max,
    # Calculate continuous month position (e.g. 1.0 to 12.97)
    decimal_month = month_num + (day - 1) / days_in_month(date)
  ) |>
  filter(!is.na(temp))

# Define highlighting for the extremes
extremes <- data.frame(
  date = as.Date(c("2021-02-15", "2000-09-04", "2023-08-27", "2022-12-23")),
  temp = c(29.0, 107.2, 107.1, 31.0),
  label = c(
    "Winter Storm Uri\n(Feb 15, 2021: 29°F)",
    "Labor Day Heatwave\n(Sep 4, 2000: 107.2°F)",
    "Late Summer Heatwave\n(Aug 27, 2023: 107.1°F)",
    "Christmas Freeze\n(Dec 23, 2022: 31°F)"
  ),
  decimal_month = c(
    2 + (15 - 1) / 28,
    9 + (4 - 1) / 30,
    8 + (27 - 1) / 31,
    12 + (23 - 1) / 31
  ),
  nudge_x = c(1.5, 1.5, -1.5, -1.5),
  nudge_y = c(-8, 6, 6, -8)
)

# Custom color palette (cool blue to warm orange/red for years)
# To show the decadal warming trend
color_scale <- scale_color_gradientn(
  colors = c("#1e3c72", "#2a5298", "#4a00e0", "#8e2de2", "#f12711", "#f5af19"),
  name = "Year"
)

# Build the plot
p <- ggplot(data, aes(x = decimal_month, y = temp)) +
  # Background scatter of daily high temps
  geom_point(aes(color = year), alpha = 0.2, size = 0.8) +
  # Add color gradient for points
  scale_color_viridis_c(option = "inferno", name = "Year", alpha = 0.3) +
  # Add trendline (GAM smooth with cyclic boundary)
  geom_smooth(aes(group = 1), method = "gam", formula = y ~ s(x, bs = "cc"),
              color = "#00d2d3", size = 0.8, se = TRUE, fill = "#00d2d3", alpha = 0.15) +
  # Highlight the extreme points
  geom_point(data = extremes, aes(x = decimal_month, y = temp), 
             color = "#ff4757", size = 3, shape = 21, stroke = 1.5, fill = "white") +
  # Annotate the extreme points
  geom_label_repel(data = extremes, aes(x = decimal_month, y = temp, label = label),
            size = 3.2, fontface = "bold", color = "#2c3e50", fill = "white",
            label.padding = 0.25, label.r = unit(0.15, "lines"),
            max.overlaps = Inf, box.padding = 0.6, point.padding = 0.5,
            segment.colour = "grey50", segment.size = 0.5) +
  # Format x-axis with Month names
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    limits = c(1, 13)
  ) +
  # Format y-axis
  scale_y_continuous(
    breaks = seq(20, 110, 10),
    labels = paste0(seq(20, 110, 10), "°F")
  ) +
  # Labels
  labs(
    title = "Houston Daily High Temperatures (1996 - 2025)",
    subtitle = "Highlighting Seasonal Cycles, Winter Variability, and Historic Extremes",
    x = "Month of the Year",
    y = "Daily High Temperature",
    caption = "Data Source: Open-Meteo Historical Weather API\nPlot shows 30 years of daily max temperature observations (~11,000 points)."
  ) +
  # Theme
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 11, color = "#34495e"),
    axis.text = element_text(size = 9.5, color = "#2c3e50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ecf0f1", size = 0.5),
    panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5),
    plot.caption = element_text(size = 7.5, color = "#bdc3c7", hjust = 0, margin = margin(t = 15))
  )

# Add visual annotations for the variability differences
p <- p + 
  annotate("text", x = 1.5, y = 100, label = "Winter Highs:\nWide Spread (SD ~9.8°F)\nRange: 32°F to 84°F", 
           size = 3, color = "#2980b9", fontface = "italic", hjust = 0) +
  annotate("segment", x = 1.5, xend = 1.5, y = 35, yend = 80, 
           color = "#2980b9", size = 0.5, arrow = arrow(length = unit(0.15, "cm"), ends = "both")) +
  annotate("text", x = 7.2, y = 60, label = "Summer Highs:\nNarrow Spread (SD ~4.4°F)\nConsistent Subtropical Ridge", 
           size = 3, color = "#d35400", fontface = "italic", hjust = 0) +
  annotate("segment", x = 7.5, xend = 7.5, y = 77, yend = 102, 
           color = "#d35400", size = 0.5, arrow = arrow(length = unit(0.15, "cm"), ends = "both"))

# Save the plot
ggsave("houston_temp_plot.png", plot = p, width = 10, height = 6.5, dpi = 300)
