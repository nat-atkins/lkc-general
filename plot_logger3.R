# logger 3 data viz
# burn occurred 11/9/25



library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read CSV — skip metadata row so row 2 is header
df <- read.csv("logger_three.csv", skip = 1, header = TRUE, check.names = FALSE)

# Rename first two columns
colnames(df)[1:2] <- c("Index", "DateTime")

# Parse DateTime - USE mdy_hms() because the format includes seconds
df$DateTime <- mdy_hms(df$DateTime)

# Find the four temperature columns
sensor_cols <- c(
  A = grep("LBL: *A", names(df), value = TRUE),
  B = grep("LBL: *B", names(df), value = TRUE),
  C = grep("LBL: *C", names(df), value = TRUE),
  D = grep("LBL: *D", names(df), value = TRUE)
)

# Check all four found
if (any(sensor_cols == "")) {
  stop("One or more sensor columns (A–D) were not found. 
        Check the CSV column names and the LBL pattern.")
}

# Rename the columns to A, B, C, D
for (letter in names(sensor_cols)) {
  names(df)[names(df) == sensor_cols[[letter]]] <- letter
}

#plot for the entire duration of time series

# Pivot A–D to long format (using the unfiltered data)
df_long_full <- df %>%
  pivot_longer(
    cols = c(A, B, C, D),
    names_to = "Sensor",
    values_to = "Temperature"
  )

# Plot all four sensors across the entire dataset
ggplot(df_long_full, aes(x = DateTime, y = Temperature, color = Sensor)) +
  geom_line(size = 1) +
  labs(
    title = "Burn Temperature - Full Time Series (Logger 3)",
    x = "Date/Time",
    y = "Temperature (°F)",
    color = "Sensor"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
ggsave("full_temp_plots_logger3.png", width = 10, height = 6, dpi = 300)

# Time filter -- set based on looking at where burn starts
start_time <- ymd_hms("2025-11-09 9:30:00")
end_time   <- ymd_hms("2025-11-09 10:30:00")

df_filtered <- df %>% filter(DateTime >= start_time & DateTime <= end_time)

# Pivot A–D to long format
df_long <- df_filtered %>%
  pivot_longer(
    cols = c(A, B, C, D),
    names_to = "Sensor",
    values_to = "Temperature"
  )

# Plot all four sensors -- during just the time of burn
ggplot(df_long, aes(x = DateTime, y = Temperature, color = Sensor)) +
  geom_line(size = 1) +
  labs(
    title = "Burn Temperature (Logger 3)",
    x = "Date/Time",
    y = "Temperature (°F)",
    color = "Sensor"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

ggsave("temperature_plot_logger3.png", width = 10, height = 6, dpi = 300)
#ggsave("temperature_plot_logger3.pdf", width = 10, height = 6)

#### separating plots

# Define the color scheme to match the combined plot
sensor_colors <- scales::hue_pal()(4)
names(sensor_colors) <- c("A", "B", "C", "D")

# Create individual plots for each sensor
plot_A <- df_long %>%
  filter(Sensor == "A") %>%
  ggplot(aes(x = DateTime, y = Temperature)) +
  geom_line(size = 1, color = sensor_colors["A"]) +
  labs(
    title = "Burn Temperature - Sensor A (Logger 3)",
    x = "Date/Time",
    y = "Temperature (°F)"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

plot_B <- df_long %>%
  filter(Sensor == "B") %>%
  ggplot(aes(x = DateTime, y = Temperature)) +
  geom_line(size = 1, color = sensor_colors["B"]) +
  labs(
    title = "Burn Temperature - Sensor B (Logger 3)",
    x = "Date/Time",
    y = "Temperature (°F)"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

plot_C <- df_long %>%
  filter(Sensor == "C") %>%
  ggplot(aes(x = DateTime, y = Temperature)) +
  geom_line(size = 1, color = sensor_colors["C"]) +
  labs(
    title = "Burn Temperature - Sensor C (Logger 3)",
    x = "Date/Time",
    y = "Temperature (°F)"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

plot_D <- df_long %>%
  filter(Sensor == "D") %>%
  ggplot(aes(x = DateTime, y = Temperature)) +
  geom_line(size = 1, color = sensor_colors["D"]) +
  labs(
    title = "Burn Temperature - Sensor D (Logger 3)",
    x = "Date/Time",
    y = "Temperature (°F)"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Display all plots (they will appear one after another)
print(plot_A)
print(plot_B)
print(plot_C)
print(plot_D)

# Use patchwork to stitch plots together
library(patchwork)
(plot_A | plot_B) / (plot_C | plot_D)
ggsave("separate_temp_plots_logger3.png", width = 10, height = 6, dpi = 300)