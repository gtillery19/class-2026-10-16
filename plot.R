library(ggplot2)

weather <- read.csv(
  "sarasota_hourly_weather.csv",
  colClasses = c("character", rep("numeric", 4))
)
midnight <- grepl("^\\d{4}-\\d{2}-\\d{2}$", weather$time)
weather$time[midnight] <- paste(weather$time[midnight], "00:00:00")
weather$time <- as.POSIXct(
  weather$time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "America/New_York"
)
weather$hour <- as.numeric(
  difftime(weather$time, min(weather$time), units = "hours")
)

temperature_extremes <- weather[
  c(which.min(weather$temperature_2m), which.max(weather$temperature_2m)),
]
temperature_extremes$label <- c("LOW", "HIGH")

humidity_extremes <- weather[
  c(which.min(weather$relative_humidity_2m),
    which.max(weather$relative_humidity_2m)),
]
humidity_extremes$label <- c("LOW", "HIGH")

temperature_plot <- ggplot(weather, aes(x = hour, y = temperature_2m)) +
  geom_area(fill = "#FF5A1F", alpha = 0.18) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.15,
    se = FALSE,
    color = "#FF7518",
    linewidth = 1.3
  ) +
  geom_point(
    data = temperature_extremes,
    color = "#B6F23A",
    size = 3
  ) +
  geom_label(
    data = temperature_extremes,
    aes(label = paste0(label, "  ", round(temperature_2m, 1), "\u00b0C")),
    color = "#171322",
    fill = "#B6F23A",
    fontface = "bold",
    linewidth = 0,
    nudge_y = 1.5
  ) +
  scale_x_continuous(
    breaks = seq(0, max(weather$hour), by = 168),
    labels = \(hour) format(
      min(weather$time) + hour * 3600,
      "%b %d",
      tz = "America/New_York"
    )
  ) +
  labs(
    title = "SARASOTA",
    subtitle = "THREE MONTHS IN WEATHER  /  HOURLY TEMPERATURE",
    x = NULL,
    y = "Temperature (\u00b0C)",
    caption = "Source: Open-Meteo  \u2022  Local time"
  )

humidity_plot <- ggplot(
  weather,
  aes(x = hour, y = relative_humidity_2m)
) +
  geom_area(fill = "#8E44AD", alpha = 0.18) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.15,
    se = FALSE,
    color = "#C77DFF",
    linewidth = 1.3
  ) +
  geom_point(
    data = humidity_extremes,
    color = "#B6F23A",
    size = 3
  ) +
  geom_label(
    data = humidity_extremes,
    aes(label = paste0(label, "  ", round(relative_humidity_2m), "%")),
    color = "#171322",
    fill = "#B6F23A",
    fontface = "bold",
    linewidth = 0,
    nudge_y = 3
  ) +
  scale_x_continuous(
    breaks = seq(0, max(weather$hour), by = 168),
    labels = \(hour) format(
      min(weather$time) + hour * 3600,
      "%b %d",
      tz = "America/New_York"
    )
  ) +
  labs(
    title = "SARASOTA",
    subtitle = "THREE MONTHS IN WEATHER  /  HOURLY HUMIDITY",
    x = NULL,
    y = "Relative humidity (%)",
    caption = "Source: Open-Meteo  \u2022  Local time"
  )

precipitation_plot <- ggplot(
  weather,
  aes(x = hour, y = precipitation)
) +
  geom_line(color = "#B6F23A", linewidth = 0.45, alpha = 0.7) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.15,
    se = FALSE,
    color = "#FF7518",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(0, max(weather$hour), by = 168),
    labels = \(hour) format(
      min(weather$time) + hour * 3600,
      "%b %d",
      tz = "America/New_York"
    )
  ) +
  labs(
    title = "SARASOTA",
    subtitle = "THREE MONTHS IN WEATHER  /  HOURLY PRECIPITATION",
    x = NULL,
    y = "Precipitation (mm)",
    caption = "Source: Open-Meteo  \u2022  Local time"
  )

wind_plot <- ggplot(weather, aes(x = hour, y = wind_speed_10m)) +
  geom_line(color = "#C77DFF", linewidth = 0.45, alpha = 0.7) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.15,
    se = FALSE,
    color = "#B6F23A",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(0, max(weather$hour), by = 168),
    labels = \(hour) format(
      min(weather$time) + hour * 3600,
      "%b %d",
      tz = "America/New_York"
    )
  ) +
  labs(
    title = "SARASOTA",
    subtitle = "THREE MONTHS IN WEATHER  /  HOURLY WIND SPEED",
    x = NULL,
    y = "Wind speed (km/h)",
    caption = "Source: Open-Meteo  \u2022  Local time"
  )

weather_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#171322", color = NA),
    panel.background = element_rect(fill = "#171322", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#382B46", linewidth = 0.4),
    plot.title = element_text(
      color = "#FFF0D7",
      face = "bold",
      size = 28,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(color = "#FF7518", size = 11),
    axis.text = element_text(color = "#E5D5F3"),
    axis.title.y = element_text(color = "#E5D5F3", margin = margin(r = 10)),
    plot.caption = element_text(color = "#A895B8"),
    plot.margin = margin(24, 28, 18, 20)
  )

ggsave(
  "sarasota-weather2.png",
  plot = patchwork::wrap_plots(
    temperature_plot + weather_theme,
    humidity_plot + weather_theme,
    precipitation_plot + weather_theme,
    wind_plot + weather_theme,
    ncol = 1,
    heights = c(1, 1, 1, 1)
  ),
  width = 11,
  height = 24,
  dpi = 300,
  bg = "#171322"
)
