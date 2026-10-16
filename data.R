library(jsonlite)

start_date <- format(
  seq(Sys.Date(), length.out = 2, by = "-3 months")[2],
  "%Y-%m-%d"
)
recent_days <- 8
archive_end_date <- format(Sys.Date() - recent_days, "%Y-%m-%d")
hourly_variables <- paste(
  "temperature_2m",
  "relative_humidity_2m",
  "precipitation",
  "wind_speed_10m",
  sep = ","
)

parse_hourly <- \(url) {
  response <- jsonlite::fromJSON(url)

  if (isTRUE(response$error)) {
    stop(response$reason)
  }

  hourly <- as.data.frame(response$hourly)
  midnight <- grepl("^\\d{4}-\\d{2}-\\d{2}$", hourly$time)
  hourly$time[midnight] <- paste(hourly$time[midnight], "T00:00")
  hourly$time <- as.POSIXct(
    hourly$time,
    format = "%Y-%m-%dT%H:%M",
    tz = "America/New_York"
  )
  hourly
}

archive_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive",
  "?latitude=27.3364",
  "&longitude=-82.5307",
  "&hourly=", hourly_variables,
  "&start_date=", start_date,
  "&end_date=", archive_end_date,
  "&timezone=America%2FNew_York"
)

recent_url <- paste0(
  "https://api.open-meteo.com/v1/forecast",
  "?latitude=27.3364",
  "&longitude=-82.5307",
  "&hourly=", hourly_variables,
  "&past_days=", recent_days,
  "&forecast_days=0",
  "&timezone=America%2FNew_York"
)

hourly_weather <- rbind(parse_hourly(archive_url), parse_hourly(recent_url))
hourly_weather <- hourly_weather[
  hourly_weather$time >= as.POSIXct(start_date, tz = "America/New_York") &
    hourly_weather$time <= Sys.time(),
]
hourly_weather <- hourly_weather[
  !duplicated(hourly_weather$time, fromLast = TRUE),
]
hourly_weather <- hourly_weather[order(hourly_weather$time), ]

if (nrow(hourly_weather) == 0) {
  stop("No hourly weather data was returned for the requested date range.")
}

utils::write.csv(
  hourly_weather,
  "sarasota_hourly_weather.csv",
  row.names = FALSE
)

correlation <- cor(
  hourly_weather$temperature_2m,
  hourly_weather$relative_humidity_2m,
  use = "complete.obs"
)

if (!is.finite(correlation)) {
  stop("Could not calculate temperature-humidity correlation.")
}

association <- if (correlation < 0) {
  "negative (warmer hours tended to coincide with lower humidity)"
} else if (correlation > 0) {
  "positive (warmer hours tended to coincide with higher humidity)"
} else {
  "near zero (little linear association)"
}

message(
  "Saved ", nrow(hourly_weather), " hourly observations from ",
  format(min(hourly_weather$time), "%Y-%m-%d"), " to ",
  format(max(hourly_weather$time), "%Y-%m-%d"), "."
)
message(
  "Pearson correlation between temperature and humidity: r = ",
  round(correlation, 2), ", a ", association,
  ". Correlation describes association, not causation."
)
