
cat(
  "Progress report on adding projected data to weather database:",
  fill = TRUE
)

con <- RSQLite::dbConnect(
  RSQLite::SQLite(),
  "../0_WeatherDatabase/dbWeatherData_WesternUS-part1_gridMET-MACA_CMIP5.sqlite3",
  #"../0_WeatherDatabase/dbWeatherData_WesternUS_gridMET-MACA_CMIP5.sqlite3",
  flags = RSQLite::SQLITE_RO
)

# Count unique projected weather data objects
tmp <- RSQLite::dbGetQuery(
  con,
  paste(
    "SELECT Scenario, COUNT(DISTINCT Site_id) FROM WeatherData ",
    "WHERE Scenario > 1",
    "GROUP BY Scenario"
  )
)
cat("Unique projected weather objects, n =", sum(tmp[, 2]), fill = TRUE)

# Count total projected weather data objects
tmp <- RSQLite::dbGetQuery(
  con,
  "SELECT COUNT(*) FROM WeatherData WHERE Scenario > 1"
)
cat("Total projected weather objects, n =", as.integer(tmp), fill = TRUE)


# Count sites with any projected weather data
tmp <- RSQLite::dbGetQuery(
  con,
  "SELECT COUNT(DISTINCT Site_id) FROM WeatherData WHERE Scenario > 1"
)
cat("Sites with projected objects, n =", as.integer(tmp), fill = TRUE)

RSQLite::dbDisconnect(con)
