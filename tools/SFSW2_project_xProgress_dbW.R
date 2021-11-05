
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

tmp <- RSQLite::dbGetQuery(
  con,
  "SELECT COUNT(*) FROM WeatherData WHERE Scenario > 1"
)
cat("Projected weather objects, n =", as.integer(tmp), fill = TRUE)


tmp <- RSQLite::dbGetQuery(
  con,
  "SELECT COUNT(DISTINCT Site_Id) FROM WeatherData WHERE Scenario > 1"
)
cat("Sites with projected objects, n =", as.integer(tmp), fill = TRUE)

RSQLite::dbDisconnect(con)
