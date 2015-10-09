# Uses ISO 3166-1 alpha-3 country codes

oecd.countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

oecd.get.dsd <- function(key)
  rsdmx::readSDMX(agencyId = "OECD", operation = "GetDataStructure", key = key)

oecd.query <- function(key, filter, startPeriod, endPeriod, frequency, simplify.names = TRUE)
  sdmx.query(agencyId = "OECD", operation = "GetData", key = key, filter = filter,
             startPeriod, endPeriod, frequency, simplify.names = simplify.names)

oecd.output.gap <- function(countries, startPeriod, endPeriod, frequency = 1)
  oecd.query("EO", list(LOCATION = countries, VARIABLE = "GAP", FREQUENCY = "A"),
             startPeriod, endPeriod, frequency)

oecd.foreign.born.population.rate <- function(countries, startPeriod, endPeriod)
  oecd.query("MIG", list(CO2="TOTP", VAR="B14", GEN="TOT", COU=countries), startPeriod, endPeriod, 1)

oecd.foreign.population.rate <- function(countries, startPeriod, endPeriod)
  oecd.query("MIG", list(CO2="TOTP", VAR="B15", GEN="TOT", COU=countries), startPeriod, endPeriod, 1)

oecd.unemployment <- function(countries, startPeriod, endPeriod, frequency = 1)
  oecd.query("ALFS_POP_LABOUR", list(LOCATION=countries, SUBJECT="YT99UNPT_ST", SEX="TT", FREQUENCY="A"),
             startPeriod, endPeriod, frequency)
