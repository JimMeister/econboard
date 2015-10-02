# Uses ISO 3166-1 alpha-3 country codes

oecd.countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

oecd.get.dsd <- function(flowref)
  rsdmx::readSDMX(paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", flowref))

oecd.query <- function(flowref, keys, startPeriod, endPeriod, simplify.names = TRUE)
  {
    if (is.null(keys[["FREQUENCY"]]))
      {
        if (is.null(keys[["FREQ"]]))
          {
            ## No frequency set, guessing using dates format
            if (length(startPeriod) == 1)
              frq <- "A"
            else
              stop("Frequency not yet implemented")
          }
        else
          frq <- keys$FREQ
      }
    else
      frq <- keys$FREQUENCY
    
    if (frq == "A")
      {
        freq <- 1
        sp <- startPeriod
        ep <- endPeriod
      }
    else
      stop("Frequency not yet implemented:", frq)

    ## Construct query, taking into account ANDs (.) and ORs (+)
    key <- paste(lapply(keys, function(y) paste(y, collapse="+")), collapse = ".")
    url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/", flowref,
                  "/", key, "?startTime=", sp, "&endTime=", ep)
    d <- as.data.frame(rsdmx::readSDMX(url), stringsAsFactors = FALSE)
    
    ## Extract series for each combination of keys
    cartesian.product <- expand.grid(keys, stringsAsFactors = FALSE)
    result <- list()

    if (simplify.names && any(sapply(keys, length) > 1))
      dim.names.idx <- which(sapply(keys, length) > 1)
    else
      dim.names.idx <- 1:length(keys)
    
    for (i in 1:nrow(cartesian.product))
      {
        series <- paste(cartesian.product[i,dim.names.idx], collapse = ".")
        result[[series]] <- ts(NA, start = startPeriod, end = endPeriod)
        
        idx <- rep(TRUE, nrow(d))
        for (j in names(cartesian.product))
          idx <- idx & (d[[j]] == cartesian.product[[j]][i])

        for (j in which(idx))
          window(result[[series]], start = as.numeric(d$obsTime[j]), end = as.numeric(d$obsTime[j])) <- d$obsValue[j]
      }

    do.call("ts.union", result)
  }

oecd.output.gap <- function(countries, startPeriod, endPeriod, freq = 1)
  oecd.query("EO", list(LOCATION = countries, VARIABLE = "GAP", FREQUENCY = "A"),
             startPeriod, endPeriod)

oecd.foreign.born.population.rate <- function(countries, startPeriod, endPeriod)
  oecd.query("MIG", list(CO2="TOTP", VAR="B14", GEN="TOT", COU=countries), startPeriod, endPeriod)

oecd.foreign.population.rate <- function(countries, startPeriod, endPeriod)
  oecd.query("MIG", list(CO2="TOTP", VAR="B15", GEN="TOT", COU=countries), startPeriod, endPeriod)

oecd.unemployment <- function(countries, startPeriod, endPeriod)
  oecd.query("ALFS_POP_LABOUR", list(LOCATION=countries, SUBJECT="YT99UNPT_ST", SEX="TT", FREQUENCY="A"),
             startPeriod, endPeriod)
