### Uses codes close but not identical to ISO 3166-1 alpha-2 country codes
### UK instead of GB, EL instead of GR

eurostat.get.dsd <- function(flowref)
  readSDMX(paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_", flowref))

eurostat.query <- function(flowref, keys, startPeriod, endPeriod)
  {
    if (is.null(keys[["FREQ"]]))
      stop("No frequency set")
    
    if (keys$FREQ == "A")
      {
        freq <- 1
        sp <- startPeriod
        ep <- endPeriod
      }
    else if (keys$FREQ == "Q")
      {
        freq <- 4
        sp <- sprintf("%d-Q%d", startPeriod[1], startPeriod[2])
        ep <- sprintf("%d-Q%d", endPeriod[1], endPeriod[2])
      }
    else if (keys$FREQ == "M")
      {
        freq <- 12
        sp <- sprintf("%d-%d-01", startPeriod[1], startPeriod[2])
        ep <- sprintf("%d-%d-01", endPeriod[1], endPeriod[2])
      }
    else
      stop("Unknown frequency: ", keys$FREQ)

    ## Construct query, taking into account ANDs (.) and ORs (+)
    key <- paste(lapply(keys, function(y) paste(y, collapse="+")), collapse = ".")
    url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/", flowref,
                  "/", key, "?startPeriod=", sp, "&endPeriod=", ep)
    d <- as.data.frame(readSDMX(url), stringsAsFactors = FALSE)

    ## Extract series for each combination of keys
    cartesian.product <- expand.grid(keys, stringsAsFactors = FALSE)
    result <- list()
    for (i in 1:nrow(cartesian.product))
      {
        idx <- rep(TRUE, nrow(d))
        for (j in names(cartesian.product))
          idx <- idx & (d[[j]] == cartesian.product[[j]][i])

        obsTime <- d$obsTime[idx]
        stopifnot(obsTime[1] == ep && obsTime[length(obsTime)] == sp)
        result[[paste(cartesian.product[i,], collapse = ".")]] <- rev(d$obsValue[idx])
      }

    ts(as.data.frame(result), start = startPeriod, end = endPeriod, frequency = freq)
  }

growth <- function(countries, startPeriod, endPeriod, freq = 1)
  {
    eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "CLV_PCH_PRE",
                                       NA_ITEM = "B1GQ", GEO = countries),
                   startPeriod, endPeriod)
  }

unemployment <- function(countries, startPeriod, endPeriod, freq = 1)
  {
    eurostat.query("une_rt_a", list(FREQ = "A", S_ADJ = "NSA",
                                    AGE = "TOTAL", SEX = "T", GEO = countries),
                   startPeriod, endPeriod)
  }


inflation <- function(countries, startPeriod, endPeriod, freq = 1)
  {
    eurostat.query("prc_hicp_aind", list(FREQ = "A", UNIT = "RCH_A_AVG",
                                         COICOP = "CP00", GEO = countries),
                   startPeriod, endPeriod)
  }


