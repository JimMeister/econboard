# Uses ISO 3166-1 alpha-3 country codes

oecd.get.dsd <- function(flowref)
  readSDMX(paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", flowref))

oecd.query <- function(flowref, keys, startPeriod, endPeriod)
  {
    if (is.null(keys[["FREQUENCY"]]))
      {
        if (is.null(keys[["FREQ"]]))
          stop("No frequency set")
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
    else if (frq == "Q")
      {
        freq <- 4
        sp <- sprintf("%d-Q%d", startPeriod[1], startPeriod[2])
        ep <- sprintf("%d-Q%d", endPeriod[1], endPeriod[2])
      }
    else if (frq == "M")
      {
        freq <- 12
        sp <- sprintf("%d-%d-01", startPeriod[1], startPeriod[2])
        ep <- sprintf("%d-%d-01", endPeriod[1], endPeriod[2])
      }
    else
      stop("Unknown frequency:", frq)

    ## Construct query, taking into account ANDs (.) and ORs (+)
    key <- paste(lapply(keys, function(y) paste(y, collapse="+")), collapse = ".")
    url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/", flowref,
                  "/", key, "?startTime=", sp, "&endTime=", ep)
    d <- as.data.frame(readSDMX(url), stringsAsFactors = FALSE)

    ## Extract series for each combination of keys
    ## TODO: handle missing values (they are not returned by the OECD in the dataframe)
    cartesian.product <- expand.grid(keys, stringsAsFactors = FALSE)
    result <- list()
    for (i in 1:nrow(cartesian.product))
      {
        idx <- rep(TRUE, nrow(d))
        for (j in names(cartesian.product))
          idx <- idx & (d[[j]] == cartesian.product[[j]][i])

        obsTime <- d$obsTime[idx]
        stopifnot(obsTime[1] == sp && obsTime[length(obsTime)] == ep)
        result[[paste(cartesian.product[i,], collapse = ".")]] <- d$obsValue[idx]
      }

    ts(as.data.frame(result), start = startPeriod, end = endPeriod, frequency = freq)
  }

output.gap <- function(countries, startPeriod, endPeriod, freq = 1)
  {
    oecd.query("EO", list(LOCATION = countries, VARIABLE = "GAP", FREQUENCY = "A"),
               startPeriod, endPeriod)
  }
