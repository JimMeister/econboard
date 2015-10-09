sdmx.query <- function(agencyId, operation, key, filter, startPeriod, endPeriod, frequency, simplify.names = TRUE)
  {
    ## TODO: Add consistency check between FREQ/FREQUENCY key and frequency arg
    ## Or do it in upper level functions?

    if (frequency == 1)
      {
        sp <- startPeriod[1]
        ep <- endPeriod[1]
        parse.date <- function(x) as.numeric(x)
      }
    else if (frequency == 4)
      {
        sp <- sprintf("%d-Q%d", startPeriod[1], startPeriod[2])
        ep <- sprintf("%d-Q%d", endPeriod[1], endPeriod[2])
        parse.date <- function(x) as.numeric(unlist(strsplit(x, "-Q")))
      }
    else if (frequency == 12)
      {
        sp <- sprintf("%d-%d-01", startPeriod[1], startPeriod[2])
        ep <- sprintf("%d-%d-01", endPeriod[1], endPeriod[2])
        parse.date <- function(x) as.numeric(unlist(strsplit(x, "-")))[1:2]
      }
    else
      stop("Unknown frequency: ", frequency)

    d <- as.data.frame(rsdmx::readSDMX(agencyId = agencyId, operation = operation, key = key,
                                       filter = filter, start = sp, end = ep),
                       stringsAsFactors = FALSE)

    ## Extract series for each combination of filter criterion
    cartesian.product <- expand.grid(filter, stringsAsFactors = FALSE)
    result <- list()

    if (simplify.names && any(sapply(filter, length) > 1))
      dim.names.idx <- which(sapply(filter, length) > 1)
    else
      dim.names.idx <- 1:length(filter)

    for (i in 1:nrow(cartesian.product))
      {
        series <- paste(cartesian.product[i,dim.names.idx], collapse = ".")
        result[[series]] <- ts(NA, start = startPeriod, end = endPeriod, frequency = frequency)

        idx <- rep(TRUE, nrow(d))
        for (j in names(cartesian.product))
          idx <- idx & (d[[j]] == cartesian.product[[j]][i])

        for (j in which(idx))
          window(result[[series]], start = parse.date(d$obsTime[j]), end = parse.date(d$obsTime[j])) <- d$obsValue[j]
      }

    do.call("ts.union", result)
  }

sdmx.list.codes <- function(dsd)
  {
    l <- dsd@codelists@codelists
    for (x in l)
      {
        cat(x@id, " (", x@Name$en, ")", fill = TRUE, sep = "")
        for (y in x@Code)
          {
            cat("- ", y@id, " (", y@label$en, ")", fill = TRUE, sep = "")
          }
      }
  }
