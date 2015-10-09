### Uses codes close but not identical to ISO 3166-1 alpha-2 country codes
### UK instead of GB, EL instead of GR

eurostat.get.dsd <- function(key)
  rsdmx::readSDMX(paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_", key))

eurostat.convert.codes <- function(countries)
  {
    data("ISO_3166_1", package = "ISOcodes", envir = environment())
    countries.2 <- ISO_3166_1$Alpha_2[match(countries, ISO_3166_1$Alpha_3)]

    ## Eurostat uses non-standard abbrevs for Greece and UK
    countries.2[match("GR", countries.2)] <- "EL"
    countries.2[match("GB", countries.2)] <- "UK"

    countries.2
  }

eurostat.query <- function(key, filter, startPeriod, endPeriod, frequency, simplify.names = TRUE)
  {
    if ("GEO" %in% names(filter))
      filter$GEO <- eurostat.convert.codes(filter$GEO)

    sdmx.query(agencyId = "ESTAT", operation = "data", key = key, filter = filter,
               startPeriod, endPeriod, frequency, simplify.names = simplify.names)
  }

eurostat.growth <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "CLV_PCH_PRE",
                                     NA_ITEM = "B1GQ", GEO = countries),
                 startPeriod, endPeriod, frequency)

eurostat.unemployment <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("une_rt_a", list(FREQ = "A", S_ADJ = "NSA",
                                  AGE = "TOTAL", SEX = "T", GEO = countries),
                 startPeriod, endPeriod, frequency)

eurostat.inflation <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("prc_hicp_aind", list(FREQ = "A", UNIT = "RCH_A_AVG",
                                       COICOP = "CP00", GEO = countries),
                 startPeriod, endPeriod, frequency)
