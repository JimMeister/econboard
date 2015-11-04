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
    filter.subst <- list()
    if ("GEO" %in% names(filter))
      filter.subst$GEO <- eurostat.convert.codes(filter$GEO)

    sdmx.query(agencyId = "ESTAT", operation = "data", key = key, filter = filter,
               startPeriod, endPeriod, frequency, simplify.names = simplify.names,
               filter.subst = filter.subst)
  }

eurostat.growth <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "CLV_PCH_PRE",
                                     NA_ITEM = "B1GQ", GEO = countries),
                 startPeriod, endPeriod, frequency)

## Nominal GDP in euros
eurostat.nominal.gdp.euro <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "CP_MEUR",
                                     NA_ITEM = "B1GQ", GEO = countries),
                 startPeriod, endPeriod, frequency) * 1e6

eurostat.unemployment <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("une_rt_a", list(FREQ = "A", S_ADJ = "NSA",
                                  AGE = "TOTAL", SEX = "T", GEO = countries),
                 startPeriod, endPeriod, frequency)

eurostat.inflation <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("prc_hicp_aind", list(FREQ = "A", UNIT = "RCH_A_AVG",
                                       COICOP = "CP00", GEO = countries),
                 startPeriod, endPeriod, frequency)

eurostat.inflation.gdp.deflator <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "PD_PCH_PRE_EUR",
                                     NA_ITEM = "B1GQ", GEO = countries),
                 startPeriod, endPeriod, frequency)

### Balance of payments (BPM6)
## Current account balance as percentage of GDP (BPM6)
eurostat.ca.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_gdp6_q", list(FREQ = "A", UNIT = "PC_GDP", S_ADJ = "NSA",
                                    BOP_ITEM = "CA", STK_FLOW = "BAL", PARTNER = "WRL_REST",
                                    GEO = countries),
                 startPeriod, endPeriod, frequency)

## Net international investment position as percentage of GDP (BPM6)
eurostat.niip.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_gdp6_q", list(FREQ = "A", UNIT = "PC_GDP", S_ADJ = "NSA",
                                    BOP_ITEM = "FA", STK_FLOW = "N_LE", PARTNER = "WRL_REST",
                                    GEO = countries),
                 startPeriod, endPeriod, frequency)

## Exports to GDP ratio (according to balance of payments) (BPM6)
eurostat.exports.gdp.bop <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_gdp6_q", list(FREQ = "A", UNIT = "PC_GDP", S_ADJ = "NSA",
                                    BOP_ITEM = "GS", STK_FLOW = "CRE", PARTNER = "WRL_REST",
                                    GEO = countries),
                 startPeriod, endPeriod, frequency)

## Imports to GDP ratio (according to balance of payments) (BPM6)
eurostat.imports.gdp.bop <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_gdp6_q", list(FREQ = "A", UNIT = "PC_GDP", S_ADJ = "NSA",
                                    BOP_ITEM = "GS", STK_FLOW = "DEB", PARTNER = "WRL_REST",
                                    GEO = countries),
                 startPeriod, endPeriod, frequency)

### Balance of payments (BPM5)
## Current account balance as percentage of GDP (BPM5)
eurostat.ca.gdp.bpm5 <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_q_gdp", list(FREQ = "A", UNIT = "PC_GDP", POST = "993",
                                   STK_FLOW = "NET", PARTNER = "WORLD",
                                   GEO = countries),
                 startPeriod, endPeriod, frequency)

## Net international investment position as percentage of GDP (BPM5)
eurostat.niip.gdp.bpm5 <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_q_gdp", list(FREQ = "A", UNIT = "PC_GDP", POST = "995",
                                   STK_FLOW = "STK", PARTNER = "WORLD",
                                   GEO = countries),
                 startPeriod, endPeriod, frequency)

## Exports to GDP ratio (according to balance of payments) (BPM5)
eurostat.exports.gdp.bop.bpm5 <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_q_gdp", list(FREQ = "A", UNIT = "PC_GDP", POST = "991",
                                   STK_FLOW = "CRE", PARTNER = "WORLD",
                                   GEO = countries),
                 startPeriod, endPeriod, frequency)

## Imports to GDP ratio (according to balance of payments) (BPM5)
eurostat.imports.gdp.bop.bpm5 <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("bop_q_gdp", list(FREQ = "A", UNIT = "PC_GDP", POST = "991",
                                   STK_FLOW = "DEB", PARTNER = "WORLD",
                                   GEO = countries),
                 startPeriod, endPeriod, frequency)


eurostat.debt.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("gov_10dd_edpt1", list(FREQ="A", UNIT="PC_GDP", SECTOR="S13",
                                        NA_ITEM="GD", GEO=country),
                 startPeriod, endPeriod, frequency)


### Breakdown of GDP

## Exports to GDP ratio
eurostat.exports.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "PC_GDP", NA_ITEM = "P6",
                                     GEO = countries), startPeriod, endPeriod, frequency)

## Imports to GDP ratio
eurostat.imports.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "PC_GDP", NA_ITEM = "P7",
                                     GEO = countries), startPeriod, endPeriod, frequency)

## Final consumption expenditure to GDP ratio
eurostat.consumption.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "PC_GDP", NA_ITEM = "P3",
                                     GEO = countries), startPeriod, endPeriod, frequency)

## Gross fixed capital formation to GDP ratio
eurostat.investment.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "PC_GDP", NA_ITEM = "P51G",
                                     GEO = countries), startPeriod, endPeriod, frequency)

## Changes in inventories and acquisitions less disposals of valuables (in ratio of GDP)
eurostat.delta.stock.gdp <- function(countries, startPeriod, endPeriod, frequency = 1)
  eurostat.query("nama_10_gdp", list(FREQ = "A", UNIT = "PC_GDP", NA_ITEM = "P52_P53",
                                     GEO = countries), startPeriod, endPeriod, frequency)
