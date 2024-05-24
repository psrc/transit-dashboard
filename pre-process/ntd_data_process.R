# Libraries -----------------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(tidytransit)
library(psrcelmer)
library(psrccensus)
library(tidycensus)
library(sf)

source("functions.R")

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

gtfs_years <- c(seq(2016, 2024, by=1))
gtfs_service <- "spring"
latest_census <- 2022
latest_ofm <- 2023

generate_ntd_data <- "no"
generate_gtfs_data <- "no"
generate_efa_data <- "no"
generate_parcel_data <- "no"
generate_transit_population <- "no"
generate_transit_buffers <- "no"
generate_transit_layers <- "no"

hct_modes <- c("BRT", "Passenger Ferry", "Light Rail", "Streetcar", "Commuter Rail", "Auto Ferry")
bus_modes <- c("Bus", "ST Express")
ferry_modes <- c("Passenger Ferry", "Auto Ferry")
rail_modes<- c("Light Rail", "Streetcar")
transit_modes <- c("Bus", "ST Express", "BRT", "Passenger Ferry", "Light Rail", "Streetcar", "Commuter Rail", "Auto Ferry")

# NTD Data ----------------------------------------------------
if (generate_ntd_data == "yes") {
  ntd_data <- process_ntd_data()
  saveRDS(ntd_data, "data/ntd_data.rds")
} else {
  print(str_glue("Loading NTD Summary Data from stored results."))
  ntd_data <- readRDS("data/ntd_data.rds")
}

# GTFS Data ---------------------------------------------------------------
if (generate_gtfs_data == "yes") {
  transit_stops <- NULL
  for(y in gtfs_years) {
    s <- transit_stops_by_mode(year = y, service_change = gtfs_service)
    if(is.null(transit_stops)) {transit_stops <- s} else {transit_stops <- bind_rows(transit_stops, s)}
    rm(s)
  }
  saveRDS(transit_stops, "data/transit_stops.rds")
} else {
  print(str_glue("Loading GTFS based transit stop Data from stored results."))
  transit_stops <- readRDS("data/transit_stops.rds")
}

# EFA shares by Tract ----------------------------------
if (generate_efa_data == "yes") {
  
  tract_efa_data <- NULL
  for(y in gtfs_years) {
    
    poc <- acs_equity_shares(year=y, census_metric = "poc")
    poverty <- acs_equity_shares(year=y, census_metric = "poverty")
    youth <- acs_equity_shares(year=y, census_metric = "youth")
    older <- acs_equity_shares(year=y, census_metric = "older")
    lep <- acs_equity_shares(year=y, census_metric = "lep")
    disability <- acs_equity_shares(year=y, census_metric = "disability")
    
    p <- list(poc, poverty, youth, older, lep, disability) |> 
      reduce(left_join, by = c("tract", "census_year", "data_year")) |>
      select(tract_geoid = "tract", "census_year", "data_year", "poc", "poverty", "youth", "older", "lep", "disability")
    
    if (is.null(tract_efa_data)) {tract_efa_data <- p} else {tract_efa_data <- bind_rows(tract_efa_data, p)}
    rm(p, poc, poverty, youth, older, lep, disability)
  }
  
  saveRDS(tract_efa_data, "data/tract_efa_data.rds")
  
} else {
  print(str_glue("Loading EFA Tract Data from stored results."))
  tract_efa_data <- readRDS("data/tract_efa_data.rds")
  
}

# Parcel Population -------------------------------------------------------
if(generate_parcel_data == "yes") {
  
  parcel_data <- NULL
  for(y in gtfs_years) {
    
    # If latest GTFS data is newer than latest OFM data, use the most recent OFM data
    if (y > latest_ofm) {yr <- latest_ofm} else {yr <- y}
  
    # Parcel population
    print(str_glue("Loading {yr} OFM based parcelized estimates of total population"))
    if (yr < 2020) {ofm_vintage <- 2020} else {
      if(yr < 2023) {ofm_vintage <- 2022} else {ofm_vintage <- yr}}
    
    q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", yr, "")
    p <- get_query(sql = q)
    
    # Parcel Dimensions
    if (yr >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
    print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
    q <- paste0("SELECT parcel_dim_id, parcel_id, x_coord_state_plane, y_coord_state_plane, tract_geoid10, tract_geoid20 from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
    d <- get_query(sql = q)
    
    # Add Tract GEOIDs to Parcels and Final Cleanup
    print(str_glue("Final Cleanup of {y} parcelized estimates of total population"))
    p <- left_join(p, d, by="parcel_dim_id") |>
      mutate(data_year = y) |>
      select("parcel_id", x = "x_coord_state_plane", y = "y_coord_state_plane", tract10 = "tract_geoid10", tract20 = "tract_geoid20", population = "total_pop", ofm_year = "estimate_year", "data_year")

    # Append Parcel Data
    if(is.null(parcel_data)) {parcel_data <- p} else {parcel_data <- bind_rows(parcel_data, p)}
    rm(p, q, d)
  }
  
  print(str_glue("Selecting Tract 2010 or 2020 as tract_geoid based on data year."))
  parcel_data <- parcel_data |>
    mutate(tract_geoid = case_when(
      data_year < 2020 ~ tract10,
      data_year >= 2020 ~ tract20)) |>
    mutate(tract_geoid = as.character(tract_geoid))
  
  print(str_glue("Calculating parcel population by EFA."))
  parcel_data <- left_join(parcel_data, tract_efa_data, by=c("tract_geoid", "data_year")) |>
    mutate(poc = round(poc * population, 4)) |>
    mutate(poverty = round(poverty * population, 4)) |>
    mutate(lep = round(lep * population, 4)) |>
    mutate(youth = round(youth * population, 4)) |>
    mutate(older = round(older * population, 4)) |>
    mutate(disability = round(disability * population, 4)) |>
    drop_na() |>
    select("parcel_id", "x", "y", tract = "tract_geoid", gtfs_year = "data_year", "ofm_year", "census_year", "population", "poc", pov = "poverty", "lep", yth = "youth", old = "older", dis = "disability")
  
  saveRDS(parcel_data, "data/parcel_data.rds")

} else {
  print(str_glue("Loading Parcel Population Data from stored results."))
  parcel_data <- readRDS("data/parcel_data.rds")
  
}

# Population Estimates near Transit ------------------------------------
if(generate_transit_population == "yes") {
  
  transit_buffer_data <- NULL
  for(y in gtfs_years) {
    
    hct <- calculate_transit_buffer_data(yr = y, modes = hct_modes, mode_name = "High-Capacity Transit", buffer_dist = 0.50)
    bus <- calculate_transit_buffer_data(yr = y, modes = bus_modes, mode_name = "Local & Regional Bus", buffer_dist = 0.25)
    rail <- calculate_transit_buffer_data(yr = y, modes = rail_modes, mode_name = "Light Rail & Streetcar", buffer_dist = 0.50)
    ferry <- calculate_transit_buffer_data(yr = y, modes = ferry_modes, mode_name = "Ferries", buffer_dist = 1.00)
    transit_qtr <- calculate_transit_buffer_data(yr = y, modes = transit_modes, mode_name = "1/4mi all Transit", buffer_dist = 0.25)
    transit_hlf <- calculate_transit_buffer_data(yr = y, modes = transit_modes, mode_name = "1/2mi all Transit", buffer_dist = 0.50)
    
    d <- bind_rows(hct, bus, rail, ferry, transit_qtr, transit_hlf)
    if(is.null(transit_buffer_data)) {transit_buffer_data <- d} else {transit_buffer_data <- bind_rows(transit_buffer_data, d)}
    rm(hct, bus, rail, ferry, transit_qtr, transit_hlf, d)
  }
  
  saveRDS(transit_buffer_data, "data/transit_buffer_data.rds")
  
} else {
  
  print(str_glue("Loading Transit Buffer Population Data from stored results."))
  transit_buffer_data <- readRDS("data/transit_buffer_data.rds")
  
}

# Transit Layers by Year ------------------------------------
if(generate_transit_layers == "yes") {
  
  transit_layer_data <- NULL
  for(y in gtfs_years) {
    
    lyr <- transit_routes_by_mode(year = y, service_change = "spring")
    if(is.null(transit_layer_data)) {transit_layer_data <- lyr} else {transit_layer_data <- bind_rows(transit_layer_data, lyr)}
    rm(lyr)
  }
  
  saveRDS(transit_layer_data, "data/transit_layer_data.rds")
  
} else {
  
  print(str_glue("Loading Transit Layer Data from stored results."))
  transit_layer_data <- readRDS("data/transit_layer_data.rds")
  
}

# Transit Buffers for Maps ------------------------------------------------
if(generate_transit_buffers == "yes") {
  
  hct <- create_transit_buffer(modes = hct_modes, mode_name = "High-Capacity Transit", buffer_dist = 0.50)
  bus <- create_transit_buffer(modes = bus_modes, mode_name = "Local & Regional Bus", buffer_dist = 0.25)
  rail <- create_transit_buffer(modes = rail_modes, mode_name = "Light Rail & Streetcar", buffer_dist = 0.50)
  ferry <- create_transit_buffer(modes = ferry_modes, mode_name = "Ferries", buffer_dist = 1.00)
  transit_qtr <- create_transit_buffer(modes = transit_modes, mode_name = "1/4mi all Transit", buffer_dist = 0.25)
  transit_hlf <- create_transit_buffer(modes = transit_modes, mode_name = "1/2mi all Transit", buffer_dist = 0.50)
  
  transit_buffers <- bind_rows(hct, bus, rail, ferry, transit_qtr, transit_hlf)
  rm(hct, bus, rail, ferry, transit_qtr, transit_hlf)
  
  saveRDS(transit_buffers, "data/transit_buffers.rds")

} else {
  
  print(str_glue("Loading Transit Buffers from stored results."))
  transit_buffers <- readRDS("data/transit_buffers.rds")
  
}

# Regional Population Summary ---------------------------------------------
print(str_glue("Summarizing regional population by year and equity focus area."))
region_pop_summary <- parcel_data |> 
  group_by(gtfs_year) |>   
  summarise(population = sum(population), poc = sum(poc), pov = sum(pov), yth = sum(yth), old = sum(old), lep = sum(lep), dis = sum(dis)) |>
  as_tibble()
