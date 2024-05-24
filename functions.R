echarts4r::e_common(font_family = "Poppins")

# Data Processing ---------------------------------------------------------
process_ntd_data <- function() {
  
  # Location of the most recently downloaded NTD file
  process_dir <- getwd()
  file_dir <- "X:/DSA/rtp-dashboard/NTD"
  
  # Choose NTD file
  setwd(file_dir)
  data_file <- file.choose()
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Passenger Trips, Revenue-Miles and Revenue-Hours tabs
  ntd_tabs <- c("UPT", "VRM", "VRH")
  
  # Figure out which Transit Agencies serve which MPO's
  setwd(process_dir)
  agency_file <- "data/transit-agency.csv"
  print(str_glue("Figuring out which Transit agencies are in which Metro Area."))
  agencies <- read_csv(agency_file, show_col_types = FALSE) |>
    mutate(NTDID = str_pad(string=NTDID, width=5, pad="0", side=c("left"))) |>
    mutate(UACE = str_pad(string=UACE, width=5, pad="0", side=c("left")))
  
  ntd_ids <- agencies |> select("NTDID") |> distinct() |> pull()
  
  # Lookup for NTD Modes
  mode_file <- "data/ntd-modes.csv"
  ntd_modes <- read_csv(mode_file, show_col_types = FALSE)
  
  # Initial processing of NTD data
  processed <- NULL
  for (areas in ntd_tabs) {
    print(str_glue("Working on {areas} data processing and cleanup."))
    
    # Open file and filter data to only include operators for RTP analysis
    t <- as_tibble(read.xlsx(data_file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) |>
      mutate(NTD.ID = str_pad(string=NTD.ID, width=5, pad="0", side=c("left"))) |>
      filter(NTD.ID %in% ntd_ids) |> 
      mutate(Mode = case_when(
        Mode == "DR" & TOS == "DO" ~ "DR-DO",
        Mode == "DR" & TOS == "PT" ~ "DR-PT",
        Mode == "DR" & TOS == "TN" ~ "DR-TN",
        Mode == "DR" & TOS == "TX" ~ "DR-TX",
        TRUE ~ Mode)) |> 
      select(-"Legacy.NTD.ID", -contains("Status"), -"Reporter.Type", -"UACE.CD", -"UZA.Name", -"TOS", -"3.Mode") |> 
      pivot_longer(cols = 4:last_col(), names_to = "date", values_to = "estimate", values_drop_na = TRUE) |> 
      mutate(date = my(date))
    
    # Add Detailed Mode Names & Aggregate  
    t <- left_join(t, ntd_modes, by=c("Mode")) |> 
      rename(variable="mode_name") |> 
      select(-"Mode") |>
      group_by(NTD.ID, Agency, date, variable) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble()
    
    # Add Metro Area Name
    n <- agencies |> select("NTDID", "MPO_AREA", "AGENCY_NAME")
    t <- left_join(t, n, by=c("NTD.ID"="NTDID"), relationship = "many-to-many") |>
      select(-"NTD.ID", -"Agency") |>
      rename(grouping="MPO_AREA", geography="AGENCY_NAME") |>
      as_tibble() |>
      mutate(metric=areas) |>
      mutate(metric = case_when(
        metric == "UPT" ~ "Boardings",
        metric == "VRM" ~ "Revenue-Miles",
        metric == "VRH" ~ "Revenue-Hours"))
    
    rm(n)
    
    # Full Year Data
    
    max_yr <- t |> select("date") |> distinct() |> pull() |> max() |> year()
    max_mo <- t |> select("date") |> distinct() |> pull() |> max() |> month()
    
    if (max_mo <12) {
      yr <- max_yr-1
    } else {
      yr <- max_yr
    }
    
    # Trim Data so it only includes full year data and combine
    print(str_glue("Trim {areas} data to only include full year data."))
    full_yr <- t |>
      filter(year(date)<=yr) |>
      mutate(year = year(date)) |>
      group_by(year, variable, grouping, geography, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(date = ymd(paste0(year,"-12-01"))) |>
      select(-"year")
    
    # Metro Areas only need to compare at the total level
    print(str_glue("Summaring Metro Area {areas} full year data."))
    metro_total <-  full_yr |>
      group_by(date, grouping, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography=grouping, geography_type="Metro Areas", variable="All Transit Modes", grouping="Annual")
    
    # PSRC Region by Mode
    print(str_glue("Summaring PSRC Region {areas} full year data by Mode."))
    region_modes <-  full_yr |>
      filter(grouping=="Seattle") |>
      group_by(date, variable, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography="Region", geography_type="Region", grouping="Annual")
    
    # PSRC Region Total
    print(str_glue("Summaring PSRC Region {areas} full year data for All Transit."))
    region_total <-  full_yr |>
      filter(grouping=="Seattle") |>
      group_by(date, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography="Region", geography_type="Region", grouping="Annual", variable="All Transit Modes")
    
    # PSRC Region by Operator and Mode
    print(str_glue("Summaring PSRC Region {areas} full year data by Transit Operator and Modes."))
    region_operator_modes <-  full_yr |>
      filter(grouping=="Seattle") |>
      mutate(geography_type="Transit Operator", grouping="Annual")
    
    # PSRC Region by Operator
    print(str_glue("Summaring PSRC Region {areas} full year data by Transit Operator for All Transit."))
    region_operator <-  full_yr |>
      filter(grouping=="Seattle") |>
      group_by(date, geography, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography_type="Transit Operator", grouping="Annual", variable="All Transit Modes")
    
    ifelse(is.null(processed), 
           processed <- bind_rows(list(region_operator,region_operator_modes,
                                       region_total,region_modes,
                                       metro_total)), 
           processed <- bind_rows(list(processed,
                                       region_operator,region_operator_modes,
                                       region_total,region_modes,
                                       metro_total)))
    
    rm(region_operator,region_operator_modes,
       region_total,region_modes,
       metro_total, full_yr)
    
    # Year to Date
    
    # Ensure that all data is consistent - find the maximum month for YTD calculations
    max_mo <- t |> select("date") |> distinct() |> pull() |> max() |> month()
    
    # Trim Data so it only includes ytd for maximum month and combine
    print(str_glue("Trim {areas} data to only months for year to date data through month {max_mo}."))
    ytd <- t |>
      filter(month(date)<=max_mo) |>
      mutate(year = year(date)) |>
      group_by(year, variable, grouping, geography, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(date = ymd(paste0(year,"-",max_mo,"-01"))) |>
      select(-"year")
    
    # Metro Areas only need to compare at the total level
    print(str_glue("Summaring Metro Area {areas} year to date data through month {max_mo}."))
    metro_total <-  ytd |>
      group_by(date, grouping, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography=grouping, geography_type="Metro Areas", variable="All Transit Modes", grouping="YTD")
    
    # PSRC Region by Mode
    print(str_glue("Summaring PSRC region {areas} year to date data by mode through month {max_mo}."))
    region_modes <-  ytd |>
      filter(grouping=="Seattle") |>
      group_by(date, variable, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography="Region", geography_type="Region", grouping="YTD")
    
    # PSRC Region Total
    print(str_glue("Summaring PSRC region {areas} year to date data by All Transit through month {max_mo}."))
    region_total <-  ytd |>
      filter(grouping=="Seattle") |>
      group_by(date, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography="Region", geography_type="Region", grouping="YTD", variable="All Transit Modes")
    
    # PSRC Region by Operator and Mode
    print(str_glue("Summaring PSRC region {areas} year to date data by Operator and Mode through month {max_mo}."))
    region_operator_modes <-  ytd |>
      filter(grouping=="Seattle") |>
      mutate(geography_type="Transit Operator", grouping="YTD")
    
    # PSRC Region by Operator
    print(str_glue("Summaring PSRC region {areas} year to date data by Operator through month {max_mo}."))
    region_operator <-  ytd |>
      filter(grouping=="Seattle") |>
      group_by(date, geography, metric) |>
      summarise(estimate=sum(estimate)) |>
      as_tibble() |>
      mutate(geography_type="Transit Operator", grouping="YTD", variable="All Transit Modes")
    
    # Add YTD to Annual Data
    processed <- bind_rows(list(processed,
                                region_operator,region_operator_modes,
                                region_total,region_modes,
                                metro_total))
    
    rm(region_operator,region_operator_modes,
       region_total,region_modes,
       metro_total, ytd, t)
    
  }
  
  # Pivot NTD data wide and create new metric: boardings per revenue-hour
  print(str_glue("Calculating Boardings per Revenue Hour andd performing final cleanup."))
  processed_wide <- processed |> 
    pivot_wider(names_from = "metric",
                values_from = "estimate") |> 
    mutate(`Boardings-per-Hour` = ifelse(`Revenue-Hours` > 0,
                                         round(`Boardings` / `Revenue-Hours`, 2), NA))
  
  # Pivot NTD data back to long form for app use
  processed <- processed_wide |> 
    pivot_longer(cols = c("Boardings",
                          "Revenue-Miles",
                          "Revenue-Hours",
                          "Boardings-per-Hour"),
                 names_to = "metric",
                 values_to = "estimate")
  
  processed <- processed |> 
    mutate(year = as.character(year(date))) |>
    select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate") |>
    drop_na() |>
    filter(estimate >0)
  
  print(str_glue("All done."))
  
  return(processed)
  
}

transit_stops_by_mode <- function(year, service_change) {
  
  hct_file <- "data/hct_ids.csv"
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- paste0("X:/DSA/GTFS/",tolower(service_change),"/",as.character(year),".zip")
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes", "shapes"))
  
  # Load Stops
  print(str_glue("Getting the {service_change} {year} stops into a tibble." ))
  stops <- as_tibble(gtfs$stops) |> 
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("stop_id", "stop_name", "stop_lat", "stop_lon")
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & str_detect(route_id, "ct") ~ "Community Transit",
      is.na(agency_name) & str_detect(route_id, "et") ~ "Everett Transit",
      is.na(agency_name) & str_detect(route_id, "kc") ~ "King County Metro",
      is.na(agency_name) & str_detect(route_id, "kt") ~ "Kitsap Transit",
      is.na(agency_name) & str_detect(route_id, "pt") ~ "Pierce Transit",
      is.na(agency_name) & str_detect(route_id, "st") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Trips are used to get route id onto stop times
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("trip_id", "route_id")
  
  trips <- left_join(trips, routes, by=c("route_id"))
  
  # Clean Up Stop Times to get routes and mode by stops served
  print(str_glue("Getting the {service_change} {year} stop times into a tibble to add route information." ))
  stoptimes <- as_tibble(gtfs$stop_times) |>
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("trip_id", "stop_id")
  
  # Get Mode and agency from trips to stops
  print(str_glue("Getting unique stop list by modes for the {service_change} {year}." ))
  stops_by_mode <- left_join(stoptimes, trips, by=c("trip_id")) |>
    select("stop_id", "type_code", "type_name", "agency_name") |>
    distinct()
  
  stops_by_mode <- left_join(stops_by_mode, stops, by=c("stop_id")) |>
    mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(stops_by_mode)
  
}

transit_routes_by_mode <- function(year, service_change) {
  
  hct_file <- "data/hct_ids.csv"
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- paste0("X:/DSA/GTFS/",tolower(service_change),"/",as.character(year),".zip")
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes", "shapes"))
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & str_detect(route_id, "ct") ~ "Community Transit",
      is.na(agency_name) & str_detect(route_id, "et") ~ "Everett Transit",
      is.na(agency_name) & str_detect(route_id, "kc") ~ "King County Metro",
      is.na(agency_name) & str_detect(route_id, "kt") ~ "Kitsap Transit",
      is.na(agency_name) & str_detect(route_id, "pt") ~ "Pierce Transit",
      is.na(agency_name) & str_detect(route_id, "st") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Load Route Shapes to get Mode information on layers
  route_lyr <- shapes_as_sf(gtfs$shapes)
  
  # Trips are used to get route id onto shapes
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "shape_id") |>
    distinct()
  
  route_lyr <- left_join(route_lyr, trips, by=c("shape_id"))
  
  # Get Mode and agency from routes to shapes
  print(str_glue("Getting route details onto shapes for the {service_change} {year}." ))
  route_lyr <- left_join(route_lyr, routes, by=c("route_id")) |> mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(route_lyr)
  
}

acs_equity_shares <- function(year, census_metric) {
  
  # Determine Table Name, variables and labels from Variable Lookup
  print(str_glue("Loading the {census_metric} variable and label lookups."))
  lookup <- read_csv(paste0("data/census-variable-lookups.csv"), show_col_types = FALSE) |> 
    filter(metric == census_metric) |> 
    mutate(variable = paste0(table, "_", str_pad(variable, width=3, side = 'left', pad="0")))
  census_tbl <- lookup |> select("table") |> distinct() |> pull()
  census_var <- lookup |> select("variable") |> distinct() |> pull()
  census_lbl <- lookup |> select("variable", "metric", "simple_label")
  census_ord <- lookup |> arrange(order) |> select("simple_label") |> distinct() |> pull()
  
  # Use latest Census Year in Analysis year is past 
  if(year > latest_census) {y <- latest_census} else {y <- year}
  
  # Get Data by Census Block Group
  print(str_glue("Downloading {census_metric} data from {y}-ACS 5yr table {census_tbl} for tracts in the region."))
  pop <- get_acs_recs(geography="tract", table.names = census_tbl, years = y, acs.type = 'acs5') |> filter(variable %in% census_var)
  
  # Clean Up labels and group data by labels
  efa_column <- census_ord[census_ord != "Total"]
  print(str_glue("Cleaning up labels and grouping {census_metric} data from {y}-ACS 5yr table {census_tbl} for tracts in the region."))
  pop <- left_join(pop, census_lbl, by=c("variable")) |>
    select(tract="GEOID", "estimate", "moe", "year", "metric", grouping="simple_label") |>
    group_by(tract, year, metric, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    pivot_wider(names_from = grouping, values_from = estimate) |>
    mutate(!!census_metric:= .data[[efa_column]] / Total) |>
    mutate(!!census_metric:= replace_na(.data[[census_metric]], 0)) |>
    select("tract", census_year="year", all_of(census_metric)) |>
    mutate(data_year = year)
  
  return(pop)
  
}

calculate_transit_buffer_data <- function(stops=transit_stops, parcels=parcel_data, yr, modes, mode_name, buffer_dist) {
  
  print(str_glue("Creating {mode_name} stop buffer for {yr} {mode_name} stations."))
  s <- stops |> 
    filter(year(date) == yr) |> 
    filter(type_name %in% modes) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs=wgs84) |>
    st_transform(spn) |>
    st_buffer(dist = buffer_dist * 5280) |>
    mutate(stop_buffer = mode_name) |>
    select("stop_buffer")
  
  print(str_glue("Creating Parcel point layer for {yr} to intersect with {mode_name} stations"))
  p <- parcels |>
    filter(gtfs_year == yr) |>
    select(-"tract", -"gtfs_year", -"ofm_year", -"census_year") |>
    st_as_sf(coords = c("x", "y"), crs=spn) |>
    st_transform(spn)
  
  print(str_glue("Intersecting parcel point data with {mode_name} stop buffer for {yr}."))
  m <- st_intersection(p, s) |>
    st_drop_geometry() |>
    distinct() |>
    mutate(gtfs_year = yr) |>
    group_by(gtfs_year) |>
    summarise(population = round(sum(population),-2), 
              poc = round(sum(poc),-2), pov = round(sum(pov),-2), yth = round(sum(yth),-2), 
              old = round(sum(old),-2), lep = round(sum(lep),-2), dis = round(sum(dis), -2)) |>
    as_tibble()
  
  print(str_glue("Calculating regionwide totals for {yr}"))
  r <- parcels |> 
    filter(gtfs_year == yr) |>
    group_by(gtfs_year) |>   
    summarise(region_population = round(sum(population),-2), 
              region_poc = round(sum(poc),-2), region_pov = round(sum(pov),-2), region_yth = round(sum(yth),-2), 
              region_old = round(sum(old),-2), region_lep = round(sum(lep),-2), region_dis = round(sum(dis), -2)) |>
    as_tibble()
  
  print(str_glue("Calculating shares of people with {mode_name} access for {yr}"))
  d <- left_join(m, r, by="gtfs_year") |>
    mutate(population_share = population/region_population, 
           poc_share = poc/region_poc, pov_share = pov/region_pov, lep_share = lep/region_lep,
           yth_share = yth/region_yth, old_share = old/region_old, dis_share = dis/region_dis) |>
    select(year = "gtfs_year", "population", "population_share", 
           "poc", "poc_share", "pov", "pov_share", "lep", "lep_share",
           "yth", "yth_share", "old", "old_share", "dis", "dis_share") |>
    mutate(transit_buffer = mode_name)
  
  return(d)
  
}

create_transit_buffer <- function(stops=transit_stops, yrs=gtfs_years, modes, mode_name, buffer_dist) {
  
  buffers <- NULL
  for(yr in yrs) {
    
    print(str_glue("Creating {mode_name} stop buffer for {yr} {mode_name} stations."))
    s <- stops |> 
      filter(year(date) == yr) |> 
      filter(type_name %in% modes) |>
      st_as_sf(coords = c("stop_lon", "stop_lat"), crs=wgs84) |>
      st_transform(spn) |>
      st_buffer(dist = buffer_dist * 5280) |>
      mutate(stop_buffer = mode_name) |>
      select("stop_buffer")
      
    t <- st_union(s)
    
    u <- st_sf(geometry=t) |>
      mutate(stop_buffer = mode_name, year = yr) |>
      st_transform(wgs84)
    
    if(is.null(buffers)) {buffers <- u} else {buffers <- rbind(buffers, u)}
    rm(s, t, u)
  }
  
  return(buffers)
}

# General Information -------------------------------------------------------------
page_information <- function(tbl, page_name, page_section=NULL, page_info) {
  
  if(is.null(page_section)) {
    
    t <- tbl |>
      filter(page == page_name) |>
      select(all_of(page_info)) |>
      pull()
    
  } else {
    
    t <- tbl |>
      filter(page == page_name & section == page_section) |>
      select(all_of(page_info)) |>
      pull()
    
  }
  
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

# Charts ------------------------------------------------------------------

tooltip_js <- "
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }"

format_opts <- function(e, esttype, dec, title) {
  if(esttype == "number") {
    e <- e |> e_tooltip(trigger = "item")
    
  } else {
    
    if(esttype == "currency") {
      curr <- "USD"
    } else {
      curr <- NULL
    }
    
    e <- e |>
      e_y_axis(name = title, 
               nameLocation = "middle", 
               nameGap = 50,
               nameTextStyle = list(fontSize=14),
               axisLabel=list(margin=10),
               formatter = e_axis_formatter(esttype, digits = dec)) |>
      e_tooltip(trigger = "item",
                formatter =  e_tooltip_item_formatter(style = esttype, digits = 0, currency = curr)) |>
      e_tooltip(formatter =  htmlwidgets::JS(tooltip_js))
  }
  return(e)
}

e_basics <- function(e, top_padding, bottom_padding, legend, left_align) {
  e <- e |>
    e_grid(left = left_align, top = top_padding, bottom = bottom_padding) |>
    e_x_axis(axisTick=list(show = FALSE)) |>
    e_show_loading()
  
  e <- e |> e_legend(show = legend, bottom=0)
  
  return(e)
}

timeline_opts <- function(e, right_toggle, left_toggle) {
  e |>
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = right_toggle,
                               left = left_toggle,
                               currentIndex = 0,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
}

create_bar_chart <- function(df, x, y, fill, esttype="number", dec=0, color, bar_column="column", legend=TRUE, left_align='20%', top_padding=25, bottom_padding=75, title=NULL) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    e_charts_(x, timeline = FALSE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_bar_(fill_items, name = fill_items)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  # Rotate for bar chart
  if (bar_column == "bar") {
    c <- c |> e_flip_coords() |> e_legend(show = legend, top=0)
  }
  
  return(c)
  
}

create_bar_chart_toggle <- function(df, x, y, fill, toggle, esttype="number", dec=0, color, bar_column="column", legend=TRUE, left_align='20%', top_padding=100, bottom_padding=75, title=NULL, right_toggle = 200, left_toggle = 200) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y), all_of(toggle)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    group_by(.data[[toggle]]) |>
    e_charts_(x, timeline = TRUE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_bar_(fill_items, name = fill_items)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  # Add in the Timeseries Selector
  c <- timeline_opts(c, right_toggle, left_toggle)
  
  # Rotate for bar chart
  if (bar_column == "bar") {
    c <- c |> e_flip_coords() |> e_legend(show = legend, top=0)
  }
  
  return(c)
  
}

create_line_chart <- function(df, x, y, fill, esttype="number", dec=0, color, legend=TRUE, left_align='20%', top_padding=100, bottom_padding=75, title=NULL) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    e_charts_(x, timeline = FALSE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_line_(fill_items, smooth = FALSE)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  return(c)
  
}

# Maps --------------------------------------------------------------------

create_stop_buffer_map<- function(lyr=transit_buffers, buffer, yr) {
  
  # Trim Layer to Variable of Interest and Year
  lyr <- lyr |> filter(stop_buffer %in% buffer & year == yr)
  
  labels <- paste0("<b>", paste0("Transit Type: "),"</b>", lyr$stop_buffer) |> lapply(htmltools::HTML)
  
  working_map <- leaflet(data = lyr) |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c(buffer),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) |>
    
    addPolygons(data = lyr, 
                fillColor = "#91268F",
                fillOpacity = 1,
                opacity = 0,
                label = labels,
                group = buffer) |>
    
    setView(lng = -122.257, lat = 47.615, zoom = 8.5) |>
    
    addLegend(colors=c("#91268F"),
              labels=c(buffer),
              group = buffer,
              position = "bottomleft")
  
  
  
  return(working_map)
  
}

create_route_map<- function(lyr=transit_layer_data, yr) {
  
  # Trim Layer to Variable of Interest and Year
  lyr <- lyr |> filter(year == yr)
  
  # Create HCT Layers to make mapping by mode cleaner
  brt <- lyr |> filter(type_name %in% c("BRT"))
  crt <- lyr |> filter(type_name %in% c("Commuter Rail"))
  lrt <- lyr |> filter(type_name %in% c("Streetcar", "Light Rail"))
  pof <- lyr |> filter(type_name %in% c("Passenger Ferry"))
  fry <- lyr |> filter(type_name %in% c("Auto Ferry"))
  
  brt_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", brt$route_name) |> lapply(htmltools::HTML)
  crt_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", crt$route_name) |> lapply(htmltools::HTML)
  lrt_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", lrt$route_name) |> lapply(htmltools::HTML)
  pof_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", pof$route_name) |> lapply(htmltools::HTML)
  fry_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", fry$route_name) |> lapply(htmltools::HTML)
  
  working_map <- leaflet() |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("BRT", "Commuter Rail", "Light Rail", "Passenger Ferry", "Multimodal Ferry"),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) |>
    
    addPolylines(data = brt,
                 color = "#91268F",
                 weight = 4,
                 fillColor = "#91268F",
                 group = "BRT",
                 label = brt_lbl) |>
    
    addPolylines(data = crt,
                 color = "#8CC63E",
                 weight = 4,
                 fillColor = "#8CC63E",
                 group = "Commuter Rail",
                 label = crt_lbl) |>
    
    addPolylines(data = lrt,
                 color = "#F05A28",
                 weight = 4,
                 fillColor = "#F05A28",
                 group = "Light Rail",
                 label = lrt_lbl) |>
    
    addPolylines(data = pof,
                 color = "#40BDB8",
                 weight = 4,
                 fillColor = "#40BDB8",
                 group = "Passenger Ferry",
                 label = pof_lbl) |>
    
    addPolylines(data = fry,
                 color = "#00716c",
                 weight = 4,
                 fillColor = "#00716c",
                 group = "Multimodal Ferry",
                 label = fry_lbl) |>
    
    setView(lng = -122.257, lat = 47.615, zoom = 8.5) |>
    
    addLegend(colors=c("#91268F", "#8CC63E", "#F05A28", "#40BDB8", "#00716c"),
              labels=c("BRT", "Commuter Rail", "Light Rail", "Passenger Ferry", "Multimodal Ferry"),
              position = "bottomleft")
  
  return(working_map)
  
}

