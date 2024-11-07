# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(bslib)
library(shinycssloaders)

# Packages for Data Cleaning/Processing
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr)

# Packages for Charts
library(ggplot2)
library(plotly)
library(scales)
library(htmlwidgets)

# Packages for Map Creation
library(sf)
library(leaflet)

# Inputs ---------------------------------------------------------------
wgs84 <- 4326
base_yr <- "2023"
pre_pandemic <- "2019"
first_data_yr <- 2010
current_year <- 2024

gtfs_year <- 2024
service_change <- "Fall"

load_clr <- "#91268F"

# Run Modules Files ---------------------------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

# Data via RDS files ------------------------------------------------------
ntd_data <- readRDS("data/ntd_data.rds") |> mutate(year = as.numeric(year))

latest_ntd_month <- ntd_data |> filter(grouping == "YTD") |> mutate(d = as.character(month(date, label = TRUE))) |> select("d") |> unique() |> pull()
ntd_data <- ntd_data |> 
  mutate(grouping = str_replace_all(grouping, "YTD", paste0("Year to Date: Jan-",latest_ntd_month))) |>
  mutate(grouping = factor(grouping, levels = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual"))) |>
  filter(year >= first_data_yr)

transit_layer_data <- readRDS("data/transit_layer_data.rds")

transit_buffers <- readRDS("data/transit_buffers.rds")
transit_buffer_data <- readRDS("data/transit_buffer_data.rds")

transit_trip_buffers <- readRDS("data/transit_trip_buffers.rds")
transit_trip_data <- readRDS("data/transit_trip_data.rds")

# Page Information --------------------------------------------------------
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)

# Values for Drop Downs ---------------------------------------------------
ntd_metric_list <- as.character(unique(ntd_data$metric))
ntd_mode_list <- ntd_data |> select("variable") |> filter(variable != "All Transit Modes") |> distinct() |> pull()
ntd_operator_list <- ntd_data |> filter(geography_type == "Transit Operator") |> filter(!(geography %in% c("Senior Services of Snohomish County", "King County Ferry District"))) |> select("geography") |>  distinct() |> pull()
stop_buffer_list <- unique(transit_buffer_data$transit_buffer)
stop_trips_list <- unique(transit_trip_data$transit_buffer)
efa_list <- c("People of Color", "People with Lower Incomes", "People with Limited English", "People with a Disability", "People under 18", "People over 65", "Total Population", "People")

transit_links <- c("Community Transit" = "https://www.communitytransit.org/",
                   "Everett Transit" = "https://everetttransit.org/",
                   "King County Metro" = "https://kingcounty.gov/en/dept/metro",
                   "Kitsap Transit" = "https://www.kitsaptransit.com/",
                   "Pierce Transit" = "https://www.piercetransit.org/",
                   "Pierce County Ferry" = "https://www.piercecountywa.gov/1793/Ferry",
                   "Sound Transit" = "https://www.soundtransit.org/",
                   "Washington State Ferries" = "https://wsdot.wa.gov/travel/washington-state-ferries",
                   "Transit Planning at PSRC" = "https://www.psrc.org/our-work/transit"
)

links_withtags <- withTags(
  map2(transit_links[1:8], names(transit_links)[1:8], 
       ~div(class = "links-container", tags$a(class = "links", href = .x, .y, tabindex="0", target = "_blank")))
)

psrc_mission <- "Our mission is to advance solutions to achieve a thriving, racially equitable, and sustainable central Puget Sound region through leadership, visionary planning, and collaboration."
