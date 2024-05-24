# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinyBS)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(bslib)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(echarts4r)

# Packages for Map Creation
library(sf)
library(leaflet)

# Packages for Table Creation
library(DT)

# Package for Excel Data Creation
library(openxlsx)

# Run Modules Files ---------------------------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------
left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------
wgs84 <- 4326
load_clr <- "#91268F"
latest_yr <- "2023"

# Data via RDS files ------------------------------------------------------
ntd_data <- readRDS("data/ntd_data.rds")
transit_buffers <- readRDS("data/transit_buffers.rds")
transit_buffer_data <- readRDS("data/transit_buffer_data.rds")
transit_layer_data <- readRDS("data/transit_layer_data.rds") |> mutate(year = year(date))

latest_ntd_month <- ntd_data |> filter(grouping == "YTD") |> mutate(d = as.character(month(date, label = TRUE))) |> select("d") |> unique() |> pull()
ntd_data <- ntd_data |> 
  mutate(grouping = str_replace_all(grouping, "YTD", paste0("Year to Date: Jan-",latest_ntd_month))) |>
  mutate(grouping = factor(grouping, levels = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual")))

# Values for Drop Downs ---------------------------------------------------
ntd_metric_list <- as.character(unique(ntd_data$metric))
ntd_mode_list <- ntd_data |> select("variable") |> filter(variable != "All Transit Modes") |> distinct() |> pull()
ntd_operator_list <- ntd_data |> filter(geography_type == "Transit Operator") |> filter(!(geography %in% c("Senior Services of Snohomish County", "King County Ferry District"))) |> select("geography") |>  distinct() |> pull()
stop_buffer_list <- unique(transit_buffer_data$transit_buffer)

