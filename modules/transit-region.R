
transit_region_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitregion"))
  )
}

transit_region_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    region_metric <- reactive({input$NTDMetric})
    
    output$region_chart_title <- renderText(paste0("Regionwide Transit ", region_metric(), " for all Transit Modes"))
    
    # Output Values
    output$region_pre_pandemic_metric <- renderText(paste0("2019 YTD ", region_metric()))
    output$region_current_metric <- renderText(paste0(year(Sys.Date())," YTD ", region_metric()))
    
    output$region_pre_pandemic_value <- renderText({
      if (region_metric() == "Boardings-per-Hour") {
        paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 1))
      } else {
        paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")
      }
    })
    
    output$region_current_value <- renderText({
      if(region_metric() == "Boardings-per-Hour") {
        paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()), 1))
      } else {
        paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")  
      }
    })
    
    output$region_pre_pandemic_share <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    
    output$region_recent_growth <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Charts & Maps
    output$ntd_region_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                          filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric()) |> 
                                                                          mutate(year=as.character(year)),
                                                                        x = "year", y = "estimate", 
                                                                        toggle = "grouping", fill = "metric", legend=FALSE,
                                                                        esttype = "number", color = c("#00A7A0", "#F05A28", "#91268F", "#8CC63E"),
                                                                        left_align = '15%', title = "Boardings")})

    # Tab layout
    output$transitregion <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("NTDMetric"), label="Select Transit Metric:", choices=ntd_metric_list, selected = "Boardings"))),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        tags$div(class="chart_title", textOutput(ns("region_chart_title"))),
        
        br(),
        
        layout_columns(
          value_box(
            title = textOutput(ns("region_pre_pandemic_metric")), value = textOutput(ns("region_pre_pandemic_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = textOutput(ns("region_current_metric")), value = textOutput(ns("region_current_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = "% of Pre-Pandemic", value = textOutput(ns("region_pre_pandemic_share")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = paste0("% change from ", base_yr), value = textOutput(ns("region_recent_growth")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          )
        ),
        
        br(),
        
        card(
          card_body(echarts4rOutput(ns("ntd_region_chart"))),
          card_footer(class = "chart_source",
                      "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)")
          ),
        hr(style = "border-top: 1px solid #000000;"),
        
      )
    }) 
  })  # end moduleServer
}
