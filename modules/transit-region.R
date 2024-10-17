
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
    
    # Insights
    output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
    
    # Output Values
    output$region_pre_pandemic_metric <- renderUI(shiny::p(paste0("2019 YTD ", region_metric()), style = "font-size: 1.25rem"))
    output$region_current_metric <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD ", region_metric()), style = "font-size: 1.25rem"))
    output$region_pandemic_share_metric <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD ", region_metric()), style = "font-size: 1.25rem"))
    output$region_recent_share_metric <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
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
    output$ntd_region_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                       filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |> 
                                                                       mutate(year=as.character(year)),
                                                                     x = "year", y = "estimate", fill = "metric", legend=FALSE,
                                                                     esttype = "number", color = c("#00A7A0"),
                                                                     left_align = '15%', title = "Boardings")})

    output$ntd_region_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                          filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & grouping == "Annual") |> 
                                                                          mutate(year=as.character(year)),
                                                                        x = "year", y = "estimate", fill = "metric", legend=FALSE,
                                                                        esttype = "number", color = c("#8CC63E"),
                                                                        left_align = '15%', title = "Boardings")})
    
    # Tab layout
    output$transitregion <- renderUI({
      tagList(
        br(),
        
        card(
          
          card_body(radioButtons(ns("NTDMetric"), label="Select a Transit Metric:", choices=ntd_metric_list, selected = "Boardings", inline = TRUE, width = '100%'),
                    class = "selection_panel")
        ),
        
        # selectInput(ns("NTDMetric"), label="Select a Transit Metric:", choices=ntd_metric_list, selected = "Boardings")
        
        hr(style = "border-top: 1px solid #000000;"),
        
        tags$div(class="chart_title", textOutput(ns("region_chart_title"))),
        
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("region_pre_pandemic_metric")), 
            value = textOutput(ns("region_pre_pandemic_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("region_current_metric")), 
            value = textOutput(ns("region_current_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("region_pandemic_share_metric")), 
            value = textOutput(ns("region_pre_pandemic_share")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("region_recent_share_metric")), 
            value = textOutput(ns("region_recent_growth")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          )
        ),
        
        br(),
        
        navset_card_tab(
          full_screen = TRUE,
          title = NULL,
          nav_panel(
            paste0("Year to Date: Jan-",latest_ntd_month),
            echarts4rOutput(ns("ntd_region_ytd_chart"))),
          nav_panel(
            "Annual: Jan-Dec",
            echarts4rOutput(ns("ntd_region_annual_chart"))),
        ),
        
        br(),
        
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("region_insights_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        
      )
    }) 
  })  # end moduleServer
}
