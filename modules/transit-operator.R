
transit_operator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitoperator"))
  )
}

transit_operator_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Metric
    operator_metric <- reactive({input$NTDoperators})
    
    # Section Titles
    output$operator_boardings_chart_title <- renderText(paste0(operator_metric(), " Boardings"))
    output$operator_hours_chart_title <- renderText(paste0(operator_metric(), " Revenue-Hours"))
    output$operator_miles_chart_title <- renderText(paste0(operator_metric(), " Revenue-Miles"))
    output$operator_bph_chart_title <- renderText(paste0(operator_metric(), " Boardings per Hour"))
    
    # Insights
    output$operator_insights_boardings_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Boardings", page_info = "description"))})
    output$operator_insights_hours_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Hours", page_info = "description"))})
    output$operator_insights_miles_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Miles", page_info = "description"))})
    output$operator_insights_bph_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-BPH", page_info = "description"))})
    
    # Value Box Titles & Values - Boardings
    output$operator_pre_pandemic_boardings_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Boardings"), style = "font-size: 1.25rem"))
    output$operator_current_boardings_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Boardings"), style = "font-size: 1.25rem"))
    output$operator_pandemic_share_boardings_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Boardings"), style = "font-size: 1.25rem"))
    output$operator_recent_share_boardings_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$operator_pre_pandemic_boardings_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$operator_current_boardings_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$operator_pandemic_share_boardings <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$operator_recent_share_boardings <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Hours
    output$operator_pre_pandemic_hours_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Revenue Hours"), style = "font-size: 1.25rem"))
    output$operator_current_hours_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Revenue Hours"), style = "font-size: 1.25rem"))
    output$operator_pandemic_share_hours_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Revenue Hours"), style = "font-size: 1.25rem"))
    output$operator_recent_share_hours_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$operator_pre_pandemic_hours_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$operator_current_hours_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$operator_pandemic_share_hours <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$operator_recent_share_hours <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Miles
    output$operator_pre_pandemic_miles_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Revenue Miles"), style = "font-size: 1.25rem"))
    output$operator_current_miles_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Revenue Miles"), style = "font-size: 1.25rem"))
    output$operator_pandemic_share_miles_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Revenue Miles"), style = "font-size: 1.25rem"))
    output$operator_recent_share_miles_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$operator_pre_pandemic_miles_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$operator_current_miles_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$operator_pandemic_share_miles <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$operator_recent_share_miles <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Boardings per Hour
    output$operator_pre_pandemic_bph_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Boardings per Hour"), style = "font-size: 1.25rem"))
    output$operator_current_bph_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Boardings per Hour"), style = "font-size: 1.25rem"))
    output$operator_pandemic_share_bph_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Boardings per HOur"), style = "font-size: 1.25rem"))
    output$operator_recent_share_bph_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$operator_pre_pandemic_bph_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 1))})
    output$operator_current_bph_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()), 1))})
    output$operator_pandemic_share_bph <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$operator_recent_share_bph <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Charts & Maps
    output$ntd_operator_boardings_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                                   filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Boardings" & geography == operator_metric() & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                                   mutate(year=as.character(year)),
                                                                                 x = "year", y = "estimate", fill = "metric",
                                                                                 esttype = "number", color = c("#F05A28"),
                                                                                 left_align = '25%', title = "Boardings", legend=FALSE)})
    
    output$ntd_operator_boardings_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                                      filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Boardings" & geography == operator_metric() & grouping == "Annual") |>
                                                                                      mutate(year=as.character(year)),
                                                                                    x = "year", y = "estimate", fill = "metric",
                                                                                    esttype = "number", color = c("#F7A489"),
                                                                                    left_align = '25%', title = "Boardings", legend=FALSE)})
    
    
    output$ntd_operator_hours_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                               filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Revenue-Hours" & geography == operator_metric() & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                               mutate(year=as.character(year)),
                                                                             x = "year", y = "estimate", fill = "metric", 
                                                                             esttype = "number", color = c("#91268F"),
                                                                             left_align = '25%', title = "Revenue-Hours", legend=FALSE)})
    
    output$ntd_operator_hours_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                                  filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Revenue-Hours" & geography == operator_metric() & grouping == "Annual") |>
                                                                                  mutate(year=as.character(year)),
                                                                                x = "year", y = "estimate", fill = "metric", 
                                                                                esttype = "number", color = c("#C388C2"),
                                                                                left_align = '25%', title = "Revenue-Hours", legend=FALSE)})
    
    output$ntd_operator_miles_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                               filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Revenue-Miles" & geography == operator_metric() & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                               mutate(year=as.character(year)),
                                                                             x = "year", y = "estimate", fill = "metric", 
                                                                             esttype = "number", color = c("#8CC63E"),
                                                                             left_align = '25%', title = "Revenue-Miles", legend=FALSE)})
    
    output$ntd_operator_miles_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                                  filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Revenue-Miles" & geography == operator_metric() & grouping == "Annual") |>
                                                                                  mutate(year=as.character(year)),
                                                                                x = "year", y = "estimate", fill = "metric", 
                                                                                esttype = "number", color = c("#C0E095"),
                                                                                left_align = '25%', title = "Revenue-Miles", legend=FALSE)})
    
    output$ntd_operator_bph_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                             filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Boardings-per-Hour" & geography == operator_metric() & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                             mutate(year=as.character(year)),
                                                                           x = "year", y = "estimate", dec = 1, fill = "metric", 
                                                                           esttype = "number", color = c("#00A7A0"),
                                                                           left_align = '25%', title = "Boardings per Hour", legend=FALSE)})
    
    output$ntd_operator_bph_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                                filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Boardings-per-Hour" & geography == operator_metric() & grouping == "Annual") |>
                                                                                mutate(year=as.character(year)),
                                                                              x = "year", y = "estimate", dec = 1, fill = "metric", 
                                                                              esttype = "number", color = c("#73CFCB"),
                                                                              left_align = '25%', title = "Boardings per Hour", legend=FALSE)})
    
    
    
    
    # Tab layout
    output$transitoperator <- renderUI({
      tagList(
        
        br(),
        
        selectInput(ns("NTDoperators"), label="Select a Transit Operator:", choices=ntd_operator_list, selected = "Community Transit"),
        
        hr(style = "border-top: 1px solid #000000;"),
        tags$div(class="chart_title", textOutput(ns("operator_boardings_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("operator_pre_pandemic_boardings_title")), 
            value = textOutput(ns("operator_pre_pandemic_boardings_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_current_boardings_title")), 
            value = textOutput(ns("operator_current_boardings_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_pandemic_share_boardings_title")), 
            value = textOutput(ns("operator_pandemic_share_boardings")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_recent_share_boardings_title")), 
            value = textOutput(ns("operator_recent_share_boardings")),
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
            echarts4rOutput(ns("ntd_operator_boardings_ytd_chart"))),
          nav_panel(
            "Annual: Jan-Dec",
            echarts4rOutput(ns("ntd_operator_boardings_annual_chart")))),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("operator_insights_boardings_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        tags$div(class="chart_title", textOutput(ns("operator_hours_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("operator_pre_pandemic_hours_title")), 
            value = textOutput(ns("operator_pre_pandemic_hours_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_current_hours_title")), 
            value = textOutput(ns("operator_current_hours_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_pandemic_share_hours_title")), 
            value = textOutput(ns("operator_pandemic_share_hours")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_recent_share_hours_title")), 
            value = textOutput(ns("operator_recent_share_hours")),
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
            echarts4rOutput(ns("ntd_operator_hours_ytd_chart"))),
          nav_panel(
            "Annual: Jan-Dec",
            echarts4rOutput(ns("ntd_operator_hours_annual_chart")))),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("operator_insights_hours_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        tags$div(class="chart_title", textOutput(ns("operator_miles_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("operator_pre_pandemic_miles_title")), 
            value = textOutput(ns("operator_pre_pandemic_miles_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_current_miles_title")), 
            value = textOutput(ns("operator_current_miles_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_pandemic_share_miles_title")), 
            value = textOutput(ns("operator_pandemic_share_miles")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_recent_share_miles_title")), 
            value = textOutput(ns("operator_recent_share_miles")),
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
            echarts4rOutput(ns("ntd_operator_miles_ytd_chart"))),
          nav_panel(
            "Annual: Jan-Dec",
            echarts4rOutput(ns("ntd_operator_miles_annual_chart")))),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("operator_insights_miles_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        tags$div(class="chart_title", textOutput(ns("operator_bph_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("operator_pre_pandemic_bph_title")), 
            value = textOutput(ns("operator_pre_pandemic_bph_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_current_bph_title")), 
            value = textOutput(ns("operator_current_bph_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_pandemic_share_bph_title")), 
            value = textOutput(ns("operator_pandemic_share_bph")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("operator_recent_share_bph_title")), 
            value = textOutput(ns("operator_recent_share_bph")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          )
        ),
        
        navset_card_tab(
          full_screen = TRUE,
          title = NULL,
          nav_panel(
            paste0("Year to Date: Jan-",latest_ntd_month),
            echarts4rOutput(ns("ntd_operator_bph_ytd_chart"))),
          nav_panel(
            "Annual: Jan-Dec",
            echarts4rOutput(ns("ntd_operator_bph_annual_chart")))),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("operator_insights_bph_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;")
      )
      }) 
  })  # end moduleServer
}
