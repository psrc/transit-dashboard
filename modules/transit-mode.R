
transit_mode_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitmode"))
  )
}

transit_mode_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Metric
    mode_metric <- reactive({input$NTDModes})
    
    # Section Titles
    output$mode_boardings_chart_title <- renderText(paste0("Regionwide ", mode_metric(), " Boardings"))
    output$mode_hours_chart_title <- renderText(paste0("Regionwide ", mode_metric(), " Revenue-Hours"))
    output$mode_miles_chart_title <- renderText(paste0("Regionwide ",mode_metric(), " Revenue-Miles"))
    output$mode_bph_chart_title <- renderText(paste0("Regionwide ",mode_metric(), " Boardings per Hour"))
     
    # Insights
    output$mode_insights_boardings_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Boardings", page_info = "description"))})
    output$mode_insights_hours_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Hours", page_info = "description"))})
    output$mode_insights_miles_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Miles", page_info = "description"))})
    output$mode_insights_bph_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-BPH", page_info = "description"))})
     
    # Value Box Titles & Values - Boardings
    output$mode_pre_pandemic_boardings_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Boardings"), style = "font-size: 1.25rem"))
    output$mode_current_boardings_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Boardings"), style = "font-size: 1.25rem"))
    output$mode_pandemic_share_boardings_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Boardings"), style = "font-size: 1.25rem"))
    output$mode_recent_share_boardings_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$mode_pre_pandemic_boardings_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$mode_current_boardings_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$mode_pandemic_share_boardings <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$mode_recent_share_boardings <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Hours
    output$mode_pre_pandemic_hours_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Revenue Hours"), style = "font-size: 1.25rem"))
    output$mode_current_hours_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Revenue Hours"), style = "font-size: 1.25rem"))
    output$mode_pandemic_share_hours_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Revenue Hours"), style = "font-size: 1.25rem"))
    output$mode_recent_share_hours_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$mode_pre_pandemic_hours_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$mode_current_hours_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$mode_pandemic_share_hours <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$mode_recent_share_hours <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Miles
    output$mode_pre_pandemic_miles_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Revenue Miles"), style = "font-size: 1.25rem"))
    output$mode_current_miles_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Revenue Miles"), style = "font-size: 1.25rem"))
    output$mode_pandemic_share_miles_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Revenue Miles"), style = "font-size: 1.25rem"))
    output$mode_recent_share_miles_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$mode_pre_pandemic_miles_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$mode_current_miles_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$mode_pandemic_share_miles <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$mode_recent_share_miles <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Boardings per Hour
    output$mode_pre_pandemic_bph_title <- renderUI(shiny::p(paste0(pre_pandemic, " YTD Boardings per Hour"), style = "font-size: 1.25rem"))
    output$mode_current_bph_title <- renderUI(shiny::p(paste0(year(Sys.Date())," YTD Boardings per Hour"), style = "font-size: 1.25rem"))
    output$mode_pandemic_share_bph_title <- renderUI(shiny::p(paste0("% of ", pre_pandemic, " YTD Boardings per HOur"), style = "font-size: 1.25rem"))
    output$mode_recent_share_bph_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$mode_pre_pandemic_bph_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 1))})
    output$mode_current_bph_value <- renderText({paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()), 1))})
    output$mode_pandemic_share_bph <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$mode_recent_share_bph <- renderText(paste0(round((ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    
    # Charts & Maps
    output$ntd_mode_boardings_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                               filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                               mutate(year=as.character(year)),
                                                                             x = "year", y = "estimate", fill = "metric", 
                                                                             esttype = "number", color = c("#F05A28"),
                                                                             left_align = '20%', title = "Boardings", legend=FALSE)})
    
    output$ntd_mode_boardings_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                                  filter(variable == mode_metric() & geography == "Region" & metric == "Boardings" & grouping == "Annual") |>
                                                                                  mutate(year=as.character(year)),
                                                                                x = "year", y = "estimate", fill = "metric", 
                                                                                esttype = "number", color = c("#F7A489"),
                                                                                left_align = '20%', title = "Boardings", legend=FALSE)})
    
    
    
    output$ntd_mode_hours_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                           filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                           mutate(year=as.character(year)),
                                                                         x = "year", y = "estimate", fill = "metric", 
                                                                         esttype = "number", color = c("#91268F"),
                                                                         left_align = '20%', title = "Revenue-Hours", legend=FALSE)})
    
    output$ntd_mode_hours_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                              filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Hours" & grouping == "Annual") |>
                                                                              mutate(year=as.character(year)),
                                                                            x = "year", y = "estimate", fill = "metric", 
                                                                            esttype = "number", color = c("#C388C2"),
                                                                            left_align = '20%', title = "Revenue-Hours", legend=FALSE)})
    
    output$ntd_mode_miles_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                           filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                           mutate(year=as.character(year)),
                                                                         x = "year", y = "estimate", fill = "metric", 
                                                                            esttype = "number", color = c("#8CC63E"),
                                                                            left_align = '20%', title = "Revenue-Miles", legend=FALSE)})
    
    output$ntd_mode_miles_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                              filter(variable == mode_metric() & geography == "Region" & metric == "Revenue-Miles" & grouping == "Annual") |>
                                                                              mutate(year=as.character(year)),
                                                                            x = "year", y = "estimate", fill = "metric",
                                                                            esttype = "number", color = c("#C0E095"),
                                                                            left_align = '20%', title = "Revenue-Miles", legend=FALSE)})
    
    output$ntd_mode_bph_ytd_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |> 
                                                                         filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & grouping == paste0("Year to Date: Jan-",latest_ntd_month)) |>
                                                                         mutate(year=as.character(year)),
                                                                       x = "year", y = "estimate", dec=1, fill = "metric", 
                                                                       esttype = "number", color = c("#00A7A0"),
                                                                       left_align = '20%', title = "Boardings per Hour", legend=FALSE)})
    
    output$ntd_mode_bph_annual_chart <- renderEcharts4r({create_bar_chart(df = ntd_data |>
                                                                            filter(variable == mode_metric() & geography == "Region" & metric == "Boardings-per-Hour" & grouping == "Annual") |>
                                                                            mutate(year=as.character(year)),
                                                                          x = "year", y = "estimate", dec=1, fill = "metric", 
                                                                          esttype = "number", color = c("#73CFCB"),
                                                                          left_align = '20%', title = "Boardings per Hour", legend=FALSE)})
    
    # Tab layout
    output$transitmode <- renderUI({
      tagList(
        
        br(),
        
        selectInput(ns("NTDModes"), label="Select a Transit Mode:", choices=ntd_mode_list, selected = "Bus"),
        
        hr(style = "border-top: 1px solid #000000;"),
        tags$div(class="chart_title", textOutput(ns("mode_boardings_chart_title"))),
        br(),
         
        layout_column_wrap(
                       width = 0.25,
                       value_box(
                         title = htmlOutput(ns("mode_pre_pandemic_boardings_title")), 
                         value = textOutput(ns("mode_pre_pandemic_boardings_value")),
                         theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
                         showcase = NULL, showcase_layout = "left center",
                         full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
                       ),
                       value_box(
                         title = htmlOutput(ns("mode_current_boardings_title")), 
                         value = textOutput(ns("mode_current_boardings_value")),
                         theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
                         showcase = NULL, showcase_layout = "left center",
                         full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
                       ),
                       value_box(
                         title = htmlOutput(ns("mode_pandemic_share_boardings_title")), 
                         value = textOutput(ns("mode_pandemic_share_boardings")),
                         theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
                         showcase = NULL, showcase_layout = "left center",
                         full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
                       ),
                       value_box(
                         title = htmlOutput(ns("mode_recent_share_boardings_title")), 
                         value = textOutput(ns("mode_recent_share_boardings")),
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
           echarts4rOutput(ns("ntd_mode_boardings_ytd_chart"))),
         nav_panel(
           "Annual: Jan-Dec",
           echarts4rOutput(ns("ntd_mode_boardings_annual_chart")))),
         
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("mode_insights_boardings_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        tags$div(class="chart_title", textOutput(ns("mode_hours_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("mode_pre_pandemic_hours_title")), 
            value = textOutput(ns("mode_pre_pandemic_hours_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_current_hours_title")), 
            value = textOutput(ns("mode_current_hours_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_pandemic_share_hours_title")), 
            value = textOutput(ns("mode_pandemic_share_hours")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_recent_share_hours_title")), 
            value = textOutput(ns("mode_recent_share_hours")),
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
           echarts4rOutput(ns("ntd_mode_hours_ytd_chart"))),
         nav_panel(
           "Annual: Jan-Dec",
           echarts4rOutput(ns("ntd_mode_hours_annual_chart")))),
         
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("mode_insights_hours_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
         
        tags$div(class="chart_title", textOutput(ns("mode_miles_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("mode_pre_pandemic_miles_title")), 
            value = textOutput(ns("mode_pre_pandemic_miles_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_current_miles_title")), 
            value = textOutput(ns("mode_current_miles_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_pandemic_share_miles_title")), 
            value = textOutput(ns("mode_pandemic_share_miles")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_recent_share_miles_title")), 
            value = textOutput(ns("mode_recent_share_miles")),
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
           echarts4rOutput(ns("ntd_mode_miles_ytd_chart"))),
         nav_panel(
           "Annual: Jan-Dec",
           echarts4rOutput(ns("ntd_mode_miles_annual_chart")))),
         
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("mode_insights_miles_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
         
        tags$div(class="chart_title", textOutput(ns("mode_bph_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("mode_pre_pandemic_bph_title")), 
            value = textOutput(ns("mode_pre_pandemic_bph_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_current_bph_title")), 
            value = textOutput(ns("mode_current_bph_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_pandemic_share_bph_title")), 
            value = textOutput(ns("mode_pandemic_share_bph")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("mode_recent_share_bph_title")), 
            value = textOutput(ns("mode_recent_share_bph")),
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
           echarts4rOutput(ns("ntd_mode_bph_ytd_chart"))),
         nav_panel(
           "Annual: Jan-Dec",
           echarts4rOutput(ns("ntd_mode_bph_annual_chart")))),
        
         br(),
         tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
         
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("mode_insights_bph_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;")
      )
    }) 
  })  # end moduleServer
}
