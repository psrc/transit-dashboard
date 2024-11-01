
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
    
    period_boardings <- reactive({input$PERIODBoardings})
    period_hours <- reactive({input$PERIODHours})
    period_miles <- reactive({input$PERIODMiles})
    period_bph <- reactive({input$PERIODBph})
    
    filtered_boardings_df <- reactive({
      ntd_data |> 
        filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & grouping == period_boardings())
    })
    
    filtered_hours_df <- reactive({
      ntd_data |> 
        filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & grouping == period_hours())
    })
    
    filtered_miles_df <- reactive({
      ntd_data |> 
        filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & grouping == period_miles())
    })
    
    filtered_bph_df <- reactive({
      ntd_data |> 
        filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & grouping == period_bph())
    })
    
    # Section Titles
    output$boardings_chart_title <- renderText(paste0(operator_metric(), " Boardings"))
    output$hours_chart_title <- renderText(paste0(operator_metric(), " Revenue-Hours"))
    output$miles_chart_title <- renderText(paste0(operator_metric(), " Revenue-Miles"))
    output$bph_chart_title <- renderText(paste0(operator_metric(), " Boardings per Hour"))
    
    # Insights
    output$insights_boardings_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Boardings", page_info = "description"))})
    output$insights_hours_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Hours", page_info = "description"))})
    output$insights_miles_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Miles", page_info = "description"))})
    output$insights_bph_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-BPH", page_info = "description"))})
    
    # Value Box Titles & Values - Boardings
    output$pre_pandemic_boardings_title <- renderUI(paste0(pre_pandemic, " YTD Boardings"))
    output$current_boardings_title <- renderUI(paste0(year(Sys.Date())," YTD Boardings"))
    output$pandemic_share_boardings_title <- renderUI(paste0("% of ", pre_pandemic, " YTD Boardings"))
    output$recent_share_boardings_title <- renderUI(paste0("% change from ", base_yr))
    
    output$pre_pandemic_boardings_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$current_boardings_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$pandemic_share_boardings <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$recent_share_boardings <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Hours
    output$pre_pandemic_hours_title <- renderUI(paste0(pre_pandemic, " YTD Revenue Hours"))
    output$current_hours_title <- renderUI(paste0(year(Sys.Date())," YTD Revenue Hours"))
    output$pandemic_share_hours_title <- renderUI(paste0("% of ", pre_pandemic, " YTD Revenue Hours"))
    output$recent_share_hours_title <- renderUI(paste0("% change from ", base_yr))
    
    output$pre_pandemic_hours_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$current_hours_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$pandemic_share_hours <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$recent_share_hours <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Hours" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Miles
    output$pre_pandemic_miles_title <- renderUI(paste0(pre_pandemic, " YTD Revenue Miles"))
    output$current_miles_title <- renderUI(paste0(year(Sys.Date())," YTD Revenue Miles"))
    output$pandemic_share_miles_title <- renderUI(paste0("% of ", pre_pandemic, " YTD Revenue Miles"))
    output$recent_share_miles_title <- renderUI(paste0("% change from ", base_yr))
    
    output$pre_pandemic_miles_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$current_miles_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull())/1000000, 1), "M")})
    output$pandemic_share_miles <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$recent_share_miles <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Revenue-Miles" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Value Box Titles & Values - Boardings per Hour
    output$pre_pandemic_bph_title <- renderUI(paste0(pre_pandemic, " YTD Boardings per Hour"))
    output$current_bph_title <- renderUI(paste0(year(Sys.Date())," YTD Boardings per Hour"))
    output$pandemic_share_bph_title <- renderUI(paste0("% of ", pre_pandemic, " YTD Boardings per HOur"))
    output$recent_share_bph_title <- renderUI(paste0("% change from ", base_yr))
    
    output$pre_pandemic_bph_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 1))})
    output$current_bph_value <- renderText({paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()), 1))})
    output$pandemic_share_bph <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == pre_pandemic & grouping != "Annual") |> select("estimate") |> pull()), 3)*100, "%"))
    output$recent_share_bph <- renderText(paste0(round((ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == as.character(year(Sys.Date())) & grouping != "Annual") |> select("estimate") |> pull()) / (ntd_data |> filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & geography == operator_metric() & metric == "Boardings-per-Hour" & year == base_yr & grouping != "Annual") |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Charts & Maps
    output$boardings_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_column_chart(df = filtered_boardings_df(), x = "year", y = "estimate", fill = "metric", colors = c("#F05A28")), legend=TRUE)
      
      # Use onRender to apply JavaScript for responsiveness
      p %>% onRender("
      function(el, x) {
        var resizeLabels = function() {
          var layout = el.layout;
          var width = el.clientWidth;
          var fontSize = width < 600 ? 10 : width < 800 ? 11 : 12;
          var numTicks = width < 600 ? 2 : width < 800 ? 2 : 1;
          var legendSize = width < 600 ? 12 : width < 800 ? 14 : 20;
          
          layout.xaxis = { dtick: numTicks };
          layout.xaxis.tickfont = { size: fontSize};
          layout.yaxis.tickfont = { size: fontSize };
          layout.legend.font = {size: legendSize};
          
          Plotly.relayout(el, layout);
        };
        
        // Run the function initially and on window resize
        resizeLabels();
        window.addEventListener('resize', resizeLabels);
      }
    ")
      
    })
    
    output$hours_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_column_chart(df = filtered_hours_df(), x = "year", y = "estimate", fill = "metric", colors = c("#91268F")), legend=TRUE)
      
      # Use onRender to apply JavaScript for responsiveness
      p %>% onRender("
      function(el, x) {
        var resizeLabels = function() {
          var layout = el.layout;
          var width = el.clientWidth;
          var fontSize = width < 600 ? 10 : width < 800 ? 11 : 12;
          var numTicks = width < 600 ? 2 : width < 800 ? 2 : 1;
          var legendSize = width < 600 ? 12 : width < 800 ? 14 : 20;
          
          layout.xaxis = { dtick: numTicks };
          layout.xaxis.tickfont = { size: fontSize};
          layout.yaxis.tickfont = { size: fontSize };
          layout.legend.font = {size: legendSize};
          
          Plotly.relayout(el, layout);
        };
        
        // Run the function initially and on window resize
        resizeLabels();
        window.addEventListener('resize', resizeLabels);
      }
    ")
      
    })
    
    output$miles_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_column_chart(df = filtered_miles_df(), x = "year", y = "estimate", fill = "metric", colors = c("#8CC63E")), legend=TRUE)
      
      # Use onRender to apply JavaScript for responsiveness
      p %>% onRender("
      function(el, x) {
        var resizeLabels = function() {
          var layout = el.layout;
          var width = el.clientWidth;
          var fontSize = width < 600 ? 10 : width < 800 ? 11 : 12;
          var numTicks = width < 600 ? 2 : width < 800 ? 2 : 1;
          var legendSize = width < 600 ? 12 : width < 800 ? 14 : 20;
          
          layout.xaxis = { dtick: numTicks };
          layout.xaxis.tickfont = { size: fontSize};
          layout.yaxis.tickfont = { size: fontSize };
          layout.legend.font = {size: legendSize};
          
          Plotly.relayout(el, layout);
        };
        
        // Run the function initially and on window resize
        resizeLabels();
        window.addEventListener('resize', resizeLabels);
      }
    ")
      
    })
    
    output$bph_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_column_chart(df = filtered_bph_df(), x = "year", y = "estimate", fill = "metric", colors = c("#00A7A0")), legend=TRUE)
      
      # Use onRender to apply JavaScript for responsiveness
      p %>% onRender("
      function(el, x) {
        var resizeLabels = function() {
          var layout = el.layout;
          var width = el.clientWidth;
          var fontSize = width < 600 ? 10 : width < 800 ? 11 : 12;
          var numTicks = width < 600 ? 2 : width < 800 ? 2 : 1;
          var legendSize = width < 600 ? 12 : width < 800 ? 14 : 20;
          
          layout.xaxis = { dtick: numTicks };
          layout.xaxis.tickfont = { size: fontSize};
          layout.yaxis.tickfont = { size: fontSize };
          layout.legend.font = {size: legendSize};
          
          Plotly.relayout(el, layout);
        };
        
        // Run the function initially and on window resize
        resizeLabels();
        window.addEventListener('resize', resizeLabels);
      }
    ")
      
    })
    
    
    
    
    # Tab layout
    output$transitoperator <- renderUI({
      tagList(
        
        br(),
        
        selectInput(ns("NTDoperators"), label="Select a Transit Operator:", choices=ntd_operator_list, selected = "Community Transit"),
        
        # Boardings section
        hr(style = "border-top: 1px solid #000000;"),
        h2(textOutput(ns("boardings_chart_title"))),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("pre_pandemic_boardings_title")), 
            value = textOutput(ns("pre_pandemic_boardings_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("current_boardings_title")), 
            value = textOutput(ns("current_boardings_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pandemic_share_boardings_title")), 
            value = textOutput(ns("pandemic_share_boardings")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("recent_share_boardings_title")), 
            value = textOutput(ns("recent_share_boardings")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          )
        ),
        
        br(),
        
        card(
          full_screen = FALSE,
          
          layout_column_wrap(
            width = 1,
            radioButtons(ns("PERIODBoardings"), label = NULL, choices = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual"), inline = TRUE)
          ),
          
          plotlyOutput(ns("boardings_chart"))
        ),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("insights_boardings_text")),
                    class = "insights_panel")
        ),
        
        # Revenue-Hours section
        hr(style = "border-top: 1px solid #000000;"),
        h2(textOutput(ns("hours_chart_title"))),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("pre_pandemic_hours_title")), 
            value = textOutput(ns("pre_pandemic_hours_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("current_hours_title")), 
            value = textOutput(ns("current_hours_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pandemic_share_hours_title")), 
            value = textOutput(ns("pandemic_share_hours")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("recent_share_hours_title")), 
            value = textOutput(ns("recent_share_hours")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          )
        ),
        
        br(),
        
        card(
          full_screen = FALSE,
          
          layout_column_wrap(
            width = 1,
            radioButtons(ns("PERIODHours"), label = NULL, choices = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual"), inline = TRUE)
          ),
          
          plotlyOutput(ns("hours_chart"))
        ),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("insights_hours_text")),
                    class = "insights_panel")
        ),
        
        # Revenue-Miles section
        hr(style = "border-top: 1px solid #000000;"),
        h2(textOutput(ns("miles_chart_title"))),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("pre_pandemic_miles_title")), 
            value = textOutput(ns("pre_pandemic_miles_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("current_miles_title")), 
            value = textOutput(ns("current_miles_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pandemic_share_miles_title")), 
            value = textOutput(ns("pandemic_share_miles")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("recent_share_miles_title")), 
            value = textOutput(ns("recent_share_miles")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          )
        ),
        
        br(),
        
        card(
          full_screen = FALSE,
          
          layout_column_wrap(
            width = 1,
            radioButtons(ns("PERIODMiles"), label = NULL, choices = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual"), inline = TRUE)
          ),
          
          plotlyOutput(ns("miles_chart"))
        ),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("insights_miles_text")),
                    class = "insights_panel")
        ),
        
        # Boardings-per-Hour section
        hr(style = "border-top: 1px solid #000000;"),
        h2(textOutput(ns("bph_chart_title"))),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("pre_pandemic_bph_title")), 
            value = textOutput(ns("pre_pandemic_bph_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("current_bph_title")), 
            value = textOutput(ns("current_bph_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pandemic_share_bph_title")), 
            value = textOutput(ns("pandemic_share_bph")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("recent_share_bph_title")), 
            value = textOutput(ns("recent_share_bph")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          )
        ),
        
        card(
          full_screen = FALSE,
          
          layout_column_wrap(
            width = 1,
            radioButtons(ns("PERIODBph"), label = NULL, choices = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual"), inline = TRUE)
          ),
          
          plotlyOutput(ns("bph_chart"))
        ),
        
        br(),
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("insights_bph_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;")
      )
      }) 
  })  # end moduleServer
}
