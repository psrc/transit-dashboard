
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
    period_metric <- reactive({input$NTDPeriod})
    
    filtered_df <- reactive({
      ntd_data |> 
        filter(variable == "All Transit Modes" & geography == "Region" & metric == region_metric() & grouping == period_metric())
    })
    
    output$region_chart_title <- renderText(paste0("Regionwide Transit ", region_metric(), " for all Transit Modes"))
    
    # Insights
    output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
    
    # Output Values
    output$region_pre_pandemic_metric <- renderUI(paste0("2019 YTD ", region_metric()))
    output$region_current_metric <- renderUI(paste0(year(Sys.Date())," YTD ", region_metric()))
    output$region_pandemic_share_metric <- renderUI(paste0("% of ", pre_pandemic, " YTD ", region_metric()))
    output$region_recent_share_metric <- renderUI(paste0("% change from ", base_yr))
    
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
    output$ntd_region_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_column_chart(df = filtered_df(), x = "year", y = "estimate", fill = "metric", colors = c("#00A7A0")), legend=TRUE)
      
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
    output$transitregion <- renderUI({
      tagList(

        selectInput(ns("NTDMetric"), label="Select a Transit Metric:", choices=ntd_metric_list, selected = "Boardings"),
          
        hr(style = "border-top: 1px solid #000000;"),
        
        h2(textOutput(ns("region_chart_title"))),
        
        layout_column_wrap(
          width = 1/4,
          value_box(
            title = htmlOutput(ns("region_pre_pandemic_metric")), 
            value = textOutput(ns("region_pre_pandemic_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("region_current_metric")), 
            value = textOutput(ns("region_current_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("region_pandemic_share_metric")), 
            value = textOutput(ns("region_pre_pandemic_share")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("region_recent_share_metric")), 
            value = textOutput(ns("region_recent_growth")),
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
            radioButtons(ns("NTDPeriod"), label = NULL, choices = c(paste0("Year to Date: Jan-",latest_ntd_month), "Annual"), inline = TRUE)
            ),
          
          plotlyOutput(ns("ntd_region_chart"))
        ),

        br(),
        
        tags$div(class = "chart_source", "Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("region_insights_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) 
  })  # end moduleServer
}
