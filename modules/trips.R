
transit_trips_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transittrips"))
  )
}

transit_trips_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Metric
    buffer_metric <- reactive({input$TRIPbuffer})
    efa_metric <- reactive({input$TRIPrace})
    buffer_dist <- reactive({input$TRIPdist})
    
    filtered_df <- reactive({
      transit_trip_data |> 
        filter(transit_buffer == buffer_metric() & buffer == buffer_dist()) |> 
        select("year", !(contains("share"))) |>
        pivot_longer(cols = !c(year, transit_buffer)) |>
        mutate(year = as.character(year), 
               name = str_remove_all(name, "_share"),
               name = str_replace_all(name, "population", "Total Population"),
               name = str_replace_all(name, "poc", "People of Color"),
               name = str_replace_all(name, "pov", "People with Lower Incomes"),
               name = str_replace_all(name, "lep", "People with Limited English"),
               name = str_replace_all(name, "yth", "People under 18"),
               name = str_replace_all(name, "old", "People over 65"),
               name = str_replace_all(name, "dis", "People with a Disability")) |>
        filter(name %in% c(efa_metric()))
      })
    
    filtered_chart_df <- reactive({
      transit_trip_data |> 
        filter(transit_buffer == buffer_metric() & buffer == buffer_dist()) |> 
        select("year", "transit_buffer", contains("share")) |>
        pivot_longer(cols = !c(year, transit_buffer)) |>
        mutate(name = str_remove_all(name, "_share"),
               name = str_replace_all(name, "population", "Total Population"),
               name = str_replace_all(name, "poc", "People of Color"),
               name = str_replace_all(name, "pov", "People with Lower Incomes"),
               name = str_replace_all(name, "lep", "People with Limited English"),
               name = str_replace_all(name, "yth", "People under 18"),
               name = str_replace_all(name, "old", "People over 65"),
               name = str_replace_all(name, "dis", "People with a Disability")) |>
        filter(name %in% c("Total Population", efa_metric()))
    })
    
    # Section Titles
    output$chart_title <- renderText(paste0(efa_metric(), ": ",buffer_metric(), " stops"))
    
    # Insights
    output$insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Trips", page_info = "description"))})
    
    # Value Box Titles & Values
    output$pop_pre_pandemic_title <- renderUI(paste0("people in ", pre_pandemic, " near a transit stop with at least ", buffer_metric()))
    output$pop_current_title <- renderUI(paste0("people in ", gtfs_year, " near a transit stop with at least ", buffer_metric()))
    output$pop_pandemic_share_title <- renderUI(paste0("% change from ", pre_pandemic))
    output$pop_recent_share_title <- renderUI(paste0("% change from ", base_yr))
    
    output$pop_pre_pandemic_value <- renderText({format(round((filtered_df() |> filter(year == as.numeric(pre_pandemic)) |> select("value") |> pull()), -1), nsmall=0, big.mark=",")})
    output$pop_current_value <- renderText({format(round((filtered_df() |> filter(year == year(Sys.Date())) |> select("value") |> pull()), -1), nsmall=0, big.mark=",")})
    output$pop_pandemic_share_value <- renderText({paste0(round(((filtered_df() |> filter(year == year(Sys.Date())) |> select("value") |> pull()) / (filtered_df() |> filter(year == as.numeric(pre_pandemic)) |> select("value") |> pull()))*100-100, 0),"%")})
    output$pop_recent_share_value <- renderText({paste0(round(((filtered_df() |> filter(year == year(Sys.Date())) |> select("value") |> pull()) / (filtered_df() |> filter(year == as.numeric(base_yr)) |> select("value") |> pull()))*100-100, 1),"%")})
    
    # Charts & Maps
    output$transit_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_line_chart(df = filtered_chart_df(), x = "year", y = "value", fill = "name", ymax = max(filtered_chart_df()$value)*1.2,labels=scales::label_percent(), colors = c("#91268F", "#4C4C4C")), legend=TRUE)
      
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
    
    output$transit_map <- renderLeaflet({create_stop_buffer_map(lyr=transit_trip_buffers, buffer_name = buffer_metric(), buffer_distance = buffer_dist())})

    # Tab layout
    output$transittrips <- renderUI({
      tagList(
        
        br(),
        
        layout_column_wrap(
          width = 1/3,
          selectInput(ns("TRIPbuffer"), label="Select a Trip Frequency:", choices=stop_trips_list, selected = "4 trips per hour"),
          selectInput(ns("TRIPrace"), label = "Select a Population of Interest", choices = efa_list, selected = "People of Color"),
          radioButtons(ns("TRIPdist"), label = "Select a Buffer Distance:", choiceNames = c("1/4 mile", "1/2 mile"), choiceValues = c(0.25, 0.50), inline = TRUE)
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        h1(textOutput(ns("chart_title"))),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("pop_pre_pandemic_title")), 
            textOutput(ns("pop_pre_pandemic_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pop_current_title")), 
            textOutput(ns("pop_current_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pop_pandemic_share_title")), 
            textOutput(ns("pop_pandemic_share_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pop_recent_share_title")), 
            textOutput(ns("pop_recent_share_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          )
        ),
        
        br(),
        
        card(
          full_screen = TRUE,
          card_body(
            layout_columns(
              col_widths = c(7,5),
              plotlyOutput(ns("transit_chart")),
              leafletOutput(ns("transit_map"))
            )
          )
        ),
        
        br(),
        tags$div(class="chart_source","Source: US Census Bureau ACS Data, OFM Small Area Estimate & General Transit Feed Specification (GTFS) service data"),
        
        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("insights_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) 
  })  # end moduleServer
}
