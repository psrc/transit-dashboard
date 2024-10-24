
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
               name = str_replace_all(name, "yth", "Youth"),
               name = str_replace_all(name, "old", "Older Adults"),
               name = str_replace_all(name, "dis", "People with a Disability")) |>
        filter(name %in% c(efa_metric()))
      })
    
    # Section Titles
    output$trip_chart_title <- renderText(paste0(efa_metric(), ": ",buffer_metric(), " stops"))
    
    # Insights
    output$trip_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Trips", page_info = "description"))})
    
    # Value Box Titles & Values
    output$trip_pop_pre_pandemic_title <- renderUI(shiny::p(paste0(pre_pandemic, " EFA Population near ", buffer_metric()), style = "font-size: 1.25rem"))
    output$trip_pop_current_title <- renderUI(shiny::p(paste0(year(Sys.Date()), " EFA Population near ", buffer_metric()), style = "font-size: 1.25rem"))
    output$trip_pop_pandemic_share_title <- renderUI(shiny::p(paste0("% change from ", pre_pandemic), style = "font-size: 1.25rem"))
    output$trip_pop_recent_share_title <- renderUI(shiny::p(paste0("% change from ", base_yr), style = "font-size: 1.25rem"))
    
    output$trip_pop_pre_pandemic_value <- renderText({format(round((filtered_df() |> filter(year == as.numeric(pre_pandemic)) |> select("value") |> pull()), -1), nsmall=0, big.mark=",")})
    output$trip_pop_current_value <- renderText({format(round((filtered_df() |> filter(year == year(Sys.Date())) |> select("value") |> pull()), -1), nsmall=0, big.mark=",")})
    output$trip_pop_pandemic_share_value <- renderText({paste0(round(((filtered_df() |> filter(year == year(Sys.Date())) |> select("value") |> pull()) / (filtered_df() |> filter(year == as.numeric(pre_pandemic)) |> select("value") |> pull()))*100-100, 0),"%")})
    output$trip_pop_recent_share_value <- renderText({paste0(round(((filtered_df() |> filter(year == year(Sys.Date())) |> select("value") |> pull()) / (filtered_df() |> filter(year == as.numeric(base_yr)) |> select("value") |> pull()))*100-100, 1),"%")})
    
    # Charts & Maps
    output$transit_trip_chart <- renderEcharts4r({create_line_chart(df = transit_trip_data |> 
                                                                      filter(transit_buffer == buffer_metric() & buffer == buffer_dist()) |> 
                                                                      select("year", contains("share")) |>
                                                                      pivot_longer(cols = contains("share")) |>
                                                                      mutate(year = as.character(year), 
                                                                             name = str_remove_all(name, "_share"),
                                                                             name = str_replace_all(name, "population", "Total Population"),
                                                                             name = str_replace_all(name, "poc", "People of Color"),
                                                                             name = str_replace_all(name, "pov", "People with Lower Incomes"),
                                                                             name = str_replace_all(name, "lep", "People with Limited English"),
                                                                             name = str_replace_all(name, "yth", "Youth"),
                                                                             name = str_replace_all(name, "old", "Older Adults"),
                                                                             name = str_replace_all(name, "dis", "People with a Disability")) |>
                                                                      filter(name %in% c("Total Population", efa_metric())),
                                                                    x="year", y="value", fill="name",
                                                                    esttype="percent", top_padding = 50,
                                                                    color=c("#4C4C4C","#91268F"),
                                                                    left_align = '20%', title = "% of Population",
                                                                    legend = TRUE)})
    
    output$transit_trip_map <- renderLeaflet({create_stop_buffer_map(lyr=transit_trip_buffers, buffer_name = buffer_metric(), buffer_distance = buffer_dist())})

    # Tab layout
    output$transittrips <- renderUI({
      tagList(
 
        br(),
        
        layout_column_wrap(
          width = 1/3,
          selectInput(ns("TRIPbuffer"), label="Select a Trip Frequency:", choices=stop_trips_list, selected = "4 trips per hour"),
          selectInput(ns("TRIPrace"), label = "Select an Equity Focus Area", choices = efa_list, selected = "People of Color"),
          radioButtons(ns("TRIPdist"), label = "Select a Buffer Distance:", choiceNames = c("1/4 mile", "1/2 mile"), choiceValues = c(0.25, 0.50), inline = TRUE)
        ),
        
        hr(style = "border-top: 1px solid #000000;"),
        tags$div(class="chart_title", textOutput(ns("trip_chart_title"))),
        br(),
        
        layout_column_wrap(
          width = 0.25,
          value_box(
            title = htmlOutput(ns("trip_pop_pre_pandemic_title")), 
            textOutput(ns("trip_pop_pre_pandemic_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("trip_pop_current_title")), 
            textOutput(ns("trip_pop_current_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("trip_pop_pandemic_share_title")), 
            textOutput(ns("trip_pop_pandemic_share_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          ),
          value_box(
            title = htmlOutput(ns("trip_pop_recent_share_title")), 
            textOutput(ns("trip_pop_recent_share_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = TRUE, fill = TRUE, height = NULL, align = "center"
          )
        ),
        
        br(),

        card(
          full_screen = TRUE,
          card_body(
            layout_columns(
              col_widths = c(7,5),
              echarts4rOutput(ns("transit_trip_chart")),
              leafletOutput(ns("transit_trip_map"))
            )
          )
        ),
        
        br(),
        tags$div(class="chart_source","Source: US Census Bureau ACS Data & GTFS Service Data"),

        hr(style = "border-top: 1px solid #000000;"),
        
        card(
          card_body(htmlOutput(ns("trip_insights_text")),
                    class = "insights_panel")
        ),
        
        hr(style = "border-top: 1px solid #000000;"),

      )
    }) 
  })  # end moduleServer
}
