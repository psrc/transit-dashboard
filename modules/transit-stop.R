
transit_stop_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitstop"))
  )
}

transit_stop_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Charts & Maps
    output$transit_stop_chart <- renderEcharts4r({create_line_chart(df = transit_buffer_data |> 
                                                                      filter(transit_buffer == input$STOPbuffer) |> 
                                                                      select("year", contains("share")) |>
                                                                      pivot_longer(cols = contains("share")) |>
                                                                      mutate(year = as.character(year), 
                                                                             name = str_remove_all(name, "_share"),
                                                                             name = str_replace_all(name, "population", "Total"),
                                                                             name = str_replace_all(name, "poc", "People of Color"),
                                                                             name = str_replace_all(name, "pov", "People with Lower Incomes"),
                                                                             name = str_replace_all(name, "lep", "People with Limited English"),
                                                                             name = str_replace_all(name, "yth", "Youth"),
                                                                             name = str_replace_all(name, "old", "Older Adults"),
                                                                             name = str_replace_all(name, "dis", "People with a Disability")) |>
                                                                      filter(name %in% c("Total", input$STOPrace)),
                                                                    x="year", y="value", fill="name",
                                                                    esttype="percent", top_padding = 50,
                                                                    color=c("#4C4C4C","#91268F","#8CC63E","#F05A28","#00A7A0","#C388C2","#C0E095"),
                                                                    left_align = '10%', title = "% of Population",
                                                                    legend = TRUE)})
    
    output$transit_stop_map <- renderLeaflet({create_stop_buffer_map(buffer = input$STOPbuffer, yr = input$STOPyear)})

    
    # Tab layout
    output$transitstop <- renderUI({
      tagList(
 
        br(),
        fluidRow(column(7, selectInput(ns("STOPbuffer"), label="Select Transit Type:", choices=stop_buffer_list, selected = "High-Capacity Transit")),
                 column(5, sliderInput(ns("STOPyear"), 
                                       label="Select Transit Year", 
                                       min = 2016, max = 2024, value = 2024, 
                                       sep = NULL, step = 1, 
                                       animate = TRUE, ticks = FALSE))),
        fluidRow(column(12, selectInput(ns("STOPrace"), 
                                         label = "Select Equity Focus Area", 
                                         choices = c("People of Color", "People with Lower Incomes", 
                                                     "People with Limited English", 
                                                     "Youth", "Older Adults", 
                                                     "People with a Disability"), 
                                         selected = "People of Color",
                                        width = '100%'))),
        fluidRow(column(7,echarts4rOutput(ns("transit_stop_chart"))),
                 column(5,leafletOutput(ns("transit_stop_map")))),
        br(),
        tags$div(class="chart_source","Source: US Census Bureau ACS Data & GTFS Service Data"),
        br(),
        
      )
    }) 
  })  # end moduleServer
}
