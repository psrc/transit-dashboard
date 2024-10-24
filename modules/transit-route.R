
transit_route_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitroute"))
  )
}

transit_route_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Charts & Maps
    output$transit_route_map <- renderLeaflet({create_route_map()})

    # Tab layout
    output$transitroute <- renderUI({
      tagList(
 
        br(),
        # fluidRow(column(12, sliderInput(ns("ROUTEyear"),
        #                                 label="Select Transit Year", 
        #                                 min = 2016, max = 2024, value = 2024, 
        #                                 sep = NULL, step = 1, 
        #                                 animate = TRUE, ticks = FALSE))),
        
        card(
          full_screen = TRUE,
              leafletOutput(ns("transit_route_map"))
        ),
        
        #fluidRow(column(12,leafletOutput(ns("transit_route_map")))),
        br(),
        tags$div(class="chart_source","Source: GTFS Service Data"),
        br(),
        
      )
    }) 
  })  # end moduleServer
}
