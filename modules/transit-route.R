
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
 
        hr(style = "border-top: 1px solid #000000;"),
        tags$div(class="chart_title", "Fall 2024 Transit Routes"),
        br(),

        card(
          full_screen = TRUE,
              leafletOutput(ns("transit_route_map"))
        ),
        
        br(),
        tags$div(class="chart_source","Source: GTFS Service Data"),
        br(),
        
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) 
  })  # end moduleServer
}
