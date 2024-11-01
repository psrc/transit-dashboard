
transit_route_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitroute"))
  )
}

transit_route_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$route_map_title <- renderText(paste0(str_to_title(service_change)," ", gtfs_year, " Transit Routes by Mode"))
    output$route_map_source <- renderText(paste0("Source: ",str_to_title(service_change)," ", gtfs_year, " General Transit Feed Specification (GTFS) data by Transit Agency"))
    
    # Charts & Maps
    output$transit_route_map <- renderLeaflet({create_route_map()})

    # Tab layout
    output$transitroute <- renderUI({
      tagList(
        
        h1(textOutput(ns("route_map_title"))),

        card(
          full_screen = TRUE,
              leafletOutput(ns("transit_route_map"))
        ),
        
        br(), 
        tags$div(class="chart_source", textOutput(ns("route_map_source"))),
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) 
  })  # end moduleServer
}
