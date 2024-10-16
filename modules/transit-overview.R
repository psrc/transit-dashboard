
transit_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitoverview"))
  )
}

transit_overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$transit_overview_text <- renderText({page_information(tbl=page_text, page_name="Transit", page_section = "Overview", page_info = "description")})
    
    # Overview UI
    output$transitoverview <- renderUI({
      tagList(
        br(),
        textOutput(ns("transit_overview_text")),
        br(),
      )
    })
  })  # end moduleServer
}
