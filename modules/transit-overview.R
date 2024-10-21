
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
        
        card(
          
          height = "450px",
          
          bs_carousel(id = "transit_photos", use_indicators = TRUE, use_controls = TRUE) |>
            bs_append(content = bs_carousel_image(src = "ct.jpg")) |>
            bs_append(content = bs_carousel_image(src = "kc-fast-ferry.jpg")) |>
            bs_append(content = bs_carousel_image(src = "et.jpg")) |>
            bs_append(content = bs_carousel_image(src = "line1.jpg")) |>
            bs_append(content = bs_carousel_image(src = "wsf.jpg")) |>
            bs_append(content = bs_carousel_image(src = "kcm.jpg")) |>
            bs_append(content = bs_carousel_image(src = "pt.jpg")) |>
            bs_append(content = bs_carousel_image(src = "slu.jpg")) |>
            bs_append(content = bs_carousel_image(src = "line2.jpg")) |>
            bs_append(content = bs_carousel_image(src = "kt-fast-ferry.jpg")) |>
            bs_append(content = bs_carousel_image(src = "sounder.jpg"))
          
        )
      )
    })
  })  # end moduleServer
}
