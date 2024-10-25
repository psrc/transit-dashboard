
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
    output$transit_overview_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Overview", page_info = "description"))})
    output$transit_hct_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Overview-HCT", page_info = "description"))})
    
    # Overview UI
    output$transitoverview <- renderUI({
      tagList(
        
        br(),

        htmlOutput(ns("transit_overview_text")),
        br(),
        
        card(
          
          card_body(
            layout_column_wrap(
              width= "300px",
              htmlOutput(ns("transit_hct_text")),
              bs_carousel(id = "transit_photos", use_indicators = TRUE, use_controls = TRUE) |>
                bs_append(content = bs_carousel_image(src = "ct_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "kc-fast-ferry_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "wsf_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "kcm_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "pt_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "line2_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "kt-fast-ferry_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "sounder_adj.jpg"))
            )
          )
          
          
          
        ),
        
        hr(style = "border-top: 1px solid #000000;")
        
      )
    })
  })  # end moduleServer
}
