
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
    
    links_withtags <- withTags(
      map2(transit_links[1:8], names(transit_links)[1:8], 
           ~div(class = "links-container", tags$a(class = "links", href = .x, .y, tabindex="0", target = "_blank")))
    )
    
    # Overview UI
    output$transitoverview <- renderUI({
      tagList(
        
        br(),

        htmlOutput(ns("transit_overview_text")),
        br(),
        
        htmlOutput(ns("transit_hct_text")),
        br(),
        
        card(
          
          layout_columns(
            col_widths = c(4,8),
            
            card_body(
              div("Transit Resources", class = "m-menu__title"),
              div(a(class = "source_url left-panel-url", href = "https://www.transit.dot.gov/ntd/ntd-data", "National Transit Database", target="_blank"), class = "focus", tabindex="0"),
              div(a(class = "source_url left-panel-url", "Transit Planning at PSRC", href = "https://www.psrc.org/our-work/transit", target = "_blank"), class = "focus", tabindex="0"),
              bsCollapse(id = "transit-collapse", open = NULL, bsCollapsePanel("Transit Agency Websites", links_withtags))
              ),
            
            card_body(
              bs_carousel(id = "transit_photos", use_indicators = TRUE, use_controls = TRUE) |>
                bs_append(content = bs_carousel_image(src = "ct_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "kc-fast-ferry_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "wsf_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "kcm_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "pt_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "line2_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "kt-fast-ferry_adj.jpg")) |>
                bs_append(content = bs_carousel_image(src = "sounder_adj.jpg"))
              ),
            ), # end of layout_columns
          ), # end of card with links and carousel
        
        hr(style = "border-top: 1px solid #000000;")
        
      )
    })
  })  # end moduleServer
}
