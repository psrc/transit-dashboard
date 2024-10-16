# Create the left panel

# https://chsamii.medium.com/how-to-focus-on-a-non-input-element-96db50b36e97
# https://codepen.io/shiroshiro/pen/Rwmgqxv

left_panel_ui <- function(id) {
  ns <- NS(id)
  
  cdf <- left_panel_info |> 
    select(starts_with('contact'))
  
  links_withtags <- withTags(
    map2(transit_links[1:8], names(transit_links)[1:8], 
         ~div(class = "links-container", tags$a(class = "links", href = .x, .y, tabindex="0", target = "_blank")))
  )

  tagList(
    
    div("Transit Resources",
        class = "m-menu__title"),
    
    div(a(class = "source_url left-panel-url", 
          href = "https://www.transit.dot.gov/ntd/ntd-data", 
          "National Transit Database", 
          target="_blank"),
        class = "focus",
        tabindex="0"),
    
    div(a(class = "source_url left-panel-url", 
          "Transit Planning at PSRC",
          href = "https://www.psrc.org/our-work/transit",
          target = "_blank"),
        class = "focus",
        tabindex="0"),
    
    bsCollapse(id = "transit-collapse", 
               open = NULL,
               bsCollapsePanel("Transit Agency Websites",
                               links_withtags
                               )
               ),
    
    # Contact ----
    contact_container_ui('contact-data',
                         name = cdf$contact_name, 
                         title = cdf$contact_title, 
                         email = cdf$contact_email, 
                         phone = cdf$contact_phone)
    
  ) # end taglist
}

left_panel_server <- function(id, page_nm) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns

    
  }) # end moduleServer
  
}
