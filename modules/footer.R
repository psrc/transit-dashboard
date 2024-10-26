# Display footer

footer_ui <- function(id) {
  ns <- NS(id)
  
  mission <- "Our mission is to advance solutions to achieve a thriving, racially equitable, 
      and sustainable central Puget Sound region through leadership, visionary planning, and collaboration."
  
  tagList( 
    
    div(class = "footer-container",
        div(class = "details-container",
            div(class = 'details',
                tags$article(
                  h3("About PSRC"),
                  p(mission)
                ),
                tags$article(div()),
                tags$article(
                  h3("Connect with PSRC"),
                  div(class = "psrc-contacts",
                      
                      div(class = "psrc-location",
                          icon("location-dot"),
                          div(div("1201 Third Avenue, Suite 500"), "Seattle, WA 98101-3055")
                      ),
                      
                      div(icon("envelope"),
                          tags$a(class = "footer_url", href = paste0("mailto:","info@psrc.org","?"), "info@psrc.org")),
                      div(
                        icon("phone-volume"),  
                        "206-464-7090")
                  ) # end div
                )
                
            )
        )
        
        ,
        
        div(class = "soc-media-container",
            div(class = 'soc-media',
                div(
                  a(class = "footer_url", href="https://www.facebook.com/PugetSoundRegionalCouncil", icon("facebook"), target="_blank"),
                  a(class = "footer_url", href="https://twitter.com/SoundRegion", icon("x-twitter"), target="_blank"),
                  a(class = "footer_url", href="https://www.instagram.com/soundregion/", icon("instagram"), target="_blank"),
                  a(class = "footer_url", href="https://www.linkedin.com/company/soundregion", icon("linkedin"), target="_blank")
                ),
                div(
                  p("Dashboard by ", a(class = "footer_url", "PSRC Data Science"))
                )
            ) 
        )
        
    )
    
  )
  
}

footer_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
  }) # end moduleServer
  
}