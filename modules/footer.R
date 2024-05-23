# Display footer

footer_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('afooter'))
  )
  
}

footer_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$afooter <- renderUI({
      mission <- "Our mission is to advance solutions to achieve a thriving, racially equitable, 
      and sustainable central Puget Sound region through leadership, visionary planning, and collaboration."
      
      region_tag <- HTML("<br/>We rely on you to keep the Puget Sound region healthy and vibrant as it grows.")
      
      bs4Jumbotron(
        
        title =  strong(div(class="footer_title",
                            
                            fluidRow(column(1, div(img(src = "footer-logo.png", width = "100%", height = "100%"))),
                                     column(11, region_tag)),
                            hr(),
                            fluidRow(column(12, "Our Mission"))
        )),
        lead = div(class="footer_mission",  
                   fluidRow(column(12, mission)),
                   hr(),
        ),
        
        strong(a(class = "footer_title", "Connect with PSRC: ")),
        a(class = "footer_url", href="https://www.facebook.com/PugetSoundRegionalCouncil", icon("facebook"), target="_blank"),
        a(class = "footer_url", href="https://twitter.com/SoundRegion", icon("x-twitter"), target="_blank"),
        a(class = "footer_url", href="https://www.instagram.com/soundregion/", icon("instagram"), target="_blank"),
        a(class = "footer_url", href="https://www.linkedin.com/company/soundregion", icon("linkedin"), target="_blank"),
        a(class = "footer_url", href="mailto:info@psrc.org?", icon("envelope")),
        
        status = "info",
        btnName = NULL
      )
      
    })
    
  }) # end moduleServer
  
}