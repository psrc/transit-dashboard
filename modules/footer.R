# Display footer

footer_ui <- function(id) {
  ns <- NS(id)

  tagList( 
    
    card(
      
      layout_columns(
        col_widths = c(2,7,3),
        
        card_body(tags$img(src='footer-logo.png', style="margin-top: 0x; padding-left: 0px; padding-right: 0px;", class = "responsive-image"), class = "footer_panel"),
        
        card_body(
          tags$div(class = "footer_heading", HTML(paste0("About PSRC<br>", tags$div(class = "footer_about", psrc_mission)))),
        ),
        
        card_body(
          tags$div(class = "footer_heading", HTML(paste0("Connect with PSRC<br>", 
                                                         tags$div(class = "psrc-location", HTML("1201 Third Avenue, Suite 500<br>Seattle, WA 98101-3055")),
                                                         tags$div(class = "psrc-phone", "206-464-7090"),
                                                         tags$a(class = "psrc_email", href = paste0("mailto:","info@psrc.org","?"), "info@psrc.org")
          ))),
        ),
      ),
      
      card_footer(
        layout_columns(
          col_widths = c(9,3),
          card_body(div(
            a(class = "footer_url", href="https://www.facebook.com/PugetSoundRegionalCouncil", icon("facebook"), target="_blank"),
            a(class = "footer_url", href="https://twitter.com/SoundRegion", icon("x-twitter"), target="_blank"),
            a(class = "footer_url", href="https://www.instagram.com/soundregion/", icon("instagram"), target="_blank"),
            a(class = "footer_url", href="https://www.linkedin.com/company/soundregion", icon("linkedin"), target="_blank")
          )),
          
          tags$div(class = "footer_about", "Dashboard by", tags$div(class = "footer_url", "PSRC Data Science")),
        ),
        class = "footer_footer"),
      class = "footer_panel"
    )
  )
  
}

footer_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
  }) # end moduleServer
  
}