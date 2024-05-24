shinyUI(

  fluidPage(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "Ferries",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = "Transit in the PSRC Region",
    
    theme = "styles.css",
    
    fluidRow(column(4, tags$a(div(tags$img(src='psrc-logo.png',
                                           style="margin-top: 10px; padding-left: 40px;",
                                           height = "80")
                                  ), href="https://www.psrc.org", target="_blank")),
             column(8, br(), strong(tags$div(class="mainpage_title", "Transit in the Central Puget Sound")))),
    
    hr(style = "border-top: 1px solid #000000;"),
    
    sidebarLayout(
      sidebarPanel(left_panel_ui('leftMain')),
      
      mainPanel(
        fluidRow(column(12, style='padding-left:25px; padding-right:50px;',
                        tabsetPanel(type = "pills",
                                    tabPanel("Overview", transit_overview_ui('OVERVIEWtransit')),
                                    tabPanel("Region", transit_region_ui('REGIONtransit')),
                                    tabPanel("Mode", transit_mode_ui('MODEtransit')),
                                    tabPanel("Operator", transit_operator_ui('OPERATORtransit')),
                                    tabPanel("Stops", transit_stop_ui('STOPtransit')),
                                    tabPanel("Routes", transit_route_ui('ROUTEtransit')))
                        )) # end of fluid row for Transit tab
        ) # End of mainPanel
    ), # End of sidebarLayout
    
    tags$footer(footer_ui('psrcfooter'))
  
    ) # End of fluid page
) # end of shiny app
