shinyUI(

  page_fillable(
    
    tags$head(
      tags$script(src = HTML("js-functions.js"))
    ),
    
    id = "AppID",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = "Transit in the PSRC Region",
    
    theme = "styles.css",
  
    fluidRow(column(4, tags$a(div(tags$img(src='psrc-logo.png',
                                      style="margin-top: 10px; padding-left: 20px;",
                                        height = "60")
                               ), href="https://www.psrc.org", target="_blank")),
          column(8, br(), strong(tags$div(class="mainpage_title", "Transit in the Central Puget Sound")))),
    
    navset_pill(
    
      nav_panel("Overview", transit_overview_ui('OVERVIEWtransit')),
      nav_panel("Region", transit_region_ui('REGIONtransit')),
      nav_panel("Mode", transit_mode_ui('MODEtransit')),
      nav_panel("Operator", transit_operator_ui('OPERATORtransit')),
      nav_panel("Type", transit_equity_ui('EQUITYtransit')),
      nav_panel("Frequency", transit_trips_ui('TRIPtransit')),
      nav_panel("Routes", transit_route_ui('ROUTEtransit')),
      
    ),

    tags$footer(footer_ui('psrcfooter'))
    
    ) # End of page_fillable
) # end of shiny app
