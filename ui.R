shinyUI(
  
  page_navbar(
    
    position = c("static-top"),
    
    title = tags$a(div(tags$img(src='psrc-logo.png', style="margin-top: 10px; padding-left: 20px; padding-right: 30px;", height = "65", alt = "Link to PSRC Homepage")), href="https://www.psrc.org", target="_blank"),
    fillable = FALSE,
    theme = psrc_theme,
  
    nav_panel("Overview", transit_overview_ui('OVERVIEWtransit')),
    nav_panel("Region", transit_region_ui('REGIONtransit')),
    nav_panel("Mode", transit_mode_ui('MODEtransit')),
    nav_panel("Operator", transit_operator_ui('OPERATORtransit')),
    nav_panel("Type", transit_type_ui('TYPEtransit')),
    nav_panel("Frequency", transit_trips_ui('TRIPtransit')),
    nav_panel("Routes", transit_route_ui('ROUTEtransit')),
    
    br(), br(),
    
    footer = (footer_ui('psrcfooter'))
  
  ) # end of page_navbar
) # end of Shiny App

