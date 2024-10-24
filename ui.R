shinyUI(

  page_fillable(
    
    tags$head(
      tags$script(src = HTML("js-functions.js"))
    ),
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "AppID",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = "Transit in the PSRC Region",
    
    theme = "styles.css",
    
    fluidRow(column(4, tags$a(div(tags$img(src='psrc-logo.png',
                                          style="margin-top: 10px; padding-left: 20px;",
                                          height = "60")
                                 ), href="https://www.psrc.org", target="_blank")),
            column(8, br(), strong(tags$div(class="mainpage_title", "Transit in the Central Puget Sound")))),
    
    #br(),
    
    navset_pill(
      
      nav_panel("Overview", transit_overview_ui('OVERVIEWtransit')),
      nav_panel("Region", transit_region_ui('REGIONtransit')),
      nav_panel("Mode", transit_mode_ui('MODEtransit')),
      nav_panel("Operator", transit_operator_ui('OPERATORtransit')),
      nav_panel("Type", transit_equity_ui('EQUITYtransit')),
      nav_panel("Frequency", transit_trips_ui('TRIPtransit')),
      nav_panel("Routes", transit_route_ui('ROUTEtransit'))
      
    ),
    
    # fluidRow(
    #   # Left Panel of the page using the left panel module
    #   column(width = 4, left_panel_ui('leftMain')),
    #   
    #   mainPanel(
    #     fluidRow(column(12, style='padding-left:5px; padding-right:25px;',
    #                     tabsetPanel(type = "pills",
    #                                 tabPanel("Overview", transit_overview_ui('OVERVIEWtransit')),
    #                                 tabPanel("Region", transit_region_ui('REGIONtransit')),
    #                                 tabPanel("Mode", transit_mode_ui('MODEtransit')),
    #                                 tabPanel("Operator", transit_operator_ui('OPERATORtransit')),
    #                                 tabPanel("Type", transit_equity_ui('EQUITYtransit')),
    #                                 tabPanel("Frequency", transit_trips_ui('TRIPtransit')),
    #                                 tabPanel("Routes", transit_route_ui('ROUTEtransit')))
    #     )) # end of fluid row for Transit tab
    #   ), # End of mainPanel
    #   
    #   style = "margin-bottom: 2rem;"),# end of fluidRow
    
    tags$footer(footer_ui('psrcfooter'))
    
    ) # End of fluid page
) # end of shiny app
