shinyUI(
  
  tags$html(
    
    lang = "en",
    
    tags$head(tags$title("Public Transportation in the Central Puget Sound Region")),
    
    page_navbar(
    
      # JavaScript to modify tabindex values to 0 so you can tab to them as well as using a mouse
      tags$script(HTML("
      $(document).ready(function() {
        function setTabindex() {
          $('.nav-link').attr('tabindex', '0');
        }
        // Set tabindex initially
        setTabindex();
  
        // Listen for tab changes and reset tabindex
        $('.nav-link').on('shown.bs.tab', function() {
          setTabindex();
        });
      });
    ")),
    
      position = c("static-top"),
    
      title = tags$a(div(tags$img(src='psrc-logo.png', style="margin-top: 10px; padding-left: 20px; padding-right: 30px;", height = "65", alt = "Link to PSRC Homepage")), href="https://www.psrc.org", target="_blank"),
      fillable = FALSE,
      theme = psrc_theme,

      nav_panel("Overview", 
                h1("Transit in the Central Puget Sound Region"),
                transit_overview_ui('OVERVIEWtransit'),
                h2("What is in this dashboard?"),
                htmlOutput("transit_howto_text")),
      
      nav_panel("Region", 
                card_body(
                  selectizeInput(
                    "RegionMetric",
                    label = "Select a Transit Metric:",
                    choices = ntd_metric_list,
                    selected = "Boardings",
                    options = list(dropdownParent = 'body')
                  ),
                  class = "selection_panel"
                ),
                
                hr(style = "border-top: 1px solid #000000;"),
                h1(textOutput("region_page_title")),
                value_box_ui('REGIONvaluebox'),
                hr(style = "border-top: 1px solid #000000;"),
                h2(textOutput("region_chart_title")),
                bar_chart_ui('REGIONbarchart'),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(htmlOutput("region_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;")
                ),
      
      nav_panel("Mode", transit_mode_ui('MODEtransit')),
      nav_panel("Operator", transit_operator_ui('OPERATORtransit')),
      nav_panel("Type", transit_type_ui('TYPEtransit')),
      nav_panel("Frequency", transit_trips_ui('TRIPtransit')),
      nav_panel("Routes", transit_route_ui('ROUTEtransit')),
    
      br(), br(),
    
      footer = (footer_ui('psrcfooter'))
      
      ) # end of page_navbar
  ) # end of HTML tag for UI
) # end of Shiny App

