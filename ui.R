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
                    label = tags$label("Select a Transit Metric:", `for` = "RegionMetric"),
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
                bar_chart_ui('REGIONbarchart') |> withSpinner(color=load_clr),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(htmlOutput("region_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;")
                ),
      
      nav_panel("Mode", 
                card_body(
                  selectizeInput(
                    "NTDModes",
                    label = tags$label("Select a Transit Mode:", `for` = "NTDModes"),
                    choices = ntd_mode_list,
                    selected = "Bus",
                    options = list(dropdownParent = 'body')
                  ),
                  class = "selection_panel"
                ),
                
                hr(style = "border-top: 1px solid #000000;"),
                
                # Boardings Section
                h1("Boardings"),
                value_box_ui('MODEBoardingsvaluebox'),
                hr(style = "border-top: 1px solid #000000;"),
                h2(textOutput("mode_boardings_chart_title")),
                bar_chart_ui('MODEBoardingsbarchart') |> withSpinner(color=load_clr),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(htmlOutput("mode_boardings_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;"),
                
                # Revenue Hours Section
                h1("Revenue Hours"),
                value_box_ui('MODEHoursvaluebox'),
                hr(style = "border-top: 1px solid #000000;"),
                h2(textOutput("mode_hours_chart_title")),
                bar_chart_ui('MODEHoursbarchart'),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(htmlOutput("mode_hours_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;"),
                
                # Revenue Miles Section
                h1("Revenue Miles"),
                value_box_ui('MODEMilesvaluebox'),
                hr(style = "border-top: 1px solid #000000;"),
                h2(textOutput("mode_miles_chart_title")),
                bar_chart_ui('MODEMilesbarchart'),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(htmlOutput("mode_miles_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;"),
                
                # Boardings per Hour Section
                h1("Boardings per Hour"),
                value_box_ui('MODEBPHvaluebox'),
                hr(style = "border-top: 1px solid #000000;"),
                h2(textOutput("mode_bph_chart_title")),
                bar_chart_ui('MODEBPHbarchart'),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(htmlOutput("mode_bph_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;")
                ),
      
      nav_panel("Operator", transit_operator_ui('OPERATORtransit')),
      nav_panel("Type", transit_type_ui('TYPEtransit')),
      nav_panel("Frequency", transit_trips_ui('TRIPtransit')),
      nav_panel("Routes", transit_route_ui('ROUTEtransit')),
    
      br(), br(),
    
      footer = (footer_ui('psrcfooter'))
      
      ) # end of page_navbar
  ) # end of HTML tag for UI
) # end of Shiny App

