
transit_mode_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitmode"))
  )
}

transit_mode_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Charts
    output$ntd_mode_boardings_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                                  filter(variable == input$NTDModes & geography == "Region" & metric == "Boardings") |> 
                                                                                  mutate(year=as.character(year)),
                                                                                x = "year", y = "estimate", 
                                                                                toggle = "grouping", fill = "metric", 
                                                                                esttype = "number", color = c("#F05A28"),
                                                                                left_align = '25%', title = "Boardings",
                                                                                right_toggle = 100, left_toggle = 100)})
    
    output$ntd_mode_hours_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                              filter(variable == input$NTDModes & geography == "Region" & metric == "Revenue-Hours") |> 
                                                                                  mutate(year=as.character(year)),
                                                                            x = "year", y = "estimate", 
                                                                            toggle = "grouping", fill = "metric", 
                                                                            esttype = "number", color = c("#91268F"),
                                                                            left_align = '25%', title = "Revenue-Hours",
                                                                            right_toggle = 100, left_toggle = 100)})
    
    output$ntd_mode_miles_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                              filter(variable == input$NTDModes & geography == "Region" & metric == "Revenue-Miles") |> 
                                                                              mutate(year=as.character(year)),
                                                                            x = "year", y = "estimate", 
                                                                            toggle = "grouping", fill = "metric", 
                                                                            esttype = "number", color = c("#8CC63E"),
                                                                            left_align = '25%', title = "Revenue-Miles",
                                                                            right_toggle = 100, left_toggle = 100)})
    
    output$ntd_mode_bph_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                            filter(variable == input$NTDModes & geography == "Region" & metric == "Boardings-per-Hour") |> 
                                                                            mutate(year=as.character(year)),
                                                                          x = "year", y = "estimate", dec=1, 
                                                                          toggle = "grouping", fill = "metric", 
                                                                          esttype = "number", color = c("#00A7A0"),
                                                                          left_align = '25%', title = "Boardings per Hour",
                                                                          right_toggle = 100, left_toggle = 100)})
    
    
    
    # Tab layout
    output$transitmode <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("NTDModes"), label="Select Transit Mode:", choices=ntd_mode_list, selected = "Bus"))),
        fluidRow(column(6,echarts4rOutput(ns("ntd_mode_boardings_chart"))),
                 column(6,echarts4rOutput(ns("ntd_mode_hours_chart")))),
        br(),
        fluidRow(column(6,echarts4rOutput(ns("ntd_mode_miles_chart"))),
                 column(6,echarts4rOutput(ns("ntd_mode_bph_chart")))),
        tags$div(class="chart_source","Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
      )
    }) 
  })  # end moduleServer
}
