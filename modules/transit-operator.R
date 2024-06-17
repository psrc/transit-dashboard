
transit_operator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitoperator"))
  )
}

transit_operator_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Charts & Maps
    output$ntd_operator_boardings_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                                      filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Boardings") |>
                                                                                      filter(geography == input$NTDoperators) |>
                                                                                      mutate(year=as.character(year)),
                                                                                    x = "year", y = "estimate", 
                                                                                    toggle = "grouping", fill = "metric", 
                                                                                    esttype = "number", color = c("#F05A28"),
                                                                                    left_align = '25%', title = "Boardings",
                                                                                    right_toggle = 100, left_toggle = 100)})
    
    output$ntd_operator_hours_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                                  filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Revenue-Hours") |>
                                                                                  filter(geography == input$NTDoperators) |>
                                                                                  mutate(year=as.character(year)),
                                                                                x = "year", y = "estimate", 
                                                                                toggle = "grouping", fill = "metric", 
                                                                                esttype = "number", color = c("#91268F"),
                                                                                left_align = '25%', title = "Revenue-Hours",
                                                                                right_toggle = 100, left_toggle = 100)})
    
    output$ntd_operator_miles_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                                  filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Revenue-Miles") |>
                                                                                  filter(geography == input$NTDoperators) |>
                                                                                  mutate(year=as.character(year)),
                                                                                x = "year", y = "estimate", 
                                                                                toggle = "grouping", fill = "metric", 
                                                                                esttype = "number", color = c("#8CC63E"),
                                                                                left_align = '25%', title = "Revenue-Miles",
                                                                                right_toggle = 100, left_toggle = 100)})
    
    output$ntd_operator_bph_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                                filter(variable == "All Transit Modes" & geography_type == "Transit Operator" & metric == "Boardings-per-Hour") |>
                                                                                filter(geography == input$NTDoperators) |>
                                                                                mutate(year=as.character(year)),
                                                                              x = "year", y = "estimate", dec = 1,
                                                                              toggle = "grouping", fill = "metric", 
                                                                              esttype = "number", color = c("#00A7A0"),
                                                                              left_align = '25%', title = "Boardings per Hour",
                                                                              right_toggle = 100, left_toggle = 100)})
    
    
    
    # Tab layout
    output$transitoperator <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("NTDoperators"), label="Select Transit Operator:", choices=ntd_operator_list, selected = "Community Transit"))),
        fluidRow(column(6,echarts4rOutput(ns("ntd_operator_boardings_chart"))),
                 column(6,echarts4rOutput(ns("ntd_operator_hours_chart")))),
        br(),
        fluidRow(column(6,echarts4rOutput(ns("ntd_operator_miles_chart"))),
                 column(6,echarts4rOutput(ns("ntd_operator_bph_chart")))),
        tags$div(class="chart_source","Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
      )
    }) 
  })  # end moduleServer
}
