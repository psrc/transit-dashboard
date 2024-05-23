
transit_region_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitregion"))
  )
}

transit_region_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$transit_region_text <- renderText({page_information(tbl=page_text, page_name="Transit", page_section = "NTD", page_info = "description")})
    
    # Charts
    output$ntd_region_chart <- renderEcharts4r({create_bar_chart_toggle(df = ntd_data |> 
                                                                          filter(variable == "All Transit Modes" & geography == "Region" & metric == input$NTDMetric) |> 
                                                                          mutate(year=as.character(year)),
                                                                        x = "year", y = "estimate", 
                                                                        toggle = "grouping", fill = "metric", 
                                                                        esttype = "number", color = c("#00A7A0", "#F05A28", "#91268F", "#8CC63E"),
                                                                        left_align = '15%', title = "Boardings")})

    # Tab layout
    output$transitregion <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("NTDMetric"), label="Select Transit Metric:", choices=ntd_metric_list, selected = "Boardings"))),
        fluidRow(column(12,echarts4rOutput(ns("ntd_region_chart")))),
        tags$div(class="chart_source","Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
      )
    }) 
  })  # end moduleServer
}
