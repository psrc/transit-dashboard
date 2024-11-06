shinyServer(function(input, output) {
  
  footer_server('psrcfooter')
  
  # Overview Page
  transit_overview_server('OVERVIEWtransit')
  output$transit_howto_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Overview-HowTo", page_info = "description"))})
  
  # Region Page
  output$region_page_title <- renderText(paste0(input$RegionMetric, " for all Transit Modes"))
  output$region_chart_title <- renderText(paste0("Regional ",input$RegionMetric))
  output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
  value_box_server('REGIONvaluebox', df=ntd_data, m=reactive(input$RegionMetric), v="All Transit Modes", g="Region", gt="Region", gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('REGIONbarchart', df=ntd_data, m=reactive(input$RegionMetric), v="All Transit Modes", g="Region", gt="Region", color = c("#00A7A0"))
  
  transit_mode_server('MODEtransit')
  transit_operator_server('OPERATORtransit')
  transit_type_server('TYPEtransit')
  transit_trips_server('TRIPtransit')
  transit_route_server('ROUTEtransit')

})

