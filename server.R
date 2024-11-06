shinyServer(function(input, output) {
  
  footer_server('psrcfooter')
  
  # Overview Page
  transit_overview_server('OVERVIEWtransit')
  output$transit_howto_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Overview-HowTo", page_info = "description"))})
  
  # Region Page
  output$region_page_title <- renderText(paste0(input$RegionMetric, " for all Transit Modes"))
  output$region_chart_title <- renderText(paste0("Regional ",input$RegionMetric))
  output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
  value_box_server('REGIONvaluebox', df=ntd_data, m=reactive(input$RegionMetric), v=reactive("All Transit Modes"), g="Region", gt="Region", gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('REGIONbarchart', df=ntd_data, m=reactive(input$RegionMetric), v=reactive("All Transit Modes"), g="Region", gt="Region", color = c("#00A7A0"))
  
  # Mode Page
  
  # Boardings
  output$mode_boardings_chart_title <- renderText(paste0("Regional ", input$NTDModes, " Boardings"))
  output$mode_boardings_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Boardings", page_info = "description"))})
  value_box_server('MODEBoardingsvaluebox', df=ntd_data, m=reactive("Boardings"), v=reactive(input$NTDModes), g="Region", gt="Region", gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEBoardingsbarchart', df=ntd_data, m=reactive("Boardings"), v=reactive(input$NTDModes), g="Region", gt="Region", color = c("#F05A28"))
 
  # Revenue Hours
  output$mode_hours_chart_title <- renderText(paste0("Regional ", input$NTDModes, " Revenue-Hours"))
  output$mode_hours_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Hours", page_info = "description"))})
  value_box_server('MODEHoursvaluebox', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive(input$NTDModes), g="Region", gt="Region", gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEHoursbarchart', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive(input$NTDModes), g="Region", gt="Region", color = c("#91268F"))
  
  # Revenue Miles
  output$mode_miles_chart_title <- renderText(paste0("Regional ", input$NTDModes, " Revenue-Miles"))
  output$mode_miles_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Miles", page_info = "description"))})
  value_box_server('MODEMilesvaluebox', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive(input$NTDModes), g="Region", gt="Region", gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEMilesbarchart', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive(input$NTDModes), g="Region", gt="Region", color = c("#8CC63E"))
  
  # Boardings per Hour
  output$mode_bph_chart_title <- renderText(paste0("Regional ", input$NTDModes, " Boardings per Hour"))
  output$mode_bph_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-BPH", page_info = "description"))})
  value_box_server('MODEBPHvaluebox', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive(input$NTDModes), g="Region", gt="Region", gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEBPHbarchart', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive(input$NTDModes), g="Region", gt="Region", color = c("#00A7A0"))
  
  
  
  
  
  
  
  
  transit_operator_server('OPERATORtransit')
  transit_type_server('TYPEtransit')
  transit_trips_server('TRIPtransit')
  transit_route_server('ROUTEtransit')

})

