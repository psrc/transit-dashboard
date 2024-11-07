shinyServer(function(input, output) {
  
  footer_server('psrcfooter')

# Overview Page -----------------------------------------------------------

  transit_overview_server('OVERVIEWtransit')
  output$transit_howto_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Overview-HowTo", page_info = "description"))})

# Region Summary Page -----------------------------------------------------

  output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
  value_box_server('REGIONvaluebox', df=ntd_data, m=reactive(input$RegionMetric), v=reactive("All Transit Modes"), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('REGIONbarchart', df=ntd_data, m=reactive(input$RegionMetric), v=reactive("All Transit Modes"), g=reactive("Region"), gt=reactive("Region"), color = c("#00A7A0"))

# Mode Summary Page -------------------------------------------------------

  # Boardings
  output$mode_boardings_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Boardings", page_info = "description"))})
  value_box_server('MODEBoardingsvaluebox', df=ntd_data, m=reactive("Boardings"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEBoardingsbarchart', df=ntd_data, m=reactive("Boardings"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#F05A28"))
 
  # Revenue Hours
  output$mode_hours_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Hours", page_info = "description"))})
  value_box_server('MODEHoursvaluebox', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEHoursbarchart', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#91268F"))
  
  # Revenue Miles
  output$mode_miles_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Miles", page_info = "description"))})
  value_box_server('MODEMilesvaluebox', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEMilesbarchart', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#8CC63E"))
  
  # Boardings per Hour
  output$mode_bph_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-BPH", page_info = "description"))})
  value_box_server('MODEBPHvaluebox', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('MODEBPHbarchart', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#00A7A0"))

# Operator Summary Page ---------------------------------------------------
  
  # Boardings
  output$operator_boardings_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Boardings", page_info = "description"))})
  value_box_server('OPERATORBoardingsvaluebox', df=ntd_data, m=reactive("Boardings"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('OPERATORBoardingsbarchart', df=ntd_data, m=reactive("Boardings"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#F05A28"))
  
  # Revenue Hours
  output$operator_hours_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Hours", page_info = "description"))})
  value_box_server('OPERATORHoursvaluebox', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('OPERATORHoursbarchart', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#91268F"))
  
  # Revenue Miles
  output$operator_miles_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Miles", page_info = "description"))})
  value_box_server('OPERATORMilesvaluebox', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('OPERATORMilesbarchart', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#8CC63E"))
  
  # Boardings per Hour
  output$operator_bph_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-BPH", page_info = "description"))})
  value_box_server('OPERATORBPHvaluebox', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
  bar_chart_server('OPERATORBPHbarchart', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#00A7A0"))
  
  transit_type_server('TYPEtransit')
  transit_trips_server('TRIPtransit')

# Route Map Page ----------------------------------------------------------

  output$route_map_source <- renderText(paste0("Source: ",str_to_title(service_change)," ", gtfs_year, " General Transit Feed Specification (GTFS) data by Transit Agency"))
  output$transit_route_map <- renderLeaflet({create_route_map()})

}) # end of shinyServer function 

