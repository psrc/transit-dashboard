shinyServer(function(input, output) {
  
  footer_server('psrcfooter')
  transit_overview_server('OVERVIEWtransit')
  
  output$transit_howto_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Overview-HowTo", page_info = "description"))})
  
  transit_region_server('REGIONtransit')
  transit_mode_server('MODEtransit')
  transit_operator_server('OPERATORtransit')
  transit_type_server('TYPEtransit')
  transit_trips_server('TRIPtransit')
  transit_route_server('ROUTEtransit')

})

