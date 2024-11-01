shinyServer(function(input, output) {
  
  footer_server('psrcfooter')
  transit_overview_server('OVERVIEWtransit')
  transit_region_server('REGIONtransit')
  transit_mode_server('MODEtransit')
  transit_operator_server('OPERATORtransit')
  transit_type_server('TYPEtransit')
  transit_trips_server('TRIPtransit')
  transit_route_server('ROUTEtransit')

})

