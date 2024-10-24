# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  transit_overview_server('OVERVIEWtransit')
  
  # Regional NTD metrics
  transit_region_server('REGIONtransit')
  
  # Regional NTD metrics by Mode
  transit_mode_server('MODEtransit')
  
  # Regional NTD metrics by Operator
  transit_operator_server('OPERATORtransit')
  
  # Stops by Mode
  transit_equity_server('EQUITYtransit')
  
  # Stops by Frequency
  transit_trips_server('TRIPtransit')
  
  # Routes
  transit_route_server('ROUTEtransit')
  
})    
