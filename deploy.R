
# official version ----
library(rsconnect)
deployApp(account = 'psrcwa',
         appName = 'transit-dashboard',
         appTitle = 'Transit Data Dashboard')

# Development version -----------------------------------------------------
library(rsconnect)
deployApp(account = 'psrcwa',
         appName = 'transit-dashboard-dev',
         appTitle = 'Dev Version Transit Data Dashboard')
 
