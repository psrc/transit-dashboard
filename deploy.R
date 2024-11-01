library(rsconnect)

# official version ----

deployApp(account = 'psrcwa',
         appName = 'transit-dashboard',
         appTitle = 'Transit Data Dashboard')

# Development version -----------------------------------------------------

deployApp(account = 'psrcwa',
         appName = 'transit-dashboard-dev',
         appTitle = 'Dev Version Transit Data Dashboard')
 
