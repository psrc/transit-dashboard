contact_container_ui <- function(id, name, title, email, phone) {
  ns <- NS(id)
  
  tagList(
    div(class = "contacts-container",
        p(class = "m-menu__title", "Connect With Us"),
        
        div(class = "contacts",
            
            div(class = "contacts-details",
                div(  
                  div(p(paste(name, "-", title))),
                  
                  div(class = "contacts-icons",
                      div(icon("envelope"),
                          tags$a(class = "links", href = paste0("mailto:",email,"?"), "Email")),
                      
                      div(
                        icon("phone-volume"),  
                        phone)
                  ) # end div
                ) # end div
            ) # end div
            
            
        ) # end div
    ) # end div
  )
}


contact_container_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    
  }) # end moduleServer
  
}
