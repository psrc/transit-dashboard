
value_box_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("summary_boxes"))
  )
}

value_box_server <- function(id, df, m, v, g, gt, gr) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    filtered_df <- reactive(df |> filter(variable == v() & geography == g & geography_type == gt & metric == m() & grouping == gr))
    
    # Output Values
    output$pre_pandemic_title <- renderUI(paste0(pre_pandemic, " YTD ", m()))
    output$current_title <- renderUI(paste0(year(Sys.Date())," YTD ", m()))
    output$pandemic_share_title <- renderUI(paste0("% of ", pre_pandemic, " YTD ", m()))
    output$recent_share_title <- renderUI(paste0("% change from ", base_yr))
    
    output$pre_pandemic_value <- renderText({
      if (m() == "Boardings-per-Hour") {
        paste0(round((filtered_df() |> filter(year == pre_pandemic) |> select("estimate") |> pull()), 1))
      } else {
        paste0(round((filtered_df() |> filter(year == pre_pandemic) |> select("estimate") |> pull())/1000000, 1), "M")
      }
    })
    
    output$current_value <- renderText({
      if(m() == "Boardings-per-Hour") {
        paste0(round((filtered_df() |> filter(year == current_year) |> select("estimate") |> pull()), 1))
      } else {
        paste0(round((filtered_df() |> filter(year == current_year) |> select("estimate") |> pull())/1000000, 1), "M")  
      }
    })
    
    output$pre_pandemic_share <- renderText(paste0(round((filtered_df() |> filter(year == current_year) |> select("estimate") |> pull()) / (filtered_df() |> filter(year == pre_pandemic) |> select("estimate") |> pull()), 3)*100, "%"))
    
    output$recent_growth <- renderText(paste0(round((filtered_df() |> filter(year == current_year) |> select("estimate") |> pull()) / (filtered_df() |> filter(year == base_yr) |> select("estimate") |> pull())*100-100, 1), "%"))
    
    # Tab layout
    output$summary_boxes <- renderUI({
      tagList(
        
        layout_column_wrap(
          width = 1/4,
          value_box(
            title = htmlOutput(ns("pre_pandemic_title")), 
            value = textOutput(ns("pre_pandemic_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("current_title")), 
            value = textOutput(ns("current_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("pandemic_share_title")), 
            value = textOutput(ns("pre_pandemic_share")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
            title = htmlOutput(ns("recent_share_title")), 
            value = textOutput(ns("recent_growth")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          )
        )
      )
      
      })  # end renderui
  }) # end module server
    
}
