# General Information -------------------------------------------------------------
page_information <- function(tbl, page_name, page_section=NULL, page_info) {
  
  if(is.null(page_section)) {
    
    t <- tbl |>
      filter(page == page_name) |>
      select(all_of(page_info)) |>
      pull()
    
  } else {
    
    t <- tbl |>
      filter(page == page_name & section == page_section) |>
      select(all_of(page_info)) |>
      pull()
    
  }
  
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

# Charts ------------------------------------------------------------------

psrc_style <- function() {
  font <- "Poppins"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and color of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       face="bold",
                                       size=13, 
                                       color='#4C4C4C'),
    plot.title.position = "plot",
    
    #This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=12,
                                          margin=ggplot2::margin(9,0,9,0)),
    
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    plot.caption =  ggplot2::element_text(family=font,
                                          size=10,
                                          face="italic",
                                          color="#4C4C4C",
                                          hjust=0),
    plot.caption.position = "plot",
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#4C4C4C"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(family=font, size=12, color="#2f3030"),
    axis.text = ggplot2::element_text(family=font, size=11, color="#2f3030"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background color from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background sets the panel background for facet-wrapped plots to PSRC Gray and sets the title size of the facet-wrap title
    strip.background = ggplot2::element_rect(fill="#BCBEC0"),
    strip.text = ggplot2::element_text(size  = 12,  hjust = 0)
  )
}

psrc_infogram_style <- function() {
  font <- "Poppins"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and color of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       face="bold",
                                       size=16, 
                                       color='black'),
    plot.title.position = "plot",
    
    #This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          color='black',
                                          size=14,
                                          margin=ggplot2::margin(9,0,9,0)),
    
    #This sets the caption text element
    plot.caption =  ggplot2::element_text(family=font,
                                          size=12,
                                          face="plain",
                                          color='black',
                                          hjust=0),
    plot.caption.position = "plot",
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="black"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font, size=12, color="black"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color="#BCBEC0"),
    panel.grid.major.x = ggplot2::element_line(linewidth = 0.2, color="#BCBEC0"),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background color from the plot
    panel.background = ggplot2::element_blank(),
    
    
  )
}

psrc_column_chart <- function(df, x, y, fill, colors, labels=scales::label_comma(), dec=0, chart_style=psrc_infogram_style(), title=NULL, source=NULL, pos="dodge", legend = TRUE) {
  
  c <- ggplot(data=df,
              aes(x=.data[[x]],
                  y=.data[[y]],
                  fill=.data[[fill]],
                  text = paste0(.data[[fill]], ": ", format(round(.data[[y]], dec), nsmall=0, big.mark=","))))  + 
    geom_bar(position=pos, stat="identity", na.rm=TRUE) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = labels, expand=expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
    labs(title=title, caption=source) +
    chart_style
  
  if (legend == FALSE) {
    
    c <- c + theme(legend.position = "none")
  }
  
  return (c)
}

psrc_line_chart <- function(df, x, y, fill, lwidth=1, colors, ymin =0, ymax = 1, labels=scales::label_comma(), dec=0, breaks=NULL, title=NULL, source=NULL, legend = TRUE, chart_style=psrc_infogram_style()) {
  
  c <- ggplot(data=df,
              aes(x=.data[[x]],
                  y=.data[[y]],
                  group=.data[[fill]],
                  color=.data[[fill]],
                  text = paste0(.data[[fill]], ": ", format(round(.data[[y]]*100, dec), nsmall=0, big.mark=","), "%")))  + 
    geom_line(linewidth=lwidth, linejoin = "round", na.rm=TRUE) +
    geom_point(fill = "white", shape = 21, stroke = 0.5) +
    scale_color_manual(values = colors)  +
    scale_y_continuous(labels = labels, limits = c(ymin, ymax))  +   
    labs(title=title, caption=source) +
    scale_x_continuous(n.breaks=breaks) +
    chart_style
  
  return(c)
  
}

psrc_make_interactive <- function(plot_obj, legend=FALSE, hover=y) {
  
  c <- plotly::ggplotly(plot_obj, tooltip = "text")
  
  c <- plotly::layout(c,
                      showlegend = legend,
                      legend=list(orientation="h", xanchor="center", xref="container", x=0.5, y=-0.10,         
                                  title="", font=list(family="Poppins", size=20, color="black"),
                                  pad=list(b=50, t=50)),
                      hoverlabel = list(bgcolor = "#EDF9FF", font = list(size=16, color = "#2F3030", face="bold"))
                      )
  
  return(c)
  
}


# Maps --------------------------------------------------------------------

create_stop_buffer_map<- function(lyr=transit_buffers, buffer_name, buffer_distance=0) {
  
  # Trim Layer to Variable of Interest and Year
  lyr <- lyr |> filter(stop_buffer %in% buffer_name & buffer == buffer_distance)
  
  labels <- paste0("<b>", paste0("Transit Type: "),"</b>", lyr$stop_buffer) |> lapply(htmltools::HTML)
  
  working_map <- leaflet(data = lyr) |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c(buffer_name),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) |>
    
    addPolygons(data = lyr, 
                fillColor = "#91268F",
                fillOpacity = 1,
                opacity = 0,
                label = labels,
                group = buffer_name) |>
    
    setView(lng = -122.257, lat = 47.615, zoom = 8.5) |>
    
    addLegend(colors=c("#91268F"),
              labels=c(buffer_name),
              group = buffer_name,
              position = "bottomleft")
  
  
  
  return(working_map)
  
}

create_route_map<- function(lyr=transit_layer_data, yr=current_year) {
  
  # Trim Layer to Variable of Interest and Year
  lyr <- lyr |> filter(year == yr)
  
  # Create HCT Layers to make mapping by mode cleaner
  brt <- lyr |> filter(type_name %in% c("BRT"))
  crt <- lyr |> filter(type_name %in% c("Commuter Rail"))
  lrt <- lyr |> filter(type_name %in% c("Streetcar", "Light Rail"))
  pof <- lyr |> filter(type_name %in% c("Passenger Ferry"))
  fry <- lyr |> filter(type_name %in% c("Auto Ferry"))
  bus <- lyr |> filter(type_name %in% c("Bus"))
  
  brt_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", brt$route_name) |> lapply(htmltools::HTML)
  crt_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", crt$route_name) |> lapply(htmltools::HTML)
  lrt_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", lrt$route_name) |> lapply(htmltools::HTML)
  pof_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", pof$route_name) |> lapply(htmltools::HTML)
  fry_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", fry$route_name) |> lapply(htmltools::HTML)
  bus_lbl <- paste0("<b>", paste0("Route Name: "),"</b>", bus$route_name) |> lapply(htmltools::HTML)
  
  working_map <- leaflet() |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("BRT", "Commuter Rail", "Light Rail", "Passenger Ferry", "Multimodal Ferry", "Bus"),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) |>
    
    addPolylines(data = bus,
                 color = "#BCBEC0",
                 weight = 2,
                 fillColor = "#BCBEC0",
                 group = "Bus",
                 label = bus_lbl) |>
    
    addPolylines(data = brt,
                 color = "#91268F",
                 weight = 4,
                 fillColor = "#91268F",
                 group = "BRT",
                 label = brt_lbl) |>
    
    addPolylines(data = crt,
                 color = "#8CC63E",
                 weight = 4,
                 fillColor = "#8CC63E",
                 group = "Commuter Rail",
                 label = crt_lbl) |>
    
    addPolylines(data = lrt,
                 color = "#F05A28",
                 weight = 4,
                 fillColor = "#F05A28",
                 group = "Light Rail",
                 label = lrt_lbl) |>
    
    addPolylines(data = pof,
                 color = "#40BDB8",
                 weight = 4,
                 fillColor = "#40BDB8",
                 group = "Passenger Ferry",
                 label = pof_lbl) |>
    
    addPolylines(data = fry,
                 color = "#00716c",
                 weight = 4,
                 fillColor = "#00716c",
                 group = "Multimodal Ferry",
                 label = fry_lbl) |>
    
    setView(lng = -122.257, lat = 47.615, zoom = 8.5) |>
    
    addLegend(colors=c("#91268F", "#8CC63E", "#F05A28", "#40BDB8", "#00716c", "#BCBEC0"),
              labels=c("BRT", "Commuter Rail", "Light Rail", "Passenger Ferry", "Multimodal Ferry", "Bus"),
              position = "bottomleft")
  
  return(working_map)
  
}

