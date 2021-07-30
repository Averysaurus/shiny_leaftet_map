library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)


ev_df <- sf::st_read("sf_ev_map.geojson")

bins <- c(0, 20, 40, 60, 80, 100, 120,140, 160, 180, Inf)
colorpal <- colorBin("viridis", domain = ev_df$n, bins = bins)

ui <- bootstrapPage(
    tags$style(type = "text/css",
               "html, body {width:100%;height:100%}"),
    leafletOutput('map', width = "100%", height = "100%"),
    
    absolutePanel(
        top = 10, right = 10, style = "z-index:500; text-align: right;",
        tags$h1("San Francisco: 20 Years of Eviction Notices"),
        HTML(paste("Data Source: <a href='https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5'> San Francisco Open Data Portal</a> <p><p>")),
        HTML(paste("Author: <a href= 'https://www.linkedin.com/in/averysaurus/' > Avery Richards</a> <p><p>")),
      
    
    absolutePanel(bottom = 5,  
                  fixed = T,
                  sliderInput(
                      "range",
                      label = h3("Year"),
                      min(as.numeric(ev_df$file_date)),
                      max(as.numeric(ev_df$file_date)),
                      value = 2000, sep = "",
                      step = 1,  
                      width = "400px")
    )
    ))

server <- function(input, output, session){
    
    filteredData <- reactive({
        #ev_df[ev_df$file_date == input$range[1],]
        ev_df %>%  dplyr::filter(file_date == input$range[1])
        
        
    })

    output$map <- renderLeaflet({
        leaflet(filteredData()) %>%
            addProviderTiles("Esri.WorldGrayCanvas") %>%
            setView(-122.392, 37.76, zoom = 12.5) %>%
            addResetMapButton() %>% 
            addLegend(position = "bottomright",
                      pal = colorpal, values = ~n, title = "Evictions Filed"
            )
        
    })
    
    
    observe({
     
        leafletProxy("map", data = filteredData()) %>% 
            clearShapes() %>% 
            addPolygons(color = ~colorpal(n),
                        fillOpacity = 0.65, weight = 1, 
                        popup = filteredData()$name)  %>%
            addResetMapButton() 
           
        
    })

}   

shinyApp(ui, server)