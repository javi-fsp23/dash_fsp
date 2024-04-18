shinyServer( function(input, output, session) {
  reactive({Sys.sleep(30000)})
  auth <- secure_server(check_credentials = check_credentials(credentials)) 
  observeEvent(session$input$logout,{  session$reload() })
  
  observeEvent(auth$user, {
    mensaje_telegram(paste("El usuario", auth$user, "ha ingresado al directorio"))
  })
  
  output$busca_1<-renderUI({
          selectInput(
          inputId = "search_pharm", label = "", selected="-", multiple = F, width = "250px", choices = c("-", dir$unidad) ) })
  
  mapa_1<-reactive({
    leaflet() %>%setView( -100.34, 20.7, 4.5 ) %>% addTiles() %>%
      addPolygons(data = mx, weight = 2, fillOpacity = 0, color = '#e88b0c',  label = ~nom_ent ) %>%
      addAwesomeMarkers(data = dir, lat = ~lat, lng = ~long,  label = ~htmlEscape(nombre), icon = awesomeIconList(icons("house-medical", "green", "#1788d3")), 
                        clusterOptions = markerClusterOptions(), 
                        popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",dir$nombre,  "</div>"),
                                      "<b>", "Gerente de farmacia: ","</b>", dir$dir_mapa, "<br>")) %>%
      addDrawToolbar(
        targetGroup='datos',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  })
  
  output$Mapas_1<-renderLeaflet({ mapa_1() })
  dsele<-reactive({ filter_map_1(input$search_pharm) })
  
  
  observeEvent(input$search_pharm, {
    if(input$search_pharm!="-"){
    proxy <- leafletProxy('Mapas_1')
    row_selected<-dsele()
    lg <- row_selected$long
    lt <- row_selected$lat
  proxy %>%  clearGroup(group = "added_point") %>% 
        addAwesomeMarkers(data = row_selected, lat = ~lat, lng = ~long, label = ~htmlEscape(nombre),  group  = "added_point",
                          icon = awesomeIconList(icons("house-medical", "red", "#1788d3")), 
                          clusterOptions = markerClusterOptions(), 
                          popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ", row_selected$nombre,  "</div>"),
                                        row_selected$dir_mapa, "<br>")) %>% setView(lg, lt, 11) 
    }
  })
  
  })

