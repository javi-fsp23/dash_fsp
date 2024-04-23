source("2023.R", encoding = "UTF-8")
shinyServer( function(input, output, session) {
  reactive({Sys.sleep(30000)})
  auth <- secure_server(check_credentials = check_credentials(credentials)) 
  observeEvent(session$input$logout,{  session$reload() })
  
  observeEvent(auth$user, {
    mensaje_telegram(paste("El usuario", auth$user, "ha ingresado a la app directorio"))
  })
  
  output$busca_1<-renderUI({
          selectInput( inputId = "search_pharm", label = "", selected="-", multiple = F, width = "170px", choices = c("-", direc_fil()$unidad) ) }) 
  
  direc_fil<-reactive({req(input$search_busines)
    filter_table(dir,input$search_busines, input$search_size)
  })
  
  mapa_1<-reactive({
    dire<-direc_fil()
    leaflet() %>%setView( -100.34, 20.7, 4.5 ) %>% addTiles() %>%
      addPolygons(data = mx, weight = 2, fillOpacity = 0, color = '#e88b0c',  label = ~nom_ent ) %>%
      # addMarkers(data = dire, lat = ~lat, lng = ~long,  label = ~htmlEscape(nombre), icon = ipablo, 
      #            clusterOptions = markerClusterOptions(), 
      #            popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",dire$nombre,  "</div>"),
      #                          dire$dir_mapa, "<br>"))
      addAwesomeMarkers(data = dire, lat = ~lat, lng = ~long,  label = ~htmlEscape(nombre), icon = awesomeIconList(icons(dire$icon_ncio, "green", dire$color, FALSE )),
                        clusterOptions = markerClusterOptions(),
                        popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",dire$nombre,  "</div>"),
                                      dire$dir_mapa, "<br>")) #%>%
      # addDrawToolbar(targetGroup='datos', position = "bottomleft",
      #   editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  })
  
  output$Mapas_1<-renderLeaflet({ mapa_1() })
  dsele<-reactive({ filter_map_1(direc_fil(),input$search_pharm) })
  
  observeEvent(input$search_pharm, {
    if(input$search_pharm!="-"){
    proxy <- leafletProxy('Mapas_1')
    row_selected<-dsele()
    lg <- row_selected$long
    lt <- row_selected$lat
    
  proxy %>% clearGroup(group = "added_point") %>% 
            addMarkers(data = row_selected, lat = row_selected$lat, lng = row_selected$long, label = ~htmlEscape(nombre),  group  = "added_point",
                       icon = ipabloa, #awesomeIconList(icons(row_selected$icon_ncio, "red", "#1788d3", TRUE)), 
                       popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ", row_selected$nombre,  "</div>"),
                       row_selected$dir_mapa, "<br>")) %>% setView(lg, lt, 11)
    }
  })
  
  })

