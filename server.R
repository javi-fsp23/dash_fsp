
shinyServer( function(input, output, session) {
  reactive({Sys.sleep(30000)})
  auth <- secure_server(check_credentials = check_credentials(credentials)) 
  observeEvent(session$input$logout,{  session$reload() })
  
  observeEvent(auth$user, {
    mensaje_telegram(paste("El usuario", auth$user, "ha ingresado a la app BI CAM"))
  })
  
output$sel_date<-renderUI({
  dateRangeInput(
    inputId="select_date",
    label = "Fecha",
    start = paste0(substr(Sys.Date(), 1,8), "01"),
    end=Sys.Date(),min = "2023-01-01", max=Sys.Date(), 
    language="es", separator = "a", width = "100%")})
  
  output$sel_inc<-renderUI({ req(input$select_date)
          selectInput( inputId = "select_incident", label = "Incidente", selected="-", multiple = F, width = "100%", choices = control_select_inci(input$select_date) ) })
  
  output$sel_sub_inc<-renderUI({req(input$select_incident)
          selectInput( inputId = "select_sub_incident", label =  "Sub-Incidente", selected="-", multiple = F, width = "100%", choices = control_select_sub_inci(input$select_date,input$select_incident ) ) })

  ###---------------------------------------------------------------------
  
  base_0_2<-reactive({
   tabla<- conn %>%  
      DBI::dbGetQuery("SELECT Recepcion, COUNT(*) 
                      FROM data_lake dc
                      WHERE  DATE(dc.Recepcion)>='2024-04-01' AND  DATE(dc.Recepcion)<='2024-04-30' AND dc.Incidente ='Emergencia' AND dc.Subincidente ='Apoyo de SSC'
                      GROUP BY 1;")})
  
  GRAFICA_0_2 <- reactive({
    tabla<-base_0_2() 
    dia<-"Día"
    Grafica_elabora( tabla, "Activos", dia)})
  
  
  output$box_Met_Tot <- renderUI({
    grafica<-GRAFICA_0_2()
    tot<-1
    tabla<- base_0_2()
    dia<-"Día"
      value_box_inciden_1( Graf=grafica, Met_tot= tot, tabla =tabla, selecion=dia,
                           Let_inc=  div(h4("tickets", class="incidente-letra"), div(actionLink('act_Data_0_1', icon("display"), class="down-icon"), class= "incidente-down"), class="conten-letras"),
                           iconos= "user-shield", color= "#C8DC27", sizes=4,
                           nombre= "big_card_1", js_code=NULL)})
  ###-----------------
  
  base_0_4<-reactive({
    tabla<- conn %>%  
      DBI::dbGetQuery("SELECT Recepcion, COUNT(*) 
                      FROM data_lake dc
                      WHERE  DATE(dc.Recepcion)>='2024-04-01' AND  DATE(dc.Recepcion)<='2024-04-30' AND dc.Incidente ='Emergencia' AND dc.Subincidente ='Apoyo de SSC'
                      GROUP BY 1;")})
  
  GRAFICA_0_4 <- reactive({
    tabla<-base_0_4() 
    dia<-"Día"
    Grafica_elabora( tabla, "adje", dia)})
  
  output$box_Met_Tot_1 <- renderUI({
    grafica<-GRAFICA_0_4()
    tot<-2
    tabla<- base_0_4()
    dia<-"Día"
    value_box_inciden_1( Graf=grafica, Met_tot= tot, tabla =tabla, selecion=dia,
                         Let_inc=  div(h4("Robos", class="incidente-letra"), div(actionLink('act_Data_0_2', icon("display"), class="down-icon"), class= "incidente-down"), class="conten-letras"),
                         iconos= "user-shield", color= "#C8DC27", sizes=4,
                         nombre= "big_card_2", js_code=NULL)})

  ###-----------------  
  base_0_3<-reactive({
    tabla<- conn %>%  
      DBI::dbGetQuery("SELECT Recepcion, COUNT(*) 
                      FROM data_lake dc
                      WHERE  DATE(dc.Recepcion)>='2024-04-01' AND  DATE(dc.Recepcion)<='2024-04-30' AND dc.Incidente ='Emergencia' AND dc.Subincidente ='Apoyo de SSC'
                      GROUP BY 1;")})
  
  GRAFICA_0_3 <- reactive({
    tabla<-base_0_3() 
    dia<-"Día"
    Grafica_elabora( tabla, "adje", dia)})
  
  output$box_Met_Tot_5 <- renderUI({
    grafica<-GRAFICA_0_3() 
    tot<-3
    tabla<- base_0_3()
    dia<-"Día"
    value_box_inciden_1( Graf=grafica, Met_tot= tot, tabla =tabla, selecion=dia,
                         Let_inc=  div(h4("totaes", class="incidente-letra"), div(actionLink('act_Data_0_2', icon("display"), class="down-icon"), class= "incidente-down"), class="conten-letras"),
                         iconos= "user-shield", color= "#C8DC27", sizes=4,
                         nombre= "big_card_4", js_code=NULL) })
  
  


 
  
  # direc_fil<-reactive({req(input$search_busines)
  #   filter_table(dir,input$search_busines, input$search_size)
  # })
  
  # mapa_1<-reactive({
  #   dire<-direc_fil()
  #   leaflet() %>%setView( -100.34, 20.7, 4.5 ) %>% addTiles() %>%
  #     addPolygons(data = mx, weight = 2, fillOpacity = 0, color = '#e88b0c',  label = ~nom_ent ) %>%
  #     addAwesomeMarkers(data = dire, lat = ~lat, lng = ~long,  label = ~htmlEscape(nombre), icon = awesomeIconList(icons(dire$icon_ncio, "green", dire$color, FALSE )),
  #                       clusterOptions = markerClusterOptions(),
  #                       popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",dire$nombre,  "</div>"),
  #                                     dire$dir_mapa, "<br>")) })
  # 
  # output$Mapas_1<-renderLeaflet({ mapa_1() })
  # dsele<-reactive({ filter_map_1(direc_fil(),input$search_pharm) })
  # 
  # observeEvent(input$search_pharm, {
  #   if(input$search_pharm!="-"){
  #   proxy <- leafletProxy('Mapas_1')
  #   row_selected<-dsele()
  #   lg <- row_selected$long
  #   lt <- row_selected$lat
  #   
  # proxy %>% clearGroup(group = "added_point") %>% 
  #           addMarkers(data = row_selected, lat = row_selected$lat, lng = row_selected$long, label = ~htmlEscape(nombre),  group  = "added_point",
  #                      icon = ipabloa, #awesomeIconList(icons(row_selected$icon_ncio, "red", "#1788d3", TRUE)), 
  #                      popup = paste(paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ", row_selected$nombre,  "</div>"),
  #                      row_selected$dir_mapa, "<br>")) %>% setView(lg, lt, 11)
  #   }
  # })
  
  })

