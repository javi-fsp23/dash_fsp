porcentaje_dia<-function(us1, sele){#us1<-tabla
  if(nrow(us1)>=2){
    if(sele=="Día"){   
      us0<-round(100*(1- (us1[(nrow(us1)-1),2]/us1[(nrow(us1)),2]) ),2) 
      us1<-ifelse(us0<=0, paste( paste("<b><span style=\"color:#88185B\">",us0,"%", sep=""), "menos el último día", " &#8681;"),  paste("<b><span style=\"color:#88185B\">","+", paste(us0,"%", sep=""), "más el último día",  "&#8679;"))
      us1=list( us0, us1)
    }else if(sele=="Semana"){  
      us0<-round(100*(1- (us1[(nrow(us1)-1),2]/us1[(nrow(us1)),2]) ),2) 
      us1<-ifelse(us0<=0, paste( paste("<b><span style=\"color:#88185B\">",us0,"%", sep=""), "menos la última semana", " &#8681;"),  paste("<b><span style=\"color:#88185B\">","+", paste(us0,"%", sep=""), "más la última semana",  "&#8679;"))
      us1=list( us0, us1)
    }else{
      us0<-round(100*(1- (us1[(nrow(us1)-1),2]/us1[(nrow(us1)),2]) ),2) 
      us1<-ifelse(us0<=0, paste( paste("<b><span style=\"color:#88185B\">",us0,"%", sep=""), "menos el último mes", " &#8681;"),  paste("<b><span style=\"color:#88185B\">","+", paste(us0,"%", sep=""), "más el último mes",  "&#8679;"))
      us1=list( us0, us1)}
  }else{us1=list(paste("."), paste("."))}
  return(us1)}


value_box_inciden_1<-function( Let_inc, tabla, Met_tot, selecion, Graf, iconos, color0 , sizes, nombre, js_code){#selecion="Día"
  col=column(style_value(color0, nombre), width = sizes, class="incidente-columna", onclick=paste0(js_code),
             div(id=paste0("incidente-card-",nombre), class="incidente-card",  
                 h4(Let_inc, class="incidente-letra"), 
                 div(h1(Met_tot, class="incidente-metrica"), h4(paste0("(",cal_prom(tabla), ")"), class="incidente-media"), class="incidente-numbers"), 
                 div(Graf, class="inciden-graph"),
                 div(HTML(paste(porcentaje_dia(tabla, selecion)[[2]])), class="inciden-compa"),
                 div(icon(iconos), class="incidente-icon"), style=paste("color:", color0)))
  return(col)}

style_value<-function(colores0, nombre){
  as<-tags$style(HTML(paste0("
  #incidente-card-", nombre, "{             
      width: 100%;
      height: 100%;
      padding: 2em 1.5em;
      background: linear-gradient(#fafbfd 50%, ", colores0, " 50%);
      background-size: 100% 200%;
      background-position: 0 9%;
      border-radius: 12px;
      -webkit-box-shadow: 3px 7px 5px 0px rgba(122,117,122,1);
      -moz-box-shadow: 3px 7px 5px 0px rgba(122,117,122,1);
       box-shadow: 3px 7px 5px 0px rgba(122,117,122,1);
      transition: 0.5s;
      padding: 0px !important;}" )))
  return(as)}

cal_prom<-function(base1){
  if(base1[1,1]!=0 & nrow(base1)>1){
    base1<-format(round(sum(base1$n)/ as.numeric(difftime(max(base1$fecha),min(base1$fecha), unit="days")), 0), big.mark = ",")
  }else{
    base1<-0}
  return(base1)}

tot_metrica<-function(base1){
  if(base1[1,1]!=0){
    base1<-format(sum(base1$n), big.mark = ",")
  }else{
    base1<-0}
  return(base1)
}

hc_theme_sparkline <- function(...) {
  theme <- list(
    chart = list(backgroundColor = NULL, margins = c(0, 0, 0, 0),
                 spacingTop = 0, spacingRight = 0, spacingBottom = 0, spacingLeft = 0,
                 plotBorderWidth = 0, borderWidth = 0, style = list(overflow = "visible") ),
    xAxis = list(visible = FALSE, endOnTick = FALSE, startOnTick = FALSE, lineWidth = 0 ),
    yAxis = list( visible = FALSE, endOnTick = FALSE, startOnTick = FALSE, lineWidth = 0 ),
    tooltip = list(outside = FALSE, shadow = FALSE, borderColor = "transparent", botderWidth = 0,
                   backgroundColor = "transparent", style = list(textOutline = "5px white") ),
    credits = list(enabled = FALSE, text = ""))
  theme <- structure(theme, class = "hc_theme")
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...))
  }
  theme
}

Grafica_elabora<-function(ca, nombres, agrup){
  names(ca)<-c("Fecha", "n" )
  grafica<- hchart( ca, name=nombres, fillOpacity = 0.3, "area", hcaes(x =Fecha, y = n )) %>% 
    hc_size(height = 100) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline()) %>% 
    hc_chart(zoomType = "x") %>%
    hc_tooltip(crosshairs = T, shared = F, 
               borderWidth = 3,
               sort = TRUE,
               style = list(align = "center",color = "#184b76", fillColor="#E8E0E7", fontFamily = "Montserrat"),
               headerFormat = paste(agrup, "<b>{point.key}</b><br>")) %>% 
    hc_plotOptions( series = list(marker = list(enabled = FALSE),
                                  shadow = FALSE, fillOpacity = 1, color = "#1788d3",
                                  fillColor = list(linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
                                                   stops = color_stops(n = 2, colors = c("#1788d3","white")))))
  return(grafica)}
