icons<- function(icono, color_m, color, selec){
  makeAwesomeIcon(icon= icono, library="fa",
                  markerColor=color_m,spin = selec,
                  iconColor= color)}


filter_map_1<-function(puntos,filtro_uni){
if(is.null(filtro_uni) ){
  puntos<-NULL
}else{puntos<-puntos %>% filter(unidad==filtro_uni)}
return(puntos)}


filter_table<-function(dir,selec, selec_1){
if(selec=="Unidad"){
    dir<-dir %>% distinct(unidad, .keep_all = T)
} else{
    dir<-dir %>% filter(negocio==selec)} 
  
  if(selec_1=="Todo"){
    dir<-dir
  }else{
    dir<-dir %>% filter(modelo_fcia==selec_1)
  }
return(dir)}

ipablo <- makeIcon(iconUrl = "www/pablito.PNG", iconWidth = 32, iconHeight = 32)
ipabloa <- makeIcon(iconUrl = "www/pablitoA.PNG", iconWidth = 32, iconHeight = 32)

