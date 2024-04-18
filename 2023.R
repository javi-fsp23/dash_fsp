
library(DBI)
library(RSQLite)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/javier.ruiz/Desktop/apps/CAM/patrimonial.db")

dir<-con %>% 
  dbGetQuery("SELECT * from mapa_dir md ;")

bot_token <- "7018311523:AAEB-UhXqmcJVSvhwhwhtbL7rwVpt4bLFbM"
mensaje_telegram<-function(mensaje){
  chat_id <- "-1002029344439"#"1451164616"# 
  future::future(Bot(token = bot_token)$sendMessage(chat_id=chat_id, text = mensaje, parse_mode = "Markdown"))
  cat(mensaje, "\n")}

icon<- function(icono, color_m ,color){
  makeAwesomeIcon(icon= icono, 
                  markerColor=color_m,
                  iconColor  = color)
  }


filter_map_1<-function(filtro_uni){
if(is.null(filtro_uni) ){
  dir<-NULL
}else{
  dir<-dir %>% filter(unidad==filtro_uni)
}
return(dir)}


save(credentials, dir, mx, filter_map_1, icons, bot_token, mensaje_telegram, file = ".RData")
