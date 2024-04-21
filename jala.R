bot_token <- "7018311523:AAEB-UhXqmcJVSvhwhwhtbL7rwVpt4bLFbM"
mensaje_telegram<-function(mensaje){
  chat_id <- "-1002029344439"#"1451164616"# 
  future::future(Bot(token = bot_token)$sendMessage(chat_id=chat_id, text = mensaje, parse_mode = "Markdown"))
  cat(mensaje, "\n")}

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/javier.ruiz/Desktop/apps/CAM/patrimonial.db")
dir<-con %>% dbGetQuery("SELECT unidad, md.negocio, modelo_fcia, nombre_fcia, lat, long, dir_mapa, nombre, mdi.icon_ncio, mdi.color  
FROM mapa_dir md
left join mapa_dir_icons mdi on md.negocio= mdi.negocio  ;")

iconos<-con %>%dbGetQuery("SELECT * from mapa_dir_icons mdi;")
tams<-array(c("Todo", unique(dir$modelo_fcia)[1:6]))

credentials <- data.frame(
  user = c("javi", "Patrimonial",  "matias"),
  password = c(scrypt::hashPassword("javi"), scrypt::hashPassword("sanpablo2024"), scrypt::hashPassword("mati2024*")),
  is_hashed_password = TRUE,
  comment = c("super", "user_1", "user_2"),
  stringsAsFactors = FALSE)

puntos=dir %>% distinct(unidad) %>% pull() 
iconos<-rbind(c("Unidad", "", ""), iconos)
tams<-array(c("Todo", unique(dir$modelo_fcia)[1:6]))

save(credentials, dir, mx, filter_map_1, icons, bot_token, mensaje_telegram, iconos, tams,filter_table, puntos, file = ".RData")

