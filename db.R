library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/javier.ruiz/Desktop/apps/CAM/patrimonial.db")

dir_far<-con %>% dbGetQuery("SELECT * from dir_farmacias df ;")
dir_cons<-con %>% dbGetQuery("SELECT * from dir_consultorio dc ;")
farmacias<-con %>% dbGetQuery("SELECT * from cat_domicilio_unidades ;")
iconos<-readxl::read_excel("C:/Users/javier.ruiz/Desktop/apps/CAM/data/Farmacias.xlsx", 
                        sheet = "iconos")

map_dir<-function(dir_far, dir_cons, farmacias){
  ad<-farmacias %>% left_join(dir_cons) %>%  left_join(dir_far) 
  ad[is.na(ad)] <- "SN INFO"
  ad<- ad %>% mutate(dir_mapa=paste("<b>", "Ger. de farmacia: ","</b>", ger_fcia, "<br>",
                                    "<b>", "Tel. Ger. de farmacia: ","</b>", paste0("<a href=tel:+52", tel_ger_fcia,">", tel_ger_fcia, "</a>"), "<br>",
                                    "<b>", "Ger. de zona: ","</b>", ger_zona, "<br>",
                                    "<b>", "Tel. Ger. de zona: ","</b>", paste0("<a href=tel:+52", tel_ger_zona,">", tel_ger_zona, "</a>"), "<br>",
                                    "<b>", "Tel. consultorio: ","</b>", paste0("<a href=tel:+52", cel_consul,">", cel_consul, "</a>"), "<br>",
                                    "<b>", "Ger. de zona Consul: ","</b>", ger_zona_consul, "<br>",
                                    "<b>", "Tel. Ger. de zona Consul:: ","</b>", paste0("<a href=tel:+52", tel_ger_zona_consul,">", tel_ger_zona_consul, "</a>"), "<br>",
                                    "<b>", "Ger. siniestro consultorio: ","</b>", siniestro_consul, "<br>",
                                    "<b>", "Tel. Ger. siniestro consultorio: ","</b>", paste0("<a href=tel:+52", tel_siniestro_consul,">", tel_siniestro_consul, "</a>"), "<br>")) %>% 
    select(unidad,negocio, modelo_fcia, nombre_fcia, lat, long, dir_mapa) %>% distinct(unidad,negocio, .keep_all = T) %>% mutate(nombre=paste0(unidad,": " ,nombre_fcia)) %>% 
    mutate(lat=as.numeric(lat), long=as.numeric(long))
  return(ad)}

mapa_dir<-map_dir(dir_far, dir_cons, farmacias)
dbWriteTable(con, "mapa_dir", mapa_dir, overwrite = TRUE)   
#dbCreateTable(con, "mapa_dir_icons", iconos)
dbWriteTable(con, "mapa_dir_icons", iconos, overwrite = TRUE)              