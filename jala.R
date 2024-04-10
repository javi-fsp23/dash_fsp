library(RODBC)
library(lubridate)
library(dplyr)
library(stringr)
library(telegram.bot)

hoy<-Sys.Date()-days(8)

read_semaforo<-function(){
semaforo <- readxl::read_excel("data/semaforo.xlsx", sheet = "semaforo") %>%  distinct(Incidente, Color)
return(semaforo)}

read_farmacias <- function() {
  mensaje_telegram("iniciando lectura de archivo de *domicilio de unidades*...")
  if (file.exists("data/Farmacias.xlsx")) {
    tryCatch({
      farmacias <- readxl::read_excel("data/Farmacias.xlsx") %>%
        tidyr::separate(Code, sep = ",", c("lat", "long")) %>%
        mutate(lat = as.numeric(lat), long = as.numeric(long)) %>%
        select(1:5)
      mensaje_telegram("Lectura y proceso con exito...!")
    }, error = function(e) {
      mensaje_telegram("Mensaje de error:", conditionMessage(e))
    })
  } else {
    mensaje_telegram("*No se encontró el archivo de domicilio de unidades* (data/Farmacias.xlsx)...")
    farmacias <- NULL
  }
  return(farmacias)
}


read_iconos<-function(){
iconos<- readxl::read_excel("data/semaforo.xlsx", sheet = "icon")
return(iconos)}

directorio_farmacia<-function(hoy){
  dir_far<- readxl::read_excel(paste("data/DIRECTORIO FARMACIA", str_to_upper(format(hoy, "%B %Y")) ,"POR ESTADO.xlsx"), sheet = paste(str_to_title(format(hoy, "%B"))))
  dir_far<-dir_far[which(dir_far[,2]=="Nombre"):nrow(dir_far), 1:28]
  names(dir_far)<-dir_far[1,]
  dir_far<-dir_far[2:nrow(dir_far),]
  dir_far<-dir_far[is.na(dir_far[,1])==F,]
  names(dir_far)[c(1,3,4,8,9,11,12)]<-c("unidad", "modelo_fcia", "tipo_fcia", "ger_fcia", "tel_ger_fcia", "ger_zona", "tel_ger_zona" )
  dir_far<-dir_far %>% select(c(1,3,4,8,9,11,12)) %>% mutate(ger_fcia=str_to_title(ger_fcia), ger_zona=str_to_title(ger_zona))
  return(dir_far)}


directorio_consul<-function(hoy){
  dir_cons<-readxl::read_excel(paste0("data/Directorio Consultorios " , str_to_title(format(hoy, "%B_%Y")), ".xlsx"))
  dir_cons<-dir_cons[which(dir_cons[,2]=="Núm"):nrow(dir_cons),]
  dir_cons<-dir_cons[is.na(dir_cons[,2])==F,]
  names(dir_cons)<-dir_cons[1,]
  dir_cons<-dir_cons[3:nrow(dir_cons),2:ncol(dir_cons)]
  names(dir_cons)[c(3,7,8,18,19,20)]<-c("unidad","cel_consul", "mail_consul", "ger_siniestro_consul","tel_ger_consul", "zona")
  dir_cons<-dir_cons %>% select(c(3,7,8,18,19,20)) %>% mutate(tel_ger_consul= gsub(" ", "", tel_ger_consul)) %>% 
  mutate(cel_consul=gsub(" ", "", cel_consul)) 
  #mutate(tel_ger_consul=mapply(separar_con_espacio,tel_ger_consul))
  return(dir_cons)}

map_icon<-function(semaforo, iconos){
as<-semaforo%>%left_join(iconos)
return(as)}

map_dir<-function(dir_far, dir_cons, farmacias){
ad<-dir_far %>% left_join(dir_cons) %>% left_join(farmacias) %>% 
  mutate(dir_mapa=paste("<b>", "Gerente de farmacia: ","</b>", ger_fcia, "<br>",
                        "<b>", "Tel. Gerente de farmacia: ","</b>", paste0("<a href=tel:+", tel_ger_fcia,">", tel_ger_fcia, "</a>"), "<br>",
                        "<b>", "Gerente de zona: ","</b>", ger_zona, "<br>",
                        "<b>", "Tel. Gerente de zona: ","</b>", paste0("<a href=tel:+", tel_ger_zona,">", tel_ger_zona, "</a>"), "<br>",
                        "<b>", "Tel. consultorio: ","</b>", paste0("<a href=tel:+", cel_consul,">", cel_consul, "</a>"), "<br>",
                        "<b>", "Ger. siniestro consultorio: ","</b>", ger_siniestro_consul, "<br>",
                        "<b>", "Tel. Ger. siniestro consultorio: ","</b>", paste0("<a href=tel:+", tel_ger_consul,">", tel_ger_consul, "</a>"), "<br>")) %>% 
  select(unidad, dir_mapa)
return(ad)}

read_cam<-function(){
  conn <-  odbcConnectAccess2007("Z://Matriz/data/LDBN.accdb")
  tablas_conn<-sqlTables(conn, tableType = "TABLE")$TABLE_NAME
  IData <- sqlFetch(conn, "IData") 
  names(IData)<-c("Id", "Folio", "Operador", "Unidad", "Recepcion", "Asignacion", "Creacion", "Incidente",    
                  "Subincidente", "Reporta", "Cargo", "Cierre", "Observaciones", "Realizo" ) 
  
  IData<-IData%>% 
    mutate(unidad= gsub("[^0-9]", "", Unidad)) %>% mutate(unidad=as.character(as.numeric(unidad))) %>% 
    mutate(Unidad=iconv(toupper(Unidad), "latin1", "ASCII", sub = "")) %>% 
    mutate(Unidad=case_when(Unidad=="SALA 128"~"F-SALA 128", .default = as.character(Unidad))) %>%    
    mutate(negocio = case_when(grepl("^[0-9]+$", Unidad )~"Farmacia",
                               .default = as.character(Unidad))) %>% 
    mutate(negocio = case_when(grepl("F-", Unidad )~"Farmacia",
                               grepl("C-", Unidad )~"Consultorio",
                               grepl("CL-", Unidad )~"Clinica",
                               .default = as.character(negocio) )) %>% 
    mutate(negocio= case_when(grepl("CCSJ", negocio )~"CALLSJ",
                              grepl("CN", negocio )~"CNAP",
                              grepl("CC", negocio )~"CCOY",
                              grepl("CCU", negocio )~"CALLUN",
                              grepl("PUE", negocio )~"SCPUE",
                              grepl("ISIDRO", negocio )~"SC152",
                              grepl("REZ", negocio )~"SCPUE",
                              grepl("TLAQ", negocio )~"SC164",
                              .default = as.character(negocio))) %>% 
    mutate(unidad= case_when((is.na(unidad)==T & grepl("CCSJ", Unidad ))~"CALLSJ", 
                             (is.na(unidad)==T & grepl("CN", Unidad ))~"CNAP",
                             (is.na(unidad)==T & grepl("CC", Unidad ))~"CCOY",
                             (is.na(unidad)==T & grepl("CCU", Unidad ))~"CALLUN",
                             (is.na(unidad)==T & grepl("PUE", Unidad ))~"SCPUE",
                             (is.na(unidad)==T & grepl("ISIDRO", Unidad ))~"152",
                             (is.na(unidad)==T & grepl("REZ", Unidad ))~"SCPUE",
                             (is.na(unidad)==T & grepl("TLAQ", Unidad ))~"164",
                             .default = as.character(unidad))) %>% 
    mutate(negocio=gsub(" ", "", negocio)) %>% 
    mutate(negocio=case_when(grepl("SALA164", negocio)~"SC164", .default = as.character(negocio)) ) %>% 
    mutate(Unidad=negocio) %>% mutate(negocio=case_when(grepl("SC", negocio)~"Sala de Capacitación",
                                                        grepl("CALL", negocio)~"Call Center",
                                                        grepl("CNAP", negocio)~"Corporativo",
                                                        grepl("CCOY", negocio)~"Corporativo",
                                                        .default = as.character(negocio))) %>% 
  mutate(unidad=case_when(is.na(unidad)==T~Unidad, .default = as.character(unidad))) %>% 
  select(-Reporta,-Unidad ) %>% mutate(Id=as.character(Id), Folio=as.character(Folio)) %>% 
  mutate(Asignacion=as.character(dmy_hm(Asignacion)), Creacion=as.character(dmy_hm(Creacion)), Recepcion=as.character(dmy_hm(Recepcion)), Cierre=as.character(dmy_hm(Cierre))) 
  odbcClose(conn)
  return(IData)}


bot_token <- "7018311523:AAEB-UhXqmcJVSvhwhwhtbL7rwVpt4bLFbM"
mensaje_telegram<-function(mensaje){
  chat_id <- "-1002029344439"#"1451164616"# 
  future::future(Bot(token = bot_token)$sendMessage(chat_id=chat_id, text = mensaje, parse_mode = "Markdown"))
  cat(mensaje, "\n")}



separar_con_espacio <- function(texto) {
  caracteres_con_espacio <- character(0)
  for (i in seq(1, nchar(texto), by = 2)) {
    sub <- substr(texto, i, i + 1)
    caracteres_con_espacio <- c(caracteres_con_espacio, sub)
  }
  return(paste(caracteres_con_espacio, collapse = " "))}



# Conectarse a la base de datos

#write.csv(IData, "base.csv" ) 

uni<-unidades %>% group_by(unidad) %>% summarise(n=n()) 
neg<-IData %>% group_by(negocio) %>% summarise(n=n()) 


unidad<-IData %>% group_by(Unidad) %>% summarise(n=n())
IData %>% mutate(Creación=as_date(Creación)) %>% filter(Creación==Sys.Date()) %>% nrow()




# semaforo<-readxl::read_excel("data/semaforo.xlsx", sheet = "semaforo") %>%  distinct(incidente, color)
# farmacias<-readxl::read_excel("data/Farmacias.xlsx") %>% tidyr::separate(Code, sep=",",c("lat", "long")) %>% 
#   mutate(lat=as.numeric(lat), long=as.numeric(long))

as<-IData %>% mutate(Recepcion=as_date(Recepcion)) %>%  filter(Recepcion==(Sys.Date()-days(1))) %>% left_join(semaforo) %>%left_join(iconos) %>% 
  left_join(dir_far) %>% left_join(dir_cons) %>% left_join(farmacias)
