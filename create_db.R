library(DBI)
library(RSQLite)
source("jala.R")
con <- dbConnect(RSQLite::SQLite(), "patrimonial.db")


semaforo<-read_semaforo()
dbWriteTable(con, "cat_semaforo", semaforo, overwrite = TRUE)


farmacias<-read_farmacias()
#dbCreateTable(con, "cat_domicilio_unidades", farmacias)
dbWriteTable(con, "cat_domicilio_unidades", farmacias, overwrite = TRUE)              

iconos<-read_iconos()
#dbCreateTable(con, "cat_iconos_semaforo", iconos)
dbWriteTable(con, "cat_iconos_semaforo", iconos, overwrite = TRUE)              

dir_far<-directorio_farmacia(hoy)
#dbCreateTable(con, "dir_farmacias", dir_far)
dbWriteTable(con, "dir_farmacias", dir_far, overwrite = TRUE)              

dir_cons<-directorio_consul(hoy)
#dbCreateTable(con, "dir_consultorio", dir_cons)
dbWriteTable(con, "dir_consultorio", dir_cons, overwrite = TRUE)              


mapa_icono<-map_icon(semaforo, iconos)
#dbCreateTable(con, "mapa_icono", mapa_icono)
dbWriteTable(con, "mapa_icono", mapa_icono, overwrite = TRUE)              

mapa_dir<-map_dir(dir_far, dir_cons, farmacias)
#dbCreateTable(con, "mapa_dir", mapa_dir)
dbWriteTable(con, "mapa_dir", mapa_dir, overwrite = TRUE)              

data_cam<-read_cam()
#dbCreateTable(con, "data_cam", data_cam)
column_types <- c(Asignacion = "DATETIME", Creacion = "DATETIME", Recepcion = "DATETIME", Cierre = "DATETIME")
dbWriteTable(con, "data_cam", data_cam, overwrite = TRUE, row.names = FALSE, types = column_types)              
