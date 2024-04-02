library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), "datos_cap.db")

dbCreateTable(con, "datos_personales_cap", 
              fields = c(
                uid="text",
                nombre = "text",
                apellido_pat = "text",
                apellido_mat = "text",
                fecha_nacimiento = "date",
                edad="integer",
                rfc="text",
                sexon="integer",
                curp="text", 
                estatus="text",
                lugar_trabajo="text",
                Created_At = "date",
                Created_By = "text",
                Modified_At = "date",
                Modified_By = "text"))

dbCreateTable(con, "datos_sexo", 
              fields = c(
                sexon="integer",
                sexo="text"))
