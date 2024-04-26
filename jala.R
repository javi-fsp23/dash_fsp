bot_token <- "7018311523:AAEB-UhXqmcJVSvhwhwhtbL7rwVpt4bLFbM"
mensaje_telegram<-function(mensaje){
  chat_id <- "-1002029344439"#"1451164616"# 
  future::future(Bot(token = bot_token)$sendMessage(chat_id=chat_id, text = mensaje, parse_mode = "Markdown"))
  cat(mensaje, "\n")}


con_db %>% DBI::dbGetQuery(
  "
SELECT subconsulta.unidad,nombre_fcia, lat, long, dc.zona
FROM (
    SELECT cdu.unidad, cdu.nombre_fcia,  cdu.lat, cdu.long, COUNT(*) AS cantidad
    FROM cat_domicilio_unidades cdu 
    GROUP BY cdu.unidad, cdu.lat, cdu.long
) AS subconsulta
LEFT JOIN dir_consultorio dc ON subconsulta.unidad = dc.unidad;")
