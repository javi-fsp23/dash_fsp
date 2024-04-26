control_select_inci<-function(fecha){#
as<- c("Todo", conn %>%  DBI::dbGetQuery(
paste0("SELECT dc.Incidente
FROM data_lake dc
WHERE  DATE(dc.Recepcion)>= '",fecha[1], "' AND  DATE(dc.Recepcion)<= '",fecha[2], 
"' GROUP BY dc.Incidente;")) %>% pull())
return(as)}


control_select_sub_inci<-function(fecha, inci){ 
if(inci!="Todo"){
as<-c("Todo", conn %>%  DBI::dbGetQuery(
paste0("SELECT dc.Subincidente
FROM data_lake dc
WHERE  DATE(dc.Recepcion)>= '",fecha[1], "' AND  DATE(dc.Recepcion)<= '",fecha[2], "' AND dc.Incidente = '", inci, "'", 
"GROUP BY dc.Subincidente;")) %>% pull())
}else{
as<-c("Todo", conn %>%  DBI::dbGetQuery(
paste0("SELECT dc.Subincidente
FROM data_lake dc
WHERE  DATE(dc.Recepcion)>= '",fecha[1], "' AND  DATE(dc.Recepcion)<= '",fecha[2], "'", 
"GROUP BY dc.Subincidente;")) %>% pull())
}
return(as)}


