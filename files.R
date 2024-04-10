conn<-dbConnect(RSQLite::SQLite(), "patrimonial.db")

ids <- reactive({
  out <- NULL
  tryCatch({
    out <- conn %>% tbl('data_cam') %>% collect() %>%
      mutate(created_at = as.POSIXct(created_at, tz = "UTC"), modified_at = as.POSIXct(modified_at, tz = "UTC") ) %>% arrange(desc(modified_at))
  }, error = function(err) {
    cat("Database Connection Error")
    cat(error)
    showToast("error", msg)})
  out
})


as<-IData %>% mutate(Recepcion=as_date(Recepcion)) %>%  filter(Recepcion==(Sys.Date()-days(1))) %>% left_join(semaforo) %>%left_join(iconos) %>% 
  left_join(dir_far) %>% left_join(dir_cons) %>% left_join(farmacias)



