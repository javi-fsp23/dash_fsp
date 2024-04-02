function(input, output, session) {
    session$userData$email="Javier"
    #auth <- secure_server(check_credentials = check_credentials(credentials))
   callModule(ids_table_module, "ids_table")
}


# credentials <- data.frame(
#   user = c("javi", "javi_1",  "javi_2"),
#   password = c(scrypt::hashPassword("javi"), scrypt::hashPassword("javi_1"), scrypt::hashPassword("javi_2")),
#   is_hashed_password = TRUE,
#   comment = c("super", "user", "user"),
#   stringsAsFactors = FALSE)
