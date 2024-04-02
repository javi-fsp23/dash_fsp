
id_edit_module <- function(input, output, session, modal_title, id_to_edit, modal_trigger) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- id_to_edit()
    
    showModal(
      modalDialog(
        fluidRow(
          column(width = 6,
              textInput(
              ns("model"), 'Unidad',
              value = ifelse(is.null(hold), "", hold$model)),
            selectInput(
              ns('am'), 'Incidente',
              choices = c('Automatic', 'Manual'),
              selected = ifelse(is.null(hold), "", hold$am)),
            numericInput( 
              ns('hp'), 'Asignación',
              value = ifelse(is.null(hold), "", hold$hp),
              min = 0, step = 1)),
          column(
            width = 6,
            numericInput(
              ns('mpg'),'Negocio',
              value = ifelse(is.null(hold), "", hold$mpg),
              min = 0,step = 0.1),
            numericInput(
              ns('disp'), 'Sub incidente',
              value = ifelse(is.null(hold), "", hold$disp),
              min = 0, step = 0.1),
            numericInput(
              ns('drat'), 'Estatus',
              value = ifelse(is.null(hold), "", hold$drat),
              min = 0,step = 0.01))
        ),
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancel'),
          actionButton(
            ns('submit'),
            'Añadir', class = "btn-primary",
          )
        )
      )
    )
    
    # Observe event for "Model" text input in Add/Edit Car Modal
    # `shinyFeedback`
    observeEvent(input$model, {
      if (input$model == "") {
        shinyFeedback::showFeedbackDanger(
          "model", text = "Must enter model of id!")
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("model")
        shinyjs::enable('submit')
      }
    })
  })
  
  
  edit_id_dat <- reactive({
    hold <- id_to_edit()
    
    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        "model" = input$model,
        "mpg" = input$mpg,
        "cyl" = input$cyl,
        "disp" = input$disp,
        "hp" = input$hp,
        "drat" = input$drat,
        "wt" = input$wt,
        "qsec" = input$qsec,
        "vs" = input$vs,
        "am" = input$am,
        "gear" = input$gear,
        "carb" = input$carb
      )
    )
    
    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
    
    if (is.null(hold)) {
      # adding a new id
      out$data$created_at <- time_now
      out$data$created_by <- session$userData$email
    } else {
      # Editing existing id
      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }
    
    out$data$modified_at <- time_now
    out$data$modified_by <- session$userData$email
    
    out
  })
  
  validate_edit <- eventReactive(input$submit, {
    dat <- edit_id_dat()
    
    # Logic to validate inputs...
    
    dat
  })
  
  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()
    
    tryCatch({
      if (is.na(dat$uid)) {
        # creating a new id
        uid <- uuid::UUIDgenerate()
        
        dbExecute(
          conn,
          "INSERT INTO mtcars (uid, model, mpg, cyl, disp, hp, drat, wt, qsec, vs, am,
          gear, carb, created_at, created_by, modified_at, modified_by) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)",
          params = c(
            list(uid),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing id
        dbExecute(
          conn,
          "UPDATE mtcars SET model=$1, mpg=$2, cyl=$3, disp=$4, hp=$5, drat=$6,
          wt=$7, qsec=$8, vs=$9, am=$10, gear=$11, carb=$12, created_at=$13, created_by=$14,
          modified_at=$15, modified_by=$16 WHERE uid=$17",
          params = c(
            unname(dat$data),
            list(dat$uid)
          )
        )
      }
      
      session$userData$DtId_trigger(session$userData$DtId_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {
      
      msg <- paste0(modal_title, " Error")
      
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })
  
}
 
# id_edit_module <- function(input, output, session, modal_title, id_to_edit, modal_trigger) {
#   ns <- session$ns
#   
#   observeEvent(modal_trigger(), {
#     hold <- id_to_edit()
#     
#     showModal(
#       modalDialog(
#         fluidRow(
#           column(
#             width = 6,  
#             textInput(ns("nombre"), 'Nombre', value = ifelse(is.null(hold), "", hold$model)),
#             textInput(ns("apellido_pat"), 'Apellido paterno', value = ifelse(is.null(hold), "", hold$apellido_pat)),
#             textInput(ns("apellido_mat"), 'Apellido Materno', value = ifelse(is.null(hold), "", hold$apellido_mat)),
#             numericInput( ns('edad'), 'Edad', value = ifelse(is.null(hold), "", hold$edad), min = 17, step = 1),
#             dateInput( ns('fecha_nacimiento'), value = ifelse(is.null(hold), "", hold$fecha_nacimiento), "Fecha de nacimiento", min = Sys.Date()-years(18), language = "es")),
#           column(    
#             width = 6,
#             textInput(ns("rfc"), 'RFC', placeholder =13, value = ifelse(is.null(hold), "", hold$rfc)),
#             textInput(ns("sexon"), 'Sexo',placeholder =1, value = ifelse(is.null(hold), "", hold$sexon)),
#             textInput(ns("curp"), 'CURP', placeholder =17, value = ifelse(is.null(hold), "", hold$curp)),
#             selectInput(ns("estatus"), "Estatus", c("Disponible", "Ocupado"), selected = ifelse(is.null(hold), "", hold$estatus)),
#             textInput(ns("lugar_trabajo"), 'Lugar de trabajo', value = ifelse(is.null(hold), "", hold$lugar_trabajo)))),
#         title = modal_title,
#         size = 'm',
#         footer = list(
#           modalButton('Cancel'),
#           actionButton(
#             ns('submit'),
#             'Añadir',
#             class = "btn btn-primary",
#             style = "color: white"))))
#     
# 
#     observeEvent(input$nombre, {
#       if (input$nombre == "") {
#         shinyFeedback::showFeedbackDanger("nombre", text = "Debe ser un nombre!")
#         shinyjs::disable('submit')
#       } else {
#         shinyFeedback::hideFeedback("nombre")
#         shinyjs::enable('submit')
#       }
#     })
#     })
#   
#   
#   edit_data <- reactive({
#     hold <- id_to_edit()
#     
#     out <- list(
#       uid = if (is.null(hold)) NA else hold$uid,
#       data = list(
#         "nombre" = input$nombre,
#         "apellido_pat" = input$apellido_pat,
#         "apellido_mat" = input$apellido_mat, 
#         "edad" = input$edad,
#         "fecha_nacimiento" = input$fecha_nacimiento,
#         "rfc" = input$rfc,
#         "sexon" = input$sexon,
#         "curp" = input$curp,
#         "estatus" = inp$estatus, 
#         "lugar_trabajo" = input$lugar_trabajo))
# # "nombre", "apellido_pat", "apellido_mat", "edad", "fecha_nacimiento", "rfc", "sexon", "curp", "estatus",  "lugar_trabajo"      
#     time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
#     if (is.null(hold)) {
#       out$data$created_at <- time_now
#       out$data$created_by <- session$userData$email
#     } else {
#       out$data$created_at <- as.character(hold$created_at)
#       out$data$created_by <- hold$created_by
#     }
#     
#     out$data$modified_at <- time_now
#     out$data$modified_by <- session$userData$email
#     
#     out
#   })
#   
#   
#   validate_edit <- eventReactive(input$submit, {
#     dat <- edit_data()
#     dat})
#   
#   
#   observeEvent(validate_edit(), {
#     removeModal()
#     dat <- validate_edit()
#     
#     tryCatch({
#       if (is.na(dat$uid)) {
#         # creating a new car
#         uid <- uuid::UUIDgenerate()
#         
#         dbExecute(
#           conn,
#           "INSERT INTO datos_personales_cap (nombre, apellido_pat, apellido_mat, edad, fecha_nacimiento, rfc, sexon, curp, estatus, lugar_trabajo, created_at, created_by, modified_at, modified_by) VALUES
#           (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)",
#           params = c(list(uid), unname(dat$data))
#         )
#       } else {
#         # editing an existing car
#         dbExecute(
#           conn,
#           "UPDATE datos_personales_cap SET nombre=$1, apellido_pat=$2, apellido_mat=$3, edad=$4, fecha_nacimiento=$5, rfc=$6,
#           sexon=$7, curp=$8, estatus=$9, lugar_trabajo=$10, created_at=$11, created_by=$12, modified_at=$13, modified_by=$14,
#            WHERE uid=$17",
#           params = c( unname(dat$data), list(dat$uid)) )
#       }
#       
#       session$userData$dt_trigger(session$userData$dt_trigger() + 1)
#       showToast("success", paste0(modal_title, " Successs"))
#     }, error = function(error) {
#       
#       msg <- paste0(modal_title, " Error")
#       print(msg)
#       print(error)
#       showToast("error", msg)
#     })
#   })
#   
# }