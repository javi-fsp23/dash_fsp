
ids_table_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
      div(actionButton(ns("add_id"), "", class = "btn-success", icon = icon('plus'),  width = '100%' ), class="container-success"),
    fluidRow(
      column( width = 12, title = "Incidentes",
        DTOutput(ns('id_table')) %>% withSpinner(),
        tags$br(),
        tags$br()
      ) ),
    tags$script(src = "crud_table_module.js"),
    tags$script(paste0("ids_table_module_js('", ns(''), "')"))
    
  )
}


ids_table_module <- function(input, output, session) {
  
  session$userData$DtId_trigger <- reactiveVal(0)
  
  ids <- reactive({
    session$userData$DtId_trigger()
    out <- NULL
    tryCatch({
      out <- conn %>% tbl('mtcars') %>% collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"), modified_at = as.POSIXct(modified_at, tz = "UTC") ) %>% arrange(desc(modified_at))
    }, error = function(err) {
      cat("Database Connection Error")
      cat(error)
     showToast("error", msg)})
    out
  })
  
  
  id_table_prep <- reactiveVal(NULL)
  
  observeEvent(ids(), {
    out <- ids()
    ids <- out$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )})
    out <- out %>%
      select(-uid)
    out <- cbind(tibble(" " = actions),out)
    
    if (is.null(id_table_prep())) {
       id_table_prep(out)
      
    } else {
      replaceData(id_table_proxy, out, resetPaging = FALSE, rownames = FALSE)}
  })
  
  output$id_table <- renderDT({
    req(id_table_prep())
    out <- id_table_prep()
    
    datatable( out, rownames = FALSE,
      colnames = c('Model', 'Miles/Gallon', 'Cylinders', 'Displacement (cu.in.)',
                   'Horsepower', 'Rear Axle Ratio', 'Weight (lbs)', '1/4 Mile Time',
                   'Engine', 'Transmission', 'Forward Gears', 'Carburetors', 'Created At',
                   'Created By', 'Modified At', 'Modified By'),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1) %>%
      formatDate(
        columns = c("created_at", "modified_at"),
        method = 'toLocaleString') })
  
  id_table_proxy <- DT::dataTableProxy('id_table')
  
  callModule(
    id_edit_module, "add_id",
    modal_title = "Añadir incidente",
    id_to_edit = function() NULL,
    modal_trigger = reactive({input$add_id}) )
  
  id_to_edit <- eventReactive(input$id_id_to_edit, { ids() %>%  filter(uid == input$id_id_to_edit) })
  
  callModule(
    id_edit_module,"edit_id", 
    modal_title = "Editar incidente",
    id_to_edit = id_to_edit,
    modal_trigger = reactive({input$id_id_to_edit}))
  
  id_to_delete <- eventReactive(input$id_id_to_delete, {
      ids() %>% filter(uid == input$id_id_to_delete) %>% as.list()})
  
  callModule(
    id_delete_module, "delete_id",
    modal_title = "Eliminar incidente",
    id_to_delete = id_to_delete,
    modal_trigger = reactive({input$id_id_to_delete}) )
}

# id_table_module_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(
#       column( width = 2,
#         actionButton(ns("add_id"),"Add", class = "btn-success", style = "color: #fff;", icon = icon('plus'), width = '100%'),
#         tags$br(),
#         tags$br())),
#     fluidRow(
#       column( width = 12, title = "Motor Trend Car Road Tests",
#         DTOutput(ns('id_table')) %>%
#           withSpinner(),
#         tags$br(),
#         tags$br()
#       )
#     ),
#     tags$script(src = "crud_table_module.js"),
#     tags$script(paste0("crud_table_module_js('", ns(''), "')")))
# }
# 
#  
# 
# id_table_module <- function(input, output, session) {
#   
#   session$userData$dt_trigger <- reactiveVal(0)
#   id_table_module <- reactive({
#     session$userData$dt_trigger()
#     #dbReadTable(con, "datos_personales_cap")
#     out <- NULL
#     tryCatch({
#       out <- conn %>%
#         tbl('datos_personales_cap') %>%
#         collect() #%>%
#         #mutate( created_at = as.POSIXct(created_at, tz = "UTC"), modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
#       #  arrange(desc(modified_at))
#     }, error = function(err) {
#       msg <- "Database Connection Error"
#       print(msg)
#       print(error)
#       showToast("error", msg)
#     })
#     out
#   })
#   
#   
#   id_table_prep <- reactiveVal(NULL)
#   
#   observeEvent(id_table_module(), {
#     out <- id_table_module()
#     
#     ids <- out$uid
#     
#     actions <- purrr::map_chr(ids, function(id_) {
#       paste0(
#         '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
#           <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
#           <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
#         </div>'
#       )
#     })
#     
#     # Remove the `uid` column. We don't want to show this column to the user
#     out <- out %>%
#       select(-uid)
#     
#     # Set the Action Buttons row to the first column of the `DtId` table
#     out <- cbind(
#       tibble(" " = actions),
#       out
#     )
#     
#     if (is.null(id_table_prep())) {
#       id_table_prep(out)
#     } else {
#       replaceData(id_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
#     }
#   })
#   
#   output$id_table <- renderDT({
#      id_table_prep()})
#   
#   id_table_proxy <- DT::dataTableProxy('id_table')
#   
#   callModule(
#     id_edit_module, "add_id",
#     modal_title = "Add Id",
#     id_table_to_edit = function() NULL,
#     modal_trigger = reactive({input$add_id}))
# 
#   id_table_to_edit <- eventReactive(input$id_to_edit, {
#     id_table_module() %>%
#       filter(uid == input$id_to_edit)})
#   
#   # callModule(
#   #   id_edit_module,
#   #   "edit_car",
#   #   modal_title = "Id Car",
#   #   id_table_to_edit = id_table_to_edit,
#   #   modal_trigger = reactive({input$id_to_edit})
#   # )
#   # 
#   # id_to_delete <- eventReactive(input$id_to_delete, {
#   #   id_table_module() %>%
#   #     filter(uid == input$id_to_delete) %>%
#   #     as.list() })
#   
#   # callModule(
#   #   id_delete_module,
#   #   "delete_id",
#   #   modal_title = "Delete Id",
#   #   id_to_delete = id_to_delete,
#   #   modal_trigger = reactive({input$id_to_delete}))
# }