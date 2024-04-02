
id_delete_module <- function(input, output, session, modal_title, id_to_delete, modal_trigger) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    # Authorize who is able to access particular buttons (here, modules)
    #req(session$userData$email == 'tycho.brahe@tychobra.com')
    showModal(
      modalDialog(
        div( style = "padding: 30px;", class = "text-center",
          h2(style = "line-height: 1.75;",
            paste0( 'Are you sure you want to delete the "', id_to_delete()$model,'"?' ))),
        title = modal_title, size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(ns("submit_delete"), "Delete Car", class = "btn-danger", style="color: #fff;"  ) )
      ) )
  })
  
  observeEvent(input$submit_delete, {
    req(id_to_delete())
    removeModal()
    
    tryCatch({
      uid <- id_to_delete()$uid
      DBI::dbExecute(conn, "DELETE FROM mtcars WHERE uid=$1", params = c(uid))
      session$userData$DtId_trigger(session$userData$DtId_trigger() + 1)
      showToast("success", "Id Successfully Deleted")
    }, error = function(error) {
      msg <- "Error Deleting Id"
      print(msg)
      print(error)
      showToast("error", msg)
    })
  })
}

# id_delete_module <- function(input, output, session, modal_title, id_to_delete, modal_trigger) {
#   ns <- session$ns
#   # Observes trigger for this module (here, the Delete Button)
#   observeEvent(modal_trigger(), {
#     showModal(
#       modalDialog(
#         div(style = "padding: 30px;",  class = "text-center",
#           h2(style = "line-height: 1.75;",
#             paste0( 'Are you sure you want to delete the "', id_to_delete()$model, '"?'))),
#         title = modal_title,
#         size = "m",
#         footer = list(
#           modalButton("Cancel"),
#           actionButton(
#             ns("submit_delete"),
#             "Delete Car",
#             class = "btn-danger",
#             style="color: #fff;"
#           )
#         )
#       )
#     )
#   })
#   
#   observeEvent(input$submit_delete, {
#     req(id_to_delete())
#     removeModal()
#     tryCatch({
#       uid <- id_to_delete()$uid
#       DBI::dbExecute( conn, "DELETE FROM datos_personales_cap WHERE nombre=$1", params = c(uid) )
#       session$userData$dt_trigger(session$userData$dt_trigger() + 1)
#       showToast("success", "Car Successfully Deleted")
#     }, error = function(error) {
#       
#       msg <- "Error Deleting Car"
#       print(msg)
#       print(error)
#       showToast("error", msg)
#     })
#   })
# }