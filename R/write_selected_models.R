write_selected_models <- function(details, res.dir, folder) {
  
  write.table(
    details,
    file = file.path(res.dir, folder, "selected_models.csv"),
    append = TRUE,
    sep = ";",
    row.names = FALSE,
    col.names = !file.exists(file.path(res.dir, folder, "selected_models.csv"))
  )
}