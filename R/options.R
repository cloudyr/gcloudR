to_load <- c("bigQueryR",
             # "googleAnalyticsR",
             "googleAuthR",
             "googleCloudStorageR",
             "googleComputeEngineR",
             "googleKubernetesR",
             "googleLanguageR"
             # "searchConsoleR"
             )

.onAttach <- function(...) {
  needed <- to_load[!is_attached(to_load)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  attach_me()

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
