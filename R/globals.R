#' Global Project
#'
#' Wrapper for \link{bqr_get_global_project()} that can be used for any GCP service if it needs the project-id
#'
#' @export
gcp_get_project <- function(){
  bqr_get_global_project()
}
