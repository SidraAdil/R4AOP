#' List stressors
#'
#' @param q Free-text search term.
#' @param aop Only stressors used in this AOP id.
#' @param chemical Only stressors linked to this chemical (CAS number).
#' @param has_chemical If TRUE, only stressors that have a chemical mapping.
#' @return A tibble with one row per stressor.
#' @export
#' @examples
#' \dontrun{
#' stressor_list(q = "benzene")
#' stressor_list(aop = 1)
#' }
stressor_list <- function(q = NULL, aop = NULL, chemical = NULL,
                          has_chemical = NULL) {
  if (!is.null(has_chemical)) {
    has_chemical <- tolower(as.character(isTRUE(has_chemical)))
  }
  aop_request_all(
    "stressors",
    list(q = q, aop = aop, chemical = chemical, has_chemical = has_chemical)
  )
}

#' Get a single stressor
#'
#' @param stressor_id Stressor identifier.
#' @return A tibble with one row.
#' @export
#' @examples
#' \dontrun{
#' stressor_get(1)
#' }
stressor_get <- function(stressor_id) {
  check_id(stressor_id, "stressor_id")
  as_row(aop_request(paste0("stressors/", stressor_id))$data)
}
