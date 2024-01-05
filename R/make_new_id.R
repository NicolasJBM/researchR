#' @name make_new_id
#' @title New id for databases
#' @author Nicolas Mangin
#' @description Create new ids by incrementing existing ones.
#' @param x Character vector. Vector of existing ids.
#' @param y Character. Prefix of the new id.
#' @return Character. New id
#' @importFrom purrr map
#' @export

make_new_id <- function(x,y){
  if (base::length(x) > 0){
    idnbr <- x |>
      purrr::map(stringr::str_extract_all, pattern = "[0-9]", simplify = TRUE) |>
      purrr::map(paste0, collapse = "") |>
      base::unlist() |>
      base::as.numeric() |>
      base::max()
    idnbr <- idnbr+1
  } else idnbr <- 0
  base::paste0(
    y,
    base::paste0(base::rep(0, 7-base::nchar(idnbr)), collapse  = ""),
    idnbr, collapse  = ""
  )
}

