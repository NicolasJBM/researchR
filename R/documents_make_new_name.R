#' @name documents_make_new_name
#' @title Create a document name
#' @author Nicolas Mangin
#' @description Function listing the names of the existing documents and making a new name by adding one to the highest value. This ensures that no old number is assigned again.
#' @param firstletter Character. "L" for literature, "N" for note, "A" for article, "T" for Thesis, "P" for presentation, "V" for video, and "F" for feedback.
#' @return Character. Name of a new document to be created.
#' @importFrom stringr str_remove_all
#' @export


documents_make_new_name <- function(firstletter){
  existing <- base::list.files("3_documents") |>
    stringr::str_remove_all(".Rmd")
  existing <- existing[stringr::str_detect(existing, base::paste0("^", firstletter))]
  if (base::length(existing) > 0){
    maximum <- base::max(base::as.numeric(stringr::str_remove_all(existing, "[A-Z]")))
    newname <- maximum + 1
  } else newname <- 1
  newname <- base::paste(
    c(firstletter,base::paste(base::rep(0,9-base::nchar(newname)), collapse = ""), newname, ".Rmd"),
    collapse = ""
  )
  return(newname)
}

