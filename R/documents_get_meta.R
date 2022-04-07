#' @name documents_get_meta
#' @title Get a document's meta-information
#' @author Nicolas Mangin
#' @description Function reading a document and retrieving the tags in meta-information.
#' @param path character. Path to the document for which meta-information should be retrieved.
#' @return A single-row tibble with all the tags from a document.
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove_all
#' @importFrom tidyr separate
#' @importFrom tidyr pivot_wider
#' @export


documents_get_meta <- function(path){
  content <- NULL
  lines <- base::readLines(path)
  metainfo <- lines[base::match('Meta-information', lines):base::length(lines)][-c(1,2)]
  metainfo <- metainfo[stringr::str_detect(metainfo, "exextra")]
  metainfo <- tibble::tibble(
    metainfo = metainfo
  ) |>
    dplyr::mutate(metainfo = stringr::str_remove_all(metainfo, "exextra\\[")) |>
    dplyr::mutate(metainfo = stringr::str_remove_all(metainfo, "\\]")) |>
    dplyr::mutate(metainfo = base::trimws(metainfo)) |>
    tidyr::separate("metainfo", into = c("tag","content"), sep = ":")|>
    base::suppressWarnings() |>
    dplyr::mutate(
      tag = base::trimws(tag),
      content = base::trimws(content)
    ) |>
    tidyr::pivot_wider(names_from = "tag", values_from = "content")
}

