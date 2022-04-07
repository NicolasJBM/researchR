#' @name documents_list
#' @title List all document
#' @author Nicolas Mangin
#' @description Function listing all the documents, checking whether they were changed, and update information about changed documents.
#' @return Save the document list as a tibble in the folder "2_documents" and returns this list
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom tools md5sum
#' @importFrom tidyr unnest
#' @importFrom dplyr inner_join
#' @importFrom dplyr anti_join
#' @importFrom tidyr separate
#' @importFrom stringr str_remove_all
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @export

documents_list <- function(){
  path <- NULL
  documents <- tibble::tibble(
    file = base::list.files("3_documents", full.names = FALSE),
    path = base::list.files("3_documents", full.names = TRUE)
  ) |>
    dplyr::mutate(
      tags = purrr::map(path, searchR::documents_get_meta)
    ) |>
    tidyr::unnest(tags)
  pdf <- base::list.files("www/pdf")
  missinglitnotes <- base::setdiff(pdf, documents$tag_pdf)
  if (base::length(missinglitnotes) > 0){
    for (pdfname in missinglitnotes){
      base::writeLines(
        c(
          "",
          "",
          "",
          "Meta-information",
          "================",
          "exextra[title]:Title.  ",
          "exextra[keywords]:  ",
          "exextra[type]:Literature  ",
          base::paste0("exextra[tag_pdf]:", pdfname, "  "),
          ""
        ),
        base::paste0(
          "3_documents/",
          searchR::documents_make_new_name("L")
        )
      )
    }
  }
  documents <- tibble::tibble(
    file = base::list.files("3_documents", full.names = FALSE),
    path = base::list.files("3_documents", full.names = TRUE)
  ) |>
    dplyr::mutate(
      tags = purrr::map(path, searchR::documents_get_meta)
    ) |>
    tidyr::unnest(tags) |>
    dplyr::left_join(searchR::data_document_types, by = "type")
  return(documents)
}

