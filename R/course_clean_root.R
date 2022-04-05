#' @name course_clean_root
#' @title Remove temporary files in root
#' @author Nicolas Mangin
#' @description Remove non-essential files and folders from the root of the project.
#' @return Clean project root folder
#' @importFrom usethis create_project
#' @importFrom stringr str_detect
#' @export


course_clean_root <- function(){
  files <- base::list.files(recursive = FALSE)
  pngfiles <- files[stringr::str_detect(files, ".png$")]
  base::suppressMessages(base::file.remove(pngfiles))
  if (base::dir.exists("figure")) base::unlink("figure", recursive = TRUE)
}
