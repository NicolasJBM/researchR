

data_folders_structure <- tibble::tribble(
  ~folder,      ~path,
  "www","www",
  "www","www/pdf",
  "targets", "_targets",
  "data", "1_data",
  "data","1_data/unstructured",
  "data","1_data/structured",
  "functions","2_functions",
  "documents","3_documents",
  "classification","4_classification",
  "communications", "5_communication"
)
base::save(data_folders_structure, file = "data/data_folders_structure.RData")


data_document_types <- tibble::tibble(
  type = c(
    "Literature","Note","Article","Thesis","Presentation","Video","Feedback"
  ),
  icon = c(
    "book-open","microscope","copy","book","images","video","comment"
  ),
  boxcolor = c(
    "navy","primary","info","teal","success","orange","danger"
  ),
  description = c(
    "Reading notes on and tagging of the literature.",
    "Research notes and ad hoc analyses destined to be published on a blog.",
    "Sections and subsections of a scientific article.",
    "Sections and subsections of a thesis.",
    "Slide shows to communicate findings in meetings and conferences.",
    "Script of videos presenting findings.",
    "Feedback gathered on what is communicated."
  )
)
base::save(data_document_types, file = "data/data_document_types.RData")


data_tags <- tibble::tibble(
  tag = base::character(0),
  order = base::integer(0),
  value = base::character(0),
  count = base::integer(0),
  icon = base::character(0),
  boxcolor = base::character(0)
)
base::save(data_tags, file = "data/data_tags.RData")


data_documents <- tibble::tibble(
  file = base::character(0),
  code = base::character(0),
  language = base::character(0),
  other_languages = base::character(0),
  path = base::character(0),
  modified = base::character(0),
  md5 = base::character(0),
  title = base::character(0),
  type = base::factor(levels = c(
    "Note","Page","Slide","Video","Game",
    "Statements","Alternatives","Computation",
    "Essay","Problem","Case"
  )),
  document = base::character(0)
) |>
  dplyr::mutate(modified = lubridate::as_datetime(modified))
base::save(data_documents, file = "data/data_documents.RData")


data_references <- tibble::tibble(
  key = base::as.character("Mangin2022"),
  order = base::as.character(1),
  bibtype = base::factor("Online", levels = c(
    "Article","InCollection","Book","Unpublished","InProceedings",
    "Online","Review","Letter","Misc","Booklet","InBook","Manual",
    "MasterThesis","PhdThesis","Proceedings")
  ),
  author = "Mangin, Nicolas",
  title = "teachR: Design and continuously improve teaching, learning, and testing materials.",
  journal = base::as.character(NA),
  jnl = base::as.character(NA),
  issn = base::as.character(NA),
  field = "Education",
  year = 2022,
  volume = base::as.integer(NA),
  number = base::as.integer(NA),
  pages = base::as.character(NA),
  doi = base::as.character(NA),
  abstract = base::as.character(NA),
  keywords = base::as.character(NA),
  url = "https://github.com/NicolasJBM/teachR",
  publisher = base::as.character(NA),
  booktitle = base::as.character(NA),
  editor = base::as.character(NA),
  address = base::as.character(NA),
  chapter = base::as.character(NA),
  edition = base::as.character(NA),
  isbn = base::as.character(NA)
)
base::save(data_references, file = "data/data_references.RData")

