#' @name diagram_initialize
#' @title Create a starting diagram
#' @author Nicolas Mangin
#' @description Function creating a basic diagram serving as a basis to be adjusted.
#' @return A list of tibbles specifying nodes, relations, and moderations.
#' @importFrom tibble tibble
#' @export

diagram_initialize <- function(){
  
  nodes <- tibble::tibble(
    label = base::as.character(c("cause", "consequence", "moderator")),
    shape = base::factor(
      c("rectangle", "rectangle", "rectangle"),
      levels = c("ellipse","rectangle")
    ),
    x = c(0, 4, 2),
    y = c(0, 0, 1),
    width = 1,
    height = 0.5,
    penwidth = 1,
    color = base::as.character("black"),
    fillcolor = base::as.character("white"),
    fontsize = 12,
    fontcolor = base::as.character("black"),
    include = base::as.logical(TRUE)
  )
  
  relations <- tibble::tibble(
    relation = base::as.character("cause2consequence"),
    source = base::factor(
      "cause",
      levels = base::unique(nodes$label)
    ),
    target = base::factor(
      "consequence",
      levels = base::unique(nodes$label)
    ),
    style = base::factor("solid", levels = c("solid","dashed")),
    color = base::as.character("black"),
    fontcolor = base::as.character("black"),
    fontsize = 10,
    penwidth = 1,
    arrowhead = base::factor("normal", levels = c("normal","none")),
    label = base::as.character("increases"),
    include = base::as.logical(TRUE)
  )
  
  moderations <- tibble::tibble(
    source = base::factor(
      "moderator",
      levels = base::unique(nodes$label)
    ),
    target = base::factor(
      "cause2consequence",
      levels = unique(relations$relation)
    ),
    style = base::factor("solid", levels = c("solid","dashed")),
    color = base::as.character("black"),
    fontcolor = base::as.character("black"),
    fontsize = 10,
    penwidth = 1,
    arrowhead = base::factor("normal", levels = c("normal","none")),
    label = base::as.character("accentuates"),
    include = base::as.logical(TRUE)
  )
  
  diagram <- base::list(
    nodes = nodes,
    relations = relations,
    moderations = moderations
  )
  
  return(diagram)
}