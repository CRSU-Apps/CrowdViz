#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_continuous geom_line xlim ylim annotate theme_void theme ggtitle element_text
#' 
NULL

person_solid <- file.path("inst","person-solid.svg")
person_narrow <- file.path("inst","person-narrow.svg")
person_super_narrow <- file.path("inst","person-super-narrow.svg")

.onLoad <- function (libname, pkgname) {
  ns <- topenv()
  ns$person_solid <- system.file("person-solid.svg", package = "PopViz")
  ns$person_narrow <- system.file("person-narrow.svg", package = "PopViz")
  ns$person_super_narrow <- system.file("person-super-narrow.svg", package = "PopViz")
}