person_solid <- NA
person_narrow <- NA
person_super_narrow <- NA
.onLoad <- function (libname, pkgname) {
  warn("TEST")
  ns <- topenv()
  #assign("person_solid",system.file("person-solid.svg", package = "PopViz"), envir = topenv())
  ns$person_solid <- system.file("person-solid.svg", package = "PopViz")
  ns$person_narrow <- system.file("person-narrow.svg", package = "PopViz")
  ns$person_super_narrow <- system.file("person-super-narrow.svg", package = "PopViz")
}