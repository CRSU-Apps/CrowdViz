#' Read text content from an SVG file.
#'
#' @return Text extracted from the SVG file
#'
#' @examples
#' GetSvgText()
#' GetSvgText("cat.svg")
ReadSvgText <- function(filename=system.file("person-super-narrow.svg", package="PopViz")) {
  svg_text <- readtext::readtext(filename, verbosity = 0)$text |>
    stringr::str_replace_all("\n\\s*", " ")
  
  return(svg_text)
}
