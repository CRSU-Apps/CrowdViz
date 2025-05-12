#' Modify SVG text with the given styling.
#'
#' @param svg_text Text content of a `.svg` file
#' @param colour Hexidecimal colour to set on all paths within the svg. Defaults to "#000000"
#' @param start_proportion Proportion by which to truncate the left side of the image. Defaults to 0
#' @param end_proportion Proportion by which to include along the right side of the image. Defaults to 1
#'
#' @return SVG text modified with the given styling
#'
#' @examples
#' svg_text <- GetSvgText()
#' 
#' ModifySvgText(svg_text)
#' ModifySvgText(svg_text, "#33aaff", 0.2, 0.8)
ModifySvgText <- function(svg_text, colour="#000000", start_proportion=0, end_proportion=1) {
  width = as.integer(stringr::str_extract(svg_text, '<svg.*viewBox="(?:[0-9.]+ ){2}([0-9.]+)', 1))
  start = as.integer(start_proportion * width)
  end = as.integer((end_proportion - start_proportion) * width)
  
  svg_text <- svg_text |>
    # Set the view box to truncate the image left or right
    # Viewbox is of form "X-position Y-position width height"
    stringr::str_replace('(<svg.*viewBox=")[0-9.]+ ([0-9.]+) [0-9.]+ ([0-9.]+")', glue::glue('\\1{start} \\2 {end} \\3')) |>
    # Remove any existing styling
    stringr::str_replace_all('(<path.*?)style=".*?" ?', glue::glue('\\1')) |>
    # Add new colour styling
    stringr::str_replace_all('(<path.*?)( ?\\/>)', glue::glue('\\1 style="fill:{colour}"/>\\2'))
  
  return(svg_text)
}
