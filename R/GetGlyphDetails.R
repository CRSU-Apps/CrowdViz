#' Get the glyph details based on the number of people on the plot.
#'
#' @param glyph_count The number of glyphs on the plot
#' @param glyph Name of builtin glyph, or filepath to `.svg` file. Defaults to NULL, where the glyph will be chosen based on `glyph_count`
#' Builtin glyphs:
#' - "person"
#' - "person-narrow"
#' - "person-super-narrow"
#' - "person-dress"
#' - "person-dress-narrow"
#' - "person-dress-super-narrow"
#' @param glyph_size Size of the glyph to render. It is related to the size of the used `.svg` file.
#' Defaults to NULL where the size is chosen based on `glyph`
#' @param glyph_resolution Resolution of the glyph to render.
#' Defaults to NULL where the resolution is chosen based on `glyph`
#'
#' @return List containing:
#' - "filename": The path to the glyph file
#' - "size": the size of the glyph to be rendered
#' - "resolution": The resolution of each glyph
GetGlyphDetails <- function(glyph_count, glyph=NULL, glyph_size=NULL, glyph_resolution=NULL) {
  if ((is.null(glyph) && glyph_count <= 20) || (!is.null(glyph) && glyph == "person")) {
    dynamic_person_file <- system.file("person-solid.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 8, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 200, glyph_resolution)
    
  } else if ((is.null(glyph) && glyph_count <= 50) || (!is.null(glyph) && glyph == "person-narrow")) {
    dynamic_person_file <- system.file("person-narrow.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 5, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 100, glyph_resolution)
    
  } else if ((is.null(glyph) && glyph_count <= 100) || (!is.null(glyph) && glyph == "person-super-narrow")) {
    dynamic_person_file <- system.file("person-super-narrow.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 3, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 100, glyph_resolution)
    
  } else if (!is.null(glyph) && glyph == "person-dress") {
    dynamic_person_file <- system.file("person-dress-solid.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 8, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 200, glyph_resolution)
    
  } else if (!is.null(glyph) && glyph == "person-dress-narrow") {
    dynamic_person_file <- system.file("person-dress-narrow.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 5, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 100, glyph_resolution)
    
  } else if (!is.null(glyph) && glyph == "person-dress-super-narrow") {
    dynamic_person_file <- system.file("person-dress-super-narrow.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 3, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 100, glyph_resolution)
    
  } else {
    dynamic_person_file <- glyph
    dynamic_person_size <- ifelse(is.null(glyph_size), 5, glyph_size)
    dynamic_person_resolution <- ifelse(is.null(glyph_resolution), 100, glyph_resolution)
  }
  
  return(
    list(
      filename = dynamic_person_file,
      size = dynamic_person_size,
      resolution = dynamic_person_resolution
    )
  )
}
