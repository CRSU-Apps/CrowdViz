#' Create a population visualisation for absolute effects.
#'
#' @param glyph_count Number of glyphs being rendered
#' @param person_multiplier The number of people that each glyph represents. Defaults to 1
#' @param event_desired TRUE if events are desired, else FALSE
#' @param outcome_name Name of event being measured
#' @param reference_name Name of reference intervention
#' @param treatment_name Name of intervention being compared
#' @param outcome_type Type of outcome being measured
#' @param reference_probability Probability of event in reference intervention
#' @param relative_effect Point estimate of relative effect
#' @param relative_confidence_interval Vector of lower and upper limits of confidence interval of relative effect
#' @param title Title to add to graphic. If NULL, the title will be created from the data. Defaults to NULL
#' @param colour_palette Either "auto" or "colourblind" for predefined palettes, or a list containing the colours to display:
#' - "base" Colour for all unaffected glyphs
#' - "positive_relative_effect" Colour for all glyphs positively affected by the treatment
#' - "negative_relative_effect" Colour for all glyphs negatively affected by the treatment
#' - "common_effect" Colour for all glyphs affected by both the reference and the treatment
#' Defaults to "auto"
#' @param glyph Name of builtin glyph, or filepath to `.svg` file. Defaults to NULL, where the glyph will be chosen based on `glyph_count`
#' Builtin glyphs:
#' - "person"
#' - "person-narrow"
#' - "person-super-narrow"
#' - "person-dress"
#' @param glyph_size Size of the glyph to render. It is related to the size of the used `.svg` file.
#' Defaults to NULL where the size is chosen based on `glyph`
#'
#' @return ggplot2 plot object
#' @export
#' 
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_continuous geom_line xlim ylim annotate theme_void theme ggtitle element_text
#'
#' @examples
#' # For some reason, {ggplot2} must be loaded explicitly
#' library(ggplot2)
#' 
#' PopViz::PopViz(
#'   glyph_count = 5,
#'   event_desired = FALSE,
#'   outcome_name = "Adverse Events",
#'   reference_name = "Standard Care",
#'   treatment_name = "Treatment",
#'   outcome_type = "RD",
#'   reference_probability = 0.5,
#'   relative_effect = 0.3,
#'   relative_confidence_interval = c(0.1, 0.5)
#' )
#' 
#' PopViz::PopViz(
#'   glyph_count = 15,
#'   person_multiplier = 4,
#'   event_desired = FALSE,
#'   outcome_name = "Adverse Events",
#'   reference_name = "Standard Care",
#'   treatment_name = "Treatment",
#'   outcome_type = "RD",
#'   reference_probability = 0.5,
#'   relative_effect = -0.35,
#'   relative_confidence_interval = c(-0.4, -0.3),
#'   colour_palette = "colourblind"
#' )
PopViz <- function(
    glyph_count,
    person_multiplier = 1,
    event_desired,
    outcome_name,
    reference_name,
    treatment_name,
    outcome_type,
    reference_probability,
    relative_effect,
    relative_confidence_interval,
    title=NULL,
    colour_palette="auto",
    glyph=NULL,
    glyph_size=NULL
    ) {
  
  if (glyph_count > 100) {
    stop("Please specify 100 or fewer people")
  }

  if (outcome_type == "RD") {
    treatment_probability <- reference_probability + relative_effect
    treatment_confidence_interval <- reference_probability + relative_confidence_interval
  } else if (outcome_type == "RR") {
    treatment_probability <- reference_probability * relative_effect
    treatment_confidence_interval <- reference_probability * relative_confidence_interval
  } else if (outcome_type == "OR") {
    treatment_probability <- (reference_probability * relative_effect) / (1 - reference_probability + reference_probability * relative_effect)
    treatment_confidence_interval <- (reference_probability * relative_confidence_interval) / (1 - reference_probability + reference_probability * relative_confidence_interval)
  } else {
    stop(glue::glue("Outcome type'{OutcomeType}' not supported. Please use one of ['RD', 'RR', 'OR']"))
  }
  
  person_count = glyph_count * person_multiplier
  
  reference_person_count = round(person_count * reference_probability)
  treatment_person_count = round(person_count * treatment_probability)
  glyph_spacing = 1 / (glyph_count - 1)
  
  # Plot formatting
  plot <- ggplot() +
    ylim(0.5, 1.5) +
    xlim(0, 1.2) +
    theme_void() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  plot <- .PlotConfidenceInterval(
    plot,
    glyph_spacing * (treatment_person_count / person_multiplier - 0.5),
    treatment_confidence_interval
  )
  plot <- .PlotAxes(
    plot,
    glyph_count,
    person_multiplier,
    reference_person_count,
    treatment_person_count,
    glyph_spacing,
    reference_name,
    treatment_name,
    reference_probability,
    treatment_probability,
    treatment_confidence_interval
  )
  plot <- .PlotGlyphs(
    plot,
    glyph_count,
    person_multiplier,
    reference_person_count,
    treatment_person_count,
    glyph_spacing,
    event_desired,
    colour_palette,
    glyph,
    glyph_size
  )
  plot <- .PlotTitle(
    plot,
    title,
    person_count,
    outcome_name,
    reference_name,
    treatment_name,
    reference_probability,
    treatment_probability
  )
  
  return(plot)
}

#' Plot the axes, ticks and labels
#'
#' @param plot {ggplot2} object to which to add elements
#' @param glyph_count Number of glyphs being rendered
#' @param person_multiplier The number of people that each glyph represents
#' @param reference_person_count Number of people affected by the reference
#' @param treatment_person_count number of people affected by the treatment
#' @param glyph_spacing Spacing between glyphs
#' @param reference_name Name of reference
#' @param treatment_name Name of treatment
#' @param reference_probability Probability of reference affecting a person
#' @param treatment_probability Probability of treatment affecting a person
#' @param treatment_confidence_interval Confidence interval of treatment effect
#'
#' @return {ggplot2} object
.PlotAxes <- function(
    plot,
    glyph_count,
    person_multiplier,
    reference_person_count,
    treatment_person_count,
    glyph_spacing,
    reference_name,
    treatment_name,
    reference_probability,
    treatment_probability,
    treatment_confidence_interval
    ) {
  # Horizontal lines
  reference_axis_position <- data.frame(
    x = c(0, 1),
    y = c(1.25, 1.25)
  )
  
  treatment_axis_position <- data.frame(
    x = c(0, 1),
    y = c(0.75, 0.75)
  )
  
  reference_tick_position_x = glyph_spacing * (reference_person_count / person_multiplier - 0.5)
  treatment_tick_position_x = glyph_spacing * (treatment_person_count / person_multiplier - 0.5)
  
  # Tick marks, rounding to the nearest person
  reference_tick_position <- data.frame(
    x = c(reference_tick_position_x, reference_tick_position_x),
    y = c(1.275, 1.225)
  )
  treatment_tick_position <- data.frame(
    x = c(treatment_tick_position_x, treatment_tick_position_x),
    y = c(0.725, 0.7755)
  )
  
  person_count = glyph_count * person_multiplier
  
  # Reference axis, tick and label
  plot <- plot +
    geom_line(
      data = reference_axis_position,
      mapping = aes(x, y),
      linewidth = 1.5
    ) +
    geom_line(
      data = reference_tick_position,
      mapping = aes(x, y),
      linewidth = 1.5,
    ) +
    annotate(geom = "text", x = 1.11, y = 1.25, label = reference_name)
  
  # Reference label
  plot <- plot +
    annotate(
      geom = "text",
      label = glue::glue("{round(person_count * reference_probability)} out of {person_count}"),
      x = reference_tick_position_x,
      y = 1.35
    )
    
  # Treatment axis, tick and label
  plot <- plot +
    geom_line(
      data = treatment_axis_position,
      mapping = aes(x,y),
      linewidth = 1.5,
      colour = "blue"
    ) +
    geom_line(
      data = treatment_tick_position,
      mapping = aes(x,y),
      linewidth = 1.5,
      colour = "white"
    ) +
    annotate(geom = "text", x = 1.11, y = 0.75, label = treatment_name)
  
  # Treatment labels
  plot <- plot +
    annotate(
      geom = "text",
      label = glue::glue("{round(person_count * treatment_probability)} out of {person_count}"),
      x = treatment_tick_position_x,
      y = 0.65
    ) +
    annotate(
      geom = "text",
      label = paste0(
        "95% CI: ",
        round(person_count * treatment_confidence_interval[1]),
        " to ",
        round(person_count * treatment_confidence_interval[2])
      ),
      x = treatment_tick_position_x,
      y = 0.58,
      size = 3.2
    )
  
  return(plot)
}

#' Render the people on the plot.
#'
#' @param plot {ggplot2} object to which to add elements
#' @param glyph_count Number of glyphs being rendered
#' @param person_multiplier The number of people that each glyph represents
#' @param reference_person_count Number of people affected by the reference
#' @param treatment_person_count number of people affected by the treatment
#' @param glyph_spacing Spacing between glyphs
#' @param event_desired TRUE if events are desired, else FALSE
#' @param colour_palette Either "auto" or "colourblind" for predefined palettes, or a list containing the colours to display:
#' - "base" Colour for all unaffected glyphs
#' - "positive_relative_effect" Colour for all glyphs positively affected by the treatment
#' - "negative_relative_effect" Colour for all glyphs negatively affected by the treatment
#' - "common_effect" Colour for all glyphs affected by both the reference and the treatment
#' @param glyph Name of builtin glyph, or filepath to `.svg` file. Defaults to NULL, where the glyph will be chosen based on `glyph_count`
#' Builtin glyphs:
#' - "person"
#' - "person-narrow"
#' - "person-super-narrow"
#' - "person-dress"
#' @param glyph_size Size of the glyph to render. It is related to the size of the used `.svg` file.
#' Defaults to NULL where the size is chosen based on `glyph`
#'
#' @return {ggplot2} object
.PlotGlyphs <- function(
    plot,
    glyph_count,
    person_multiplier,
    reference_person_count,
    treatment_person_count,
    glyph_spacing,
    event_desired,
    colour_palette="auto",
    glyph=NULL,
    glyph_size=NULL
    ) {
  
  if (is.character(colour_palette)) {
    if (colour_palette == "auto") {
      colour_palette <- list(
        base = "#999999",
        positive_relative_effect = "#00ff00",
        negative_relative_effect = "#ff0000",
        common_effect = "#ffaa00"
      )
    } else if (colour_palette == "colourblind") {
      # Colours chosen using: https://davidmathlogic.com/colorblind/#%23999999-%23117733-%23882255-%23DDAA00
      colour_palette <- list(
        base = "#999999",
        positive_relative_effect = "#117733",
        negative_relative_effect = "#882255",
        common_effect = "#ddaa00"
      )
    }
  }
  
  all_people_positions <- data.frame(
    x = seq(0, 1, length = glyph_count),
    y = rep(1, glyph_count)
  )
  
  common_affected_person_count = min(treatment_person_count, reference_person_count)
  common_affected_glyph_count = as.integer(common_affected_person_count / person_multiplier)
  common_affected_person_positions <- data.frame(
    x = seq(0, glyph_spacing * (common_affected_glyph_count - 1), length = common_affected_glyph_count),
    y = rep(1, common_affected_glyph_count)
  )
  
  relative_affected_person_count = max(treatment_person_count, reference_person_count)
  relative_affected_glyph_count = as.integer(relative_affected_person_count / person_multiplier)
  relative_effected_person_positions <- data.frame(
    x = seq(0, glyph_spacing * (relative_affected_glyph_count - 1), length = relative_affected_glyph_count),
    y = rep(1, relative_affected_glyph_count)
  )
  
  glyph_data <- .GetGlyphFile(glyph_count, glyph, glyph_size)
  
  svg_text_raw <- ReadSvgText(filename = glyph_data$filename)
  svg_text_base <- ModifySvgText(svg_text_raw, colour = colour_palette$base)
  svg_text_affected <- ModifySvgText(svg_text_raw, colour = colour_palette$common_effect)
  
  if (xor(treatment_person_count < reference_person_count, event_desired)) {
    relative_affected_colour <- colour_palette$positive_relative_effect
  } else {
    relative_affected_colour <- colour_palette$negative_relative_effect
  }
  svg_text_relative_affected <- ModifySvgText(svg_text_raw, colour = relative_affected_colour)
  
  plot <- plot +
    # All people in base colour
    ggsvg::geom_point_svg(
      data = all_people_positions,
      mapping = aes(x, y),
      svg = svg_text_base,
      size = glyph_data$size
    ) +
    # People showing relative effect, from zero, up to maximum of comparator or treatment
    ggsvg::geom_point_svg(
      data = relative_effected_person_positions,
      mapping = aes(x, y),
      svg = svg_text_relative_affected,
      size = glyph_data$size
    ) +
    # People showing common effect, from zero, up to minimum of comparator or treatment
    ggsvg::geom_point_svg(
      data = common_affected_person_positions,
      mapping = aes(x, y),
      svg = svg_text_affected,
      size = glyph_data$size
    )
  
  # Plot partial people for relative effects
  plot <- .PlotPartialGlyph(
    plot,
    relative_affected_person_count,
    person_multiplier,
    glyph_spacing,
    relative_affected_colour,
    glyph_data$size,
    svg_text_raw
  )
  # Plot partial people for common effects
  plot <- .PlotPartialGlyph(
    plot,
    common_affected_person_count,
    person_multiplier,
    glyph_spacing,
    colour_palette$common_effect,
    glyph_data$size,
    svg_text_raw
  )
  
  return(plot)
}

#' Plot a partial person overlapping whole people.
#' 
#' @param plot {ggplot2} object to which to add elements
#' @param person_count total number of people represented in this category. Not the whole number represented in the graphic
#' @param person_multiplier The number of people that each glyph represents
#' @param glyph_spacing Spacing between glyphs
#' @param glyph_colour Colour to display the glyph
#' @param glyph_size Size of glyph
#' @param svg_text_raw Raw text for SVG to be modified
#'
#' @return {ggplot2} object
.PlotPartialGlyph <- function(plot, person_count, person_multiplier, glyph_spacing, glyph_colour, glyph_size, svg_text_raw) {
  glyph_count = as.integer(person_count / person_multiplier)
  proportion = (person_count / person_multiplier) %% 1
  
  if (proportion != 0) {
    person_position <- data.frame(
      x = glyph_spacing * glyph_count,
      y = 1
    )
    
    svg_text_partial <- ModifySvgText(svg_text_raw, colour = glyph_colour, end_proportion = proportion)
    
    plot <- plot +
      # Partial person showing relative effect
      ggsvg::geom_point_svg(
        data = person_position,
        mapping = aes(x, y),
        svg = svg_text_partial,
        size = glyph_size * proportion,
        hjust = (1 - proportion) / (2 * proportion) + 0.5
      )
  }
  
  return(plot)
}

#' Get the glyph file based on the number of people on the plot.
#'
#' @param glyph_count The number of glyphs on the plot
#' @param glyph Name of builtin glyph, or filepath to `.svg` file. Defaults to NULL, where the glyph will be chosen based on `glyph_count`
#' Builtin glyphs:
#' - "person"
#' - "person-narrow"
#' - "person-super-narrow"
#' - "person-dress"
#' @param glyph_size Size of the glyph to render. It is related to the size of the used `.svg` file.
#' Defaults to NULL where the size is chosen based on `glyph`
#'
#' @return List containing:
#' - "filename": The path to the glyph file
#' - "size": the size of the glyph to be rendered
.GetGlyphFile <- function(glyph_count, glyph=NULL, glyph_size=NULL) {
  if ((is.null(glyph) && glyph_count <= 20) || (!is.null(glyph) && glyph == "person")) {
    dynamic_person_file <- system.file("person-solid.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 8, glyph_size)
  } else if ((is.null(glyph) && glyph_count <= 50) || (!is.null(glyph) && glyph == "person-narrow")) {
    dynamic_person_file <- system.file("person-narrow.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 5, glyph_size)
  } else if ((is.null(glyph) && glyph_count <= 100) || (!is.null(glyph) && glyph == "person-super-narrow")) {
    dynamic_person_file <- system.file("person-super-narrow.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 3, glyph_size)
  } else if (!is.null(glyph) && glyph == "person-dress") {
    dynamic_person_file <- system.file("person-solid-dress.svg", package="PopViz")
    dynamic_person_size <- ifelse(is.null(glyph_size), 5, glyph_size)
  } else {
    dynamic_person_file <- glyph
    dynamic_person_size <- ifelse(is.null(glyph_size), 5, glyph_size)
  }
  
  return(
    list(
      filename = dynamic_person_file,
      size = dynamic_person_size
    )
  )
}

#' Plot the confidence interval gradient.
#'
#' @param plot {ggplot2} object to which to add elements
#' @param treatment_probability Probability of treatment affecting a person
#' @param treatment_confidence_interval Confidence interval of treatment effect
#'
#' @return {ggplot2} object
.PlotConfidenceInterval <- function(plot, treatment_probability, treatment_confidence_interval) {
  tile_data <- data.frame(
    x = seq(0, 1, length.out = 1001),
    y = 0.75
  ) |>
    dplyr::mutate(
      dens = dnorm(
        x,
        treatment_probability,
        # Make gradient match 95% CI, which is 1.96 standard deviations either side of the mean
        (treatment_confidence_interval[2] - treatment_confidence_interval[1]) / (2 * 1.96)
      )
    )
  
  plot <- plot +
    geom_tile(data = tile_data,
              aes(x = x, y = y, fill = dens),
              height = 0.1,
              width = 0.001,
              hjust = 0) +
    scale_fill_continuous(low = "white", high = "#5555ff")
  
  return(plot)
}

#' Add the title to the plot.
#'
#' @param plot {ggplot2} object to which to add elements
#' @param title Title to add to graphic. If NULL, the title will be created from the data. Defaults to NULL
#' @param person_count Number of people being represented
#' @param outcome_name Name of event being measured
#' @param reference_name Name of reference
#' @param treatment_name Name of treatment
#' @param reference_probability Probability of reference affecting a person
#' @param treatment_probability Probability of treatment affecting a person
#'
#' @return {ggplot2} object
.PlotTitle <- function(
    plot,
    title,
    person_count,
    outcome_name,
    reference_name,
    treatment_name,
    reference_probability,
    treatment_probability
    ) {
  
  if (!is.null(title)) {
    plot <- plot +
      ggtitle(label = title)
    
    return(plot)
  }
  
  if (treatment_probability > reference_probability) {
    plot <- plot +
      ggtitle(
        label = stringr::str_wrap(
          paste0(
            "In a group of ",
            person_count,
            " People, ",
            treatment_name,
            " increases the number of ",
            outcome_name,
            " by ",
            round(person_count * treatment_probability) - round(person_count * reference_probability),
            " on average ",
            "compared to ",
            reference_name
          ),
          width = 60
        )
      )
  } else if (treatment_probability == reference_probability) {
    plot <- plot +
      ggtitle(
        label = stringr::str_wrap(
          paste0(
            "In a group of ",
            person_count,
            " People, ",
            treatment_name,
            " does not change the number of ",
            outcome_name,
            " on average ",
            "compared to ",
            reference_name
          ),
          width = 60
        )
      )
  } else if (treatment_probability < reference_probability) {
    plot <- plot +
      ggtitle(
        label = stringr::str_wrap(
          paste0(
            "In a group of ",
            person_count,
            " People, ",
            treatment_name,
            " decreases the number of ",
            outcome_name,
            " by ",
            round(person_count * reference_probability) - round(person_count * treatment_probability),
            " on average ",
            "compared to ",
            reference_name
          ),
          width = 60
        )
      )
  }
  
  return(plot)
}
