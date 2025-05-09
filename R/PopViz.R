#' Create a population visualisation for absolute effects.
#'
#' @param person_count Number of people to display
#' @param event_desired TRUE if events are desired, else FALSE
#' @param outcome_name Name of event being measured
#' @param reference_name Name of reference intervention
#' @param treatment_name Name of intervention being compared
#' @param outcome_type Type of outcome being measured
#' @param reference_probability Probability of event in reference intervention
#' @param relative_effect Point estimate of relative effect
#' @param relative_confidence_interval Vector of lower and upper limits of confidence interval of relative effect
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
#'   person_count = 5,
#'   event_desired = FALSE,
#'   outcome_name = "Adverse Events",
#'   reference_name = "Standard Care",
#'   treatment_name = "Treatment",
#'   outcome_type = "RD",
#'   reference_probability = 0.5,
#'   relative_effect = 0.3,
#'   relative_confidence_interval = c(0.1, 0.5)
#' )
PopViz <- function(
    person_count,
    event_desired,
    outcome_name,
    reference_name,
    treatment_name,
    outcome_type,
    reference_probability,
    relative_effect,
    relative_confidence_interval,
    title=NULL
    ) {

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
  
  reference_person_count = round(person_count * reference_probability)
  treatment_person_count = round(person_count * treatment_probability)
  person_spacing = 1 / (person_count - 1)
  
  # Plot formatting
  plot <- ggplot() +
    ylim(0, 2) +
    xlim(0, 1.2) +
    theme_void() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  plot <- .PlotConfidenceInterval(plot, treatment_probability, treatment_confidence_interval)
  plot <- .PlotAxes(plot, person_count, reference_person_count, treatment_person_count, person_spacing, reference_name, treatment_name, treatment_probability, reference_probability, treatment_confidence_interval)
  plot <- .PlotGlyphs(plot, person_count, reference_person_count, treatment_person_count, person_spacing, event_desired)
  plot <- .PlotTitle(plot, title, person_count, outcome_name, reference_name, treatment_name, reference_probability, treatment_probability)
  
  return(plot)
}

#' Plot the axes, ticks and labels
#'
#' @param plot {ggplot2} object to which to add elements
#' @param person_count Number of people being rendered
#' @param reference_person_count Number of people affected by the reference
#' @param treatment_person_count number of people affected by the treatment
#' @param person_spacing Spacing between people
#' @param reference_name Name of reference
#' @param treatment_name NAme of treatment
#' @param treatment_probability Probability of treatment affecting a person
#' @param reference_probability Probability of reference affecting a person
#' @param treatment_confidence_interval Confidence interval of treatment effect
#'
#' @return {ggplot2} object
.PlotAxes <- function(
    plot,
    person_count,
    reference_person_count,
    treatment_person_count,
    person_spacing,
    reference_name,
    treatment_name,
    treatment_probability,
    reference_probability,
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
  
  reference_tick_position_x = person_spacing * (reference_person_count - 0.5)
  treatment_tick_position_x = person_spacing * (treatment_person_count - 0.5)
  
  # Tick marks, rounding to the nearest person
  reference_tick_position <- data.frame(
    x = c(reference_tick_position_x, reference_tick_position_x),
    y = c(1.275, 1.225)
  )
  treatment_tick_position <- data.frame(
    x = c(treatment_tick_position_x, treatment_tick_position_x),
    y = c(0.725, 0.7755)
  )
  
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
#' @param person_count Number of people being rendered
#' @param reference_person_count Number of people affected by the reference
#' @param treatment_person_count number of people affected by the treatment
#' @param person_spacing Spacing between people
#' @param event_desired 
#'
#' @return {ggplot2} object
.PlotGlyphs <- function(plot, person_count, reference_person_count, treatment_person_count, person_spacing, event_desired) {
  all_people_positions <- data.frame(
    x = seq(0, 1, length = person_count),
    y = rep(1, person_count)
  )
  
  common_affected_person_count = min(treatment_person_count, reference_person_count)
  common_affected_person_positions <- data.frame(
    x = seq(0, person_spacing * (common_affected_person_count - 1), length = common_affected_person_count),
    y = rep(1, common_affected_person_count)
  )
  
  relative_affected_person_count = max(treatment_person_count, reference_person_count)
  relative_effected_person_positions <- data.frame(
    x = seq(0, person_spacing * (relative_affected_person_count - 1), length = relative_affected_person_count),
    y = rep(1, relative_affected_person_count)
  )
  
  glyph_data <- .GetGlyphFile(person_count)
  
  svg_text_base <- GetSvgText(filename = glyph_data$filename, colour = "#444444")
  svg_text_affected <- GetSvgText(filename = glyph_data$filename, colour = "#ffaa00")
  
  if (xor(treatment_person_count < reference_person_count, event_desired)) {
    svg_text_relative_affected <- GetSvgText(filename = glyph_data$filename, colour = "#00ff00")
  } else {
    svg_text_relative_affected <- GetSvgText(filename = glyph_data$filename, colour = "#ff0000")
  }
  
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
  
  return(plot)
}

#' Get the glyph file based on the number of people on the plot.
#'
#' @param person_count The number of people on the plot
#'
#' @return List containing:
#' - "filename": The path to the glyph file
#' - "size": the size of the glyph to be rendered
.GetGlyphFile <- function(person_count) {
  if (person_count <= 20) {
    dynamic_person_file <- system.file("person-solid.svg", package="PopViz")
    dynamic_person_size <- 8
  } else if (20 < person_count & person_count <= 50) {
    dynamic_person_file <- system.file("person-narrow.svg", package="PopViz")
    dynamic_person_size <- 5
  } else if (50 < person_count & person_count <= 100) {
    dynamic_person_file <- system.file("person-super-narrow.svg", package="PopViz")
    dynamic_person_size <- 3
  } else {
    stop("Please specify 100 or fewer people")
  }
  
  return(
    list(
      filename = dynamic_person_file,
      size = dynamic_person_size
    )
  )
}

#' Plot the conifdence interval gradient.
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
#' @param title Title of the plot. Can be `NULL` if non specified
#' @param person_count Number of people being rendered
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
