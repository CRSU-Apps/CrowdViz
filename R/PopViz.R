library(ggplot2)

#' Create a population visualisation for absolute effects.
#'
#' @param NoPeople Number of people to display
#' @param DesireEvent TRUE if events are desired, else FALSE
#' @param OutcomeName Name of event being measured
#' @param TreatmentName Name of intervention being compared
#' @param ComparatorName Name of reference intervention
#' @param OutcomeType Type of outcome being measured
#' @param RelEff Point estimate of relative effect
#' @param RelConfInt Vector of lower and upper limits of confidence interval of relative effect
#' @param ComProb Probability of event in reference intervention
#'
#' @return ggplot2 plot object
#' @export
#'
#' @examples
#' PopViz(NoPeople = 50,
#' DesireEvent = FALSE,
#' OutcomeName = "Adverse Effects",
#' TreatmentName = "Treatment",
#' ComparatorName = "Standard Care",
#' OutcomeType = "RD",
#' ComProb = 0.5,
#' RelEff = 0.2,
#' RelConfInt = c(0.1, 0.3)
#' )
PopViz <- function(NoPeople, DesireEvent, OutcomeName, TreatmentName, ComparatorName,
                   OutcomeType, RelEff, RelConfInt, ComProb, ComConfInt, Title=NULL) {

  if (OutcomeType == "RD") {

    TrtProb <- ComProb + RelEff
    TrtConfInt <- ComProb + RelConfInt

  }

  if (OutcomeType == "RR") {

    TrtProb <- ComProb * RelEff
    TrtConfInt <- ComProb * RelConfInt

  }

  if (OutcomeType == "OR") {

    TrtProb <- (ComProb * RelEff) / (1 - ComProb + ComProb * RelEff)
    TrtConfInt <- (ComProb * RelConfInt) / (1 - ComProb + ComProb * RelConfInt)

  }

  TrtCount = round(NoPeople * TrtProb)
  ComCount = round(NoPeople * ComProb)

  person_spacing = 1 / (NoPeople - 1)
  AllPeoplePos <- data.frame(
    x = seq(0, 1, length = NoPeople),
    y = rep(1, NoPeople)
  )

  # horizontal lines
  common_affected_person_count = min(TrtCount, ComCount)
  CommonAffectedPeoplePos <- data.frame(
    x = seq(0, person_spacing * (common_affected_person_count - 1), length = common_affected_person_count),
    y = rep(1, common_affected_person_count)
  )

  relative_affected_person_count = max(TrtCount, ComCount)
  RelativeAffectedPeoplePos <- data.frame(
    x = seq(0, person_spacing * (relative_affected_person_count - 1), length = relative_affected_person_count),
    y = rep(1, relative_affected_person_count)
  )

  LinePos1 <- data.frame(
   x = c(0, 1),
   y = c(0.75, 0.75)
  )

  LinePos2 <- data.frame(
    x = c(0, 1),
    y = c(1.25, 1.25)
  )

  # tick marks
  # rounding down to the nearest person
  LinePos3 <- data.frame(
    x = c( (2 * round(NoPeople * TrtProb, 0) - 1) / (2 * (NoPeople - 1)),
           (2 * round(NoPeople * TrtProb, 0) - 1) / (2 * (NoPeople - 1))
           ),
    y = c(0.725, 0.7755)
  )

  LinePos4 <- data.frame(
    x = c( (2 * round(NoPeople * ComProb, 0) - 1) / (2 * (NoPeople - 1)),
           (2 * round(NoPeople * ComProb, 0) - 1) / (2 * (NoPeople - 1))
    ),
    y = c(1.275, 1.225)
  )


  tile_dat <-
    data.frame(
      x = seq(1, 0, length.out = 1000),
      y = LinePos1$y[1]) |>
    dplyr::mutate(
      dens = dnorm(x, LinePos3$x[1], 0.05)
    )

## Dynamic Glyphing
  
  if (NoPeople <= 10) {
    dynamic_person_file <- system.file("person-solid.svg", package="PopViz")
  } else if (10 < NoPeople & NoPeople <= 50) {
    dynamic_person_file <- system.file("person-narrow.svg", package="PopViz")
  } else if (50 < NoPeople & NoPeople <= 100) {
    dynamic_person_file <- system.file("person-super-narrow.svg", package="PopViz")
  } else {
    stop("Please specify 100 or fewer people")
  }
  
  
  svg_text_base <- GetSvgText(filename = dynamic_person_file, colour = "#444444")
  svg_text_affected <- GetSvgText(filename = dynamic_person_file, colour = "#ffaa00")

  if (xor(TrtCount < ComCount, DesireEvent)) {
    svg_text_relative_affected <- GetSvgText(filename = dynamic_person_file, colour = "#00ff00")
  } else {
    svg_text_relative_affected <- GetSvgText(filename = dynamic_person_file, colour = "#ff0000")
  }



  plot <- ggplot() +

    geom_tile(data = tile_dat,
              aes(x = x, y = y, fill = dens),
              height = 0.1,
              width = 0.01) +
    scale_fill_continuous(low="white", high="black") +

    # All people in base colour
    ggsvg::geom_point_svg(
      data = AllPeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text_base,
      size     = 3
    ) +

    # People showing relative effect, from zero, up to maximum of
    # comparator or treatment
    ggsvg::geom_point_svg(
      data = RelativeAffectedPeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text_relative_affected,
      size     = 3,
      svg_width = 100
    ) +

    # People showing common effect, from zero, up to minimum of
    # comparator or treatment
    ggsvg::geom_point_svg(
      data = CommonAffectedPeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text_affected,
      size     = 3,
      svg_width = 100
    ) +

    geom_line(
      data = LinePos1,
      mapping = aes(x,y),
      linewidth = 1.5,
      colour = "blue"
    ) +

    geom_line(
      data = LinePos2,
      mapping = aes(x,y),
      linewidth = 1.5
    ) +

    geom_line(
      data = LinePos3,
      mapping = aes(x,y),
      linewidth = 1.5,
      colour = "white"
    ) +

    geom_line(
      data = LinePos4,
      mapping = aes(x,y),
      linewidth = 1.5,
    ) +

    ylim(0, 2) + xlim(0, 1.2) +

    annotate("text", x = 1.11, y = 0.75, label = TreatmentName) +

    annotate("text", x = 1.11, y = 1.25, label = ComparatorName) +

    annotate("text",
             label = paste0(round(NoPeople*TrtProb,0), " out of ", NoPeople),
             x = (2 * round(NoPeople * TrtProb, 0) - 1) / (2 * (NoPeople - 1)), y = 0.65) +

    annotate("text",
             label = paste0("95% CI: ", round(NoPeople*TrtConfInt[1], 0), " to ", round(NoPeople*TrtConfInt[2], 0)),
             x = (2 * round(NoPeople * TrtProb, 0) - 1) / (2 * (NoPeople - 1)), y = 0.58,
             size = 3.2) +

    annotate("text",
             label = paste0(round(NoPeople*ComProb,0), " out of ", NoPeople),
             x = (2 * round(NoPeople * ComProb, 0) - 1) / (2 * (NoPeople - 1)), y = 1.35) +

    theme_void() + theme(legend.position = "none")

    # # Dynamic default title

    if (is.null(Title)) {

      if (TrtProb > ComProb) {
        plot <- plot + ggtitle(label = stringr::str_wrap(
                                      paste0("In a group of ", NoPeople, " People, ",
                                      TreatmentName, " increases the number of ",
                                      OutcomeName, " by ",
                                      (round(NoPeople*TrtProb,0)) -
                                      (round(NoPeople*ComProb,0)), " on average ",
                                      "compared to ", ComparatorName),
                                      60)
                                     ) +
          theme(plot.title = element_text(hjust = 0.5))
      }

       if (TrtProb == ComProb) {
         plot <- plot + ggtitle(label = paste0("In a group of ", NoPeople, " People, ",
                                               TreatmentName, " does not change the number of ",
                                               OutcomeName, " on average ",
                                               "compared to ", ComparatorName)
         ) +
           theme(plot.title = element_text(hjust = 0.5))
      }

      if (TrtProb < ComProb) {
        plot <- plot + ggtitle(label = paste0("In a group of ", NoPeople, " People, ",
                                              TreatmentName, " decreases the number of ",
                                              OutcomeName, " by ",
                                              (round(NoPeople*ComProb,0)) -
                                                (round(NoPeople*TrtProb,0)), " on average ",
                                              "compared to ", ComparatorName)
        ) +
          theme(plot.title = element_text(hjust = 0.5))
      }

    } else {

      plot <- plot + ggtitle(label=Title) +
        theme(plot.title = element_text(hjust = 0.5))

    }

  return(plot)

}

PopViz(NoPeople = 5,
       DesireEvent = FALSE,
       OutcomeName = "Adverse Events",
       TreatmentName = "Treatment",
       ComparatorName = "Standard Care",
       OutcomeType = "RD",
       ComProb = 0.5,
       RelEff = 0.2,
       RelConfInt = c(0.1, 0.3)
)

