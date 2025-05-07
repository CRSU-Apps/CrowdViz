library(ggplot2)

#=============================================================================

# {metafor} object extraction (rma.uni)

MetaforExtract <- function(model) {

  RelEff <- model$b
  RelConfInt <- c(model$ci.lb, model$ci.ub)
  OutcomeType <- model$measure

}






#==============================================================================

  # NoPeople = number of people to be displayed in the chart
  # DesireEvent = is the outcome/event favourable/desirable (<TRUE/FALSE>)
  # OutcomeName
  # TreatmentName
  # OutcomeType (Relative Risk, Odds Ratio, Risk Difference)
  # RelEff = Relative effect point estimate calculated from meta-analysis
  # RelConfInt = Relative effect confidence interval <c(a,b)>
  # ComProb = Probability of event in comparator group
  # ComConfInt = Confidence interval of probability of event in comparator group

PopViz <- function(NoPeople, DesireEvent, OutcomeName, TreatmentName, ComparatorName,
                   OutcomeType, RelEff, RelConfInt, ComProb, ComConfInt) {

  if (OutcomeType == "RD") {

    TrtProb <- ComProb + RelEff
    TrtConfInt <- ComConfInt + RelConfInt

  }

  if (OutcomeType == "RR") {

    TrtProb <- ComProb * RelEff
    TrtConfInt <- ComConfInt * RelConfInt

  }

  if (OutcomeType == "OR") {

    TrtProb <- (ComProb * RelEff) / (1 - ComProb + ComProb * RelEff)
    TrtConfInt <- (ComConfInt * RelConfInt) / (1 - ComConfInt + ComConfInt * RelConfInt)

  }
  
  TrtCount = round(NoPeople * TrtProb)
  ComCount = round(NoPeople * ComProb)
  
  person_spacing = 1 / (NoPeople - 1)
  AllPeoplePos <- data.frame(
    x = seq(0, 1, length = NoPeople),
    y = rep(1, NoPeople)
  )
  
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


  
  svg_text_base <- .GetSvgText(filename = "svgs/person-super-narrow.svg", colour = "#444444")
  svg_text_affected <- .GetSvgText(filename = "svgs/person-super-narrow.svg", colour = "#ffaa00")
  
  if (xor(TrtCount < ComCount, DesireEvent)) {
    svg_text_relative_affected <- .GetSvgText(filename = "svgs/person-super-narrow.svg", colour = "#00ff00")
  } else {
    svg_text_relative_affected <- .GetSvgText(filename = "svgs/person-super-narrow.svg", colour = "#ff0000")
  }
  
  ggplot() +
    ggsvg::geom_point_svg(
      data = AllPeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text_base,
      size     = 3
    ) +
    ggsvg::geom_point_svg(
      data = RelativeAffectedPeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text_relative_affected,
      size     = 3,
      svg_width = 100
    ) +
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
      colour = "blue"
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
             label = paste0(round(NoPeople*ComProb,0), " out of ", NoPeople),
             x = (2 * round(NoPeople * ComProb, 0) - 1) / (2 * (NoPeople - 1)), y = 1.35) +



    theme_void()

}

PopViz(NoPeople = 50,
       DesireEvent = TRUE,
       # OutcomeName = "Outcome",
       TreatmentName = "Treatment",
       ComparatorName = "Standard Care",
       OutcomeType = "RD",
       ComProb = 0.5,
       ComConfInt = c(0.4, 0.6),
       RelEff = 0.2,
       RelConfInt = c(0.1, 0.3),
       )

