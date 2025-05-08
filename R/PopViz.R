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

  PeoplePos <- data.frame(
    x = seq(0, 1, length = NoPeople),
    y = rep(1, NoPeople)
  )

  # horizontal lines
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
  
  svg_text <- .GetSvgText()
  
  ggplot() +
    
    geom_tile(data = tile_dat,
              aes(x = x, y = y, fill = dens),
              height = 0.1,
              width = 0.01) +
    scale_fill_continuous(low="white", high="black") +
    
    ggsvg::geom_point_svg(
      data = PeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text,
      size     = 3
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
             label = paste0(round(NoPeople*ComProb,0), " out of ", NoPeople),
             x = (2 * round(NoPeople * ComProb, 0) - 1) / (2 * (NoPeople - 1)), y = 1.35) +

    theme_void() +
    theme(legend.position = "none")

}

PopViz(NoPeople = 100,
       DesireEvent = TRUE,
       # OutcomeName = "Outcome",
       TreatmentName = "Treatment",
       ComparatorName = "Standard Care",
       OutcomeType = "RD",
       ComProb = 0.5,
       ComConfInt = c(0.4, 0.6),
       RelEff = 0,
       RelConfInt = c(0.1, 0.3),
       )

