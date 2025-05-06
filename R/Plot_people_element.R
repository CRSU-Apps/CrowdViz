parts <- data.frame(
  names = c("No change", "Remain same", "Improve", "Worse off"),
  vals = c(80, 10, 20, 0)
)

waffle::waffle(parts, 
               rows = 1,
               legend_pos = "bottom",
               colors = c("#969696", "yellow", "green", "red"),
               size = 1,
               use_glyph = "medkit")
