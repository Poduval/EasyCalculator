
library(hexSticker)

sticker_icon <- sticker(
  "pictures/tax_calculator.png", 
  s_x = 1, s_y = .75, 
  s_width = 0.4, s_height = 0.4,
  p_size = 20, p_color = colorDarkBrown, p_family = "Aller_Rg", p_fontface = "plain",
  h_size = 1.2, h_fill = "lightgrey", h_color = "#D78113",
  spotlight = FALSE,
  dpi = 300,
  package = "taxcalculator", 
  filename = "stickers/tax_calculator_logo.png")

sticker_icon
