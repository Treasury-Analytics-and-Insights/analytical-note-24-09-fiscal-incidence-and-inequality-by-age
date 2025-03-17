library(magick)
library(callr)

r(function(path) source(path), args = list(path = "Figure13a_GiniCoefficientsSharingEffect.R"))
r(function(path) source(path), args = list(path = "Figure13b_RedistributiveEffectsSharingEffect.R"))

gini_figure_filename <- "../../figures/Figure13a_GiniCoefficientsSharingEffect.png"
re_figure_filename <- "../../figures/Figure13b_RedistributiveEffectsSharingEffect.png"
gini_re_figure_filename <-
  "../../figures/Figure13_GiniCoefficientsRedistributiveEffectsSharingEffect.png"

img1 <- image_read(gini_figure_filename)
img2 <- image_read(re_figure_filename)
img3 <- image_append(c(img1, img2), stack = FALSE)
image_write(img3, path = gini_re_figure_filename)

unlink(c(gini_figure_filename, re_figure_filename))
