library(magick)
library(callr)

r(function(path) source(path), args = list(path = "Figure07a_GiniCoefficients.R"))
r(function(path) source(path), args = list(path = "Figure07b_RedistributiveEffects.R"))

gini_figure_filename <- "../../figures/Figure07a_GiniCoefficients.png"
re_figure_filename <- "../../figures/Figure07b_RedistributiveEffects.png"
gini_re_figure_filename <- "../../figures/Figure07_GiniCoefficientsRedistributiveEffects.png"

img1 <- image_read(gini_figure_filename)
img2 <- image_read(re_figure_filename)
img3 <- image_append(c(img1, img2), stack = FALSE)
image_write(img3, path = gini_re_figure_filename)

unlink(c(gini_figure_filename, re_figure_filename))
