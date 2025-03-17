library(magick)
library(callr)

r(function(path) source(path),
  args = list(path = "Figure18a_FamilyPrincipalAgeGiniCoefficients.R"))
r(function(path) source(path),
  args = list(path = "Figure18b_FamilyPrincipalAgeRedistributiveEffects.R"))

gini_figure_filename <- "../../figures/Figure18a_FamilyPrincipalAgeGiniCoefficients.png"
re_figure_filename <- "../../figures/Figure18b_FamilyPrincipalAgeRedistributiveEffects.png"
gini_re_figure_filename <-
  "../../figures/Figure18_FamilyPrincipalAgeGiniCoefficientsRedistributiveEffects.png"

img1 <- image_read(gini_figure_filename)
img2 <- image_read(re_figure_filename)
img3 <- image_append(c(img1, img2), stack = FALSE)
image_write(img3, path = gini_re_figure_filename)

unlink(c(gini_figure_filename, re_figure_filename))
