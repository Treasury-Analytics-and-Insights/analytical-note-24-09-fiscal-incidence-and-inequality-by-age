library(magick)
library(callr)

r(function(path) source(path), args = list(path = "Figure11a_LabourForceParticipationRate.R"))
r(function(path) source(path), args = list(path = "Figure11b_BenefitReceipt.R"))

lfpr_figure_filename <- "../../figures/Figure11a_LabourForceParticipationRate.png"
bens_figure_filename <- "../../figures/Figure11b_BenefitReceipt.png"
lfpr_etc_figure_filename <- "../../figures/Figure11_LabourForceParticipationRateBenefitReceipt.png"

img1 <- image_read(lfpr_figure_filename)
img2 <- image_read(bens_figure_filename)
img3 <- image_append(c(img1, img2), stack = FALSE)
image_write(img3, path = lfpr_etc_figure_filename)

unlink(c(lfpr_figure_filename, bens_figure_filename))
