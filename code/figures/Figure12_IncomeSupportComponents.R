library(magick)
library(callr)

r(function(path) source(path), args = list(path = "Figure12a-c_IncomeSupportComponents1.R"))
r(function(path) source(path), args = list(path = "Figure12d-e_IncomeSupportComponents2.R"))

inc_sup_cpts_top_figure_filename <- "../../figures/Figure12a-c_IncomeSupportComponents1.png"
inc_sup_cpts_bot_figure_filename <- "../../figures/Figure12d-e_IncomeSupportComponents2.png"
inc_sup_cpts_figure_filename <- "../../figures/Figure12_IncomeSupportComponents.png"

img1 <- image_read(inc_sup_cpts_top_figure_filename)
img2 <- image_read(inc_sup_cpts_bot_figure_filename)
img3 <- image_blank(width = 7800, height = 2400, color = "white")
img4 <- image_append(c(img1, img3), stack = TRUE)
img5 <- image_composite(img4, img2, offset = "+1200+2400")
image_write(img5, path = inc_sup_cpts_figure_filename)

unlink(c(inc_sup_cpts_top_figure_filename, inc_sup_cpts_bot_figure_filename))
