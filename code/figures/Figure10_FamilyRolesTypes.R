library(magick)
library(callr)

r(function(path) source(path), args = list(path = "Figure10a_FamilyRolePopulations.R"))
r(function(path) source(path), args = list(path = "Figure10b_FamilyTypePopulations.R"))
r(function(path) source(path), args = list(path = "Figure10c_FamilyTypeFamilies.R"))

fam_role_figure_filename <- "../../figures/Figure10a_FamilyRolePopulations.png"
fam_type_figure_filename <- "../../figures/Figure10b_FamilyTypePopulations.png"
fam_type_fam_figure_filename <- "../../figures/Figure10c_FamilyTypeFamilies.png"
fam_roles_types_figure_filename <- "../../figures/Figure10_FamilyRolesTypes.png"

img1 <- image_read(fam_role_figure_filename)
img2 <- image_read(fam_type_figure_filename)
img3 <- image_read(fam_type_fam_figure_filename)
img4 <- image_append(c(img1, img2), stack = FALSE)
img5 <- image_blank(width = 7200, height = 2400, color = "white")
img6 <- image_append(c(img4, img5), stack = TRUE)
img7 <- image_composite(img6, img3, offset = "+1800+2400")
image_write(img7, path = fam_roles_types_figure_filename)

unlink(c(fam_role_figure_filename, fam_type_figure_filename, fam_type_fam_figure_filename))
