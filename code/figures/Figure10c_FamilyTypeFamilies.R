library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure10c_FamilyTypeFamilies.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Single-member families", "Sole-parent families", "Couple families",
                                "Couple-with-children families"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`, Note)]

fambyfamtype_plot_data <-
  data_subset[, .(family_type = gsub("(\\w+)\\s\\w+", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  population = Estimate,
                  population_ase = `Sampling error`,
                  note = Note)]

fambyfamtype_plot_data[, family_type := gsub("(.*)-member", "\\1", family_type)]

fambyfamtype_plot_data[family_type == "Couple", family_type := "Couple without\nchildren"]
fambyfamtype_plot_data[family_type == "Couple-with-children",
                       family_type := "Couple with\nchildren"]
fambyfamtype_plot_data[family_type == "Sole-parent", family_type := "Sole\nparent"]

fambyfamtype_plot_data[, family_type := factor(family_type,
                                               levels = c("Single", "Couple without\nchildren",
                                                          "Couple with\nchildren", "Sole\nparent"))]

fambyfamtype_plot_data[, age := factor(age, levels = unique(age))]

setorder(fambyfamtype_plot_data, family_type, age)

fambyfamtype_plot_data[, x := as.numeric(age)]
fambyfamtype_age_labels <- fambyfamtype_plot_data[, levels(age)]

fambyfamtype_plot_labels <- fambyfamtype_plot_data[, .(population, family_type, age, x, note)]
fambyfamtype_plot_labels[note == "Suppressed", label := "S"]
fambyfamtype_plot_labels[, `:=`(population = 0, note = NULL)]

fambyfamtype_plot_labels_cpl <- fambyfamtype_plot_labels[family_type == "Couple without\nchildren"]
fambyfamtype_plot_labels_cwc <- fambyfamtype_plot_labels[family_type == "Couple with\nchildren"]
fambyfamtype_plot_labels_spa <- fambyfamtype_plot_labels[family_type == "Sole\nparent"]

fambyfamtype_plot_labels_cpl[label == "S", population := c(1.37e5)]
fambyfamtype_plot_labels_cpl[label == "S", x := x + c(-0.35)]
fambyfamtype_plot_labels_cwc[label == "S", population := c(1.37e5, 3.02e5, 1.68e5,
                                                           1.35e5, 0.98e5, 1.37e5)]
fambyfamtype_plot_labels_cwc[label == "S", x := x + c(0, 0, -0.25, -0.25, -0.25, -0.25)]
fambyfamtype_plot_labels_spa[label == "S", population := c(1.37e5, 2.12e5, 2.04e5, 1.68e5,
                                                           1.35e5, 0.98e5, 1.37e5)]
fambyfamtype_plot_labels_spa[label == "S", x := x + c(0.35, 0, 0, 0.25, 0.25, 0.25, 0.25)]

fambyfamtype_plot_labels_sup <- fambyfamtype_plot_labels_cpl[4]
fambyfamtype_plot_labels_sup[, label := "S - Suppressed"]
fambyfamtype_plot_labels_sup[, population := 2.8e5]

fambyfamtype_plot_labels_panel <- fambyfamtype_plot_data[age == "15-19" & family_type == "Single"]

fambyfamtype_plot_labels_panel[, label := "(c)"]
fambyfamtype_plot_labels_panel[, population := 0.98 * 3e5]

p <-
  ggplot(fambyfamtype_plot_data, aes(x = x, y = population, fill = family_type)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_x_continuous(breaks = 4:length(fambyfamtype_age_labels),
                     labels = fambyfamtype_age_labels[4:length(fambyfamtype_age_labels)],
                     limits = c(3.5, length(fambyfamtype_age_labels) + 0.5)) +
  scale_y_continuous(limits = c(0, 3e5), breaks = seq(0, 3e5, 1e5),
                     labels = scales::comma_format()) +
  labs(x = "Age group (family principal earner)",
       y = "Families") +
  scale_fill_manual(values = fi_palette) +
  ggtitle("Family types") +
  geom_text(data = fambyfamtype_plot_labels_cpl, nudge_x = 0, nudge_y = 0,
            label = fambyfamtype_plot_labels_cpl$label, colour = fi_palette[2], size = 5) +
  geom_text(data = fambyfamtype_plot_labels_cwc, nudge_x = 0, nudge_y = 0,
            label = fambyfamtype_plot_labels_cwc$label, colour = fi_palette[3], size = 5) +
  geom_text(data = fambyfamtype_plot_labels_spa, nudge_x = 0, nudge_y = 0,
            label = fambyfamtype_plot_labels_spa$label, colour = fi_palette[4], size = 5) +
  geom_text(data = fambyfamtype_plot_labels_sup, nudge_x = 10.5, nudge_y = 0,
            label = fambyfamtype_plot_labels_sup$label, colour = "black", size = 5) +
  geom_text(data = fambyfamtype_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = fambyfamtype_plot_labels_panel$label, colour = "black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 6, height = 4, units = "in", dpi = 600, type = "cairo")
