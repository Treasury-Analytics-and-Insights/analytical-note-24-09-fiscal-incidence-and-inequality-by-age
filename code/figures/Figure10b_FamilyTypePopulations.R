library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure10b_FamilyTypePopulations.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Members of single-member families",
                                "Members of sole-parent families",
                                "Members of couple families",
                                "Members of couple-with-children families"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`, Note)]

indbyfamtype_plot_data <-
  data_subset[, .(family_type = gsub("\\w+\\s\\w+\\s(.*)\\s\\w+", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  population = Estimate,
                  population_ase = `Sampling error`,
                  note = Note)]

indbyfamtype_plot_data[, family_type := gsub("^(\\w)", "\\U\\1", family_type, perl = TRUE)]
indbyfamtype_plot_data[, family_type := gsub("(.*)-member", "\\1", family_type)]

indbyfamtype_plot_data[family_type == "Couple", family_type := "Couple without\nchildren"]
indbyfamtype_plot_data[family_type == "Couple-with-children",
                       family_type := "Couple with\nchildren"]
indbyfamtype_plot_data[family_type == "Sole-parent", family_type := "Sole\nparent"]

indbyfamtype_plot_data[, family_type := factor(family_type,
                                               levels = c("Single",
                                                          "Couple without\nchildren",
                                                          "Couple with\nchildren",
                                                          "Sole\nparent"))]

indbyfamtype_plot_data[, age := factor(age, levels = unique(age))]

setorder(indbyfamtype_plot_data, family_type, age)

indbyfamtype_plot_data[, x := as.numeric(age)]
indbyfamtype_age_labels <- indbyfamtype_plot_data[, levels(age)]

indbyfamtype_plot_labels <- indbyfamtype_plot_data[, .(population, family_type, age, x, note)]
indbyfamtype_plot_labels[note == "Suppressed", label := "S"]
indbyfamtype_plot_labels[, `:=`(population = 0, note = NULL)]

indbyfamtype_plot_labels_cpl <- indbyfamtype_plot_labels[family_type == "Couple without\nchildren"]
indbyfamtype_plot_labels_cwc <- indbyfamtype_plot_labels[family_type == "Couple with\nchildren"]
indbyfamtype_plot_labels_spa <- indbyfamtype_plot_labels[family_type == "Sole\nparent"]

indbyfamtype_plot_labels_cpl[label == "S", population := 3.14e5]
indbyfamtype_plot_labels_cwc[label == "S", population := c(2.49e5, 2.07e5, 1.43e5, 1.77e5)]
indbyfamtype_plot_labels_cwc[label == "S", x := x + c(-0.25, -0.25, -0.25, -0.25)]
indbyfamtype_plot_labels_spa[label == "S", population := c(3.31e5, 2.86e5, 2.49e5,
                                                           2.07e5, 1.43e5, 1.77e5)]
indbyfamtype_plot_labels_spa[label == "S", x := x + c(0, 0, 0.25, 0.25, 0.25, 0.25)]

indbyfamtype_plot_labels_panel <- indbyfamtype_plot_data[age == "0-4" & family_type == "Single"]

indbyfamtype_plot_labels_panel[, label := "(b)"]
indbyfamtype_plot_labels_panel[, population := 0.98 * 3.7e5]

p <-
  ggplot(indbyfamtype_plot_data, aes(x = x, y = population, fill = family_type)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_x_continuous(breaks = 1:length(indbyfamtype_age_labels), labels = indbyfamtype_age_labels,
                     limits = c(0.5, length(indbyfamtype_age_labels) + 0.5)) +
  scale_y_continuous(limits = c(0, 3.7e5), breaks = seq(0, 3e5, 1e5),
                     labels = scales::comma_format()) +
  labs(x = "Age group",
       y = "Individuals") +
  scale_fill_manual(values = fi_palette) +
  ggtitle("Individuals in each family type") +
  geom_text(data = indbyfamtype_plot_labels_cpl, nudge_x = 0, nudge_y = 0,
            label = indbyfamtype_plot_labels_cpl$label, colour = fi_palette[2], size = 5) +
  geom_text(data = indbyfamtype_plot_labels_cwc, nudge_x = 0, nudge_y = 0,
            label = indbyfamtype_plot_labels_cwc$label, colour = fi_palette[3], size = 5) +
  geom_text(data = indbyfamtype_plot_labels_spa, nudge_x = 0, nudge_y = 0,
            label = indbyfamtype_plot_labels_spa$label, colour = fi_palette[4], size = 5) +
  geom_text(data = indbyfamtype_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = indbyfamtype_plot_labels_panel$label, colour = "black", size = 5) +
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
