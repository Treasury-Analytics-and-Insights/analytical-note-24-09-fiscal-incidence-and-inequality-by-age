library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure10a_FamilyRolePopulations.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Principal Earners (Family)",
                                "Partners of Principal Earners (Family)",
                                "Dependents (Family)"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`, Note)]

indbyfamrole_plot_data <-
  data_subset[, .(role = gsub("(.*)\\s\\(.*\\)", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  population = Estimate,
                  population_ase = `Sampling error`,
                  note = Note)]

indbyfamrole_plot_data[, role := gsub("s", "", role)]

indbyfamrole_plot_data[role == "Principal Earner", role := "Principal\nearner"]
indbyfamrole_plot_data[role == "Partner of Principal Earner",
                       role := "Partner of\nprincipal earner"]

indbyfamrole_plot_data[, role := factor(role,
                                        levels = c("Principal\nearner",
                                                   "Partner of\nprincipal earner",
                                                   "Dependent"))]

indbyfamrole_plot_data[, age := factor(age, levels = unique(age))]
indbyfamrole_age_labels <- indbyfamrole_plot_data[, levels(age)]

setorder(indbyfamrole_plot_data, role, age)

indbyfamrole_plot_data[, x := as.numeric(age)]

indbyfamrole_plot_labels <- indbyfamrole_plot_data[, .(population, role, age, x, note)]
indbyfamrole_plot_labels[note == "Suppressed", label := "S"]
indbyfamrole_plot_labels[, `:=`(population = 0, note = NULL)]

indbyfamrole_plot_labels_par <- indbyfamrole_plot_labels[role == "Partner of\nprincipal earner"]

indbyfamrole_plot_labels_panel <- indbyfamrole_plot_data[age == "0-4" & role == "Principal\nearner"]

indbyfamrole_plot_labels_panel[, label := "(a)"]
indbyfamrole_plot_labels_panel[, population := 0.98 * 3.7e5]

p <-
  ggplot(indbyfamrole_plot_data, aes(x = x, y = population, fill = role)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_x_continuous(breaks = 1:length(indbyfamrole_age_labels), labels = indbyfamrole_age_labels,
                     limits = c(0.5, length(indbyfamrole_age_labels) + 0.5)) +
  scale_y_continuous(limits = c(0, 3.7e5), breaks = seq(0, 3e5, 1e5),
                     labels = scales::comma_format()) +
  labs(x = "Age group",
       y = "Individuals") +
  scale_fill_manual(values = fi_palette) +
  ggtitle("Individuals in each family role") +
  geom_text(data = indbyfamrole_plot_labels_par, nudge_x = 0, nudge_y = 3.15e5,
            label = indbyfamrole_plot_labels_par$label, colour = fi_palette[2], size = 5) +
  geom_text(data = indbyfamrole_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = indbyfamrole_plot_labels_panel$label, colour = "black", size = 5) +
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
