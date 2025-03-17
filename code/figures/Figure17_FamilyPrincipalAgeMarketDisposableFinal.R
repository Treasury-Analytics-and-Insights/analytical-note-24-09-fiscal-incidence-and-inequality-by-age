library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure17_FamilyPrincipalAgeMarketDisposableFinal.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Average Market income", "Average Disposable income",
                                "Average Final income") &
                 `Sharing assumption` == "Family",
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

fammdf_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s(\\w+\\s\\w+)", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  income = Estimate,
                  income_ase = `Sampling error`)]

fammdf_plot_data[, income_type := factor(income_type,
                                         levels = c("Market income",
                                                    "Disposable income",
                                                    "Final income"))]

fammdf_plot_data[, age := factor(age, levels = unique(age))]

setorder(fammdf_plot_data, income_type, age)

fammdf_age_labels <- fammdf_plot_data[, levels(age)]

p <-
  ggplot(fammdf_plot_data,
         aes(x = as.integer(age), y = income,
             colour = income_type, shape = income_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = income - income_ase,
                    ymax = income + income_ase),
                width = 0.3,
                show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(fammdf_age_labels), labels = fammdf_age_labels) +
  scale_y_continuous(limits = c(0, 1e5), breaks = seq(0, 1e5, 2.5e4),
                     labels = scales::dollar_format()) +
  labs(x = "Age group (family principal earner)",
       y = "Average equivalised income ($2019)") +
  scale_colour_manual(values = fi_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
