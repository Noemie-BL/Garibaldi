# Aims:
# 1. Plot Bayesian model results

# Author: Philippa Stone
# Date created: 17 May 2023
# Date updated: 

# # LIBRARIES # #
library(ggplot2)
library(tidybayes)
library(tidyverse)

#set wd to Garibaldi

# # INPUT FILES # #
# [rds model files for each species' trait]
phyemp_height_nb <- readRDS("trampling_analyses/outputs/ms_results/phyemp_height_nb.rds")
height_nb_vacova <- readRDS("trampling_analyses/outputs/ms_results/height_nb_vacova.rds")
height_nb_casmer <- readRDS("trampling_analyses/outputs/ms_results/height_nb_casmer.rds")
height_nb_carspp <- readRDS("trampling_analyses/outputs/ms_results/height_nb_carspp.rds")

phyemp_diam_nb <- readRDS("trampling_analyses/outputs/ms_results/phyemp_diam_nb.rds")
diam_nb_casmer <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_casmer.rds")
diam_nb_carspp <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_carspp.rds")
diam_nb_vacova <- readRDS("trampling_analyses/outputs/April2023/diam_nb_vacova.rds")

repro_beta_vacova <- readRDS("trampling_analyses/outputs/ms_results/repro_beta_vacova.rds")
repro_beta_casmer <- readRDS("trampling_analyses/outputs/ms_results/repro_beta_casmer.rds")
phyemp_repro_beta <- readRDS("trampling_analyses/outputs/ms_results/phyemp_repro_beta.rds")

percentCover <- readRDS("trampling_analyses/outputs/ms_results/perc-cov_beta.rds")

# [quadrat dataframe]
load("trampling_analyses/compiled_data/quad.RData")
dat <- quad

# # OUTPUT FILES # #
# [PDFs for plots of models for each species' trait]


# # THEME # #
mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 30, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), legend.text = element_text(size = 22)) 

theme_set(mytheme)

#may need this line later for making species names in plots italicised
# theme(plot.title = element_text(face = "italic"))

# # PLOTS # #


# # HEIGHT # #

#phyemp
(PhyempHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_height_nb, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "phyemp")) +
    labs(title = "Phyllodoce empetriformis") +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempHeightModPlot.pdf', width = 10, height = 8)

#casmer
(CasmerHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(height_nb_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "casmer")) +
    labs(title = "Cassiope mertensiana") +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CasmerHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerHeightModPlot.pdf', width = 10, height = 8)

#vacova
(VacovaHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(height_nb_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "vacova")) +
    labs(title = "Vaccinium ovalifolium") +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaHeightModPlot.pdf', width = 10, height = 8)

#carspp
(CarexHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(height_nb_carspp, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "carspp")) +
    labs(title = "Carex spp.") +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexHeightModPlot.pdf', width = 10, height = 8)



# # DIAMETER # #

#phyemp
(PhyempDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_diam_nb, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "phyemp")) +
    labs(title = "Phyllodoce empetriformis") +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempDiamModPlot.pdf', width = 10, height = 8)

#casmer
(CasmerDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "casmer")) +
    labs(title = "Cassiope mertensiana") +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerDiamModPlot.pdf', width = 10, height = 8)

#vacova
(VacovaDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "vacova")) +
    labs(title = "Vaccinium ovalifolium") +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaDiamModPlot.pdf', width = 10, height = 8)

#carspp
(CarexDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb_carspp, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat %>% filter(species == "carspp")) +
    labs(title = "Carex spp.") +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexDiamModPlot.pdf', width = 10, height = 8)



# # REPRODUCTIVE OUTPUT # #

#phyemp
(PhyempReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_repro_beta, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat %>% filter(species == "phyemp")) +
    labs(title = "Phyllodoce empetriformis") +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempReproModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempReproModPlot.pdf', width = 10, height = 8)

#casmer
(CasmerReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat %>% filter(species == "casmer")) +
    labs(title = "Cassiope mertensiana") +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CasmerReproModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerReproModPlot.pdf', width = 10, height = 8)

#vacova
(VacovaReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat %>% filter(species == "vacova")) +
    labs(title = "Vaccinium ovalifolium") +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaReproModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaReproModPlot.pdf', width = 10, height = 8)



# # PERCENT COVER # #

(PercentCoverModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(percentCover, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("percent cover") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PercentCoverModPlot, file = 'trampling_analyses/outputs/ms_figs/PercentCoverModPlot.pdf', width = 10, height = 8)





# # PANEL FIGURES # #


