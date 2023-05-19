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
phyemp_height_nb <- readRDS("trampling_analyses/outputs/ms_results/phyemp_height_nb.rds").
height_nb_vacova <- readRDS("trampling_analyses/outputs/ms_results/height_nb_vacova.rds").
height_nb_casmer <- readRDS("trampling_analyses/outputs/ms_results/height_nb_casmer.rds").
height_nb_carspp <- readRDS("trampling_analyses/outputs/ms_results/height_nb_carspp.rds")

phyemp_diam_nb <- readRDS("trampling_analyses/outputs/ms_results/phyemp_diam_nb.rds").
diam_nb_casmer <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_casmer.rds").
diam_nb_carspp <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_carspp.rds")

repro_beta_vacova <- readRDS("trampling_analyses/outputs/ms_results/repro_beta_vacova.rds")
repro_beta_casmer <- readRDS("trampling_analyses/outputs/ms_results/repro_beta_casmer.rds").
phyemp_repro_beta <- readRDS("trampling_analyses/outputs/ms_results/phyemp_repro_beta.rds").


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


# # PLOTS # #


# # HEIGHT # #

#phyemp
(PhyempHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_height_nb, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempHeightModPlot.pdf')

#casmer
(CasmerHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(height_nb_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CasmerHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerHeightModPlot.pdf')

#vacova
(VacovaHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(height_nb_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaHeightModPlot.pdf')

#carspp
(CarexHeightModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(height_nb_carspp, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexHeightModPlot.pdf')



# # DIAMETER # #

#phyemp
(PhyempDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_diam_nb, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempDiamModPlot.pdf')

#casmer
(CasmerDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerDiamModPlot.pdf')

#vacova
(VacovaDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaDiamModPlot.pdf')

#carspp
(CarexDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb_carspp, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexDiamModPlot.pdf')



# # REPRODUCTIVE OUTPUT # #

#phyemp
(PhyempReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_repro_beta, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat) +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempReproModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempReproModPlot.pdf')

#casmer
(CasmerReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat) +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CasmerReproModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerReproModPlot.pdf')

#vacova
(VacovaReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat) +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaReproModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaReproModPlot.pdf')



# # PANEL FIGURES # #


