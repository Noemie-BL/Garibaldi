# Aims: manuscript figures
# 1. Bayesian model results for disturbance only
# 2. Bayesian model results for disturbance + elevation
# 3. Reproductive density

# Author: Philippa Stone
# Date created: 17 May 2023
# Date updated: 14 June 2023 (NC)

rm(list=ls()) 

# # LIBRARIES # #
library(ggplot2)
library(tidybayes)
library(tidyverse)
library(gridExtra)
library(scales) #for percentage conversion on Y axis
library(ggtext) #for italics in plot titles
library(egg) #for labelling plot facets

#set wd to Garibaldi

# # INPUT FILES # #

# Data [quadrat dataframe]
load("trampling_analyses/compiled_data/quad.RData") ##updated with reproductive metric & plant area (repro.R)
dat <- quad

# # [rds model files for each species' trait]
phyemp_height_nb <- readRDS("trampling_analyses/outputs/ms_results/phyemp_height_nb.rds")
height_nb_vacova <- readRDS("trampling_analyses/outputs/ms_results/height_nb_vacova.rds")
height_nb_casmer <- readRDS("trampling_analyses/outputs/ms_results/height_nb_casmer.rds")
height_nb_carspp <- readRDS("trampling_analyses/outputs/ms_results/height_nb_carspp.rds")

phyemp_diam_nb <- readRDS("trampling_analyses/outputs/ms_results/phyemp_diam_nb.rds")
diam_nb_casmer <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_casmer.rds")
diam_nb_carspp <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_carspp.rds")
diam_nb_vacova <- readRDS("trampling_analyses/outputs/ms_results/diam_nb_vacova.rds")

repro_beta_vacova <- readRDS("trampling_analyses/outputs/ms_results/repro_beta_vacova.rds")
repro_beta_casmer <- readRDS("trampling_analyses/outputs/ms_results/repro_beta_casmer.rds")
phyemp_repro_beta <- readRDS("trampling_analyses/outputs/ms_results/phyemp_repro_beta.rds")

percentCover <- readRDS("trampling_analyses/outputs/ms_results/perc-cov_beta.rds")


#https://stackoverflow.com/questions/59925895/labelling-plots-arranged-with-grid-arrange
#for grid arrange

# # OUTPUT FILES # #
# [PDFs for plots of models for each species' trait]



####################################################################################################

# # Fig. [RESULTS]: disturbance only # # 

# # THEME # #
mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 36, angle = 30, hjust = 1), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 36), #y axis text size
        axis.title.x = element_text(size = 40), #x axis label size
        axis.title.y = element_text(size = 40), #x axis label size
        plot.title = element_text(size = 44, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 36), legend.text = element_text(size = 35), #legend text size
        plot.margin = margin(1.5,1.5,1.5,0.5, "cm"))  #increased plot margins on right so axis text seen in full

theme_set(mytheme)


# # HEIGHT # #

#phyemp
cond <- conditional_effects(phyemp_height_nb, effect = 'dist')
est <- as.data.frame(cond[[1]])

phyemp_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
    labs(title = "Phyllodoce empetriformis") +
    ylab("Plant height (mm)") +
    xlab("Disturbance") +
    geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
    geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
    scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
    theme(legend.title = element_blank(), legend.position = c(0.8, 0.83), plot.title = element_text(face = "italic"))

#casmer
cond <- conditional_effects(height_nb_casmer, effect = 'dist')
est <- as.data.frame(cond[[1]])

casmer_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
    labs(title = "Cassiope mertensiana") +
    ylab("Plant height (mm)") +
    xlab("Disturbance") +
    geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
    geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
    scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
    theme(legend.position = "none", plot.title = element_text(face = "italic"))

#vacova
cond <- conditional_effects(height_nb_vacova, effect = 'dist')
est <- as.data.frame(cond[[1]])

vacova_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
    labs(title = "Vaccinium ovatum") +
    ylab("Plant height (mm)") +
    xlab("Disturbance") +
    geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
    geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
    scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
    theme(legend.position = "none", plot.title = element_text(face = "italic"))

#carspp
cond <- conditional_effects(height_nb_carspp, effect = 'dist')
est <- as.data.frame(cond[[1]])

carspp_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
  ggtitle(expression(atop(paste(italic("Carex "), "spp.")))) +
  ylab("Plant height (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none")


# # DIAMETER # #

#phyemp
cond <- conditional_effects(phyemp_diam_nb, effect = 'dist')
est <- as.data.frame(cond[[1]])

phyemp_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  labs(title = "Phyllodoce empetriformis") +
  ylab("Plant maximum diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.75), plot.title = element_text(face = "italic"))

#casmer
cond <- conditional_effects(diam_nb_casmer, effect = 'dist')
est <- as.data.frame(cond[[1]])

casmer_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  labs(title = "Cassiope mertensiana") +
  ylab("Plant maximum diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#vacova
cond <- conditional_effects(diam_nb_vacova, effect = 'dist')
est <- as.data.frame(cond[[1]])

vacova_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  labs(title = "Vaccinium ovatum") +
  ylab("Plant maximum diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#carspp
cond <- conditional_effects(diam_nb_carspp, effect = 'dist')
est <- as.data.frame(cond[[1]])

carspp_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  ggtitle(expression(atop(paste(italic("Carex "), "spp.")))) +
  ylab("Plant maximum diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none")


# # REPRODUCTIVE OUTPUT # #

#phyemp
cond <- conditional_effects(phyemp_repro_beta, effect = 'dist')
est <- as.data.frame(cond[[1]])

phyemp_repro_plot <- ggplot(est, aes(dist, rel_repro, color = dist)) +
  labs(title = "Phyllodoce empetriformis") +
  labs(x = expression(paste("Disturbance")),
       y = expression(paste("Reproductive output (counts/", cm^2, ")"))) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.3), plot.title = element_text(face = "italic"))

#casmer
cond <- conditional_effects(repro_beta_casmer, effect = 'dist')
est <- as.data.frame(cond[[1]])

casmer_repro_plot <- ggplot(est, aes(dist, rel_repro, color = dist)) +
  labs(title = "Cassiope mertensiana") +
  labs(x = expression(paste("Disturbance")),
       y = expression(paste("Reproductive output (counts/", cm^2, ")"))) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#vacova
cond <- conditional_effects(repro_beta_vacova, effect = 'dist')
est <- as.data.frame(cond[[1]])

vacova_repro_plot <- ggplot(est, aes(dist, rel_repro, color = dist)) +
  labs(title = "Vaccinium ovatum") +
  labs(x = expression(paste("Disturbance")),
       y = expression(paste("Reproductive output (counts/", cm^2, ")"))) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))


# # PERCENT COVER # #

cond <- conditional_effects(percentCover, effect = 'dist')
est <- as.data.frame(cond[[1]])

perccover_plot <- ggplot(est, aes(dist, perc.cov, color = dist)) +
  labs(title = "All species") +
  ylab("Percent cover") +
  xlab("Disturbance") +
  scale_y_continuous(labels = percent_format()) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 1.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 1.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 1.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none")





# # PANEL FIGURES # #


allTraitsPanelPlot <- grid.arrange(phyemp_height_plot, casmer_height_plot, vacova_height_plot, carspp_height_plot, phyemp_diam_plot, casmer_diam_plot, vacova_diam_plot, carspp_diam_plot, phyemp_repro_plot, casmer_repro_plot, vacova_repro_plot, perccover_plot, nrow=3)
ggsave(allTraitsPanelPlot, file = 'trampling_analyses/outputs/ms_figs/allTraitsPanelPlot.pdf', width = 40, height = 30)





####################################################################################################




####################################################################################################

# # Fig. S[RESULTS]: disturbance + elevation # #

# # THEME # #
mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 36), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 36), #y axis text size
        axis.title.x = element_text(size = 40), #x axis label size
        axis.title.y = element_text(size = 40), #x axis label size
        plot.title = element_text(size = 44, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 36), legend.text = element_text(size = 35), #legend text size
        plot.margin = margin(0.5,1.5,0.5,0.5, "cm"))  #increased plot margins on right so axis text seen in full

theme_set(mytheme)


# # HEIGHT # #

#phyemp
PhyempHeightModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(phyemp_height_nb, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "phyemp")) +
  labs(title = "Phyllodoce empetriformis") +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  theme(legend.title = element_blank(), legend.position = c(0.7, 0.75), plot.title = element_text(face = "italic"))

#ggsave(PhyempHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempHeightModPlot.pdf', width = 10, height = 8)

#casmer
CasmerHeightModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(height_nb_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "casmer")) +
  labs(title = "Cassiope mertensiana") +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(CasmerHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerHeightModPlot.pdf', width = 10, height = 8)

#vacova
VacovaHeightModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(height_nb_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "vacova")) +
  labs(title = "Vaccinium ovalifolium") +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(VacovaHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaHeightModPlot.pdf', width = 10, height = 8)

#carspp
CarexHeightModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(height_nb_carspp, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "carspp")) +
  ggtitle(expression(atop(paste(italic("Carex "), "spp.")))) +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  theme(legend.position = "none")

#ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexHeightModPlot.pdf', width = 10, height = 8)



# # DIAMETER # #

##phyemp
PhyempDiamModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(phyemp_diam_nb, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "phyemp")) +
  labs(title = "Phyllodoce empetriformis") +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(PhyempDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempDiamModPlot.pdf', width = 10, height = 8)

##casmer
CasmerDiamModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(diam_nb_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "casmer")) +
  labs(title = "Cassiope mertensiana") +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(CasmerDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerDiamModPlot.pdf', width = 10, height = 8)

##vacova
VacovaDiamModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(diam_nb_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "vacova")) +
  labs(title = "Vaccinium ovalifolium") +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(VacovaDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaDiamModPlot.pdf', width = 10, height = 8)

##carspp
CarexDiamModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(diam_nb_carspp, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat %>% filter(species == "carspp")) +
  ggtitle(expression(atop(paste(italic("Carex "), "spp.")))) +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none")

#ggsave(CarexDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexDiamModPlot.pdf', width = 10, height = 8)



# # REPRODUCTIVE OUTPUT # #

##phyemp
PhyempReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(phyemp_repro_beta, allow_new_levels = TRUE, re_formula = NA) %>%
    ggplot(aes(x = altitude, y = repro, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat %>% filter(species == "phyemp")) +
    labs(title = "Phyllodoce empetriformis") +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproductive output (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
    theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(PhyempReproModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempReproModPlot.pdf', width = 10, height = 8)

##casmer
CasmerReproModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(repro_beta_casmer, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = repro, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
  geom_point(data = dat %>% filter(species == "casmer")) +
  labs(title = "Cassiope mertensiana") +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("Reproductive output (counts/", cm^2, ")"))) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(CasmerReproModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerReproModPlot.pdf', width = 10, height = 8)

##vacova
VacovaReproModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(repro_beta_vacova, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = repro, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
  geom_point(data = dat %>% filter(species == "vacova")) +
  labs(title = "Vaccinium ovalifolium") +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("Reproductive output (counts/", cm^2, ")"))) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(VacovaReproModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaReproModPlot.pdf', width = 10, height = 8)



# # PERCENT COVER # #

PercentCoverModPlot <- dat %>%
  group_by(dist) %>%
  add_predicted_draws(percentCover, allow_new_levels = TRUE, re_formula = NA) %>%
  ggplot(aes(x = altitude, y = perc.cov, color = dist, fill = dist)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
  geom_point(data = dat) +
  labs(title = "All species") +
  ylab("Percent cover") +
  xlab("\nElevation (m)") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance")  +
  theme(legend.position = "none")

#ggsave(PercentCoverModPlot, file = 'trampling_analyses/outputs/ms_figs/PercentCoverModPlot.pdf', width = 10, height = 8)



# # PANEL FIGURES # #

heightAltitudePanelPlot <- grid.arrange(PhyempHeightModPlot, CasmerHeightModPlot, VacovaHeightModPlot, CarexHeightModPlot, nrow=1)
ggsave(heightPanelPlot, file = 'trampling_analyses/outputs/ms_figs/heightPanelPlot.pdf', width = 40, height = 8)

diamAltitudePanelPlot <- grid.arrange(PhyempDiamModPlot, CasmerDiamModPlot, VacovaDiamModPlot, CarexDiamModPlot, nrow=1)
ggsave(diamPanelPlot, file = 'trampling_analyses/outputs/ms_figs/diamPanelPlot.pdf', width = 40, height = 8)

reproAltitudePanelPlot <- grid.arrange(PhyempReproModPlot, CasmerReproModPlot, VacovaReproModPlot, nrow=1)
ggsave(reproPanelPlot, file = 'trampling_analyses/outputs/ms_figs/reproPanelPlot.pdf', width = 40, height = 8)

reproPerccoverAltitudePanelPlot <- grid.arrange(PhyempReproModPlot, CasmerReproModPlot, VacovaReproModPlot, PercentCoverModPlot, nrow =1)
ggsave(reproPerccoverPanelPlot, file = 'trampling_analyses/outputs/ms_figs/repro-perccoverPanelPlot.pdf', width = 40, height = 8)

allTraitsAltitudePanelPlot <- grid.arrange(PhyempHeightModPlot, CasmerHeightModPlot, VacovaHeightModPlot, CarexHeightModPlot, PhyempDiamModPlot, CasmerDiamModPlot, VacovaDiamModPlot, CarexDiamModPlot, PhyempReproModPlot, CasmerReproModPlot, VacovaReproModPlot, PercentCoverModPlot, nrow=3)
ggsave(allTraitsAltitudePanelPlot, file = 'trampling_analyses/outputs/ms_figs/allTraitsPanelPlot.pdf', width = 40, height = 24)


####################################################################################################



####################################################################################################

# # Fig. S[AREA-REPRO]: reproductive structure density # # 

# # PLOT THEME # #
mytheme <- theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 32, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), legend.text = element_text(size = 22),
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        panel.background = element_blank(), #remove panel background
        axis.line = element_line(colour = "black"), #set axis line color
        legend.key.size = unit(2, "line"), #increase size of legend key
        legend.spacing.y = unit(0.5, "cm"), #increase vertical spacing between legend items
        strip.background = element_rect(fill = "white"), #set background color for facet labels
        strip.text.x = element_text(face = "italic", size = 20), #set size for x-axis facet labels
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 

theme_set(mytheme)

# Remove Carex spp.
dat <- quad %>% 
  filter(species != "carspp")

#remove Phyllodoce outlier causing problems with visualisation for plot
dat <- filter(dat, repro <20)

# full species names
species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")

# # ALL SPECIES PLOTTED SEPARATELY

# Plot size vs number of buds, flowers, and fruits/(height * diameter) by disturbed/undisturbed
(totalReproPlotAreaBySpecies <- ggplot(dat, aes(x= plantArea_cm2, y= repro, color = dist)) +
  geom_point() +
  stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
  stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free", ncol = 3) + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  labs(x = expression(paste("Plant size (", cm^2, ")")),
       y = expression(paste("Density of reproductive structures (counts/", cm^2, ")"))) +
  theme(legend.position = c(0.2, 0.87)))

totalReproPlotAreaBySpecies <- tag_facet(totalReproPlotAreaBySpecies)

ggsave(filename = "trampling_analyses/outputs/ms_figs/totalReproPlotAreaBySpecies.pdf", plot = totalReproPlotAreaBySpecies, device = "pdf", dpi = 600, width = 20, height = 10, units = "in")


(casmerPlotAreaBySpecies <- ggplot(dat %>% filter(species == "casmer"), aes(x= plantArea_cm2, y= repro, color = dist)) +
    geom_point(data = dat %>% filter(species == "casmer")) +
    labs(title = "Cassiope mertensiana") +
    stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
    stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    coord_trans(y = "log1p") + #log10 y axis 
    labs(x = expression(paste("Plant size (", cm^2, ")")),
         y = expression(paste("Density of reproductive structures (counts/", cm^2, ")"))) +
    theme(legend.position = c(0.75, 0.75), plot.title = element_text(face = "italic")))

(phyempPlotAreaBySpecies <- ggplot(dat %>% filter(species == "phyemp"), aes(x= plantArea_cm2, y= repro, color = dist)) +
    geom_point(data = dat %>% filter(species == "phyemp")) +
    labs(title = "Phyllodoce empetriformis") +
    stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
    stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    coord_trans(y = "log1p") + #log10 y axis 
    labs(x = expression(paste("Plant size (", cm^2, ")")),
         y = expression(paste("Density of reproductive structures (counts/", cm^2, ")"))) +
    theme(legend.position = "none", plot.title = element_text(face = "italic")))

(vacovaPlotAreaBySpecies <- ggplot(dat %>% filter(species == "vacova"), aes(x= plantArea_cm2, y= repro, color = dist)) +
    geom_point(data = dat %>% filter(species == "vacova")) +
    labs(title = "Vaccinium ovalifolium") +
    stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
    stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    coord_trans(y = "log1p") + #log10 y axis 
    labs(x = expression(paste("Plant size (", cm^2, ")")),
         y = expression(paste("Density of reproductive structures (counts/", cm^2, ")"))) +
    theme(legend.position = "none", plot.title = element_text(face = "italic")))

allSpeciesReproPlot <- grid.arrange(casmerPlotAreaBySpecies, phyempPlotAreaBySpecies, vacovaPlotAreaBySpecies, nrow=1)
ggsave(allSpeciesReproPlot, file = 'trampling_analyses/outputs/ms_figs/allSpeciesReproPlot.pdf', width = 20, height = 10)

####################################################################################################
