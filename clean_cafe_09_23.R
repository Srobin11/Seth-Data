library(lmerTest)
library(tidyverse)
library(ggplot2)
library(ggpubr)

df <- read.csv("clean_cafe.csv")
df2 <- df[df$day == 8,] # only use data from July 8 (end of experiment)

### create labels that included burn treatment and vegetation type

new_labels <- c("clipped control" = "Clipped : Medium", "control" = "Unburned : Tall", "high" = "High : Short", "low" = "Low : Tall Mix", "medium" = "Moderate : Short Mix")

### plot graph
ggplot(aes(x = seed, y = percent_removed, fill = open), data = df2) + geom_boxplot() + 
  facet_wrap(~ treatment, labeller = labeller(treatment = new_labels)) + theme_bw(base_size = 16) + labs(x = "Seed", y = "Percent of Seeds Removed") +
  scale_fill_grey() + scale_fill_manual(values = c("white", "darkgrey"))

## Linear mixed model 1: plot-level random effect, all interactions
lme1 <- lmer(percent_removed ~ treatment * seed * open + (1 | plot), data = df2)
anova(lme1)
summary(lme1)

## Linear mixed model 2: plot-level random effect, no 3-way interaction
lme2 <- lmer(percent_removed ~ treatment + seed + open + treatment*seed + treatment*open + seed*open + (1 | plot), data = df2)
anova(lme2)
summary(lme2)

## Linear mixed model 3: plot-level random effect, remove non-significant 2-way interactions
lme3 <- lmer(percent_removed ~ treatment + seed + open + seed*open + (1 | plot), data = df2)
anova(lme3)
summary(lme3)


#### Just Larch ####

df3 <- df %>% filter(!grepl('wheat', seed))


ggplot(aes(x = exclosure, y = percent_removed, fill = exclosure), data = df3) + geom_boxplot() +
  facet_wrap(~ treatment, labeller = labeller(treatment = new_labels)) + theme_classic(base_size = 20) + theme(text = element_text(family = "Times New Roman")) + 
  theme(legend.position = "none" ) + labs(x = "Exclosure", y = "Percent of Larch Seeds Removed") +
  scale_fill_manual(values = c("white", "darkgrey"))


lme4 <- lmer(percent_removed ~ treatment * open + (1 | plot), data = df3)
anova(lme1)
summary(lme1)


lme5 <- lmer(percent_removed ~ treatment + open + treatment*open + (1 | plot), data = df3)
anova(lme2)
summary(lme2)
