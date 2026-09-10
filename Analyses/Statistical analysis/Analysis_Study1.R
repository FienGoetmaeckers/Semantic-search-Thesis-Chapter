library(ggplot2)
library(ggsignif)
library(gghalves)
library(factoextra)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(comprehenr)
library(afex)
library(brms)
library(effects)
library(simr)
library(sjPlot)
library(reshape2)
library(Hmisc)
library(diptest)
library(MKinfer)

#############################################################
#read in all the data, make sure only relevant data is used
#############################################################
nr_trials = 21
nr_blocks = 30
###
#real behavioral data:
###
setwd("../../Data/Study1")
datar <- read.csv(file = "data1.csv", sep = ",")
info <- read.csv(file = "info1.csv", sep = ",")
nr_pp <- length(unique(datar$subjectID))

###
#simulated data:
###
sim <- read.csv(file = "sim_data1.csv")
info_s <- read.csv(file = "sim_info1.csv")
nr_pp_s <- length(unique(sim$l_fit))

options(contrasts = c("contr.sum", "contr.poly"))
#################################################
#################################################
#H1: effective behavior
#################################################
#################################################

#################################################
#H1.A: learning within a round
#################################################

#learning curves, total scores
m <- lmer(reward ~ trial_nr_c*block_nr_c + (1+trial_nr_c*block_nr_c|subjectID), data = datar,  control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(m)
ms <- lmer(reward ~ trial_nr_c*block_nr_c + (1+trial_nr_c+block_nr_c|subjectID), data = sim,  control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(ms)

#plot effect of trial
z <- as.data.frame(effect("trial_nr_c", m))
z$trial_nrp <- c(1, 6, 11, 16, 21)
datar$trial_nrp <- datar$trial_nr + 1
sim$trial_nrp <- sim$trial_nr + 1
d <- aggregate(datar$reward, list(subjectID = datar$subjectID, trial_nr = datar$trial_nrp), FUN=mean, digits=3)
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
ds <- aggregate(sim$reward, list(subjectID = sim$subjectID, trial_nr = sim$trial_nrp), FUN=mean, digits=3)
ms <- ds %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
trial_summary_frame <- datar %>% group_by(subjectID,trial_nrp) %>% summarise(score = mean(reward))
trials <- unique(na.omit(trial_summary_frame)$trial_nrp)
trialslab <- c(1, 5, 10, 15, 20)
ggplot() + 
  geom_jitter(data = trial_summary_frame, aes(x = trial_nrp, y = score), alpha = 0.1) +
  ylim(0,80) + theme_classic() + 
  #geom_line(data = z, aes(x = trial_nrp, y = fit), size = 1, show.legend = FALSE, col = "indianred2") +
  #geom_ribbon(data = z, aes(x = trial_nrp, ymin = lower, ymax = upper), alpha = 0.5, show.legend = FALSE, col = "indianred2") +
  geom_line(data = m, aes(x = trial_nr, y = mean_x), size = 1, show.legend = FALSE, col = "indianred2") + 
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), alpha = 0.5, show.legend = FALSE, fill = "indianred2") +
  geom_line(data = ms, aes(x = trial_nr, y = mean_x), size = 1, show.legend = FALSE, col = "darkslategray4") + 
  geom_ribbon(data = ms, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), alpha = 0.5, show.legend = FALSE, fill = "darkslategray4") +
  geom_hline(yintercept = 37.5, linetype = 'dotted', col = 'red', size = 1.5) + 
  geom_hline(yintercept = 37.5, linetype = 'dotted', col = 'red', size = 1.5) + 
  labs(x = "Trial", y = "Reward") + theme(text = element_text(size = 20)) + scale_x_continuous('Trial', trialslab, trialslab) + ylim(20, 65)

#plot effect of round
z <- as.data.frame(effect("block_nr_c", m))
z$block_nrp <- c(1, 5, 15, 25, 30)
datar$block_nrp <- datar$block_nr + 1
sim$block_nrp <- sim$block_nr + 1
d <- aggregate(datar$reward, list(subjectID = datar$subjectID, block_nr = datar$block_nrp), FUN=mean, digits=3)
m <- d %>%
  group_by(block_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
ds <- aggregate(sim$reward, list(subjectID = sim$subjectID, block_nr = sim$block_nrp), FUN=mean, digits=3)
ms <- ds %>%
  group_by(block_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
trial_summary_frame <- datar %>% group_by(subjectID,block_nrp) %>% summarise(score = mean(reward))
trials <- unique(na.omit(trial_summary_frame)$block_nrp)
trialslab <- c(1, 10, 20, 30)
ggplot() + 
  geom_jitter(data = trial_summary_frame, aes(x = block_nrp, y = score), alpha = 0.3) +
  ylim(0,80) + theme_classic() + 
  geom_line(data = z, aes(x = block_nrp, y = fit), size = 1, show.legend = FALSE, col = "indianred2") +
  geom_ribbon(data = z, aes(x = block_nrp, ymin = lower, ymax = upper), alpha = 0.5, show.legend = FALSE, col = "indianred2") +
  #geom_line(data = m, aes(x = trial_nr, y = mean_x), size = 1, show.legend = FALSE, col = "indianred2") + 
  #geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), alpha = 0.5, show.legend = FALSE, fill = "indianred2") +
  #geom_line(data = ms, aes(x = trial_nr, y = mean_x), size = 1, show.legend = FALSE, col = "darkslategray4") + 
  #geom_ribbon(data = ms, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), alpha = 0.5, show.legend = FALSE, fill = "darkslategray4") +
  geom_hline(yintercept = 37.5, linetype = 'dotted', col = 'red', size = 1.5) + 
  geom_hline(yintercept = 37.5, linetype = 'dotted', col = 'red', size = 1.5) + 
  labs(x = "Round", y = "Reward") + theme(text = element_text(size = 20)) + scale_x_continuous('Round', trialslab, trialslab) #+ ylim(20, 65)

#################################################
#H1.2: decrease in exploration
#################################################

#explore exploit trait-of?
m <- glmer(new_click ~ trial_nr_c*block_nr_c + (1 + trial_nr_c + block_nr_c|subjectID), data = datar, family="binomial", control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(m)
ms <- glmer(new_click ~ trial_nr_c*block_nr_c + (1 + trial_nr_c + block_nr_c|subjectID), data = sim, family="binomial", control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(ms)
z <- as.data.frame(effect("trial_nr_c", m))
z$trial_nrp <- c(1, 6, 11, 16, 21)

d <- aggregate(datar$new_click, list(subjectID = datar$subjectID, trial_nr = datar$trial_nrp), FUN=mean, digits=3)
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
#for summary stats of sim:
ds <- aggregate(sim$new_click, list(subjectID = sim$subjectID, trial_nr = sim$trial_nrp), FUN=mean, digits=3)
ms <- ds %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "indianred2") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  #geom_line(data = z, aes(x = trial_nrp, y = fit), color = "indianred2") +  # Mean line
  #geom_ribbon(data = z, aes(x = trial_nrp, ymin = lower, ymax = upper), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "indianred2") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = ms, aes(x = trial_nr, y = mean_x), color = "darkslategray4") +  # Mean line
  geom_ribbon(data = ms, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkslategray4", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + labs(x = "Trial", y = "Novel choices") + theme(text = element_text(size = 20)) + scale_x_continuous('Trial', trialslab, trialslab)


#distance from previous click
#m <- lmer(distance_prev ~ trial_nr_c*block_nr_c + (1|subjectID), data = datar)
#m <- lmer(logdistance_prev ~ logtrial*block_nr_c + (1 + logtrial * block_nr_c|subjectID), data = datar, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
m <- lmer(logdistance_prev ~ trial_nr_c*block_nr_c + (1 + trial_nr_c * block_nr_c|subjectID), data = datar, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(m)
ms <- lmer(logdistance_prev ~ trial_nr_c*block_nr_c + (1 + trial_nr_c * block_nr_c|subjectID), data = sim, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(ms)
z <- as.data.frame(effect("trial_nr_c", m))
z$expfit <- 10^z$fit
z$explow <- 10^z$lower
z$expup <- 10^z$upper
#z <- as.data.frame(effect("logtrial", m))
z$trial_nrp <- c(1, 6, 11, 16, 21)

d <- aggregate(datar$distance_prev, list(datar$subjectID, trial_nr = datar$trial_nrp), FUN=mean, digits = 4)
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
ds <- aggregate(sim$distance_prev, list(sim$subjectID, trial_nr = sim$trial_nrp), FUN=mean, digits = 4)
ms <- ds %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))

ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  #geom_line(data = z, aes(x = trial_nrp, y = expfit), color = "indianred2") +  # Mean line
  #geom_ribbon(data = z, aes(x = trial_nrp, ymin = explow, ymax = expup), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "indianred2") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = ms, aes(x = trial_nr, y = mean_x), color = "darkslategray4") +  # Mean line
  geom_ribbon(data = ms, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkslategray4", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + labs(x = "Trial", y = "Difference from \n previous choice") + theme(text = element_text(size = 20)) + scale_x_continuous('Trial', trialslab, trialslab) + scale_y_log10()

#distance from hv cell
#m <- lmer(distance ~ trial_nr_c*block_nr_c + (1|subjectID), data = datar)
#m <- lmer(logdistance ~ logtrial*block_nr_c + (1 + logtrial * block_nr_c|subjectID), data = datar, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
m <- lmer(logdistance ~ trial_nr_c*block_nr_c + (1 + trial_nr_c * block_nr_c|subjectID), data = datar, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(m)
ms <- lmer(logdistance ~ trial_nr_c*block_nr_c + (1 + trial_nr_c * block_nr_c|subjectID), data = sim, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(ms)
z <- as.data.frame(effect("trial_nr_c", m))
z$trial_nrp <- c(1, 6, 11, 16, 21)
z$expfit <- 10^z$fit
z$explow <- 10^z$lower
z$expup <- 10^z$upper

d <- aggregate(datar$distance, list(datar$subjectID, trial_nr = datar$trial_nrp), FUN=mean, digits = 4)
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
ds <- aggregate(sim$distance, list(sim$subjectID, trial_nr = sim$trial_nrp), FUN=mean, digits = 4)
ms <- ds %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(nr_pp), upper_ci = mean_x + 1.96*sd(x)/sqrt(nr_pp))
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  #geom_line(data = z, aes(x = trial_nrp, y = expfit), color = "indianred2") +  # Mean line
  #geom_ribbon(data = z, aes(x = trial_nrp, ymin = explow, ymax = expup), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "indianred2") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = ms, aes(x = trial_nr, y = mean_x), color = "darkslategray4") +  # Mean line
  geom_ribbon(data = ms, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkslategray4", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + labs(x = "Trial", y = "Difference from most \n similar high value choice") + theme(text = element_text(size = 20)) + scale_x_continuous('Trial', trialslab, trialslab) + scale_y_log10()

#########################################################
#H1.3: adaptive search distance based on previous reward
#########################################################

datar$prev_reward_c <- scale(datar$prev_reward)
modelfull <- lmer(distance_prev ~ prev_reward_c*trial_nr_c*block_nr_c  + (1+prev_reward_c*(trial_nr_c+block_nr_c)|subjectID), data = datar, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(modelfull)
z <- as.data.frame(effect("prev_reward_c", modelfull))
z$prev_reward <- c(0, 13.333, 35.666, 53.666, 80)
breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
datar <- datar %>% mutate(prev_reward_bin = cut(prev_reward, breaks = breaks))
bin_midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2
datar <- datar %>%
  mutate(prev_reward_mid = bin_midpoints[as.numeric(prev_reward_bin)])
d <- aggregate(datar$distance_prev, list(datar$subjectID, prev_reward = datar$prev_reward_bin, prev_reward_mid = datar$prev_reward_mid), FUN=mean, digits = 4)
m <- d %>%
  group_by(prev_reward, prev_reward_mid) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - sd(x) / sqrt(n()), upper_ci = mean_x + sd(x)/sqrt(n()), .groups = "drop")
sim <- sim %>% mutate(prev_reward_bin = cut(prev_reward, breaks = breaks))
bin_midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2
sim <- sim %>%
  mutate(prev_reward_mid = bin_midpoints[as.numeric(prev_reward_bin)])
ds <- aggregate(sim$distance_prev, list(sim$subjectID, prev_reward = sim$prev_reward_bin, prev_reward_mid = sim$prev_reward_mid), FUN=mean, digits = 4)
ms <- ds %>%
  group_by(prev_reward, prev_reward_mid) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - sd(x) / sqrt(n()), upper_ci = mean_x + sd(x)/sqrt(n()), .groups = "drop")
ggplot() +
  geom_point(data = d, aes(x = prev_reward_mid, y = x), color = "grey", alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  #geom_line(data = m, aes(x = prev_reward_mid, y = mean_x), color = "indianred2") +  # Mean line
  #geom_ribbon(data = m, aes(x = prev_reward_mid, ymin = lower_ci, ymax = upper_ci), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  #geom_line(data = ms, aes(x = prev_reward_mid, y = mean_x), color = "darkslategray4") +  # Mean line
  #geom_ribbon(data = ms, aes(x = prev_reward_mid, ymin = lower_ci, ymax = upper_ci), fill = "darkslategray4", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = z, aes(x = prev_reward, y = fit), color = "indianred2") + 
  geom_ribbon(data = z, aes(x = prev_reward, ymin = lower, ymax = upper), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + labs(x = "Previous reward", y = "Difference from \n previous choice") + theme(text = element_text(size = 20)) + ylim(-1.18, 10.0)

####
#alternative: rescale previous reward on each pp change point
###
breaks <- c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
datar <- datar %>% mutate(prev_reward_bin = cut(rres, breaks = breaks))
bin_midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2
datar <- datar %>%
  mutate(prev_reward_mid = bin_midpoints[as.numeric(prev_reward_bin)])
d <- aggregate(datar$distance_prev, list(datar$subjectID, prev_reward = datar$prev_reward_bin, prev_reward_mid = datar$prev_reward_mid), FUN=mean, digits = 4)
m <- d %>%
  group_by(prev_reward, prev_reward_mid) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(n()), upper_ci = mean_x + 1.96*sd(x)/sqrt(n()), .groups = "drop")
sim <- sim %>% mutate(prev_reward_bin = cut(rres, breaks = breaks))
bin_midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2
sim <- sim %>%
  mutate(prev_reward_mid = bin_midpoints[as.numeric(prev_reward_bin)])
ds <- aggregate(sim$distance_prev, list(sim$subjectID, prev_reward = sim$prev_reward_bin, prev_reward_mid = sim$prev_reward_mid), FUN=mean, digits = 4)
ms <- ds %>%
  group_by(prev_reward, prev_reward_mid) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - 1.96*sd(x) / sqrt(n()), upper_ci = mean_x + 1.96*sd(x)/sqrt(n()), .groups = "drop")
ggplot() +
  geom_point(data = d, aes(x = prev_reward_mid, y = x), color = "grey", alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = prev_reward_mid, y = mean_x), color = "indianred2") +  # Mean line
  geom_ribbon(data = m, aes(x = prev_reward_mid, ymin = lower_ci, ymax = upper_ci), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  geom_line(data = ms, aes(x = prev_reward_mid, y = mean_x), color = "darkslategray4") +  # Mean line
  geom_ribbon(data = ms, aes(x = prev_reward_mid, ymin = lower_ci, ymax = upper_ci), fill = "darkslategray4", alpha = 0.3) +  # Confidence interval ribbon
  #geom_line(data = z, aes(x = prev_reward, y = fit), color = "indianred2") + 
  #geom_ribbon(data = z, aes(x = prev_reward, ymin = lower, ymax = upper), fill = "indianred2", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + labs(x = "Previous reward", y = "Distance from \n previous choice") + theme(text = element_text(size = 20))


##################################################
##################################################
#H2: the model estimates meaningfully every step
##################################################
##################################################

#estimated parameters
setwd("..")
version <- "locfreq"
if (version == "base"){
  est <- read.csv("est_020326B.csv") #non-localized version #260226
}else if (version == "loc"){
  est <- read.csv("est_020326.csv")  #localized #120126
}else if (version == "locfreq"){
  est <- read.csv("est_050426.csv") #localized and freq bias #070326
}else if (version == "freq"){
  est <- read.csv("est_060426.csv")
}

#if doubles: use results with best (lowest NLL)
if (length(unique(est$subjectID)) < nrow(est)){
  temp_est <- data.frame()
  for (i in 1:length(unique(est$subjectID))){
    pp <- unique(est$subjectID)[i]
    pp_est <- est[est$subjectID == unique(est$subjectID)[i],]
    new_row <- pp_est[which.min(pp_est$NLL),]
    temp_est <- rbind(temp_est, new_row)
  }
est <- temp_est
}


est$logl <- log(est$l_fit, 10)
est$logb <- log(est$beta, 10)
est$logt <- log(est$tau, 10)

mean(est$NLL)
Mrandom <- - log(1/(length(animals)))*nr_trials*nr_blocks
R2 <- 1 - mean(est$NLL)/Mrandom
print(R2)


ggplot(est, aes(x = l_fit)) +
  ggdist::stat_halfeye(adjust = 0.5, side = "right", .width = 0, show.legend = FALSE, alpha = 0.8, aes(fill="indianred2")) + 
  geom_boxplot(width = .1, alpha = .5) + geom_dotplot() +
  scale_fill_manual(values="indianred2") +
  theme_classic() + labs(x = "Generalization", y = "Frequency") +
  scale_x_log10() + theme(text = element_text(size = 20))
mean(est$l_fit)
sd(est$l_fit)
t.test(est$l_fit, mu = exp(-5))
t <- wilcox.test(est$l_fit, mu = exp(-5))
t
qnorm(t$p.value/2)
boot.t.test(est$l_fit, mu = exp(-5))

ggplot(est, aes(x = beta)) +
  ggdist::stat_halfeye(adjust = 3, side = "right", .width = 0, show.legend = FALSE, alpha = 0.8, aes(fill = "indianred2")) + 
  geom_boxplot(width = .1, alpha = .5) + geom_dotplot() +
  scale_fill_manual(values="lightpink") +
  theme_classic() + labs(x = "Uncertainty guided exploration", y = "Frequency") +
  scale_x_log10() + theme(text = element_text(size = 20))
mean(est$beta)
sd(est$beta)
t.test(est$beta, mu = exp(-5))
t <- wilcox.test(est$beta, mu = exp(-5))
t
qnorm(t$p.value/2)
boot.t.test(est$beta, mu = exp(-5))

ggplot(est, aes(x = tau)) +
  ggdist::stat_halfeye(adjust = 0.5, side = "right", .width = 0, show.legend = FALSE, alpha = 0.8, aes(fill = "indianred4")) + 
  geom_boxplot(width = .1, alpha = .5) + geom_dotplot() +
  scale_fill_manual(values = "indianred3") +
  theme_classic() + labs(x = "Random exploration", y = "Frequency") +
  scale_x_log10() + theme(text = element_text(size = 20))
mean(est$tau)
sd(est$tau)
t.test(est$tau, mu = exp(-5))
t <- wilcox.test(est$tau, mu = exp(-5))
t
qnorm(t$p.value/2)
boot.t.test(est$tau, mu = exp(-5))

if (version == "locfreq"){
  ggplot(est, aes(x = phi)) +
    ggdist::stat_halfeye(adjust = 0.5, side = "right", .width = 0, show.legend = FALSE, alpha = 0.8, aes(fill = "indianred4")) + 
    geom_boxplot(width = .1, alpha = .5) + geom_dotplot() +
    scale_fill_manual(values = "indianred4") +
    theme_classic() + labs(x = "Frequency Bias", y = "Frequency") +
    scale_x_log10() + theme(text = element_text(size = 20))
  mean(est$phi)
  sd(est$phi)
  t.test(est$phi, mu = exp(-5))
  t <- wilcox.test(est$phi, mu = exp(-5))
  t
  qnorm(t$p.value/2)
  boot.t.test(est$phi, mu = exp(-5))
}
#################################################
#################################################
#H3: moving around is costly
#################################################
#################################################

#################################################
#H3.1: locality bias visible in sampling distance
#################################################

#locality bias
#preference for nearby options?
ggplot(datar) + 
  geom_histogram(aes(x = distance_prev)) + theme_classic() + geom_vline(xintercept = meand, col = "red", linetype = "dotted") +
  labs(x = "Distance between consecutive clicks")

mean(info$av_distance_prev)
sd(info$av_distance_prev)

ggplot(info) + 
  geom_histogram(aes(x = av_distance_prev)) + theme_classic() + geom_vline(xintercept = meand, col = "red", linetype = "dotted") 

#################################################
#H3.2: localized version is better fit
#################################################

estloc <- read_csv("est_020326.csv")  #localized
estnonloc <- read.csv("est_020326B.csv") #non-localized version
mean(estloc$NLL) < mean(estnonloc$NLL)

