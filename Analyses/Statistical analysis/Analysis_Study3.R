library(ggplot2)
library(ggsignif)
library(factoextra)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(comprehenr)
library(afex)
library(brms)
library(effects)
library(sjPlot)
library(car)

###
#step 1: read in all the data, make sure only relevant data is used
###

setwd("../../Data")

#read in all relevant files
data <- read.csv("data2.csv")
info <- read.csv("info_all.csv")
#only use participants that have semantic data
info <- info[info$subjectID %in% data$subjectID,]

nr_participants <- length(unique(data$subjectID))
nr_blocks = 10
nr_trials = 21

info$performance <- info$score_se
#but first we need to add all AQ, age, gender data to the data dataframe
#group <- c()
age <- c()
pc1 <- c()
gender <- c()
SDS <- c()
srs <- c()
PAQ <- c()
for (p in 1:length(unique(data$subjectID))){
  participant = unique(data$subjectID)[p]
  #group <- append(group, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$group))
  age <- append(age, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$Age))
  pc1 <- append(pc1, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$CATI))
  gender <- append(gender, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$Sex))
  SDS <- append(SDS, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$SDS))
  srs <- append(srs, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$ASRS))
  PAQ <- append(PAQ, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$PAQ))
}
#data["group"] <- group
data["age"] <- age
data["PCA"] <- pc1
data["gender"] <- gender
data["SDS"] <- SDS
data["SRS"] <- srs
data["PAQ"] <- PAQ
#for lindear trends, we use the logdistance and the logtrial
#add the log of trial_nr and distance to the dataframes
data["logtrial"] <- log(data["trial_nr"]+0.00001,10) 
data["logdistance"] <- ifelse(data$distance < 100, log(data$distance +0.00001, 10), log(12, 10)) #make sure no inf values are in here
data["logdistance_prev"] <- log(data["distance_prev"]+0.00001, 10)


options(contrasts = c("contr.sum", "contr.poly"))

#rescale factors for L(M)Models
info_Wfilter <- info[!is.na(info$CATI),]
info_Wfilter$PCA <- scale(info_Wfilter$CATI)
info_Wfilter$gender <- as.factor(info_Wfilter$Sex)
info_Wfilter <- info_Wfilter[info_Wfilter$gender == "Female" | info_Wfilter$gender == "Male",]
info_Wfilter$age <- scale(info_Wfilter$Age)
info_Wfilter$SDS <- scale(info_Wfilter$SDS)
info_Wfilter$SRS <- scale(info_Wfilter$ASRS)
info_Wfilter$PAQ <- scale(info_Wfilter$PAQ)

data_Wfilter <- data[data$subjectID %in% info_Wfilter$subjectID,]
data_Wfilter$PCA <- scale(data_Wfilter$PCA)
data_Wfilter$gender <- as.factor(data_Wfilter$gender)
data_Wfilter$age <- scale(data_Wfilter$age)
data_Wfilter$SDS <- scale(data_Wfilter$SDS)
data_Wfilter$SRS <- scale(data_Wfilter$SRS)
data_Wfilter$PAQ <- scale(data_Wfilter$PAQ)
data_Wfilter$trial_nr <- scale(data_Wfilter$trial_nr)
data_Wfilter$logtrial <- scale(data_Wfilter$logtrial)
data_Wfilter$block_nr <- scale(data_Wfilter$block_nr)


###
###
#Section 1: Score
###
###
nrow(info_Wfilter)
cor.test(info_Wfilter$performance,info_Wfilter$PCA , method="pearson")
cor.test(info_Wfilter$performance,info_Wfilter$PCA , method="spearman", exact = FALSE)
ggplot(info_Wfilter, aes(x = PCA, y = performance)) + geom_point() + geom_smooth(method = 'lm', color = "deepskyblue4", fill = "deepskyblue") +
  #annotate("text", x = max(info_Wfilter$PCA)-2, y = max(drop_na(info_Wfilter, performance)$performance - 1), label = paste("p = ", toString(round(c$p.value, digits = 3)))) +
  theme_classic() 

###
###
#Section 2: behavioral measurements for exploration
###
###
#for plotting purposes: create summary dataframe showing one trial per group/autistic traits bin
#bin the PCA data in same bins as model: -3, -1, 0 (0.1), 2, 3
bin_centers <- c(-2, -1, 0.06, 1, 2) #edit based on fit bins 
# find the closest bin center for each value in list
find_closest_bin <- function(value, bin_centers) {
  distances <- abs(bin_centers - value)
  closest_bin <- which.min(distances)
  return(closest_bin)
}
data_Wfilter$PCAbin <- unlist(sapply(data_Wfilter$PCA, find_closest_bin, bin_centers = bin_centers))
name_W <- data_Wfilter %>% group_by(PCAbin, trial_nr)
plot_dfW <- name_W %>% summarise(Novclicks = mean(new_click), HVclicks = mean(HV_click), D = mean(distance), Dprev = mean(distance_prev))

###
#Novel clicks
modelfull <- glmer(new_click ~ PCA * trial_nr + gender + age + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelfull)   
z <- as.data.frame(effect("PCA:trial_nr", modelfull))
plot_dfW$PCA <- unique(as.factor(z$PCA))[plot_dfW$PCAbin]
ggplot() +
  geom_ribbon(data = z, aes(x = trial_nr, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  geom_line(data = z, aes(x = trial_nr, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  #geom_point(data = plot_dfW, aes(x = trial_nr, y = Novclicks, color = as.factor(PCA)), size = 3, show.legend = TRUE) +
  labs(x = "Trial", y = "Novel choices", color = "CATI", fill = "CATI") +
  theme_classic() + theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("cadetblue", "cadetblue2", "lemonchiffon3", "indianred1","indianred3")) + scale_fill_manual(values=c("cadetblue", "cadetblue2", "lemonchiffon", "indianred1","indianred3"))###

#while controlling:
modelfull_con <- glmer(new_click ~ (PCA + SDS + SRS + PAQ + trial_nr)^2 + gender + age + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelfull_con)   

###
###
#Section 3: Distance measures for exploration
###
######
#distance from previous click
###
m_dprev_nc <- lmer(logdistance_prev ~ (PCA + logtrial + block_nr)^2 + gender + age + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_dprev_nc)
z <- as.data.frame(effect("PCA:logtrial", m_dprev_nc))
ggplot() +
  geom_line(data = z, aes(x = logtrial, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = logtrial, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Trial (log trial number)", y = "Difference from previous choice (log)", color = "PCA", fill = "PCA") +
  theme_classic() + theme(text = element_text(size = 20)) +
   scale_color_manual(values = c("cadetblue", "cadetblue2", "lemonchiffon3", "indianred1","indianred3")) + scale_fill_manual(values=c("cadetblue", "cadetblue2", "lemonchiffon", "indianred1","indianred3"))###

#while controlling:
m_dprev <- lmer(logdistance_prev ~ (PCA + logtrial + block_nr + SDS + SRS + PAQ)^2 + gender + age + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_dprev)

###
#distance from most nearby high value cell
###
m_d_nc <- lmer(logdistance ~ (PCA + logtrial + block_nr)^2 + gender + age + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_d_nc)
z <- as.data.frame(effect("PCA:logtrial", m_d_nc))
ggplot() +
  geom_line(data = z, aes(x = logtrial, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = logtrial, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  #geom_point(data = plot_dfW, aes(x = trial_nr, y = log(D), color = as.factor(PCA)), size = 3, show.legend = TRUE) +
  labs(x = "Trial (log trial number)", y = "Difference from high value choice (log)", color = "PCA", fill = "PCA") +
  theme_classic() + theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("cadetblue", "cadetblue2", "lemonchiffon3", "indianred1","indianred3")) + scale_fill_manual(values=c("cadetblue", "cadetblue2", "lemonchiffon", "indianred1","indianred3"))###

#without controlling:
m_d<- lmer(logdistance ~ (PCA + logtrial + block_nr + SDS + SRS + PAQ)^2 + gender + age + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_d)

###
###
#Section 4: modelling results
###
###

infoS <- info

infoS["l"] <- infoS$l_fit_se
infoS["b"] <- infoS$beta_se
infoS["t"] <- infoS$tau_se
infoS["f"] <- info$phi
infoS["NLL"] <- infoS$NLL.x

info_W <- infoS[infoS$subjectID %in% info_Wfilter$subjectID,]
info_W["PCA"] <- scale(info_W$CATI)
info_W["gender"] <- as.factor(info_W$Sex)
info_W["age"] <- scale(info_W$Age)
info_W["SDS"] <- scale(info_W$SDS)
info_W["SRS"] <- scale(info_W$ASRS)
info_W["PAQ"] <- scale(info_W$PAQ)
info_W["logl"] <- scale(log(info_W$l, 10))
info_W["logb"] <- scale(log(info_W$b, 10))
info_W["logt"] <- scale(log(info_W$t, 10))
info_W["logf"] <- scale(log(info_W$f, 10))
info_W["NLL"] <- scale(info_W$NLL)

#tau and phi correlate strongly:
cor.test(info_W$logt, info_W$logf, method = "pearson")
cor.test(info_W$logt, info_W$logf, method = "spearman")
#model with all four model parameters:
mf_nc <- glm(formula = PCA ~ logl + logb + logt + logf + gender + age, family = gaussian, data = info_W)
summary(mf_nc)
vif(mf_nc) #too high: we can't include both


m_nc <- glm(formula = PCA ~ logl + logb + logt + gender + age, family = gaussian, data = info_W)
summary(m_nc)
z <- as.data.frame(effect("logt", m_nc))
ggplot() +
  geom_line(data = z, aes(x = logt, y = fit), size = 1, show.legend = FALSE, color = "indianred3") +
  geom_ribbon(data = z, aes(x = logt, ymin = lower, ymax = upper), alpha = 0.3, show.legend = FALSE, fill = "indianred3") +
  labs(x = "Random exploration (log t)", y = "Autism traits") +
  theme_classic() + theme(text = element_text(size = 20))

#with controlling
m <- glm(formula = PCA ~ logl + logb + logt + (SDS + SRS + PAQ)^2 + gender + age, family = gaussian, data = info_W)
summary(m)

#model with frequency bias instead of random exploraiton
mf_nc_nt <- glm(formula = PCA ~ logl + logb + logf + gender + age, family = gaussian, data = info_W)
summary(mf_nc_nt)
z <- as.data.frame(effect("logf", mf_nc_nt))
ggplot() +
  geom_line(data = z, aes(x = logf, y = fit), size = 1, show.legend = FALSE, color = "indianred3") +
  geom_ribbon(data = z, aes(x = logf, ymin = lower, ymax = upper), alpha = 0.3, show.legend = FALSE, fill = "indianred3") +
  labs(x = "Frequency bias (log phi)", y = "Autism traits") +
  theme_classic() + theme(text = element_text(size = 20))

mf_nt <- glm(formula = PCA ~ logl + logb + logf + (SDS + SRS + PAQ)^2 + gender + age, family = gaussian, data = info_W)
summary(mf_nt)
