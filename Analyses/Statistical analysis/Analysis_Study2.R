library(dplyr)
library(misty)
library(tidyverse)
setwd("../../Data/Study2")

#read in all relevant files
info <- read.csv("info_all.csv")

##############
#study overlap
##############
#assess group size:
nrow(info)


overlap <- function(data, var_name, name){
  var_se <- paste0(var_name, "_se")
  var_sp <- paste0(var_name, "_sp")
  #correlations
  print(cor.test(data[[var_se]], data[[var_sp]], method = "pearson"))
  cor <- cor.test(data[[var_se]], data[[var_sp]], method="spearman", exact = FALSE)
  p <- round(cor$p.value, 3)
  if (p < 0.001){
    ptext <- "p < .001"
  }else{
    ptext <- paste0("p = ", p)
  }
  r <- round(cor$estimate, 2)
  #plotting
  #position of text
  #first check what maximum values are:
  max_x <- sort(unique(data[[var_se]]))[length(unique(data[[var_se]]))-2] #second highest value for x-axis
  if (r > 0){
    #then bottom right
    xpos <- min(na.omit(data[[var_se]])) + 0.8*(max_x - min(na.omit(data[[var_se]])))
  }else{
    #then bottom left
    xpos <- min(na.omit(data[[var_se]])) + 0.3*(max_x - min(na.omit(data[[var_se]])))
  }
  ypos <- min(na.omit(data[[var_sp]])) + 0.05*(max(na.omit(data[[var_sp]])) - min(na.omit(data[[var_sp]])))
  max <- max(max(na.omit(data[[var_se]])), max(na.omit(data[[var_sp]])))
  min <- min(min(na.omit(data[[var_se]])), min(na.omit(data[[var_sp]])))
  #plot itself
  p <- ggplot(data, aes(x = .data[[var_se]], y = .data[[var_sp]])) + 
    geom_point() + 
    geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
    theme_classic() +
    labs(x = paste("Semantic", name), y = paste("Spatial", name)) +
    #geom_label(aes(x = xpos, y = ypos, fontface = 40, label = paste0("r = ", r, ", ", ptext))) + 
    annotate("text", x = xpos, y = ypos, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
    theme(text = element_text(size = 30))
  print(p)
  #ggsave(paste0(var_sp, "(", var_se, ").png"), plot = p, device = "png", height = 5/0.8, width = 6/0.8)
}

####score####
overlap(info, "score", "score")
overlap(info, "slope_score", "learning")

#novelclicks
overlap(info, "Novclicks", "novel clicks")
overlap(info, "slope_novel", "trend novel clicks")

#Dprev
overlap(info, "av_distance_prev", "distance from previous choice")
overlap(info, "slope_dprev", "trend distance from previous choice")

#in function of reward
overlap(info, "slope_dprew", "adaptiveness")

#DHV
overlap(info, "av_distance", "distance from high value choice")
overlap(info, "slope_dhv", "trend distance from high value choice")


####
#correlations between comp measures
####
#l_fit
cor.test(info$l_fit_se,info$l_fit_sp , method="pearson")
cor <- cor.test(info$l_fit_se,info$l_fit_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = l_fit_se, y = l_fit_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic generalization", y = "Spatial generalization") + 
  scale_x_log10() + scale_y_log10() + 
  annotate("text", x = 0.8, y = 0.3, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
  theme(text = element_text(size = 30))

#beta
cor.test(info$beta_se,info$beta_sp , method="pearson")
cor <- cor.test(info$beta_se,info$beta_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = beta_se, y = beta_sp)) + geom_point(position = "jitter") + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic UG exploration", y = "Spatial UG exploration") + 
  scale_x_log10() + scale_y_log10() + 
  annotate("text", x = 0.3, y = 0.01, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
  theme(text = element_text(size = 30))

#beta but without all the lower bounds clippers
info$beta_se <- as.numeric(round(info$beta_se, 7))
info$beta_sp <- as.numeric(round(info$beta_sp, 7))
clipper <- sort(unique(info$beta_se))[1] #lower bound
info_filter <- info[info$beta_se > clipper & info$beta_sp > clipper,]
cor.test(info_filter$beta_se,info_filter$beta_sp , method="pearson")
cor <- cor.test(info_filter$beta_se,info_filter$beta_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info_filter, aes(x = beta_se, y = beta_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic UG exploration", y = "Spatial UG exploration") + theme(text = element_text(size = 20)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_label(aes(x = 0.7, y = 0.01, label = paste0("r = ", r, ", ", ptext)))

#tau
cor.test(info$tau_se,info$tau_sp , method="pearson")
cor <- cor.test(info$tau_se,info$tau_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = tau_se, y = tau_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10() + scale_y_log10() + 
  labs(x = "Semantic random exploration", y = "Spatial random exploration") +
  annotate("text", x = 0.2, y = 0.03, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
  theme(text = element_text(size = 30))


#####
#Factor analysis
#####
library(factoextra)
library(tidyverse)

info <- info[!is.na(info$score_se) & !is.na(info$score_sp),]
info$nov_sc_se <- scale(info$Novclicks_se)
info$av_distance_se <- ifelse(info$av_distance_se > 100, 12, info$av_distance_se)
info$d_sc_se <- scale(info$av_distance_se)
info$dprev_sc_se <- scale(info$av_distance_prev_se)
info$nov_sc_sp <- scale(info$Novclicks_sp)
info$d_sc_sp <- scale(info$av_distance_sp)
info$dprev_sc_sp <- scale(info$av_distance_prev_sp)
info$score_sc_se <- scale(info$score_se)
info$slope_score_sc_se <- scale(info$slope_score_se)
info$slope_nov_sc_se <- scale(info$slope_novel_se)
info$slope_d_sc_se <- scale(info$slope_dhv_se)
info$slope_dprev_sc_se <- scale(info$slope_dprev_se)
info$slope_dprew_sc_se <- scale(info$slope_dprew_se)
info$score_sc_sp <- scale(info$score_sp)
info$slope_score_sc_sp <- scale(info$slope_score_sp)
info$slope_nov_sc_sp <- scale(info$slope_novel_sp)
info$slope_d_sc_sp <- scale(info$slope_dhv_sp)
info$slope_dprev_sc_sp <- scale(info$slope_dprev_sp)
info$slope_dprew_sc_sp <- scale(info$slope_dprew_sp)
info["logl_se"] <- scale(log(info$l_fit_se, 10))
info["logb_se"] <- scale(log(info$beta_se, 10))
info["logt_se"] <- scale(log(info$tau_se, 10))
info["logphi_se"] <- scale(log(info$phi, 10))
info["logl_sp"] <- scale(log(info$l_fit_sp, 10))
info["logb_sp"] <- scale(log(info$beta_sp, 10))
info["logt_sp"] <- scale(log(info$tau_sp, 10))


info_filter <- info[c("nov_sc_se", "d_sc_se", "dprev_sc_se", 
                      "score_sc_se", "slope_score_sc_se", 
                      "slope_nov_sc_se", "slope_d_sc_se", "slope_dprev_sc_se", 
                      "slope_dprew_sc_se",
                      "logl_se", "logb_se", "logt_se", "logphi_se",
                      "nov_sc_sp", "d_sc_sp", "dprev_sc_sp", 
                      "score_sc_sp", "slope_score_sc_sp", 
                      "slope_nov_sc_sp", "slope_d_sc_sp", "slope_dprev_sc_sp", 
                      "slope_dprew_sc_sp",
                      "logl_sp", "logb_sp", "logt_sp")]
info_filter <- na.omit(info_filter)

PCA <- prcomp(info_filter, scale = TRUE)
fviz_eig(PCA)

fviz_pca_var(PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
rot <- as.data.frame(PCA$rotation)
loadings_long <- rot[,c("PC1", "PC2", "PC3", "PC4", "PC5")] %>%
  rownames_to_column("variable") %>%
  pivot_longer(
    cols = starts_with("PC"),
    names_to = "component",
    values_to = "loading"
  )
variable_order <- colnames(info_filter)

loadings_long$variable <- factor(
  loadings_long$variable,
  levels = variable_order
)
#visualize
ggplot(loadings_long, 
       aes(x = loading, 
           y = variable,
           fill = component)) +
  
  geom_col(width = 0.7) +
  
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  
  facet_wrap(~component, nrow = 1) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12)
  ) + 
  scale_y_discrete(limits = rev(variable_order))
