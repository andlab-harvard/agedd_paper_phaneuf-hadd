##########################
### Run AgeDD Analyses ###
##########################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 2/4/26

### Rundown -----

# Inputs: task, questionnaire, and demographic data
# Computes: 
# - (histogram) DD distributions ($20 vs. $200)
# - (scatterplots, lmer) DD across age ($20 vs. $200)
# - (lmers) DD across age and SES (sensitivity analyses)
# - (histograms) cognitive mechanism distributions
# - (scatterplots, lms) cognitive mechanisms across age
# - (DAGs) age-related changes in cognitive mechanisms and DD ($20 vs. $200)
# Outputs: 
# - txt files of statistical outputs (significance tests, "parameter" estimates)
# - png files of histograms, scatterplots, and DAGs

### Set up Script -----

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       ggplot2, # for plotting
       bnlearn, # for DAGs
       Rgraphviz, # for DAGs
       sjPlot, # for plot_model()
       lmerTest, # for mixed effects models
       performance, # for check_predictions()
       car, # for Anova()
       mice, # for md.pattern()
       VIM, # for aggr()
       visdat, # for vis_dat()
       naniar) # for vis_miss()

# Load shared AgeDD variables
source("utilities.R")

# Set path to data
in_path <- '../data/'

# Set output path
out_path <- '../results/analyses/'

# Read in data
demog <- read.csv(paste0(in_path, "demog.csv"))
dd <- read.csv(paste0(in_path, "dd.csv"), check.names = FALSE)
stroop <- read.csv(paste0(in_path, "stroop.csv"))
fos <- read.csv(paste0(in_path, "fos.csv"))
nback <- read.csv(paste0(in_path, "nback.csv"))
bas <- read.csv(paste0(in_path, "bas.csv"))
range <- read.csv(paste0(in_path, "range.csv"))
ses <- read.csv(paste0(in_path, "ses.csv"))

# Merge demog with other data and get missing data counts
demog_simp <- demog[, c("StudyID", "ExactAge")]
dd <- merge(dd, demog_simp, by = "StudyID") # N = 159
stroop <- merge(stroop[, c("StudyID", "perf")], demog_simp, by = "StudyID") # N = 159
fos <- merge(fos, demog_simp, by = "StudyID") # N = 159
nback <- merge(nback[, c("StudyID", "dprime_2back")], demog_simp, by = "StudyID") # N = 145
bas <- merge(bas, demog_simp, by = "StudyID") # N = 159
range <- merge(range, demog_simp, by = "StudyID") # N = 159

### Final DD Data Processing -----

# Format DD decision data for plotting
dd_auc <- dd[, c("StudyID", "ExactAge", "auc_20", "auc_200")]
dd_auc_long <- pivot_longer(dd_auc, cols = c("auc_20", "auc_200"), names_to = "Decision", values_to = "AUC")
dd_auc_long <- dd_auc_long %>% mutate(Magnitude = ifelse(Decision == "auc_20", "$20", "$200"))
dd_auc_long$Magnitude <- factor(dd_auc_long$Magnitude, levels = c("$20", "$200"))

# Format DD decision data for SES sensitivity analyses
ses$MacArthurAvg <- (ses$MacArthur1 + ses$MacArthur2) / 2
dd_auc_long_ses <- merge(dd_auc_long, ses[, c("StudyID", "FamilyIncomeSimp", "ParentEducationSimp", "MacArthurAvg")], by = "StudyID")
str(dd_auc_long_ses)
dd_auc_long_ses$FamilyIncomeSimp <- factor(dd_auc_long_ses$FamilyIncomeSimp, levels = c("<25k", "25k-50k", "50k-75k",
                                                                                        "75k-100k", "100k-200k", ">200k",
                                                                                        "Unknown", "Unreported"))
dd_auc_long_ses$ParentEducationSimp <- factor(dd_auc_long_ses$ParentEducationSimp, levels = c("Some Sch.", "High Sch.",
                                                                                              "Some Coll.", "Bach.", "Postgrad.",
                                                                                              "Unknown", "Unreported"))
str(dd_auc_long_ses)

### Visualize DD Decision Data -----

# Quick peek at DD decision data distributions
hist(dd_auc$auc_20)
hist(dd_auc$auc_200)

# Save DD decision distribution
ggplot(data = dd_auc_long, aes(x = AUC, fill = Magnitude, color = Magnitude)) +
  scale_fill_manual(values = c(light_orange, dark_orange)) + 
  scale_color_manual(values = c(light_orange, dark_orange)) + 
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  geom_hline(yintercept = seq(0, 70, by = 14), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 70, by = 14)) + 
  geom_histogram(binwidth = .1, alpha = .75) +
  labs(x = "Delay Discounting\n(AUC)", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "dd_dist.png"), plot = last_plot(), width = 5, height = 5)

# Save DD decisions across linear age
ggplot(data = dd_auc_long, aes(x = ExactAge, y = AUC, fill = Magnitude, color = Magnitude)) +
  scale_fill_manual(values = c(light_orange, dark_orange)) + 
  scale_color_manual(values = c(light_orange, dark_orange)) + 
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(0, 1), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = c(.25, .5, .75), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 1, by = .25)) +  
  geom_point(alpha = 1, size = 2.75) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5) +
  labs(x = "Age (Years)", y = "Delay Discounting\n(AUC)") +
  agedd_theme
ggsave(paste0(out_path, "fig3.png"), width = 7, height = 5)

# Save DD decisions across smooth age
ggplot(data = dd_auc_long, aes(x = ExactAge, y = AUC, fill = Magnitude, color = Magnitude)) +
  scale_fill_manual(values = c(light_orange, dark_orange)) + 
  scale_color_manual(values = c(light_orange, dark_orange)) + 
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(0, 1), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = c(.25, .5, .75), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 1, by = .25)) +  
  geom_point(alpha = 1, size = 2.75) +
  geom_smooth(method = "loess") +
  labs(x = "Age (Years)", y = "Delay Discounting\n(AUC)") +
  agedd_theme
ggsave(paste0(out_path, "dd_age_smth.png"), width = 7, height = 5)

### Model DD Decision Data -----

# Model AUC across age -- fit model
str(dd_auc_long)
dd_auc_long$StudyID <- as.factor(dd_auc_long$StudyID)
str(dd_auc_long)
hist(dd_auc_long$AUC) # Gaussian distribution seems reasonable
auc_age <- lmer(AUC ~ ExactAge * Magnitude + (1|StudyID), data = dd_auc_long)
set.seed(123)
check_predictions(auc_age) # Good enough
qqnorm(residuals(auc_age), pch = 1, frame = FALSE)
qqline(residuals(auc_age), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions

# Model AUC across age -- interpret model
summary(auc_age)
Anova(auc_age, type = "II")
plot_model(auc_age, type = "pred", terms = c("ExactAge"))
plot_model(auc_age, type = "pred", terms = c("Magnitude"))
plot_model(auc_age, type = "pred", terms = c("ExactAge", "Magnitude"))
# AUC increases with age
# AUC is greater for $200 decisions than $20 decisions
# AUC differences between magnitudes emerge with age

# Save AUC model output
auc_model <- paste0(out_path, 'dd_age_lmer.txt')
sink(auc_model)
Anova(auc_age, type = "II")
sink()

# Model AUC across age, controlling for income -- fit model
auc_age_inc <- lmer(AUC ~ ExactAge * Magnitude + FamilyIncomeSimp + (1|StudyID), data = dd_auc_long_ses)
set.seed(123)
check_predictions(auc_age_inc) # Good enough
qqnorm(residuals(auc_age_inc), pch = 1, frame = FALSE)
qqline(residuals(auc_age_inc), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions

# Model AUC across age, controlling for education -- fit model
auc_age_edu <- lmer(AUC ~ ExactAge * Magnitude + ParentEducationSimp + (1|StudyID), data = dd_auc_long_ses)
set.seed(123)
check_predictions(auc_age_edu) # Good enough
qqnorm(residuals(auc_age_edu), pch = 1, frame = FALSE)
qqline(residuals(auc_age_edu), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions

# Model AUC across age, controlling for macarthur -- fit model
auc_age_mac <- lmer(AUC ~ ExactAge * Magnitude + MacArthurAvg + (1|StudyID), data = dd_auc_long_ses)
set.seed(123)
check_predictions(auc_age_mac) # Good enough
qqnorm(residuals(auc_age_mac), pch = 1, frame = FALSE)
qqline(residuals(auc_age_mac), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions

# SES sensitivity analyses modeling AUC across age -- interpret models
summary(auc_age_inc)
Anova(auc_age_inc, type = "II")
# No effect of income; age and magnitude results do not change
summary(auc_age_edu)
Anova(auc_age_edu, type = "II")
# No effect of education; age and magnitude results do not change
summary(auc_age_mac)
Anova(auc_age_mac, type = "II")
# No effect of macarthur; age and magnitude results do not change

# Save SES AUC sensitivity analyses
auc_ses_model <- paste0(out_path, 'dd_age_ses_lmer.txt')
sink(auc_ses_model)
cat("CATEGORICAL INCOME\n\n")
Anova(auc_age_inc, type = "II")
cat("\n\nCATEGORICAL EDUCATION\n\n")
Anova(auc_age_edu, type = "II")
cat("\n\nMACARTHUR AVERAGE\n\n")
Anova(auc_age_mac, type = "II")
sink()

### Visualize Inhibition Data -----

# Quick peek at stroop performance data distribution
hist(stroop$perf)

# Save stroop performance distribution
ggplot(data = stroop, aes(x = perf)) +
  scale_x_continuous(breaks = seq(-.6, .3, by = .15), expand = expansion(mult = c(0.02, 0.08))) +
  geom_hline(yintercept = seq(0, 60, by = 12), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 60, by = 12)) + 
  geom_histogram(binwidth = .05, alpha = .75, color = cust_blue, fill = cust_blue) +
  labs(x = "Stroop Performance", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "stroop_dist.png"), plot = last_plot(), width = 5, height = 5)

# Save stroop performance across linear age
ggplot(stroop, aes(x = ExactAge, y = perf)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(-.6, -.4, -.2, .2), color = 'grey90') +
  geom_hline(yintercept = c(0), linetype = 'dashed') +
  scale_y_continuous(breaks = round(seq(-.6, .2, by = .2), 2)) +
  geom_point(alpha = 1, size = 2.75, color = cust_blue) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5, color = cust_blue, fill = cust_blue) +
  labs(x = "Age (Years)", y = "Inhibition\n(Stroop Performance)") +
  agedd_theme
ggsave(paste0(out_path, "fig4b.png"), width = 5, height = 5)

# Save stroop performance across smooth age
ggplot(stroop, aes(x = ExactAge, y = perf)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(-.6, -.4, -.2, .2), color = 'grey90') +
  geom_hline(yintercept = c(0), linetype = 'dashed') +
  scale_y_continuous(breaks = round(seq(-.6, .2, by = .2), 2)) +
  geom_point(alpha = 1, size = 2.75, color = cust_blue) +
  geom_smooth(method = "loess", size = 1.5, alpha = .5, color = cust_blue, fill = cust_blue) +
  labs(x = "Age (Years)", y = "Inhibition\n(Stroop Performance)") +
  agedd_theme
ggsave(paste0(out_path, "stroop_age_smth.png"), width = 5, height = 5)

### Visualize Prospection Data -----

# Quick peek at FOS score data distribution
hist(fos$fos_total)

# Save FOS score distribution
ggplot(data = fos, aes(x = fos_total)) +
  scale_x_continuous(breaks = seq(30, 60, by = 10)) +
  geom_hline(yintercept = seq(0, 48, by = 12), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 48, by = 12)) + 
  geom_histogram(binwidth = 5, alpha = .75, color = cust_purple, fill = cust_purple) +
  labs(x = "FOS Scores", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "fos_dist.png"), plot = last_plot(), width = 5, height = 5)

# Save FOS scores across linear age
ggplot(fos, aes(x = ExactAge, y = fos_total)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(15, 60), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(30, 45, by = 15), colour = 'grey90') +
  scale_y_continuous(breaks = seq(15, 60, by = 15)) +  
  geom_point(alpha = 1, size = 2.75, color = cust_purple) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5, color = cust_purple, fill = cust_purple) +
  labs(x = "Age (Years)", y = "Prospection\n(FOS Scores)") +
  agedd_theme
ggsave(paste0(out_path, "fig4c.png"), width = 5, height = 5)

# Save FOS scores across smooth age
ggplot(fos, aes(x = ExactAge, y = fos_total)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(15, 60), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(30, 45, by = 15), colour = 'grey90') +
  scale_y_continuous(breaks = seq(15, 60, by = 15)) +  
  geom_point(alpha = 1, size = 2.75, color = cust_purple) +
  geom_smooth(method = "loess", size = 1.5, alpha = .5, color = cust_purple, fill = cust_purple) +
  labs(x = "Age (Years)", y = "Prospection\n(FOS Scores)") +
  agedd_theme
ggsave(paste0(out_path, "fos_age_smth.png"), width = 5, height = 5)

### Visualize Working Memory Data -----

# Quick peek at n-back performance data distribution
hist(nback$dprime_2back)

# Save n-back performance distribution
ggplot(data = nback, aes(x = dprime_2back)) +
  scale_x_continuous(breaks = seq(-7, 3, by = 2)) +
  geom_hline(yintercept = seq(0, 40, by = 8), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 40, by = 8)) + 
  geom_histogram(binwidth = 1, alpha = .75, color = dark_green, fill = dark_green) +
  labs(x = "N-Back Performance", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "nback_dist.png"), plot = last_plot(), width = 5, height = 5)

# Save n-back performance across linear age
ggplot(nback, aes(x = ExactAge, y = dprime_2back)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(-6, -4, -2, 2), color = 'grey90') +
  geom_hline(yintercept = c(0), linetype = 'dashed') +
  scale_y_continuous(breaks = round(seq(-6, 2, by = 2), 2)) +
  geom_point(alpha = 1, size = 2.75, color = dark_green) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5, color = dark_green, fill = dark_green) +
  labs(x = "Age (Years)", y = "Working Memory\n(N-Back Performance)") +
  agedd_theme
ggsave(paste0(out_path, "fig4d.png"), width = 5, height = 5)

# Save n-back performance across smooth age
ggplot(nback, aes(x = ExactAge, y = dprime_2back)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(-6, -4, -2, 2), color = 'grey90') +
  geom_hline(yintercept = c(0), linetype = 'dashed') +
  scale_y_continuous(breaks = round(seq(-6, 2, by = 2), 2)) +
  geom_point(alpha = 1, size = 2.75, color = dark_green) +
  geom_smooth(method = "loess", size = 1.5, alpha = .5, color = dark_green, fill = dark_green) +
  labs(x = "Age (Years)", y = "Working Memory\n(N-Back Performance)") +
  agedd_theme
ggsave(paste0(out_path, "nback_age_smth.png"), width = 5, height = 5)

### Visualize Reward Motivation Data -----

# Quick peek at BAS score data distribution
hist(bas$bas_total)

# Save BAS score distribution
ggplot(data = bas, aes(x = bas_total)) +
  scale_x_continuous(breaks = seq(13, 52, by = 13)) +
  geom_hline(yintercept = seq(0, 80, by = 16), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 80, by = 16)) + 
  geom_histogram(binwidth = 6.5, alpha = .75, color = light_green, fill = light_green) +
  labs(x = "BAS Scores", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "bas_dist.png"), plot = last_plot(), width = 5, height = 5)

# Save BAS scores across linear age
ggplot(bas, aes(x = ExactAge, y = bas_total)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(13, 52), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(26, 39, by = 13), colour = 'grey90') +
  scale_y_continuous(breaks = seq(13, 52, by = 13)) +  
  geom_point(alpha = 1, size = 2.75, color = light_green) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5, color = light_green, fill = light_green) +
  labs(x = "Age (Years)", y = "Reward Motivation\n(BAS Scores)") +
  agedd_theme
ggsave(paste0(out_path, "fig4e.png"), width = 5, height = 5)

# Save BAS scores across smooth age
ggplot(bas, aes(x = ExactAge, y = bas_total)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(13, 52), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(26, 39, by = 13), colour = 'grey90') +
  scale_y_continuous(breaks = seq(13, 52, by = 13)) +  
  geom_point(alpha = 1, size = 2.75, color = light_green) +
  geom_smooth(method = "loess", size = 1.5, alpha = .5, color = light_green, fill = light_green) +
  labs(x = "Age (Years)", y = "Reward Motivation\n(BAS Scores)") +
  agedd_theme
ggsave(paste0(out_path, "bas_age_smth.png"), width = 5, height = 5)

### Visualize Time Perception Data -----

# Quick peek at range value data distribution
hist(range$Range)

# Save range value distribution
ggplot(data = range, aes(x = Range)) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  geom_hline(yintercept = seq(0, 40, by = 10), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 40, by = 10)) + 
  geom_histogram(binwidth = 10, alpha = .75, color = cust_teal, fill = cust_teal) +
  labs(x = "Range Values", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "range_dist.png"), plot = last_plot(), width = 5, height = 5)

# Save range values across linear age
ggplot(range, aes(x = ExactAge, y = Range)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(0, 100), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(25, 75, by = 25), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) +  
  geom_point(alpha = 1, size = 2.75, color = cust_teal) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5, color = cust_teal, fill = cust_teal) +
  labs(x = "Age (Years)", y = "Time Perception\n(Range Values)") +
  agedd_theme
ggsave(paste0(out_path, "fig4a.png"), width = 5, height = 5)

# Save range values across smooth age
ggplot(range, aes(x = ExactAge, y = Range)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(0, 100), colour = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(25, 75, by = 25), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) +  
  geom_point(alpha = 1, size = 2.75, color = cust_teal) +
  geom_smooth(method = "loess", size = 1.5, alpha = .5, color = cust_teal, fill = cust_teal) +
  labs(x = "Age (Years)", y = "Time Perception\n(Range Values)") +
  agedd_theme
ggsave(paste0(out_path, "range_age_smth.png"), width = 5, height = 5)

### Combine Data -----

# NOTE: there are 159 usable participants for DD.
#       there are 159 usable participants for stroop, including 56 and 4.
#       --> 157 overlapping participants after 1st merge.
#       there are 145 usable participants for nback, including 56 and 4.
#       --> 143 overlapping participants after 3rd merge.
#       no additional data loss from questionnaires.

# Merge DD and all mechanism data together
all_data <- merge(dd_auc, stroop[, c("StudyID", "perf")], by = "StudyID")
all_data <- merge(all_data, fos[, c("StudyID", "fos_total")], by = "StudyID")
all_data <- merge(all_data, nback[, c("StudyID", "dprime_2back")], by = "StudyID")
all_data <- merge(all_data, bas[, c("StudyID", "bas_total")], by = "StudyID")
all_data <- merge(all_data, range[, c("StudyID", "Range")], by = "StudyID")
all_data <- rename(all_data, "stroop" = "perf")
all_data <- rename(all_data, "nback" = "dprime_2back")
all_data <- rename(all_data, "range" = "Range")

# Join DD and all mechanism data together
full_data <- full_join(dd_auc, stroop[, c("StudyID", "perf")], by = "StudyID")
full_data <- full_join(full_data, fos[, c("StudyID", "fos_total")], by = "StudyID")
full_data <- full_join(full_data, nback[, c("StudyID", "dprime_2back")], by = "StudyID")
full_data <- full_join(full_data, bas[, c("StudyID", "bas_total")], by = "StudyID")
full_data <- full_join(full_data, range[, c("StudyID", "Range")], by = "StudyID")
full_data <- rename(full_data, "stroop" = "perf")
full_data <- rename(full_data, "nback" = "dprime_2back")
full_data <- rename(full_data, "range" = "Range")

### Assess Missingness in Data -----

# We do not want to see any age-related trends in missingness since forthcoming methods
# (e.g., DAGs) require complete case analyses.

# Remove participants for failed catch trials
full_data <- full_data[!(full_data$StudyID %in% c(56, 4)), ]

# Compare sample with and without missing values
dim(full_data) # 159 observations
full_data[complete.cases(full_data), ] # Subsample, 0 missing values
dim(full_data[complete.cases(full_data), ]) # 143 observations
full_data[!complete.cases(full_data), ] # Subsample, 1+ missing values
dim(full_data[!complete.cases(full_data), ]) # 16 observations

# Visualize missingness, method 1
md.pattern(full_data, rotate.names = TRUE)
# 3 rows --> 3 missing data patterns (observed = blue, missing = red)
# See last column and rows for missing counts
# Take-away: ~10% of participants are only missing n-back

# Visualize missingness, method 2
aggr(full_data, prop = TRUE, numbers = TRUE, cex.numbers = 0.8)  
# Take-away: n-back has a substantial proportion of missing values

# Visualize missingness, method 3a
matrixplot(full_data, interactive = FALSE, sortby = "ExactAge")
# Red = missing, gray scale = observed (darker colors reflect higher values within each variable)
# Take-away: missing values for stroop, FOS, BAS, and range are evenly dispersed across age;
# missing values for n-back are slightly concentrated in children, but not egregiously so

# Visualize missingness, method 3b
marginplot(full_data[, c("ExactAge", "stroop")])
marginplot(full_data[, c("ExactAge", "fos_total")])
marginplot(full_data[, c("ExactAge", "nback")])
marginplot(full_data[, c("ExactAge", "bas_total")])
marginplot(full_data[, c("ExactAge", "range")])
# Take-away: digging into each cognitive mechanism, the blue (complete case) and red
# (incomplete case) boxplot distributions are largely overlapping for every measure
# --> suggests values might be missing completely at random (MCAR)

# Visualize missingness, method 4
vis_dat(full_data) # Take-away: all variables of interest are numeric --> missingness cannot be related to variable type
vis_miss(full_data) # Take-away: missingness percentages are very low, overall
gg_miss_var(full_data, show_pct = TRUE) # Take-away: missingness percentage is highest for n-back

# Since missingness is limited in general, and not related to age, we can conduct analyses
# with complete cases where required (i.e., will not impute data, etc.).

### Model Mechanism Data ----

# Model stroop across age -- fit, evaluate, and interpret model
hist(stroop$perf)
stroop_lm <- lm(perf ~ ExactAge, data = stroop)
set.seed(123)
check_predictions(stroop_lm) # Poor
qqnorm(residuals(stroop_lm), pch = 1, frame = FALSE)
qqline(residuals(stroop_lm), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions
summary(stroop_lm)
plot_model(stroop_lm, type = "pred", terms = c("ExactAge")) # stroop performance increases with age

# Model FOS across age -- fit, evaluate, and interpret model
hist(fos$fos_total)
fos_lm <- lm(fos_total ~ ExactAge, data = fos)
set.seed(123)
check_predictions(fos_lm) # Good enough
qqnorm(residuals(fos_lm), pch = 1, frame = FALSE)
qqline(residuals(fos_lm), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions
summary(fos_lm)
plot_model(fos_lm, type = "pred", terms = c("ExactAge")) # FOS scores increase with age

# Model n-back across age -- fit, evaluate, and interpret model
hist(nback$dprime_2back)
nback_lm <- lm(dprime_2back ~ ExactAge, data = nback)
set.seed(123)
check_predictions(nback_lm) # Great!
qqnorm(residuals(nback_lm), pch = 1, frame = FALSE)
qqline(residuals(nback_lm), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions
summary(nback_lm)
plot_model(nback_lm, type = "pred", terms = c("ExactAge")) # n-back performance increases with age

# Model BAS across age -- fit, evaluate, and interpret model
hist(bas$bas_total)
bas_lm <- lm(bas_total ~ ExactAge, data = bas)
set.seed(123)
check_predictions(bas_lm) # Great!
qqnorm(residuals(bas_lm), pch = 1, frame = FALSE)
qqline(residuals(bas_lm), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions
summary(bas_lm)
plot_model(bas_lm, type = "pred", terms = c("ExactAge")) # BAS scores increase with age

# Model range across age -- fit, evaluate, and interpret model
hist(range$Range)
range_lm <- lm(Range ~ ExactAge, data = range)
set.seed(123)
check_predictions(range_lm) # Good enough
qqnorm(residuals(range_lm), pch = 1, frame = FALSE)
qqline(residuals(range_lm), col = "red", lwd = 2) # Poor
# Gaussian distribution fulfills model assumptions
summary(range_lm)
plot_model(range_lm, type = "pred", terms = c("ExactAge")) # range values decrease with age

# Save mechanism model outputs
mech_model <- paste0(out_path, 'mech_lm.txt')
sink(mech_model)
cat("STROOP\n\n")
summary(stroop_lm)
cat("\n\nFOS\n\n")
summary(fos_lm)
cat("\n\nN-BACK\n\n")
summary(nback_lm)
cat("\n\nBAS\n\n")
summary(bas_lm)
cat("\n\nRANGE\n\n")
summary(range_lm)
sink()

### Hypothesis-Driven DAG -----

# Create DAG-specific data frame
names(all_data)
dag_data <- all_data[, c(-1)]
names(dag_data) <- c("Age (Yrs)", "$20 AUC", "$200 AUC", 
                     "Stroop", "FOS", "N-Back", "BAS", "Time")
head(dag_data)
dim(dag_data) # Still should have 143 observations
sum(complete.cases(dag_data) == FALSE) # Double-check no missing values (not allowed with bn.fit)

# Ensure all variables are numeric
str(dag_data)
dag_data$FOS <- as.numeric(dag_data$FOS)
dag_data$BAS <- as.numeric(dag_data$BAS)
dag_data$Time <- as.numeric(dag_data$Time)
str(dag_data)

# Structure DAG
dag <- empty.graph(nodes = c("Age (Yrs)", "$20 AUC", "$200 AUC", 
                             "Stroop", "FOS", "N-Back", "BAS", "Time"))
arcmat <- matrix(c("Stroop", "$20 AUC", "FOS", "$20 AUC", "N-Back", "$20 AUC", "BAS", "$20 AUC", "Time", "$20 AUC",
                   "Stroop", "$200 AUC", "FOS", "$200 AUC", "N-Back", "$200 AUC", "BAS", "$200 AUC", "Time", "$200 AUC",
                   "Age (Yrs)", "Stroop", "Age (Yrs)", "FOS", "Age (Yrs)", "N-Back", "Age (Yrs)", "BAS", "Age (Yrs)", "Time",
                   "Age (Yrs)", "$20 AUC", "Age (Yrs)", "$200 AUC"), byrow = TRUE, 
                   ncol = 2, dimnames = list(NULL, c("from", "to")))
arcmat # Edge list
arcs(dag) <- arcmat # Add arcs
dag
modelstring(dag) # Graph string notation
graphviz.plot(dag) # No cycles! --> we successfully structured our DAG

# Null hypothesis is that nodes are conditionally independent
arc.strength(dag, data = dag_data) # "Strength" values are p-values

# There are many non-significant edges; let's delete them:
dag2 <- drop.arc(dag, "Stroop", "$20 AUC")
dag2 <- drop.arc(dag2, "N-Back", "$20 AUC")
dag2 <- drop.arc(dag2, "BAS", "$20 AUC")
dag2 <- drop.arc(dag2, "Stroop", "$200 AUC")
dag2 <- drop.arc(dag2, "FOS", "$200 AUC")
dag2 <- drop.arc(dag2, "BAS", "$200 AUC")
graphviz.plot(dag2)
arc.strength(dag2, data = dag_data) # Confirming that there are no more non-significant edges
arc.strength(dag2, data = dag_data, criterion = "cor") # Confirming that "cor" is the criterion, so the strength values = p-values

# Essentially, we've produced a multiple mediation model.

# Compare original DAG with pared-down DAG (BIC is default metric)
bnlearn::score(dag, data = dag_data) # -2141.023
bnlearn::score(dag2, data = dag_data) # -2128.28
# dag2 BIC is closer to 0 --> dag2 is the better fitting DAG

# Note that there is not a way to make "prettier" version of a DAG in R (cannot change
# individual node colors).

# Save key DAG outputs
sink(paste0(out_path, 'dag.txt'))
cat("DAG #1 with non-significant edges\n")
cat("---------------------------------\n")
arc.strength(dag, data = dag_data)
cat("\n\n")
cat("DAG #2 with significant edges only\n")
cat("----------------------------------\n")
arc.strength(dag2, data = dag_data)
cat("\n\n")
cat("DAG #1 BIC\n")
cat("----------\n")
bnlearn::score(dag, data = dag_data)
cat("\n\n")
cat("DAG #2 BIC\n")
cat("----------\n")
bnlearn::score(dag2, data = dag_data)
sink()
png(paste0(out_path, 'dag.png'), width = 450, height = 450)
graphviz.plot(dag)
dev.off()
png(paste0(out_path, 'dag2.png'), width = 450, height = 450)
graphviz.plot(dag2)
dev.off()

# Estimate fit parameters for the local conditional distributions (because the variables are continuous)
#   Akin to linear regressions of a child node on its parents ("local multiple linear regression")
#   Betas are analogous to edge weights
dag2_fit <- bn.fit(dag2, dag_data)
dag2_fit # Show coefficients for all nodes
dag2_fit$`$20 AUC` # Show coefficients for individual node
dag2_fit$`$200 AUC` # Show coefficients for individual node
sink(paste0(out_path, 'dag_betas.txt'))
dag2_fit
sink()
