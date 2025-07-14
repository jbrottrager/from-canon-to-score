################### Canonisation Score: Logistic Regression #################### 
#------------------------------------------------------------------------------#

# --- Last edited: 2024-11-29 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggpubr)
library(broom)
library(ggridges)
library(ggExtra)
library(gridExtra)
library(ResourceSelection)
library(pscl)
library(dplyr)
library(factoextra)
library(reshape2)
library(e1071)  
library(ggthemes)

## German ---------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Data import ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\GER")

scores_ger <- read.csv("GER_corpus_meta.csv",
                       sep = ";", encoding = "UTF-8")
scores_ger$ID <- paste0(scores_ger$wikiname, "_", scores_ger$wikiID)

IDinfo <- subset(scores_ger, select = c(wikiname, ID))

syllabi_meta_ger <- read.csv("GER_corpus_syllabi.csv", 
                             sep = ";", encoding = "UTF-8")
syllabi_meta_ger <- merge(syllabi_meta_ger, IDinfo, by = "wikiname", 
                          all.x = TRUE)

syllabi_meta_ger$rel_freq <- syllabi_meta_ger$freq_y/10

# PCA

scores_ger_df <- subset(scores_ger, select = c(ID, freq, student_edition, works))
res.pca <- prcomp(scores_ger_df[,-1], scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#------------------------------------------------------------------------------#
## Regression: Base model  ----------------------------------------------------#
#------------------------------------------------------------------------------#

scores_ger_df$works <- factor(scores_ger_df$works, levels = c(0,1))
scores_ger_df$student_edition <- factor(scores_ger_df$student_edition, 
                                        levels = c(0,1))

max <- merge(syllabi_meta_ger, scores_ger_df, by = "ID")
max <- max[which(max$syllabi == 1),]
max$canonisation <- 0

min <- scores_ger_df[which(scores_ger_df$freq == 1),]
min$rel_freq <- 0
min$canonisation <- 0.05

threshold <- c(8,7,6,5,4,3,2,1)

predicted_canonisation <- matrix(nrow = nrow(scores_ger_df), 
                                 ncol = length(threshold))

aic <- c()
null_deviance <- c()
residual_deviance <- c()
plot_dfs <- list()
coeffs <- list()
models <- list()
label <- list()
cvs <- list()
skew <- list()

 
for (i in 1:length(threshold)) {
  message(i, "\n")
  classified_data <- max
  classified_data$canonisation[classified_data$freq_y >= threshold[i]] <- 1
  existing_entries <- classified_data$wikiname
  new_entries <- min$wikiname
  overlap <- intersect(existing_entries, new_entries)
  minimum <- min[!min$id %in% overlap,]
  classified_data <- rbind(classified_data, minimum)
  model <- glm(canonisation ~ works + student_edition + freq,
               data = classified_data, family = binomial(link = "logit"))
  null_deviance[i] <- model$null.deviance
  residual_deviance[i] <- model$deviance
  aic[i] <- model$aic
  coeffs[[i]] <-  summary(model)[12]
  predicted_canonisation[,i] <- predict(model, scores_ger_df, type="response")
  plot_dfs[[i]] <- augment(model, type.predict = "response")
  models[[i]] <- model
  label[[i]] <- threshold[i]
  cvs[i] <- sd(predicted_canonisation[,i]) / mean(predicted_canonisation[,i]) 
  skew[i] <- skewness(predicted_canonisation[,i]) 
}

predicted_canonisation_GER <- as.data.frame(predicted_canonisation)
predicted_canonisation_GER$id <- scores_ger$ID

names(predicted_canonisation_GER) <- c("model_8", "model_7", "model_6", 
                                   "model_5", "model_4", "model_3", 
                                   "model_2", "model_1", "ID")
x_melt <- melt(predicted_canonisation_GER, id=c("ID"))

x_melt$variable <- factor(x_melt$variable, levels = c("model_1", "model_2", 
                                                      "model_3", "model_4",
                                                      "model_5", "model_6", 
                                                      "model_7", "model_8"))

ggplot(x_melt, aes(x = value, group = variable)) +
  geom_histogram(alpha = 0.8, position = "identity", binwidth = 0.05) +
  xlab("canonisation score") + 
  ylab("num. of titles") +
  facet_wrap(vars(variable), nrow = 4) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


sub_ger <- subset(x_melt, variable == "model_3")
sub_ger$lang <- "GER"

eval_ger <- cbind(as.numeric(residual_deviance), as.numeric(cvs), as.numeric(skew))
eval_ger <- as.data.frame(eval_ger)
names(eval_ger) <- c("res_dev", "cvs", "skew")

eval_ger$multi_ger <- eval_ger$cvs * eval_ger$skew
eval_ger$name <- c("model_8", "model_7", "model_6", 
                   "model_5", "model_4", "model_3", 
                   "model_2", "model_1")

ggplot(eval_ger, aes(x = cvs, y = skew, label = name)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  expand_limits(x = c(0.3, 0.6), y = c(-2, 0.5)) +
  theme(line = element_blank(),  
        rect = element_blank(), 
        text = element_text(color = "black"),  
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),  
        axis.ticks = element_blank(),  
        axis.line = element_blank(), 
        panel.background = element_blank(),  
        panel.border = element_blank(),  
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        plot.background = element_blank(),  
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))

write.csv(predicted_canonisation_GER, "GER_canonisationScore.csv",
          fileEncoding= "UTF-8")

canonisation_scores_GER <- subset(predicted_canonisation_GER, select = c("model_3", "ID"))
names(canonisation_scores_GER) <- c("canonisation_score", "ID")
write.csv(canonisation_scores_GER, "GER_canonisationScore.csv",
          fileEncoding = "UTF-8")

predicted_canonisation_authors_GER <- merge(canonisation_scores_GER,
                                            scores_ger, by = "ID")

predicted_canonisation_authors_GER$author_complete <- toupper(predicted_canonisation_authors_GER$author_complete)
predicted_canonisation_authors_GER$author_complete <- gsub(" ", "_", 
                                                           predicted_canonisation_authors_GER$author_complete)

predicted_canonisation_authors_GER <- predicted_canonisation_authors_GER %>%
  group_by(author_complete) %>%
  summarise(median = median(canonisation_score), .groups = "drop")

write.csv(predicted_canonisation_authors_GER, 
          "GER_canonisationScoreAuthors.csv",
          fileEncoding= "UTF-8")


## English --------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Data import ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\ENG")

scores_eng <- read.csv("ENG_corpus_meta.csv",
                       sep = ";", encoding = "UTF-8")

scores_eng$ID <- paste0(scores_eng$wikiname, "_", scores_eng$wikiID)

IDinfo <- subset(scores_eng, select = c(wikiname, ID))

syllabi_meta_eng <- read.csv("ENG_corpus_syllabi.csv", 
                             sep = ";", encoding = "UTF-8")
syllabi_meta_eng <- merge(syllabi_meta_eng, IDinfo, by = "wikiname", 
                          all.x = TRUE)

syllabi_meta_eng$rel_freq <- syllabi_meta_eng$freq_y/8

# PCA

scores_eng_df <- subset(scores_eng, select = c(ID, freq, student_edition, works))
res.pca <- prcomp(scores_eng_df[,-1], scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#------------------------------------------------------------------------------#
## Regression: Base model  ----------------------------------------------------#
#------------------------------------------------------------------------------#

scores_eng_df$works <- factor(scores_eng_df$works, levels = c(0,1))
scores_eng_df$student_edition <- factor(scores_eng_df$student_edition, 
                                        levels = c(0,1))

max <- merge(syllabi_meta_eng, scores_eng_df, by = "ID")
max <- max[which(max$syllabi == 1),]
max$canonisation <- 0

min <- scores_eng_df[which(scores_eng_df$freq == 1),]
min$rel_freq <- 0
min$canonisation <- 0.05

threshold <- c(8,7,6,5,4,3,2,1)

predicted_canonisation <- matrix(nrow = nrow(scores_eng_df), 
                                 ncol = length(threshold))

aic <- c()
null_deviance <- c()
residual_deviance <- c()
plot_dfs <- list()
coeffs <- list()
models <- list()
label <- list()
cvs <- list()
skew <- list()


for (i in 1:length(threshold)) {
  message(i, "\n")
  classified_data <- max
  classified_data$canonisation[classified_data$freq_y >= threshold[i]] <- 1
  existing_entries <- classified_data$ID
  new_entries <- min$ID
  overlap <- intersect(existing_entries, new_entries)
  minimum <- min[!min$id %in% overlap,]
  classified_data <- rbind(classified_data, minimum)
  model <- glm(canonisation ~ works + student_edition + freq,
               data = classified_data, family = binomial(link = "logit"))
  null_deviance[i] <- model$null.deviance
  residual_deviance[i] <- model$deviance
  aic[i] <- model$aic
  coeffs[[i]] <-  summary(model)[12]
  predicted_canonisation[,i] <- predict(model, scores_eng_df, type="response")
  plot_dfs[[i]] <- augment(model, type.predict = "response")
  models[[i]] <- model
  label[[i]] <- threshold[i]
  cvs[i] <- sd(predicted_canonisation[,i]) / mean(predicted_canonisation[,i]) 
  skew[i] <- skewness(predicted_canonisation[,i]) 
}

predicted_canonisation_ENG <- as.data.frame(predicted_canonisation)
predicted_canonisation_ENG$id <- scores_eng$ID

names(predicted_canonisation_ENG) <- c("model_8", "model_7", "model_6", 
                                   "model_5", "model_4", "model_3", 
                                   "model_2", "model_1", "ID")
x_melt <- melt(predicted_canonisation_ENG, id=c("ID"))

x_melt$variable <- factor(x_melt$variable, levels = c("model_1", "model_2", 
                                                      "model_3", "model_4",
                                                      "model_5", "model_6", 
                                                      "model_7", "model_8"))

ggplot(x_melt, aes(x = value, group = variable)) +
  geom_histogram(alpha = 0.8, position = "identity", binwidth = 0.05) +
  xlab("canonisation score") + 
  ylab("num. of titles") +
  facet_wrap(vars(variable), nrow = 4) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


sub_eng <- subset(x_melt, variable == "model_3")
sub_eng$lang <- "ENG"

eval_eng <- cbind(as.numeric(residual_deviance), as.numeric(cvs), as.numeric(skew))
eval_eng <- as.data.frame(eval_eng)
names(eval_eng) <- c("res_dev", "cvs", "skew")

eval_eng$multi_ger <- eval_eng$cvs * eval_eng$skew
eval_eng$name <- c("model_8", "model_7", "model_6", 
                   "model_5", "model_4", "model_3", 
                   "model_2", "model_1")

ggplot(eval_eng, aes(x = cvs, y = skew, label = name)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  expand_limits(x = c(0.3, 0.6), y = c(-2, 0.5)) +
  theme(line = element_blank(),  
        rect = element_blank(), 
        text = element_text(color = "black"),  
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),  
        axis.ticks = element_blank(),  
        axis.line = element_blank(), 
        panel.background = element_blank(),  
        panel.border = element_blank(),  
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        plot.background = element_blank(),  
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))


write.csv(predicted_canonisation_ENG, "ENG_canonisationScore.csv",
          fileEncoding = "UTF-8")

predicted_canonisation_ENG <- subset(predicted_canonisation_ENG, select = c("model_3", "ID"))
names(predicted_canonisation_ENG) <- c("canonisation_score", "ID")
write.csv(predicted_canonisation_ENG, "ENG_canonisationScore.csv",
          fileEncoding = "UTF-8")

predicted_canonisation_authors_ENG <- merge(predicted_canonisation_ENG,
                                            scores_eng, by = "ID")

predicted_canonisation_authors_ENG$author_complete <- toupper(predicted_canonisation_authors_ENG$author_complete)
predicted_canonisation_authors_ENG$author_complete <- gsub(" ", "_", 
                                                           predicted_canonisation_authors_ENG$author_complete)

predicted_canonisation_authors_ENG <- predicted_canonisation_authors_ENG %>%
  group_by(author_complete) %>%
  summarise(median = median(canonisation_score), .groups = "drop")

write.csv(predicted_canonisation_authors_ENG, 
          "ENG_canonisationScoreAuthors.csv",
          fileEncoding= "UTF-8")


#------------------------------------------------------------------------------#
## Regression: Plotting  ------------------------------------------------------#
#------------------------------------------------------------------------------#

sub <- rbind(sub_eng, sub_ger)

ggplot(sub, aes(x = value, group = lang)) +
  geom_histogram(alpha = 0.8, binwidth = 0.05) +
  xlab("Canonisation score") + 
  ylab("Num. of titles") +
  facet_wrap(vars(lang), nrow = 1) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\canonisationScoreDistribution.png",
       dpi = 360, width = 16, height = 16, units = "cm")


summary(sub_eng$value)
low <- sub_eng$value[sub_eng$value <= 0.1]
range(low)
length(low)
length(unique(low))

high <- sub_eng$value[sub_eng$value >= 0.75]
range(high)
length(high)
length(unique(high))

summary(sub_ger$value)
low <- sub_ger$value[sub_ger$value <= 0.1]
range(low)
length(low)
length(unique(low))

high <- sub_ger$value[sub_ger$value >= 0.75]
range(high)
length(high)
length(unique(high))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
