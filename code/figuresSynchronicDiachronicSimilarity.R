############### Embedding Scores: Correlation and Visualisation ################ 
#------------------------------------------------------------------------------#

# --- Last edited: 2025-01-06 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggthemes)
library(reshape2)

#------------------------------------------------------------------------------#
## Synchronic similarities ----------------------------------------------------#
#------------------------------------------------------------------------------#

simCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\df_chunk_to_centroid_similarities.csv",
                         sep = ",", row.names = 1)
simCentroids_ENG$normalised_chunks <- c(scale(simCentroids_ENG$num_chunks, 
                                            center = FALSE))
simCentroids_ENG$lang <- "ENG"

simCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\df_chunk_to_centroid_similarities.csv",
                             sep = ",", row.names = 1)
simCentroids_GER$normalised_chunks <- c(scale(simCentroids_GER$num_chunks, 
                                              center = FALSE))
simCentroids_GER$lang <- "GER"


data <- rbind(simCentroids_ENG, simCentroids_GER)

ggplot(data, aes(x = year, y = mean)) +
  geom_errorbar(aes(ymin = Q1, ymax = Q4), width = 0.2, colour = "gray65") +
  geom_smooth(colour = "black") +
  geom_point(aes(size = normalised_chunks), colour = "black", 
             show.legend = FALSE, alpha = 0.75) +
  facet_wrap(vars(lang), nrow = 2) +
  labs(
    x = "Rolling yearly centroids",
    y = "Mean cosine similarity between chunks and yearly centroids"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroids_synchronicSimilarity.png", 
       dpi = 360, width = 16, height = 16, units = "cm")

#------------------------------------------------------------------------------#
## Diachronic similarities ----------------------------------------------------#
#------------------------------------------------------------------------------#

simCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\df_centroid_to_centroid_similarities.csv",
                             sep = ",", row.names = 1)
simCentroids_ENG$normalised_chunks <- c(scale(simCentroids_ENG$num_chunks, 
                                              center = FALSE))
simCentroids_ENG$lang <- "ENG"
dim(simCentroids_ENG[simCentroids_ENG$cosine_similarity >= 0.8,])

simCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\df_centroid_to_centroid_similarities.csv",
                             sep = ",", row.names = 1)
simCentroids_GER$normalised_chunks <- c(scale(simCentroids_GER$num_chunks, 
                                              center = FALSE))
simCentroids_GER$lang <- "GER"
dim(simCentroids_GER[simCentroids_GER$cosine_similarity >= 0.8,])

data <- rbind(simCentroids_ENG, simCentroids_GER)

ggplot(data, aes(x = year, y = cosine_similarity)) +
  geom_point(aes(size = normalised_chunks), colour = "black", 
             show.legend = FALSE, alpha = 0.5) +
  facet_wrap(vars(lang), nrow = 2) +
  scale_y_continuous(trans='logit') +
  labs(
    x = "Rolling yearly centroids",
    y = "Mean cosine similarity between chunks and yearly centroids"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroids_diachronicSimilarity.png", 
       dpi = 360, width = 16, height = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#