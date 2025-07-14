####################### Figures: 5.1. Rolling Centroids ######################## 
#------------------------------------------------------------------------------#

# --- Last edited: 2024-12-02 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggpubr)
library(broom)
library(ggridges)
library(ggExtra)
library(gridExtra)
library(dplyr)
library(ggthemes)
library(magrittr)
library(stringr)
library(cowplot)
library(lme4)

#------------------------------------------------------------------------------#
## Overall mean ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

sim_toCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_mean_GER.csv",
                                row.names = 1)
sim_toCentroids_GER$lang <- "GER"

sim_toCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_mean_ENG.csv",
                                row.names = 1)
sim_toCentroids_ENG$lang <- "ENG"

sim_toCentroids <- rbind(sim_toCentroids_GER, sim_toCentroids_ENG)

ggplot(sim_toCentroids, aes(x = Year, y = mean)) +
  geom_line(linewidth = 1) +  
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Average cosine similarity") +
  scale_y_continuous(limits = c(0.6, 0.875)) +       # Adjust y-axis limits
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  # Add vertical line at x = 0
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_mean.png", 
       dpi = 360, width = 24, height = 16, units = "cm")


#------------------------------------------------------------------------------#
## Examples -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

sim_toCentroids_pertext_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\sim_for_each_text_GER.csv",
                                        row.names = 1)
sim_toCentroids_pertext_GER <- subset(sim_toCentroids_pertext_GER, 
                                      Text %in% c("DIE_LEIDEN_DES_JUNGEN_WERTHERS_Q151883", 
                                                  "ELISA_ODER_DAS_WEIB_WIE_ES_SEYN_SOLLTE_noWikiID")) 

sim_toCentroids_pertext_GER$lang <- "GER"


sim_toCentroids_pertext_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\sim_for_each_text_ENG.csv",
                                        row.names = 1)
sim_toCentroids_pertext_ENG <- subset(sim_toCentroids_pertext_ENG, 
                                      Text %in% c("WAVERLEY_Q678251", 
                                                  "MARRIAGE_noWikiID")) 

sim_toCentroids_pertext_ENG$lang <- "ENG"

sim_toCentroids <- subset(sim_toCentroids, select = -c(std))
names(sim_toCentroids) <- c("Index", "Similarity", "lang")
sim_toCentroids$Text <- "Corpus average"

sim_toCentroids <- rbind(sim_toCentroids, sim_toCentroids_pertext_GER,
                         sim_toCentroids_pertext_ENG)
sim_toCentroids$Text <- factor(sim_toCentroids$Text, levels = unique(sim_toCentroids$Text))

examples <- ggplot(sim_toCentroids, aes(x = Index, y = Similarity, 
                            linetype = Text, colour = Text, group = Text)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 1) + 
  scale_linetype_manual(values = c(
    "Corpus average" = "solid",
    "WAVERLEY_Q678251" = "twodash", 
    "MARRIAGE_noWikiID" = "dotted", 
    "DIE_LEIDEN_DES_JUNGEN_WERTHERS_Q151883" = "twodash",
    "ELISA_ODER_DAS_WEIB_WIE_ES_SEYN_SOLLTE_noWikiID" = "dotted"
  ),
  labels = c("Corpus average", expression(italic("Waverley (1814)")),
             expression(italic("Marriage (1818)")), expression(italic("Werther (1774)")),
             expression(italic("Elisa (1795)"))),
  guide = "legend") +   
  scale_colour_manual(values = c(
    "Corpus average" = "grey90",
    "WAVERLEY_Q678251" = "grey1", 
    "MARRIAGE_noWikiID" = "grey50", 
    "DIE_LEIDEN_DES_JUNGEN_WERTHERS_Q151883" = "grey1",
    "ELISA_ODER_DAS_WEIB_WIE_ES_SEYN_SOLLTE_noWikiID" = "grey50"
  ),
  labels = c("Corpus average", expression(italic("Waverley (1814)")),
             expression(italic("Marriage (1818)")), expression(italic("Werther (1774)")),
             expression(italic("Elisa (1795)"))),
  guide = "legend") +   
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Cosine similarity",
    colour = "Text", linetype = "Text") +  
  scale_y_continuous(limits = c(0.5, 0.95)) +       
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'),
        legend.title = element_blank())

#------------------------------------------------------------------------------#
## Cannonisation --------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Binary --------------------------------------------------------------------#

sim_toCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_canonisation_sim_groupeddf_binary_GER.csv",
                                row.names = 1)

sim_toCentroids_GER$Score.Range <- factor(sim_toCentroids_GER$Score.Range, 
                                          levels = c('Low (0-0.5)', 'High (0.5-1.0)'))
sim_toCentroids_GER$lang <- "GER"

sim_toCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_canonisation_sim_groupeddf_binary_ENG.csv",
                                row.names = 1)

sim_toCentroids_ENG$Score.Range <- factor(sim_toCentroids_ENG$Score.Range, 
                                          levels = c('Low (0-0.5)', 'High (0.5-1.0)'))
sim_toCentroids_ENG$lang <- "ENG"

sim_toCentroids <- rbind(sim_toCentroids_GER, sim_toCentroids_ENG)

plot_binary <- ggplot(sim_toCentroids, aes(x = Year, y = mean, linetype = Score.Range,
                            colour = Score.Range)) +
  geom_line(linewidth = 1) +  # Black lines with varying linetypes
  scale_linetype_manual(values = c("twodash", "solid")) + 
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Average cosine similarity",
    colour = "Canonisation", linetype = "Canonisation") +
  scale_colour_grey(start = 0.8, end = 0.2) +  # Reverse grayscale
  scale_y_continuous(limits = c(0.6, 0.875)) +       # Adjust y-axis limits
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  # Add vertical line at x = 0
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

### Low-Mid-High --------------------------------------------------------------#

sim_toCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_canonisation_sim_groupeddf_three_GER.csv",
                                row.names = 1)

sim_toCentroids_GER$Score.Range <- factor(sim_toCentroids_GER$Score.Range, 
                                          levels = c('Low (0-0.25)', 'Mid (0.25-0.75)', 'High (0.75-1.0)'))
sim_toCentroids_GER$lang <- "GER"

sim_toCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_canonisation_sim_groupeddf_three_ENG.csv",
                                row.names = 1)

sim_toCentroids_ENG$Score.Range <- factor(sim_toCentroids_ENG$Score.Range, 
                                          levels = c('Low (0-0.25)', 'Mid (0.25-0.75)', 'High (0.75-1.0)'))
sim_toCentroids_ENG$lang <- "ENG"

sim_toCentroids <- rbind(sim_toCentroids_GER, sim_toCentroids_ENG)


plot_lowmidhigh <- ggplot(sim_toCentroids, aes(x = Year, y = mean, linetype = Score.Range,
                            colour = Score.Range)) +
  geom_line(linewidth = 1) +  # Black lines with varying linetypes
  scale_linetype_manual(values =c("twodash", "dotted", "solid")) +  
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Average cosine similarity",
    colour = "Canonisation", linetype = "Canonisation") +
  scale_colour_grey(start = 0.8, end = 0.2) +  # Reverse grayscale
  scale_y_continuous(limits = c(0.6, 0.875)) +       # Adjust y-axis limits
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  # Add vertical line at x = 0
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

### 0.25-packages -------------------------------------------------------------#

sim_toCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_canonisation_sim_groupeddf_GER.csv",
                            row.names = 1)

sim_toCentroids_GER$Score.Range <- factor(sim_toCentroids_GER$Score.Range, 
                                   levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)', 
                                              'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
sim_toCentroids_GER$lang <- "GER"

mean_GER <- sim_toCentroids_GER %>%
  mutate(Period = cut(
    Year,
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("50-25 years before publication", 
               "25-0 years before publication", 
               "0-25 years after publication", 
               "24-50 years after publication"),
    include.lowest = TRUE
  )) %>%
  group_by(Score.Range, Period) %>%
  summarise(
    mean_score = mean(mean, na.rm = TRUE),  # Calculate mean score
    .groups = "drop"
  )

sim_toCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_canonisation_sim_groupeddf_ENG.csv",
                                row.names = 1)

sim_toCentroids_ENG$Score.Range <- factor(sim_toCentroids_ENG$Score.Range, 
                                       levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)', 
                                                  'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
sim_toCentroids_ENG$lang <- "ENG"

mean_ENG <- sim_toCentroids_ENG %>%
  mutate(Period = cut(
    Year,
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("50-25 years before publication", 
               "25-0 years before publication", 
               "0-25 years after publication", 
               "24-50 years after publication"),
    include.lowest = TRUE
  )) %>%
  group_by(Score.Range, Period) %>%
  summarise(
    mean_score = mean(mean, na.rm = TRUE),  # Calculate mean score
    .groups = "drop"
  )

sim_toCentroids <- rbind(sim_toCentroids_GER, sim_toCentroids_ENG)

plot_canonisation <- ggplot(sim_toCentroids, aes(x = Year, y = mean, linetype = Score.Range,
                            colour = Score.Range)) +
  geom_line(linewidth = 1) +  # Black lines with varying linetypes
  scale_linetype_manual(values = c("twodash", "1342", "dotted", "solid")) +  
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Average cosine similarity",
    colour = "Canonisation", linetype = "Canonisation") +
  scale_colour_grey(start = 0.8, end = 0.2) +  # Reverse grayscale
  scale_y_continuous(limits = c(0.6, 0.875)) +       # Adjust y-axis limits
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  # Add vertical line at x = 0
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

#------------------------------------------------------------------------------#
## Reception ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

sim_toCentroids_GER_1 <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_reception_circllibs_sim_groupeddf_GER.csv",
                                row.names = 1)
sim_toCentroids_GER_1$class <- "Circulating Library Catalogues"
names(sim_toCentroids_GER_1) <- c("Year", "Binary", "Similarity", "std", "class")

sim_toCentroids_GER_2 <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_reception_reviews_sim_groupeddf_GER.csv",
                                row.names = 1)
sim_toCentroids_GER_2$class <- "Reviews"
names(sim_toCentroids_GER_2) <- c("Year", "Binary", "Similarity", "std", "class")

sim_toCentroids_GER <- rbind(sim_toCentroids_GER_1, sim_toCentroids_GER_2)
sim_toCentroids_GER$lang <- "GER"

sim_toCentroids_ENG_1 <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_reception_circllibs_sim_groupeddf_ENG.csv",
                                  row.names = 1)
sim_toCentroids_ENG_1$class <- "Circulating Library Catalogues"
names(sim_toCentroids_ENG_1) <- c("Year", "Binary", "Similarity", "std", "class")

sim_toCentroids_ENG_2 <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_reception_reviews_sim_groupeddf_ENG.csv",
                                  row.names = 1)
sim_toCentroids_ENG_2$class <- "Reviews"
names(sim_toCentroids_ENG_2) <- c("Year", "Binary", "Similarity", "std", "class")

sim_toCentroids_ENG <- rbind(sim_toCentroids_ENG_1, sim_toCentroids_ENG_2)
sim_toCentroids_ENG$lang <- "ENG"

sim_toCentroids <- rbind(sim_toCentroids_GER, sim_toCentroids_ENG)
sim_toCentroids$Binary <- factor(sim_toCentroids$Binary, levels = c(0,1))

plot_reception_binary <- ggplot(sim_toCentroids, aes(x = Year, y = Similarity, linetype = Binary,
                            colour = Binary)) +
  geom_line(linewidth = 1) +  # Black lines with varying linetypes
  scale_linetype_manual(values = c("twodash", "solid")) +  
  facet_wrap(vars(lang, class), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Average cosine similarity",
    colour = "Reception", linetype = "Reception") +
  scale_colour_grey(start = 0.8, end = 0.2) +  # Reverse grayscale
  scale_y_continuous(limits = c(0.6, 0.875)) +       # Adjust y-axis limits
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  # Add vertical line at x = 0
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

### Classes -------------------------------------------------------------------#

sim_toCentroids_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_reception_sim_groupeddf_GER.csv",
                                row.names = 1)

sim_toCentroids_GER$Reception.Class <- str_replace_all(sim_toCentroids_GER$Reception.Class, ", ", "\n")
sim_toCentroids_GER$Reception.Class <- str_replace_all(sim_toCentroids_GER$Reception.Class, " & ", "\n")
sim_toCentroids_GER$Reception.Class <- factor(sim_toCentroids_GER$Reception.Class, 
                                    levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                               "Review\nNo Catalogue", "Review\nCatalogue"))

sim_toCentroids_GER$lang <- "GER"

mean_GER <- sim_toCentroids_GER %>%
  mutate(Period = cut(
    Year,
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("50-25 years before publication", 
               "25-0 years before publication", 
               "0-25 years after publication", 
               "24-50 years after publication"),
    include.lowest = TRUE
  )) %>%
  group_by(Reception.Class, Period) %>%
  summarise(
    mean_score = mean(Similarity, na.rm = TRUE),  # Calculate mean score
    .groups = "drop"
  )

sim_toCentroids_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_reception_sim_groupeddf_ENG.csv",
                                row.names = 1)

sim_toCentroids_ENG$Reception.Class <- str_replace_all(sim_toCentroids_ENG$Reception.Class, ", ", "\n")
sim_toCentroids_ENG$Reception.Class <- str_replace_all(sim_toCentroids_ENG$Reception.Class, " & ", "\n")
sim_toCentroids_ENG$Reception.Class <- factor(sim_toCentroids_ENG$Reception.Class, 
                                              levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                                         "Review\nNo Catalogue", "Review\nCatalogue"))
sim_toCentroids_ENG$lang <- "ENG"

mean_ENG <-  sim_toCentroids_ENG %>%
  mutate(Period = cut(
    Year,
    breaks = c(-50, -25, 0, 25, 50),
    labels = c("50-25 years before publication", 
               "25-0 years before publication", 
               "0-25 years after publication", 
               "24-50 years after publication"),
    include.lowest = TRUE
  )) %>%
  group_by(Reception.Class, Period) %>%
  summarise(
    mean_score = mean(Similarity, na.rm = TRUE),  # Calculate mean score
    .groups = "drop"
  )

sim_toCentroids <- rbind(sim_toCentroids_GER, sim_toCentroids_ENG)

plot_reception_classes <- ggplot(sim_toCentroids, aes(x = Year, y = Similarity, linetype = Reception.Class,
                            colour = Reception.Class)) +
  geom_line(linewidth = 1) +  # Black lines with varying linetypes
  scale_linetype_manual(values = c("twodash", "1342", "dotted", "solid")) +  
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    x = "Years before and after publication",
    y = "Average cosine similarity",
    colour = "Classes of reception scores", linetype = "Classes of reception scores") +
  scale_colour_grey(start = 0.8, end = 0.2) +  # Reverse grayscale
  scale_y_continuous(limits = c(0.6, 0.875)) +       # Adjust y-axis limits
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +  # Add vertical line at x = 0
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


#------------------------------------------------------------------------------#
## Canonisation vs Reception --------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

canonisation_all_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_canonisation_sim.csv",
                             row.names = 1)
reception_all_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\rollingCentroids_reception_sim.csv",
                             row.names = 1)

# before publication ----------------------------------------------------------#

before_ENG_highly_canonised <- subset(canonisation_all_ENG, Year < 0 & Score.Range == 'High (0.75-1.0)')
before_ENG_highly_canonised <- subset(before_ENG_highly_canonised, select = -c(Canonisation.Score))
names(before_ENG_highly_canonised) <- c("ID", "Year", "Similarity", "Class")

before_ENG_widely_received <- subset(reception_all_ENG, Year < 0 & Reception.Class == "Review & Catalogue")
names(before_ENG_widely_received) <- c("ID", "Year", "Similarity", "Class")

eng <- rbind(before_ENG_widely_received, before_ENG_highly_canonised)
eng$TimePeriod <- ifelse(eng$Year >= -25, "0-25 years", "25-50 years")
eng$Class <- factor(eng$Class, levels = c('High (0.75-1.0)', "Review & Catalogue"))

eng %>%
  group_by(Class, TimePeriod) %>%
  summarise(mean_similarity = mean(Similarity, na.rm = TRUE))

# after publication -----------------------------------------------------------#

after_ENG_highly_canonised <- subset(canonisation_all_ENG, Year > 0 & Score.Range == 'High (0.75-1.0)')
after_ENG_highly_canonised <- subset(after_ENG_highly_canonised, select = -c(Canonisation.Score))
names(after_ENG_highly_canonised) <- c("ID", "Year", "Similarity", "Class")

after_ENG_widely_received <- subset(reception_all_ENG, Year > 0 & Reception.Class == "Review & Catalogue")
names(after_ENG_widely_received) <- c("ID", "Year", "Similarity", "Class")

eng <- rbind(after_ENG_widely_received, after_ENG_highly_canonised)
eng$TimePeriod <- ifelse(eng$Year <= 25, "0-25 years", "25-50 years")
eng$Class <- factor(eng$Class, levels = c('High (0.75-1.0)', "Review & Catalogue"))

eng %>%
  group_by(Class, TimePeriod) %>%
  summarise(mean_similarity = mean(Similarity, na.rm = TRUE))

anova_result <- aov(Similarity ~ Class * TimePeriod, data = eng)
summary(anova_result)
TukeyHSD(anova_result)

# German ----------------------------------------------------------------------#

canonisation_all_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_canonisation_sim.csv",
                                 row.names = 1)
reception_all_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\rollingCentroids_reception_sim.csv",
                              row.names = 1)

# before publication ----------------------------------------------------------#

before_GER_highly_canonised <- subset(canonisation_all_GER, Year < 0 & Score.Range == 'High (0.75-1.0)')
before_GER_highly_canonised <- subset(before_GER_highly_canonised, select = -c(Canonisation.Score))
names(before_GER_highly_canonised) <- c("ID", "Year", "Similarity", "Class")

before_GER_widely_received <- subset(reception_all_GER, Year < 0 & Reception.Class == "Review & Catalogue")
names(before_GER_widely_received) <- c("ID", "Year", "Similarity", "Class")

ger <- rbind(before_GER_widely_received, before_GER_highly_canonised)
ger$TimePeriod <- ifelse(ger$Year >= -25, "0-25 years", "25-50 years")
ger$Class <- factor(ger$Class, levels = c('High (0.75-1.0)', "Review & Catalogue"))

ger %>%
  group_by(Class, TimePeriod) %>%
  summarise(mean_similarity = mean(Similarity, na.rm = TRUE))

anova_result <- aov(Similarity ~ Class * TimePeriod, data = eng)
summary(anova_result)
TukeyHSD(anova_result)


# after publication -----------------------------------------------------------#

after_GER_highly_canonised <- subset(canonisation_all_GER, Year > 0 & Score.Range == 'High (0.75-1.0)')
after_GER_highly_canonised <- subset(after_GER_highly_canonised, select = -c(Canonisation.Score))
names(after_GER_highly_canonised) <- c("ID", "Year", "Similarity", "Class")

after_GER_widely_received <- subset(reception_all_GER, Year > 0 & Reception.Class == "Review & Catalogue")
names(after_GER_widely_received) <- c("ID", "Year", "Similarity", "Class")

ger <- rbind(after_GER_widely_received, after_GER_highly_canonised)
ger$TimePeriod <- ifelse(ger$Year <= 25, "0-25 years", "25-50 years")
ger$Class <- factor(ger$Class, levels = c('High (0.75-1.0)', "Review & Catalogue"))

ger %>%
  group_by(Class, TimePeriod) %>%
  summarise(mean_similarity = mean(Similarity, na.rm = TRUE))

anova_result <- aov(Similarity ~ Class * TimePeriod, data = ger)
summary(anova_result)
TukeyHSD(anova_result)

#------------------------------------------------------------------------------#
## Save as png ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

aligned <- align_plots(examples, plot_binary, plot_lowmidhigh, plot_canonisation,
                       plot_reception_binary, plot_reception_classes, align = "v",
                       axis="r")

ggdraw(aligned[[1]])


ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_examples.png", 
       dpi = 360, width = 24, height = 16, units = "cm")

ggdraw(aligned[[2]])


ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_binary_canonisation.png", 
       dpi = 360, width = 24, height = 16, units = "cm")

ggdraw(aligned[[3]])

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_low-mid-high_canonisation.png", 
       dpi = 360, width = 24, height = 16, units = "cm")

ggdraw(aligned[[4]])

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_canonisation.png", 
       dpi = 360, width = 24, height = 16, units = "cm")

ggdraw(aligned[[5]])

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_reception_binary.png", 
       dpi = 360, width = 24, height = 16, units = "cm")

ggdraw(aligned[[6]])

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\rollingCentroidSim_reception.png", 
       dpi = 360, width = 24, height = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#