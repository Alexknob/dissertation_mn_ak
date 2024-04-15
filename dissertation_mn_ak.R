
##%######################################################%##
#                                                          #
####            Statistical Analysis Code #             ####
####     Dissertation Ecological and Environmental      ####
####             Sciences with Management #             ####
####         Distribution Dynamics of Manganese         ####
####            in Drinking Water Catchments            ####
####              # Alexandra Knoblauch #               ####
####             s2090259 # The University              ####
####             of Edinburgh # April 2024              ####
#                                                          #
##%######################################################%##

# LIBRARIES ----

# install required packages with pacman package

if(!require(pacman))install.packages("pacman")

# load all needed packages

pacman::p_load('tidyverse', 'broom',
               'skimr', 'Hmisc', 'reshape',
               'sp', 'gstat', 'magrittr', 'zoo', 'dplyr')
pacman::p_load('tidyverse', 'scales', 'cowplot',
               'ggthemes', 'ggeffects', 'broom',
               'skimr', 'ggExtra',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report', 'corrplot', 'RColorBrewer')
pacman::p_load('tidyverse', 'scales', 'cowplot',
               'ggthemes', 'ggeffects', 'broom',
               'skimr', 'ggExtra','GGally', 'tidyr',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report', 'corrplot', 'RColorBrewer',
               'corrtable', 'PerformanceAnalytics', 'sjPlot', 'plotrix')
pacman::p_load('tidyverse', 'scales', 'cowplot',
               'ggthemes', 'ggeffects', 'broom',
               'stringr', 'ggExtra',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report', 'corrplot', 'RColorBrewer',
               'corrtable', 'PerformanceAnalytics', 'sjPlot',
               'corrr', 'ggcorrplot', 'FactoMineR',
               'factoextra', 'ggsci', 'pls', 'broom.mixed',
               'clusterSim', 'openxlsx')
pacman::p_load('tidyverse', 'scales', 'cowplot',
               'ggthemes', 'ggeffects', 'broom',
               'skimr', 'ggExtra',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report', 'corrplot', 'RColorBrewer',
               'sp', 'gstat', 'magrittr', 'zoo')
pacman::p_load('tidyverse', 'scales', 'cowplot',
               'ggthemes', 'ggeffects', 'broom',
               'skimr', 'ggExtra',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report', 'corrplot', 'RColorBrewer',
               'sp', 'gstat', 'magrittr', 'zoo')
pacman::p_load('tidyverse', 'scales', 'cowplot',
               'ggthemes', 'ggeffects', 'broom',
               'skimr', 'ggExtra', 'purrr',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report', 'corrplot', 'RColorBrewer',
               'corrtable', 'PerformanceAnalytics', 'sjPlot',
               'rstatix', 'ggsci')

# FUNCTIONS ----

# customised ggplot2 Function to format plots

# A custom ggplot2 function
theme.Alex <- function() {
  theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 12,
        vjust = 1,
        hjust = 1,
        colour = 'black'
      ),
      axis.text.y = element_text(size = 12, colour = 'black'),
      axis.title.x = element_text(size =14, face = "plain"),
      axis.title.y = element_text(size = 14, face = "plain"),
      axis.line = element_line(colour = 'black'),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 20,
        vjust = 1,
        hjust = 0.5
      ),
      legend.text = element_text(size = 20, face = "italic"),
      legend.title = element_text(size = 24, face = 'bold'),
      legend.position = c(0.9, 0.9)
    )
}


# RAW DATA FORMATING ----

d <- read.csv('data/02032024/r_coding_02032024.csv', header = T)

head(d)
str(d)
summary(d)
skim(d)
glimpse(d)



d2 <- d %>% 
  # convert to factor
  mutate_at(c('B_T', 'x', 'sample_collect'), as.factor) %>%
  # conver to number
  mutate_at(c('y'), as.numeric)   %>% 
  # remove moisture and organic matter content in g (dual information with percentage values)
  dplyr::select(-c('moisture_content_wet_g_g', 'OM_g')) %>% 
  # remove calculated concentrations (keep measured ones)
  # remove instrument outputs of peak area and CPS
  # remove dilution factor
  # remove iron 57 (since similar to iron 56)
  dplyr::select(-contains(c('calc', 'area', 'CPS', 'factor', '57'))) %>% 
  # remove long names and numbers
  rename_all(~stringr::str_replace(., 'meas_conc_undil_', '')) %>% 
  rename_all(~stringr::str_replace(., 'measure_undil_', '')) %>%
  rename_all(~stringr::str_replace(., 'X', '')) %>% 
  rename_all(~stringr::str_replace(., '55_', '')) %>%
  rename_all(~stringr::str_replace(., '75_', '')) %>% 
  rename_all(~stringr::str_replace(., '27_', '')) %>% 
  rename_all(~stringr::str_replace(., '56_', ''))

d2 <- d2 %>% 
  # rename columns for convenience
  rename_with(.cols = 20, ~ 'abs_254') %>% 
  rename_with(.cols = 21, ~ 'abs_365') %>% 
  rename_with(.cols = 22, ~ 'E254_356') %>% 
  rename_with(.cols = 23, ~ 'DOC_254') %>% 
  rename_with(.cols = 8, ~ 'moisture') %>% 
  rename_with(.cols = 9, ~ 'OM') %>% 
  rename_with(.cols = 10, ~ 'MF') %>% 
  # set lower case
  rename_with(tolower) %>% 
  # place doc column before e2/e3 column
  relocate(23, .before = 22)

# filter data set for high NA values

dfilter <- d2 %>% 
  # remove arsenic and nitrite due to high NA content
  dplyr::select(-c('as_ppb', 'c_nitrite_ppm'))

head(dfilter)
str(dfilter)
summary(dfilter)
skim(dfilter)
glimpse(dfilter)

# write csv

write.csv(dfilter, 'data/02032024/ak_rdata_formated_02032024.csv')


# DATA IMPORT AND FORMATING ----

# import formatted data sheet
d <- read.csv('data/02032024/ak_rdata_formated_02032024.csv', header = T)

head(d)
str(d)
summary(d)
skim(d)

d <- d %>% 
  dplyr::select(-c(X)) %>%
  mutate_at(c('x', 'b_t', 'sample_collect'), as.factor) %>%
  # conver to number
  mutate_at(c('y'), as.numeric)

str(d)


# DATA DISTRIBUTION AND TRANSFORMATION -----

# normality check

# plot histograms of each variable

dmelt <- melt(d) # convert to long

str(dmelt)
summary(dmelt)

# create labels to show number of observations per histogram plot
labels <- dmelt %>% 
  group_by(variable) %>% 
  na.omit() %>% 
  summarise(n = paste0('n = ', n()))

labels

# plot histogram
(histogram_overview <- ggplot(dmelt,aes(x = value)) + 
    facet_wrap(~variable, scales = "free_x") + 
    geom_histogram(bins = 10, fill = 'white', colour = 'black') +
    ylab('Frequency') +
    geom_text(data = labels, aes(x=-Inf, y = Inf, label = n), 
              vjust = 1, hjust ='inward') + 
    theme.Alex()
)

# save
ggsave(histogram_overview, filename = 'plots/histogram_overview.png',
       height = 20, width = 30, unit = 'cm')

# variables do not exhibit normal distribution

# log transform non-normal data (all variables)
# do not apply transformation for x, y, lat, lon, elev
# since physical variables

dlog <- d %>%
  mutate(across(c(moisture:e254_356), function(x) log(x)))




# SUMMARY STATISTICS ----

# ALL
# import formatted data sheet
d <- read.csv('data/02032024/ak_rdata_formated_02032024.csv', header = T)
d <- d %>% 
  dplyr::select(-c(X)) %>%
  mutate_at(c('b_t', 'sample_collect'), as.factor) %>%
  # conver to number
  mutate_at(c('y', 'x'), as.numeric)

summary_dmeas <- d %>% 
  dplyr::select(-c(b_t, sample_collect)) %>% 
  rename_all(~stringr::str_replace_all(., '_', '')) %>% 
  get_summary_stats(., type = 'common')

write.csv(summary_dmeas, 'correlation/summary_stat.csv', row.names = FALSE )

# T/B

dsummary <- d %>% 
  group_by(b_t) %>% 
  get_summary_stats(., type = 'common')

write.csv(dsummary, 'anovas/b_t/b_t_summary_stats.csv', row.names = FALSE )


# Top 0/10/20
dt <- d %>% 
  filter(b_t == 'T') %>% 
  mutate_at(c('b_t', 'sample_collect', 'x', 'y'), as.factor) 

dsummary <- dt %>% 
  group_by(x) %>% 
  get_summary_stats(., type = 'common')

write.csv(dsummary, 'anovas/stream_dist/stream_dist_summary_stats_top.csv', row.names = FALSE )



# Vertical Mn Changes with Soil Depth: T/B ----

# How does Mn change vertically with soil depth?
# H1: Bottom soils (> 20 cm) will have higher Mn concentration in pore water.
# H0: There is no difference in Mn concentration in pore water between top and bottom soils.
# Categorical x Variable: Bottom/Top (2 Levels)
# continuous y variable: Mn concentration

# ANOVAS

# Create ANOVAS for effect of T/B on each soil variable
# Simultaneously check assumptions of linear models

# create row names of variable name against T/B 
formulae <- lapply(colnames(dlog)[8:ncol(dlog)], function(x) as.formula(paste0(x, '~ b_t')))

# apply linear model on log transformed data
results <- lapply(formulae, function(x) summary(lm(x, data = dlog)))
names(results) <- format(formulae)

# tidy lm results
data <- map_df(results, tidy, .id = 'formulae')

# get statistics of lm 
stats <- map_df(results, glance, .id = 'formulae')
stats <- stats %>% 
  group_by(formulae) %>% 
  group_split(formulae) %>% 
  map_dfr(~add_row(.x, .after = Inf))

# bind lm results and statistical results together
data <- bind_cols(data, stats[2:ncol(stats)])
data

# test assummptions

# empty list to store shapiro tests for each variable
shapiro_resid <- list()

# Diagnostic plots
# loop through log transformed data from moisture to e254_356
for (i in 8:ncol(dlog)){
  
  # print column headers at each summary
  column <- names(dlog[i]) 
  
  # check assumptions of each anova with diagnostic plots
  # automatically save each plot
  mypath <- file.path("anovas/b_t/",paste("b_t_anova_diagnostic_plot_",column, ".png", sep = ""))
  
  png(file=mypath, width = 2800, height = 2800, res = 200)
  par(mfrow=c(2,2))
  # linear model
  plot((lm(dlog[,i] ~ b_t, data = dlog)))
  title(main=paste("Diagnostic Plots for soil depth (b_t) on ",column, sep=""), 
        font.main = 2, cex.main = 2, line = 2, adj = 0)
  par(mfrow=c(1,1))
  dev.off()
  
  # histogram of residuals
  # automatically save each plot
  mypath <- file.path("anovas/b_t/",paste("b_t_anova_resid_hist_plot_",column, ".png", sep = ""))
  
  png(file=mypath, width = 2800, height = 2800, res = 200)
  # plot histogram of linear model residuals
  hist((lm(dlog[,i] ~ b_t, data = dlog))$residuals, 
       main=paste("Histogram of anova residuals for soil depth (b_t) on ",column, sep=""), 
       font.main = 2, cex.main = 2, line = 2, adj = 0)
  # title(main = column, font.main = 2, cex.main = 2, line = 3, adj = 0)
  dev.off()
  
  # check normal distribution with shapiro test
  # save results in shapiro list
  shapiro_resid[[i]] <- shapiro.test((lm(dlog[,i] ~ b_t, data = dlog))$residuals)
  # if p>0.05, not significantly different from a normal distribution
}

# check for homoscedasticity
# Bartlett test
# if p > 0.05, variances are not significantly different between groups, homoscedasticity

# apply bartlett test
bartlett_log_test <- lapply(formulae, function(x) bartlett.test(x, data = dlog))

# get results
bartlett_log_unlist <- plyr::adply(bartlett_log_test,1,unlist,.id = NULL)
bartlett_log_unlist

# get statistics
bartlett_log_unlist <- bartlett_log_unlist %>% 
  group_by(data.name) %>% 
  group_split(data.name) %>% 
  map_dfr(~add_row(.x, .after = Inf))

# bind Bartlett test sets with lm results
data <- bind_cols(data, bartlett_log_unlist[5], bartlett_log_unlist[1:3])
data

# check normal distribution
# shapiro test
# if p>0.05, not significantly different from a normal distribution

# unlist shapiro test from the for loop above
shapiro_resid_unlist <- plyr::adply(shapiro_resid,1,unlist,.id = NULL)
shapiro_resid_unlist

shapiro_resid_unlist <- shapiro_resid_unlist %>% 
  group_by(p.value) %>% 
  group_split(p.value) %>% 
  map_dfr(~add_row(.x, .after = Inf))

# bind statistics and results with bartlett test
data <- bind_cols(data, shapiro_resid_unlist[3], shapiro_resid_unlist[1:2])
data

# save results
write.csv(data, 'anovas/b_t/b_t_anova_output_and_assumptions.csv', row.names = FALSE )


# NON-PARAMETRIC TEST

# since ANOVA assumptions violated, do non-parametric Kruskal-Wallis test
# test if there is a significant differnce between means of soil variables between top and bottom

# create row names
formulae <- lapply(colnames(d)[8:ncol(d)], function(x) as.formula(paste0(x, '~ b_t')))

# loop through data and apply kruskal wallis test to all variables
results <- lapply(formulae, function(x) kruskal.test(x, data = d))
names(results) <- format(formulae)

# get tidied results
data <- map_df(results, tidy, .id = 'formulae')
data

# check effect size
# 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).

# calculate effect size of kruskal wallis test
effect_size <- lapply(formulae, function(x) kruskal_effsize(x, data = d))

effect_size_unlist <- plyr::adply(effect_size,1,unlist,.id = NULL)
effect_size_unlist

# bind kruskal test and effect size together
data <- bind_cols(data, effect_size_unlist[4], effect_size_unlist[2:3], effect_size_unlist[5])
data

write.csv(data, 'anovas/b_t/b_t_kruskal_wallis_output.csv', row.names = FALSE )

# do not need pairwise Wilcox test since only have two levels in groups


# BOXPLOT

# Box for Mn

d$b_t <- factor(d$b_t, levels=rev(levels(d$b_t)), ordered = TRUE)
str(d)

( b_t_box <-
    ggplot(d, aes(
      x = b_t, y = mn_ppb, fill = b_t)) +
    geom_boxplot(colour = 'black') +
    geom_jitter(aes(
      x = b_t, y = mn_ppb), 
      size = 3, shape = 20
    ) +
    scale_x_discrete(labels = c("Top", "Bottom")) +
    scale_fill_manual(
      values = pal_d3('category20')(20)[c(16, 18)],
      name = "Soil Depth",
      labels = c("Top", "Bottom")
    ) +
    labs(x = "\n Soil Depth", y = expression('Mn Concentration µg'~L^-1~'')) +
    theme.Alex() +
    theme(axis.text.x = element_text(
      size = 14,
      colour = 'black',
      hjust = +0.5
    ),
    axis.text.y = element_text(size = 14, colour = 'black'),
    axis.title.x = element_text(size =16, face = "plain"),
    axis.title.y = element_text(size = 16, face = "plain"),
    legend.title = element_text(face = 'plain')) 
) 
)

# Saving the boxplot
ggsave(b_t_box, filename = "anovas/b_t/b_t_box/b_t_box_plot_mn.png", height = 20, width = 30, units = 'cm')


# Lateral Variation with stream distance: 0/10/20 -----


# How does Mn change laterally with distance to stream?
# H1: Soil pore waters closer to stream (transect 10) will have higher Mn concentration
# H0: There is no difference in Mn concentration in pore water between transects.
# Categorical x variable: 0/10/20 (3 Levels)
# continuous y variable: Mn concentration


# ANOVAS

# Create ANOVAS for effect of 0/10/20 on each soil variable
# Simultaneously check assumptions of linear models

# create row names of variable name against 0/10/20
formulae <- lapply(colnames(dlog)[8:ncol(dlog)], function(x) as.formula(paste0(x, '~ x')))

# apply linear model for each soil variable on log transformed data
results <- lapply(formulae, function(x) summary(lm(x, data = dlog)))
names(results) <- format(formulae)

data <- map_df(results, tidy, .id = 'formulae')

stats <- map_df(results, glance, .id = 'formulae')
stats <- stats %>% 
  group_by(formulae) %>% 
  group_modify(~add_row(.x, .after = Inf)) %>% 
  group_split(formulae) %>% 
  map_dfr(~add_row(.x, .after = Inf))

data <- bind_cols(data, stats[2:ncol(stats)])
data


# empty list to store shapiro tests for each variable
shapiro_resid <- list()

# Diagnostic plots
# loop through log transformed data from moisture to e254_356
for (i in 8:ncol(dlog)){
  
  column <- names(dlog[i]) # print column headers at each summary
  # check assumptions of each anova
  mypath <- file.path("anovas/stream_dist/",paste("stream_dist_anova_diagnostic_plot_",column, ".png", sep = ""))
  
  png(file=mypath, width = 2800, height = 2800, res = 200)
  par(mfrow=c(2,2))
  plot((lm(dlog[,i] ~ x, data = dlog)))
  title(main=paste("Diagnostic Plots for stream distance (x) on ",column, sep=""), 
        font.main = 2, cex.main = 2, line = 2, adj = 0)
  par(mfrow=c(1,1))
  dev.off()
  
  # histogram of residuals
  mypath <- file.path("anovas/stream_dist/",paste("stream_dist_anova_resid_hist_plot_",column, ".png", sep = ""))
  
  png(file=mypath, width = 2800, height = 2800, res = 200)
  hist((lm(dlog[,i] ~ x, data = dlog))$residuals, 
       main=paste("Histogram of anova residuals for stream distance (x) on ",column, sep=""), 
       font.main = 2, cex.main = 2, line = 2, adj = 0)
  # title(main = column, font.main = 2, cex.main = 2, line = 3, adj = 0)
  dev.off()
  
  # check normal distribution
  # print(shapiro.test(anova_res$residuals))
  shapiro_resid[[i]] <- shapiro.test((lm(dlog[,i] ~ x, data = dlog))$residuals)
  # if p>0.05, not significantly different from a normal distribution
}

# check for homoscedasticity
# Bartlett test
# if p > 0.05, variances are not significantly different between groups, homoscedasticity

bartlett_log_test <- lapply(formulae, function(x) bartlett.test(x, data = dlog))

bartlett_log_unlist <- plyr::adply(bartlett_log_test,1,unlist,.id = NULL)
bartlett_log_unlist

bartlett_log_unlist <- bartlett_log_unlist %>% 
  group_by(data.name) %>% 
  group_modify(~add_row(.x, .after = Inf)) %>% 
  group_split(data.name) %>% 
  map_dfr(~add_row(.x, .after = Inf))

data <- bind_cols(data, bartlett_log_unlist[5], bartlett_log_unlist[2:4])
data


# check normal distribution
# shapiro test
# if p>0.05, not significantly different from a normal distribution

shapiro_resid_unlist <- plyr::adply(shapiro_resid,1,unlist,.id = NULL)
shapiro_resid_unlist

shapiro_resid_unlist <- shapiro_resid_unlist %>% 
  group_by(p.value) %>% 
  group_modify(~add_row(.x, .after = Inf)) %>% 
  group_split(p.value) %>% 
  map_dfr(~add_row(.x, .after = Inf))

data <- bind_cols(data, shapiro_resid_unlist[3], shapiro_resid_unlist[1:2])
data

# save data
write.csv(data, 'anovas/stream_dist/stream_dist_anova_output_and_assumptions.csv', row.names = FALSE )


# NON-PARAMETRIC TEST

# since ANOVA assumptions violated, do non-parametric Kruskal-Wallis test
# test if there is a significant differnce between means of soil variables between transects
# test this for top 

# filter top and bottom separately

# top
dt <- d %>% 
  filter(b_t == 'T') # keep only top samples 

formulae <- lapply(colnames(dt)[8:ncol(dt)], function(x) as.formula(paste0(x, '~ x')))

results <- lapply(formulae, function(x) kruskal.test(x, data = dt))
names(results) <- format(formulae)

data <- map_df(results, tidy, .id = 'formulae')
data

# check effect size
# 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).

effect_size <- lapply(formulae, function(x) kruskal_effsize(x, data = dt))

effect_size_unlist <- plyr::adply(effect_size,1,unlist,.id = NULL)
effect_size_unlist

data <- bind_cols(data, effect_size_unlist[4], effect_size_unlist[2:3], effect_size_unlist[5])
data

# Pairwise Wilcoxon's test
# multiple pairwise comparison between individual groups
# to see which of the transects are different

wilcox <- lapply(formulae, function(x) wilcox_test(x, data = dt, p.adjust.method = 'bonferroni'))

names(wilcox) <- format(formulae)

wilcox_clean <- map_df(wilcox, glimpse, .id = 'formulae')
wilcox_clean <- wilcox_clean[order(wilcox_clean$formulae),]

# add 2 empty rows after each group in data
# so that wilcox_clean can be added to kruskal results
data <- data %>% 
  group_by(formulae) %>% 
  group_modify(~add_row(.x, .after = Inf)) %>% 
  group_by(formulae) %>% 
  group_modify(~add_row(.x, .after = Inf))

# add kruskal test and wilcox together together
data <- bind_cols(data, wilcox_clean)
data

write.csv(data, 'anovas/stream_dist/stream_dist_top_kruskal_wilcox_output.csv', row.names = FALSE )


# BOXPLOTS

# Boxplot for Mn T

( stream_box <-
    ggplot(dt, aes(
      x = x, y = mn_ppb, fill = x)) +
    geom_boxplot(position = position_dodge(1), colour = 'black') +
    geom_jitter(aes(
      x = x, y = mn_ppb, fill = x), 
      #position = position_dodge(1), 
      size = 3, shape = 20) +
    scale_x_discrete(labels = c("0", "10", "20")) +
    scale_fill_manual(
      values = pal_d3('category20')(20)[c(12, 11, 13)],     
      name = "Transect Number",
      labels = c("0", "10", '20')
      
    ) +
    labs(x = "\n Transect Number", y = expression('Mn Concentration µg'~L^-1~'')) +
    theme.Alex() +
    theme(axis.text.x = element_text(
      size = 14,
      colour = 'black',
      hjust = +0.5
    ),
    axis.text.y = element_text(size = 14, colour = 'black'),
    axis.title.x = element_text(size =16, face = "plain"),
    axis.title.y = element_text(size = 16, face = "plain"),
    legend.title = element_text(face = 'plain')) 
) 
)

# Saving the boxplot
ggsave(stream_box, filename = "anovas/stream_dist/stream_dist_box/stream_dist_box_plot_mn_top.png", height = 20, width = 30, units = 'cm')


# Mn Relation to Geochemical Variables -----

# Correlation

# import formatted data sheet
d <- read.csv('data/02032024/ak_rdata_formated_02032024.csv', header = T)

# Data Manipulation

head(d)
str(d)
summary(d)
skim(d)

d <- d %>% 
  dplyr::select(-c(X)) %>%
  mutate_at(c('b_t', 'sample_collect'), as.factor) %>%
  # conver to number
  mutate_at(c('y', 'x'), as.numeric)
d_num <- d %>% 
  select(-c('b_t', 'sample_collect', 'x', 'y', 'elev', 'lat', 'lon',
            'c_cl_ppm', 'c_nitrate_ppm', 'c_so4_ppm', 'abs_254',
            'abs_365'))

# Test for correlation between variables 
# do not perform Pearson since data non-normally distributed
# perform Spearman Correlation: non parametric, rank-based correlation coefficient
# or Kendall Correlation

# Spearman correlation between variables 
# Q1: How are total measured soil variables (moisture, SOM, pH, Fe, Al, DOC) related to Mn?

# Remove non-numeric columns (b_t & sample_collect)
# remove columns irrelavant for analysis
# create top data set
d_top <- d %>% 
  filter(b_t == 'T') %>% 
  select(-c('b_t', 'sample_collect', 'x', 'y', 'elev', 'lat', 'lon',
            'c_cl_ppm', 'c_nitrate_ppm', 'c_so4_ppm', 'abs_254',
            'abs_365'))

# Function to extract correlation coefficient and p-values
# from: https://stackoverflow.com/questions/34326906/extracting-and-formatting-results-of-cor-test-on-multiple-pairs-of-columns 
corrFunc <- function(var1, var2, data) {
  result = cor.test(data[,var1], data[,var2], method = 'spearman', exact = FALSE)
  data.frame(var1, var2, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=FALSE)
}

# Mn correlation for Top
## Pairs of variables for which we want correlations
# select Mn and all others
vars = data.frame(v1=names(d_num)[5], v2=names(d_num)[-5])

# Apply corrFunc to all rows of vars
corrs_mn_top <- do.call(rbind, mapply(corrFunc, vars[,1], vars[,2], MoreArgs=list(data=d_top), 
                                      SIMPLIFY=FALSE))

corrs_mn_top

# save output in table
write.csv(corrs_mn_top, 'correlation/correlation_mn_top.csv', row.names = FALSE )

# create correlation matrix ready for publishing (Table)
# save correlation matrix
# significance levels are: p < .001 = ***, p < .01 = **, p < .05 = *  
# top
save_correlation_matrix(df = d_top, filename = 'correlation/correlation_top_pretty.csv',
                        digits = 3, use = 'lower', replace_diagonal = FALSE, type = 'spearman')

# Correlation Plot

# top data
png(height=1200, width=1200, file="correlation/top_chartcorrelation.png", type = "cairo")
chart.Correlation(d_top, histogram = TRUE, method = 'spearman')
dev.off()

# correlogram top data
png(height=1000, width=1000, file="correlation/top_corrplot.png", type = "cairo")
sjp.corr(d_top, sort.corr = TRUE,
         decimals = 3, na.deletion = "pairwise",
         corr.method = 'spearman', geom.colors = "RdBu",
         wrap.title = 50, wrap.labels = 20, show.legend = TRUE,
         legend.title = NULL, show.values = TRUE, show.p = TRUE,
         p.numeric = FALSE)
dev.off()

# Scatter correlation

# remove b_t, sample_collect, x, y, lon, lat, elev from d_num
d_cont <- d %>% 
  select(-c('b_t', 'sample_collect', 'x', 'y', 'elev', 'lat', 'lon',
            'c_cl_ppm', 'c_nitrate_ppm', 'c_so4_ppm', 'abs_254',
            'abs_365'))

# for top
# remove b_t, sample_collect, x, y, lon, lat, elev from d_num
d_cont <- d %>% 
  filter(b_t == 'T') %>% 
  select(-c('b_t', 'sample_collect', 'x', 'y', 'elev', 'lat', 'lon',
            'c_cl_ppm', 'c_nitrate_ppm', 'c_so4_ppm', 'abs_254',
            'abs_365'))

# plot scatter correlation matrix of continuous variables for all data
png(height=1200, width=1200, file="correlation/total_scatter_corrplot.png", type = "cairo")
pairs(d_cont, upper.panel = NULL)
dev.off()

(total_scatter_ggpairs <- ggpairs(d_cont, 
                                  upper = list(continuous = wrap(ggally_cor, 
                                                                 method = 'spearman'))))
ggsave(total_scatter_ggpairs, filename = 'correlation/top_scatter_ggpairs.png', 
       width = 30, height = 30, units = 'cm', device = 'png')


# PCA

# import formatted data sheet
d <- read.csv('data/02032024/ak_rdata_formated_02032024.csv', header = T)

# check missing values (cannot have NA in analysis)
colSums(is.na(d))

d$X <- NULL

d <- d %>% 
  mutate_at(c('b_t', 'sample_collect', 'x', 'y'), as.factor) 

# TOP values
dt <- d %>% 
  filter(b_t == 'T') %>% # keep only top samples
  filter(sample_collect == 1) %>% #remove rows where no sample was collected
  filter(!(x == '20' & y == '0')) %>%  # remove rows where no water was extracted
  filter(!(x == '20' & y == '40')) %>% # remove sample that fell over in oven (has no mf, om data)
  dplyr::select(-c(4:7)) %>%  #remove lat, lon, elev, sample_collect
  dplyr::select(-c(abs_254, abs_365)) %>%  # remove absorbance columns since given by doc
  dplyr::select(-c(c_cl_ppm)) # remove cl due to many NA and no correlation with variables

# check missing values (cannot have NA in analysis)
colSums(is.na(dt))
summary(dt)

# create second data set without so4 and nitrate due to many variables missing
dt_nosn <- dt %>% 
  dplyr::select(-c(c_so4_ppm, c_nitrate_ppm)) # remove so4 and nitrate due to many NA

# check missing values (cannot have NA in analysis)
colSums(is.na(dt_nosn))
summary(dt_nosn)

# following tutorial: 
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#graph-customization

# do pca with dtb_nona (n=26, p=14) and dtb_nosn (n=50, p=12)
# do pca with dt_nona(n=15, p=14) and dt_nosn (n=30, p =12)


# PCA with dt_nosn (n=30, p=12)

# combine x and y for location
dt_nosn$loc <- paste(dt_nosn$x, "-", dt_nosn$y, sep = "")
# make loc row name
rownames(dt_nosn) <- dt_nosn$loc
dt_nosn$loc <- NULL

str(dt_nosn)

# subset into active variable dataset

dt_nosn_active <- dt_nosn[,4:12]
head(dt_nosn_active)

# standardise data
# can do with scale()
# but factoMineR standardises data automatically in PCA

# run PCA
# with all variables including Mn
res.pca <- PCA(dt_nosn_active, scale.unit = T, graph = T)
res.pca


# Extract information with factoextra

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Scree plot
# to determine number of PC to choose
scree_plot <- fviz_eig(res.pca, addlabels = T, ylim = c(0, 60))
ggexport(scree_plot, filename = 'pca/factoextra/scree_plot_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# extract results
var <- get_pca_var(res.pca)
var

# Coordinates
# for later scatter plot
head(var$coord)
# Cos2: quality on the factor map
# assess quality of representation for variables on the factor map
head(var$cos2)
# Contributions to the principal components
# how much each variable contributes to the PC in %
head(var$contrib)

# svd for standard deviation, loadings, scores
svd <- res.pca[['svd']]

# Standard deviation of PC
head(svd$vs)
# Loadings of PC
head(svd$V)
# Scores of individuals
head(svd$U)

# Visualisations of results

# variable correlation plot
var_corr_plot_circle <- fviz_pca_var(res.pca, col.var = "black",
                                     repel = TRUE) # Avoid text overlapping
ggexport(var_corr_plot_circle, filename = 'pca/factoextra/variable_correlation_plot_dt_nosn.png',
         width = 1200, height = 1200, res = 250)


# quality or representation cos2 plot: 
# correlation matrix plot
(rep_corr_plot <- corrplot(var$cos2))
ggexport(rep_corr_plot, filename = 'pca/factoextra/cos2_correlation_plot_dt_nosn.png',
         width = 1200, height = 1200, res = 250)
# or with bar plot
# Total cos2 of variables on Dim.1 and Dim.2
rep_corr_plot_circle <- fviz_cos2(res.pca, choice = "var", axes = 1:2)
ggexport(rep_corr_plot_circle, filename = 'pca/factoextra/cos2_bar_plot_dt_nosn.png',
         width = 1200, height = 1200, res = 250)


# contribution of each variable to PC 1 and PC2
# corrplot
(contr_corr_plot <- corrplot(var$contrib))  
ggexport(contr_corr_plot, filename = 'pca/factoextra/contribution_plot_dt_nosn.png',
         width = 1200, height = 1200, res = 250)
# or with bar plots
# Contributions of variables to PC1
contr_corr_plot_bar_pc1 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
ggexport(contr_corr_plot_bar_pc1, filename = 'pca/factoextra/contribution_bar_plot_pc1_dt_nosn.png',
         width = 1200, height = 1200, res = 250)
# Contributions of variables to PC2
contr_corr_plot_bar_pc2 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
ggexport(contr_corr_plot_bar_pc2, filename = 'pca/factoextra/contribution_bar_plot_pc2_dt_nosn.png',
         width = 1200, height = 1200, res = 250)
# total PC 1 and 2
contr_corr_plot_bar_pc1_2 <- fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
ggexport(contr_corr_plot_bar_pc1_2, filename = 'pca/factoextra/contribution_bar_plot_pc1_2_dt_nosn.png',
         width = 1200, height = 1200, res = 250)
# or with circle plot
# coloured by contribution
contr_corr_plot_circle <- fviz_pca_var(res.pca, col.var = "contrib",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
ggexport(contr_corr_plot_circle, filename = 'pca/factoextra/contribution_plot_circle_dt_nosn.png',
         width = 1200, height = 1200, res = 250)


# access dimensions of PC
# corresponds to corr plots
# identify most significantly associated variables with a given PC
res.desc <- dimdesc(res.pca)
# Description of pc 1
res.desc$Dim.1
# Description of pc 2
res.desc$Dim.2
# Description of pc 3
res.desc$Dim.3


# access results for individuals
ind <- get_pca_ind(res.pca)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

# visualisation of individuals

# quality or representation cos2 plot: 
sample_rep_corr_plot_circle <- fviz_pca_ind(res.pca, col.ind = "cos2", 
                                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                            repel = TRUE # Avoid text overlapping (slow if many points)
)
ggexport(sample_rep_corr_plot_circle, filename = 'pca/factoextra/contribution_plot_circle_samples_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# contribution of each variable to PC 1 and PC2
# Total contribution on PC1 and PC2
sample_contr_corr_plot_bar_pc1_2 <- fviz_contrib(res.pca, choice = "ind", axes = 1:2)
ggexport(sample_contr_corr_plot_bar_pc1_2, filename = 'pca/factoextra/contribution_bar_plot_samples_pc1_2_dt_nosn.png',
         width = 1200, height = 1200, res = 250)


# colour by factor of transect x
# Color variables by groups and concentration ellipse
samples_scores_x_group <- fviz_pca_ind(res.pca,
                                       geom.ind = "point", # show points only (nbut not "text")
                                       col.ind = dt_nosn$x, # color by groups
                                       palette = 'futurama',
                                       addEllipses = TRUE, # Concentration ellipses
                                       legend.title = "Transect",
                                       mean.point = F
)
ggexport(samples_scores_x_group, filename = 'pca/factoextra/scores_plot_samples_grouped_pc1_2_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# Color variables by groups and confidence ellipse
samples_scores_x_group_conf <- fviz_pca_ind(res.pca,
                                            geom.ind = "point", # show points only (nbut not "text")
                                            col.ind = dt_nosn$x, # color by groups
                                            palette = 'futurama',                                       addEllipses = TRUE, 
                                            ellipse.type = 'confidence', ellipse.level = 0.95,  # 95% Confidence ellipses
                                            legend.title = "Transect",
                                            mean.point = F
)
ggexport(samples_scores_x_group_conf, filename = 'pca/factoextra/scores_plot_samples_grouped_conf_pc1_2_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# Biplots PC 1 and 2

( stream_box <-
    ggplot(dt, aes(
      x = x, y = mn_ppb, fill = x)) +
    geom_boxplot(position = position_dodge(1)) +
    geom_jitter(aes(
      x = x, y = mn_ppb, fill = x), 
      #position = position_dodge(1), 
      size = 3, shape = 17, alpha = 0.5) +
    scale_x_discrete(labels = c("0", "10", "20")) +
    scale_fill_manual(
      values = pal_d3('category20')(20)[c(12, 11, 13)],     
      name = "Transect Number",
      labels = c("0", "10", '20')
    ) +
    labs(x = "\n Transect Number", y = 'Mn (ppb)') +
    theme.Alex() +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5),
      legend.position = c(0.8, 0.8)
    )
)

(biplot_pc1_2 <- fviz_pca_biplot(res.pca, 
                                 # individuals
                                 geom.ind = 'point',
                                 fill.ind = dt_nosn$x, # fill by groups
                                 col.ind = 'black',
                                 pointshape = 21, pointsize = 3, 
                                 palette = 'futurama',   
                                 addEllipses = TRUE, #concentration ellipse
                                 # variables
                                 alpha.var = "contrib",
                                 label = "var",
                                 col.var = 'contrib',
                                 title = NULL,
                                 gradient.cols = scales::alpha('black', c(0.4, 0.6, 0.8)),
                                 repel = TRUE,
                                 legend.title = list(fill = "Transect",
                                                     alpha = 'Contribution'
                                 ),
                                 #scale_colour_continuous(guide = 'none'),
                                 mean.point = F) +
    guides(colour = 'none')+
    scale_fill_manual(
      values = pal_d3('category20')(20)[c(12, 11, 13)],     
      name = "Transect Number",
      labels = c("0", "10", '20')
    ) +
    theme.Alex()+
    theme(
      legend.text = element_text(size = 12, face = "italic"),
      legend.title = element_text(size = 14, face = 'plain'),
      legend.position = 'right'
    )
)

ggexport(biplot_pc1_2, filename = 'pca/factoextra/biplot_pc1_2_dt_nosn.png',
         width = 1800, height = 1300, res = 250)


# PC 3 and 4
# Variables on dimensions 3 and 4
# variable correlation plot
var_corr_plot_circle <- fviz_pca_var(res.pca, axes = c(3, 4), col.var = "black",
                                     repel = TRUE) # Avoid text overlapping
ggexport(var_corr_plot_circle, filename = 'pca/factoextra/variable_correlation_plot_pc3_4_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# Individuals on dimensions 3 and 4
# quality or representation cos2 plot: 
sample_rep_corr_plot_circle <- fviz_pca_ind(res.pca,  axes = c(3, 4), col.ind = "cos2", 
                                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                            repel = TRUE # Avoid text overlapping (slow if many points)
)
ggexport(sample_rep_corr_plot_circle, filename = 'pca/factoextra/contribution_plot_circle_samples_pc3_4_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# colour by factor of transect x
# Color variables by groups and concentration ellipse
samples_scores_x_group <- fviz_pca_ind(res.pca, axes = c(3, 4),
                                       geom.ind = "point", # show points only (nbut not "text")
                                       col.ind = dt_nosn$x, # color by groups
                                       palette = 'futurama',                                       addEllipses = TRUE, # Concentration ellipses
                                       legend.title = "Transect",
                                       mean.point = F
)
ggexport(samples_scores_x_group, filename = 'pca/factoextra/scores_plot_samples_grouped_pc3_4_dt_nosn.png',
         width = 1200, height = 1200, res = 250)

# Color variables by groups and confidence ellipse
samples_scores_x_group_conf <- fviz_pca_ind(res.pca, axes = c(3, 4),
                                            geom.ind = "point", # show points only (nbut not "text")
                                            col.ind = dt_nosn$x, # color by groups
                                            palette = 'futurama',                                            addEllipses = TRUE, 
                                            ellipse.type = 'confidence', # Confidence ellipses
                                            legend.title = "Transect",
                                            mean.point = F
)
ggexport(samples_scores_x_group_conf, filename = 'pca/factoextra/scores_plot_samples_grouped_conf_pc3_4_dt_nosn.png',
         width = 1200, height = 1200, res = 250)


# Biplots PC 3 and 4

biplot_pc3_4 <- fviz_pca_biplot(res.pca, axes = c(3, 4),
                                # individuals
                                geom.ind = 'point',
                                fill.ind = dt_nosn$x, # fill by groups
                                col.ind = 'black',
                                pointshape = 21, pointsize = 2, 
                                palette = 'futurama',   
                                #addEllipses = TRUE, #concentration ellipse
                                # variables
                                alpha.var = "contrib",
                                label = "var",
                                col.var = 'contrib',
                                gradient.cols = scales::alpha('black', c(0.4, 0.6, 0.8)),
                                repel = TRUE,
                                legend.title = list(fill = "Transect",
                                                    alpha = 'Contribution'
                                ),
                                #scale_colour_continuous(guide = 'none'),
                                mean.point = F) +
  guides(colour = 'none') 
ggexport(biplot_pc3_4, filename = 'pca/factoextra/biplot_pc3_4_dt_nosn.png',
         width = 1200, height = 1200, res = 250)


# Export PCA results
write.infile(res.pca, 'pca/factoextra/pca_results_dt_nosn.csv', sep = ';')

# Export dimension description
write.infile(res.desc, 'pca/factoextra/dimension_description_dt_nosn.csv', sep = ';')

# PCR on non transformed data -----
# based on PCA with dt_nosn (n=30, p=12)

# apply PCR with pls package
# to predict mn concentration
# use active dt_nosn data from pca above

# subset into active variable dataset

dt_nosn_active <- dt_nosn[,4:12]
head(dt_nosn_active)

# normalise mn data before
# normalise (scale & mean center) mn data before plotting
dt_nosn_active$mn_ppb_norm <- data.Normalization(dt_nosn_active$mn_ppb, type = 'n1', 
                                                 normalization = 'column')

pcr_model <- pcr(mn_ppb_norm ~., 
                 data = dt_nosn_active %>% 
                   dplyr::select(-mn_ppb),
                 scale = T, #scale data
                 center = T, # center data
                 validation = 'CV' # use cross validation
)

summary(pcr_model)

# export model summary results
sink('pca/pcr_results/pcr_model_summary_mn_dt_nosn.txt')
print(summary(pcr_model))
sink()

# plot biplot of pcr_model (no Mn; X scores and X loadings)
png(file='pca/pcr_results/biplot_x_scores_loadings_pcr_norm_dt_nosn.png', width = 2800, height = 2800, res = 200)
biplot(pcr_model)
dev.off()

par(mfrow = c(2,2))
biplot(pcr_model, which = "x") # Default
#biplot(pcr_model, which = "y")
#biplot(pcr_model, which = "scores")
biplot(pcr_model, which = "loadings")
dev.off()

# plot validation error RMSEP
png(file='pca/pcr_results/validationplot_rmsep_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
validationplot(pcr_model, 
               val.type = 'RMSEP', # using Root Mean Squared Error of Prediction
               legendpos = 'topright',
               estimate = 'all')
dev.off()

# extract RMSEP
rmsep <- pls::RMSEP(pcr_model, ncomp = 1:8, estimate = 'CV')
rmsep
write.csv(rmsep, file = 'pca/pcr_results/rmsep_pcr_dt_nosn.csv', sep = ',')

# plot validation error R2
png(file='pca/pcr_results/validationplot_r2_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
validationplot(pcr_model, 
               val.type = 'R2',
               legendpos = 'bottomleft',
               # using R2
)
dev.off()

#extrect R2
r2 <- pls::R2(pcr_model, ncomp = 1:8, estimate = 'CV')
r2
write.csv(r2, file = 'pca/pcr_results/r2_pcr_dt_nosn.csv', sep = ',')

# choose which PCR model minimizes Cross validation error
min_cv_error <- which.min(RMSEP(pcr_model)$val[1,1, ]) - 1
min_cv_error
# model with 2 components minimise RMSEP error
# small RMSEP: predicted responses are close to true responses

# additionally
# plot scores of PC1, 2, 3 against mn data
# should show a pattern
scores <- pcr_model[['scores']]
scores
#plot pcr scores against normalised mn data
png(file='pca/pcr_results/scoreplot_pc1_norm_mn_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(scores[, 1], dt_nosn_active$mn_ppb_norm)
dev.off()
png(file='pca/pcr_results/scoreplot_pc2_norm_mn_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(scores[, 2], dt_nosn_active$mn_ppb_norm)
dev.off()
png(file='pca/pcr_results/scoreplot_pc3_norm_mn_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(scores[, 3], dt_nosn_active$mn_ppb_norm)
dev.off()

# COEFFICIENTS
# find corresponding estimates based on model with 2 PC
# extract coefficient estimates for each variable for model with 2 PC in estimating mn
coef <- coef(pcr_model, ncomp = 1:2)
write.csv(coef, file = 'pca/pcr_results/coef_pcr_dt_nosn.csv', sep = ',')

# plot regression coefficients
png(file='pca/pcr_results/coefplot_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
coefplot(pcr_model, ncomp = 1:2, legendpos = 'topright', labels = 'names')
dev.off()

# RESIDUALS
# extract residuals of score estimates
res <- residuals(pcr_model, ncomp = 1:2)
write.csv(res, file = 'pca/pcr_results/res_pcr_dt_nosn.csv', sep = ',')

# SCORES
# extract fitted values of Mn scores
fitted <- (fitted(pcr_model, ncomp = 1:2))
write.csv(fitted, file = 'pca/pcr_results/fitted_pcr_dt_nosn.csv', sep = ',')

# plot scores
png(file='pca/pcr_results/scoresplot_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(pcr_model, plottype = 'scores', comps = 1:2) # if still a pattern --> collinear (but do not show, so good for regression)
dev.off()

# LOADINGS
# extract loadings of variables
loadings <- loadings(pcr_model, ncomp = 1:2)
write.csv(loadings, file = 'pca/pcr_results/loadings_pcr_dt_nosn.csv', sep = ',')

# loading plot
png(file='pca/pcr_results/loadingsplot_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(pcr_model, 'loadings', comps = 1:2, legendpos = 'topright',
     labels = 'names') 
abline(h = 0)
dev.off()

# PREDICTIONS
# generate predictions for mn at each location based on components
predict <- (predict(pcr_model, ncomp = 1:2))
write.csv(predict, file = 'pca/pcr_results/predict_pcr_dt_nosn.csv', sep = ',')

# plot predicted vs measured variables of pc 1 and 2
png(file='pca/pcr_results/predplot_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
predplot(pcr_model, ncomp = min_cv_error, asp =1, line = T) # with 1:1 target line
dev.off()

# correlation plot
# correlation of each variable and selected pc
png(file='pca/pcr_results/corrplot_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(pcr_model, 'correlation', comps = 1:2
)
dev.off()

# PCR 2 with log transformed Mn data -----

# subset into active variable dataset

dt_nosn_active <- dt_nosn[,4:12]
head(dt_nosn_active)

# log transform mn data to reach normal distribution
dt_nosn_active$mn_ppb_log <- log(dt_nosn_active$mn_ppb)

# with pls package
# to predict mn concentration
# use active dt_nosn data from pca above
pcr_model_2 <- pcr(mn_ppb_log ~., 
                   data = dt_nosn_active %>% 
                     dplyr::select(-mn_ppb),
                   scale = T, #scale data
                   center = T, # center data
                   validation = 'CV' # use cross validation
)


summary(pcr_model_2)


# export model summary results
sink('pca/pcr_results/pcr_model_log_summary_mn_dt_nosn.txt')
print(summary(pcr_model_2))
sink()

# plot biplot of pcr_model (no Mn; X scores and X loadings)
png(file='pca/pcr_results/biplot_x_scores_loadings_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
biplot(pcr_model_2)
dev.off()

par(mfrow = c(2,2))
biplot(pcr_model_2, which = "x") # Default
#biplot(pcr_model_2, which = "y")
#biplot(pcr_model_2, which = "scores")
biplot(pcr_model_2, which = "loadings")
dev.off()

# plot validation error RMSEP
png(file='pca/pcr_results/validationplot_rmsep_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
validationplot(pcr_model_2, 
               val.type = 'RMSEP', # using Root Mean Squared Error of Prediction
               legendpos = 'topright',
               estimate = 'all')
dev.off()

# extract RMSEP
rmsep <- pls::RMSEP(pcr_model_2, ncomp = 1:8, estimate = 'CV')
rmsep
write.csv(rmsep, file = 'pca/pcr_results/rmsep_pcr_log_dt_nosn.csv', sep = ',')


# plot validation error R2
png(file='pca/pcr_results/validationplot_r2_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
validationplot(pcr_model_2, 
               val.type = 'R2',
               legendpos = 'bottomleft',
               # using R2
)
dev.off()

#extrect R2
r2 <- pls::R2(pcr_model_2, ncomp = 1:8, estimate = 'CV')
r2
write.csv(r2, file = 'pca/pcr_results/r2_pcr_log_dt_nosn.csv', sep = ',')

# choose which PCR model minimizes Cross validation error
min_cv_error <- which.min(RMSEP(pcr_model_2)$val[1,1, ]) - 1
min_cv_error
# model with 6 components minimise RMSEP error
# still choose 2
min_cv_error = 2
# small RMSEP: predicted responses are close to true responses

# additionally
# plot scores of PC1, 2, 3 against mn data
# should show a pattern
scores_2 <- pcr_model_2[['scores']]
scores_2

#plot pcr scores against normalised mn data
png(file='pca/pcr_results/scoreplot_pc1_log_mn_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot( scores_2[, 1], dt_nosn_active$mn_ppb_log)
dev.off()
png(file='pca/pcr_results/scoreplot_pc2_log_mn_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(scores_2[, 2], dt_nosn_active$mn_ppb_log)
dev.off()
png(file='pca/pcr_results/scoreplot_pc3_log_mn_pcr_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(scores_2[, 3], dt_nosn_active$mn_ppb_log)
dev.off()
# log transformation shows clear patterns

# COEFFICIENTS
# find corresponding estimates based on model with 2 PC
# extract coefficient estimates for each variable for model with 2 PC in estimating mn
coef <- coef(pcr_model_2, ncomp = 1:2)
write.csv(coef, file = 'pca/pcr_results/coef_pcr_log_dt_nosn.csv', sep = ',')

# plot regression coefficients
png(file='pca/pcr_results/coefplot_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
coefplot(pcr_model_2, ncomp = 1:2, legendpos = 'topright', labels = 'names')
dev.off()

# RESIDUALS
# extract residuals of score estimates
res <- residuals(pcr_model_2, ncomp = 1:2)
write.csv(res, file = 'pca/pcr_results/res_pcr_log_dt_nosn.csv', sep = ',')

# SCORES
# extract fitted values of Mn scores
fitted <- (fitted(pcr_model_2, ncomp = 1:2))
write.csv(fitted, file = 'pca/pcr_results/fitted_pcr_log_dt_nosn.csv', sep = ',')

# plot scores
png(file='pca/pcr_results/scoresplot_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(pcr_model_2, plottype = 'scores', comps = 1:2) # if still a pattern --> collinear (but do not show, so good for regression)
dev.off()

# LOADINGS
# extract loadings of variables
loadings <- loadings(pcr_model_2, ncomp = 1:2)
write.csv(loadings, file = 'pca/pcr_results/loadings_pcr_log_dt_nosn.csv', sep = ',')

# loading plot
png(file='pca/pcr_results/loadingsplot_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(pcr_model_2, 'loadings', comps = 1:2, legendpos = 'topright',
     labels = 'names') 
abline(h = 0)
dev.off()

# PREDICTIONS
# generate predictions for mn at each location based on components
predict <- (predict(pcr_model_2, ncomp = 1:2))
write.csv(predict, file = 'pca/pcr_results/predict_pcr_log_dt_nosn.csv', sep = ',')

# plot predicted vs measured variables of pc 1 and 2
png(file='pca/pcr_results/predplot_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
predplot(pcr_model_2, ncomp = 2, asp =1, line = T) # with 1:1 target line
dev.off()

# correlation plot
# correlation of each variable and selected pc
png(file='pca/pcr_results/corrplot_pcr_log_dt_nosn.png', width = 2800, height = 2800, res = 200)
plot(pcr_model_2, 'correlation', comps = 1:2
)
dev.off()


# SPATIAL -----

d <- read.csv('data/19012024/ak_rdata_formated.csv', header = T)

head(d)
str(d)
summary(d)
skim(d)

d <- d %>% 
  select(-c(X)) %>%
  mutate_at(c('b_t', 'sample_collect'), as.factor) %>%
  # conver to number
  mutate_at(c('y', 'x'), as.numeric)

# with lat and lon and raw data T

# top
dt <- d %>% 
  filter(b_t == 'T') %>% 
  mutate_at(c('y', 'x'), as.factor)


(mn_lat_lon <- dt %>%  
    ggplot(aes(lon, lat)) + 
    geom_point(aes(size=mn_ppb, colour = x)
    ) + 
    scale_size_continuous(range = c(2,14),
                          name = expression('Mn Concentration µg'~L^-1~'')) +
    scale_colour_manual(
      values = pal_d3('category20')(20)[c(12, 11, 13)],
      name = "Transect Number",
      labels = c("0", "10", '20'),
      guide = guide_legend(override.aes = list(size = 6))) +
    coord_equal() + 
    theme.Alex() + 
    
    theme(legend.position = 'right',
          axis.text.x = element_text(
            size = 14,
            colour = 'black',
            hjust = +0.6
          ),
          axis.text.y = element_text(size = 14, colour = 'black'),
          axis.title.x = element_text(size =16, face = "plain"),
          axis.title.y = element_text(size = 16, face = "plain")
    ) +
    theme(
      strip.text.x = element_text(
        size = 12, color = "black" 
      ),
      strip.background = element_rect(
        color="black", fill="white", linewidth = 1.5, linetype="solid"
      ),
      legend.title = element_text(size = 16, face = 'plain')) 
)

ggsave(mn_lat_lon, filename = 'spatial/mn_lat_lon_top.png',
       height = 20, width = 30, unit = 'cm')

# semi variogram

# select only relevant data columns for Mn analysis 
# focus only on instrument measured Mn concentrations
# only select values where a sample was collected (sample_collect =  0)
# only select X, Y, B_T, sample_collect, lat, lon, Mn_meas
# filter out remaining NA in mn (where it was below LOD)

d <- read.csv('data/19012024/r_coding_19012024.csv', header = T)

mn <- d %>% 
  select((c('x', 'y', 'B_T', 'sample_collect', 'lat', 'lon', 'X55_Mn_meas_conc_undil_ppb'))) %>% 
  mutate_at(c('B_T'), as.factor) %>% 
  rename_all(~stringr::str_replace(., 'X', '')) %>% 
  rename_all(~stringr::str_replace(., '55_', '')) %>% 
  rename_with(tolower) %>% 
  filter(!sample_collect == 0) %>% 
  filter(!mn_meas_conc_undil_ppb == 'NA') %>% 
  mutate(x = x + 10)

# check data

glimpse(mn)

# log transform
mn$mn_log <- log(mn$mn_meas_conc_undil_ppb)

# Kriging

# convert to spatial points data frame (SPDF)

mn2 <- mn

class(mn2)

str(mn2)

# specify which columns contain spatial coordinates (x and y here)
coordinates(mn2) <- ~ x + y
class(mn2)
str(mn2)

# access slots in spdf
bbox(mn2)
coordinates(mn2) %>%  glimpse
proj4string(mn2)
mn@data %>% glimpse

# fit a variogram

# sample variogram with log transformed mn 
lzn.vgm <- variogram(mn_log~1, mn2) 
png(file='spatial/semivariogramxy.png', width = 2800, height = 2800, res = 200)
plot(lzn.vgm, plot.numbers = T, xlim = c(0, 45))
dev.off()
