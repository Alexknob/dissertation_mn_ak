
##%######################################################%##
#                                                          #
####      UV Spectrometry Analysis Script # Script for  ####
####       data processing of UV-Vis Spectrometer       ####
####      data for Total Organic Carbon determination   ####
####               # Dissertation Project               ####
####          in Ecological and Environmental           ####
####       Sciences with Management (BSc Hons) #        ####
####          Year 2023/24  # Date 01.02.2024           ####
####          # Author: Alexanra Knoblauch  #           ####
####            Contact: s2090259@ed.ac.uk #            ####
####            The University of Edinburgh             ####
#                                                          #
##%######################################################%##


# Data: Folder: UV 
## Files of absorbance data measured accross 250 to 665 nm wavelength spectrum
#  for soil pore waters around stream Dhu Strand, Loch Bradan, Scotland
## data is available at: https://github.com/Alexknob/dissertation_ees/tree/main/data/UV


# Libraries ----

# install required packages with pacman package

if(!require(pacman))install.packages("pacman")

# load all needed packages

pacman::p_load('tidyverse', 'scales',
               'ggthemes', 'ggeffects', 'broom',
               'skimr', 'ggExtra',
               'ggdist', 'shades', 'Hmisc', 'reshape',
               'ggpubr', 'report')

# Package explanations 

# library(tidyverse) # important tidy packages: dplyr (data manioulation), ggplot2 (data visualisation)
# library(ggthemes) # additional themes for ggplot
# library(ggeffects) # model predictions
# library(broom) # tidy up model summaries
# library(skimr) # data summaries
# library(ggExtra) # add extra features to ggplot
# library(ggdist) # additional ggplot functions
# library(shades) # colour shades and gradients
# library(Hmisc) # histogram plots
# library(reshape)
# library(ggpubr) # for easy ggplot visualisation
# library(report) # report statistical results


# Read in .txt files and convert to .csv files
# Following: https://stackoverflow.com/questions/35238391/create-a-loop-convert-txt-to-csv-in-r

txtdirectory <- 'data/UV/spectrometer_txt'
csvdirectory <- 'data/UV/spectrometer_csv'

txt_file_name <- list.files(txtdirectory, pattern = ".txt")

files.to.read <- paste(txtdirectory, txt_file_name, sep="/") 
files.to.write <- paste(csvdirectory, paste0(sub(".txt","", txt_file_name),".csv"), sep="/")

for (i in 1:length(files.to.read)) {
  
  temp <- (read.csv(files.to.read[i], header = TRUE, skip = 39, fill = TRUE))
  write.csv(temp, file = files.to.write[i])
  
}

# combine csv files ----

# create data frame with csv files
# Following: https://benwhalley.github.io/just-enough-r/multiple-raw-data-files.html

csv_files <- data.frame(filename = list.files('data/UV/spectrometer_csv/'))

# create column with complete file path

csv_file_path <- csv_files %>% 
  mutate (filepath = paste0('data/UV/spectrometer_csv/', filename))


# write function to read each csv file and add the filename

read.csv.and.filename <- function(filepath){
  
  # read csv
  read_csv(filepath) %>%
    # add file name
    mutate(filepath=filepath)
}

# import data for each file and combine into one dataframe with read.scv.and.filename function

csv_files <- csv_file_path %>% 
  # perform 'do' function and read.csv.and.filename function
  rowwise() %>% 
  do(., read.csv.and.filename(.$filepath))

csv_files_sorted <- csv_files %>% 
  # break filepath into x, y, b_t columns
  mutate_at('filepath', str_replace,'data/UV/spectrometer_csv/', '') %>% 
  mutate_at('filepath', str_replace,'.csv', '') %>% 
  # select specific wavelengths 250, 254, 252, 275, 295, 364, 365, 452, 465, 665 nm
  filter(Wavelength..nm. %in% c(250, 254, 252, 275, 295, 364, 365, 452, 465, 665)) %>% 
  # split  filepath into x, y, B_t and notes columns
  separate(filepath, c('x', 'y', 'b_t','notes'), sep = '-') %>% 
  select(-...1) %>% 
  pivot_wider(names_from = Wavelength..nm., values_from = Absorbance)

  
# Export csv file ----

write.csv(csv_files_sorted, 'data/UV/uv_spectrometer.csv')
