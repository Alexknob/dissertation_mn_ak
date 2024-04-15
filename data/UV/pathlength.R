
# R Script to calculate optical pathlength of 96 well microplate 
# Pathlength correction to correct measured plate reader absorbance to 1 cm cuvette absorbance


# Libraries ----

# Use of fpcountr package

# Csibra E. 2021. FPCountR: Fluorescent protein calibration for plate readers. R package. 
# doi: 10.5281/zenodo.5760028 GitHub page: https://github.com/ec363/fpcountr

install.packages('remotes')
remotes::install_github('ec363/fpcountr')

library(fpcountr)

# Determine pathlength ----

# for 360 microlitres used
get_pathlength(360, plot = TRUE, outfolder = 'data/UV/pathlength_360.pdf')

# for 340 microlitres used
get_pathlength(340, plot = TRUE, outfolder = 'data/UV/pathlength_340.pdf')
