library(here)
library(dplyr)
library(ggplot2)

# read original spectra
all_spectra <- read.csv(here::here("data","all_spectra.csv"))

colnames(all_spectra)


# visualize spectra -------------------------------------------------------

# all_spectra %>%
#   ggplot(aes(wavelength_nm, ifelse(mask, NA, reflectance), group = uid.x)) + 
#   geom_path(alpha = .2) + 
#   xlab('Wavelength (nm)') + 
#   ylab('Reflectance') + 
#   facet_wrap(~taxonID) + 
#   theme_minimal()


# check consistency of species names  -------------------------------------
# are there any entries that are misspelled or duplicated? 

message(paste("Number of unique taxonID values:",
              length(unique(all_spectra$taxonID))))

message(paste("Number of unique scientificName values:",
              length(unique(all_spectra$scientificName))))





# standardize the bands  ------------------------------------------------
# spectra have different starting wavelengths (i.e. 381, 383, 385nm...)
# and increase in increments of 5nm. 
# linear interpolation to ensure that each spectrum has the same increments


# create train, evaluation, & test set ------------------------------------
# split multiple observations of individuals across sets.
# add a "set" column to the data ("train", "eval", "test")