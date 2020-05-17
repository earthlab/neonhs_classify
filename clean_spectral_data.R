library(here)
library(dplyr)
library(ggplot2)
library(stringr)

# read original spectra
all_spectra <- read.csv(here::here("data","all_spectra.csv"))

colnames(all_spectra)


# check consistency of species names  -------------------------------------

# are there any entries that are misspelled or duplicated? 

message(paste("Number of unique taxonID values:",
              length(unique(all_spectra$taxonID))))

message(paste("Number of unique scientificName values:",
              length(unique(all_spectra$scientificName))))

# how many taxon ranks are genus vs species? 

# add new column, taxonRank. 
# based on NEON taxonomic data: https://data.neonscience.org/apps/taxon 
# if scientificName contains "sp." , taxon rank is "genus". 
# otherwise, it's "species". 
all_spectra <- all_spectra %>% 
  dplyr::mutate(taxonRank = case_when(
    stringr::str_detect(scientificName, "sp.") ~ "genus",
    TRUE ~ "species"
  ))

# count the number of genus vs species spectra
all_spectra %>% 


  # visualize spectra -------------------------------------------------------

all_spectra %>%
  ggplot(aes(wavelength_nm, ifelse(mask, NA, reflectance), group = uid.x,
             # color each facet plot based on taxonRank
             color = taxonRank)) +
  geom_path(alpha = .2) +
  xlab('Wavelength (nm)') +
  ylab('Reflectance') +
  facet_wrap(~scientificName) + 
             # wrap long scientific names
             #,labeller = label_wrap_gen()) +
  theme(text = element_text(size=4)) + 
  theme_minimal()





# standardize the bands  ------------------------------------------------
# spectra have different starting wavelengths (i.e. 381, 383, 385nm...)
# and increase in increments of 5nm. 
# linear interpolation to ensure that each spectrum has the same increments


# create train, evaluation, & test set ------------------------------------
# split multiple observations of individuals across sets.
# add a "set" column to the data ("train", "eval", "test")