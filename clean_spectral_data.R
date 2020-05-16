library(here)

# read original spectra
df <- read.csv(here::here("data","all_spectra.csv"))

colnames(df)


# check consistency of species names  -------------------------------------
# are there any entries that are misspelled or duplicated? 



# standardize the bands  ------------------------------------------------
# spectra have different starting wavelengths (i.e. 381, 383, 385nm...)
# and increase in increments of 5nm. 
# linear interpolation to ensure that each spectrum has the same increments


# create train, evaluation, & test set ------------------------------------
# split multiple observations of individuals across sets.
# add a "set" column to the data ("train", "eval", "test")