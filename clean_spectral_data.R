library(here)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# read original spectra, derived from NEON hyperspectral imagery for mapped stems
# created by mbjoseph: https://gist.github.com/mbjoseph/5c18781e508460e14f64193571b98b7d 
# columns: 
#     individualID --> unique identifier for each individual plant, NEON.PLA.D##.SITE.##### 
#                      (an individual can have multiple measurements over time)
#     date.x --> date of plant measurement/observation
#     uid.x --> unique identifier for the record
#     namedLocation --> name of measurement location in NEON database
#     adjEasting, adjNorthing --> UTM coordinates of mapped stem 
#     plantStatus --> physical status of the individual (live, dead, lost)
#     taxonID --> species code
#     scientificName --> scientific name, associated with taxonID,
#                        lowest taxonomic rank that can be determined.
#     band_idx --> number of the spectral band (band1, band 2, ... band 426)
#     wavelength_nm --> band wavelength in units of nm
#     reflectance --> decimal reflectance [0,1] extracted from NEON NIS 
#                     hyperspectral imagery collected during same year
#     mask --> TRUE/LFALSE to identify water absorption bands
all_spectra <- read.csv(here::here("data","all_spectra.csv"))

colnames(all_spectra)

# check number of individuals and observations ----------------------------

message(paste("Number of unique individualID values:",
              length(unique(all_spectra$individualID))))   

message(paste("Number of unique uid.x values:",
              length(unique(all_spectra$uid.x))))


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
  dplyr::distinct(uid.x, .keep_all=TRUE) %>%
  dplyr::group_by(taxonRank) %>% 
  dplyr::tally()

# count the number of spectra per species (unique indvdls)
species_count <- all_spectra %>% 
  dplyr::distinct(uid.x, .keep_all=TRUE) %>%
  dplyr::group_by(scientificName) %>% 
  dplyr::tally()

# are there are any species classes that should be merged 
species_var <- all_spectra %>% 
  # first, duplicate the scientificName
  dplyr::mutate(scientificNameTemp = scientificName) %>% 
  # separate into genus and species using space as delimeter 
  tidyr::separate(scientificNameTemp, c("genus", "speciesTemp"), " ", extra = "merge") %>% 
  # separate into species and variation, if present
  tidyr::separate(speciesTemp, c("species", "variation"), "var.", extra = "merge") %>% 
  # select certain columns to assess
  dplyr::select(c(scientificName, genus, species, variation)) %>% 
  dplyr::distinct(scientificName, .keep_all=TRUE) %>% 
  # alphabetical order
  dplyr::arrange(scientificName) 
  
# count the number of spectra with species with multiple variations
species_count %>% 
  dplyr::filter(stringr::str_detect(scientificName, "Acer saccharum Marshall") | 
                stringr::str_detect(scientificName, "Cercis canadensis L.") | 
                stringr::str_detect(scientificName, "Pseudotsuga menziesii")) %>% 
  dplyr::summarise(sum(n))
                                         


# visualize spectra -------------------------------------------------------

# based on Max's Gist: https://gist.github.com/mbjoseph/5c18781e508460e14f64193571b98b7d 

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
  scale_color_manual(values = c("#e6550d","#1c9099")) + 
  theme_minimal()





# one individualID with numerous uid.x and dates --------------------------

spectrum <- all_spectra %>% 
  dplyr::filter(individualID == "NEON.PLA.D01.BART.02912") 

spectrum %>% 
  ggplot(aes(x = wavelength_nm, y = reflectance, group = uid.x)) +
  geom_line(aes(color = uid.x, linetype = uid.x), 
            alpha = 0.5, size = 1) +
  labs(title = spectrum$individualID) + 
  theme_bw()

# unique uid.x values: 
as.character(unique(spectrum$uid.x))

# unique date values: 
as.character(unique(spectrum$date.x))

# number of unique reflectance values, <426 
length(unique(spectrum$reflectance))



# Explore the bands ------------------------------------------------------

# how many different minimum wavelengths in the data set
#count_min_wavelengths <- all_spectra %>% 
#  dplyr::group_by(uid.x) %>% 
#  dplyr::summarise(min_wavelength_nm = min(wavelength_nm)) %>% 
#  dplyr::count(min_wavelength_nm) 

count_min_wavelengths <- all_spectra %>%
  dplyr::filter(band_idx == "band1") %>% 
  dplyr::group_by(wavelength_nm) %>%
  dplyr::count(wavelength_nm)
  

# most spectra start at 381 with increments of 5nm (381, )386, ... 2510)  

# find an individualID that starts with each of these wavelength band1 values
all_spectra$individualID[all_spectra$wavelength_nm == 381][1]
# 381 --> "NEON.PLA.D16.ABBY.01037"
# 382 --> "NEON.PLA.D16.ABBY.01515"
# 384 --> "NEON.PLA.D01.BART.02912"
# 347 --> "NEON.PLA.D20.PUUM.08313"   weird? 

# select a specific spectrum using individualID
spectrum <- all_spectra %>% 
  dplyr::filter(individualID == "NEON.PLA.D20.PUUM.08313") 

# plot individual spectrum
ggplot(data = spectrum) +
  geom_line(aes(x = wavelength_nm, y = reflectance)) +
  labs(title = spectrum$individualID) + 
  theme_bw()

# plot all 4 spectra to see their starting/ending wavelengths
all_spectra %>% 
  dplyr::filter(individualID %in% c("NEON.PLA.D20.PUUM.08313", 
                                    "NEON.PLA.D16.ABBY.01037",
                                    "NEON.PLA.D16.ABBY.01515",
                                    "NEON.PLA.D01.BART.02912")) %>%
  ggplot(aes(x = wavelength_nm, y = reflectance, group = uid.x)) +
  geom_line(aes(color = individualID, linetype = uid.x), 
            alpha = 0.4) +
  labs() + 
  #lims(x = c(340,390), y = c(0,0.5)) +  # beginning of spectra
  lims(x = c(2470,2520), y = c(0,0.5)) + # end of spectra
  theme_bw()



# standardize the bands  ------------------------------------------------
# spectra have different starting wavelengths (i.e. 381, 383, 385nm...)
# and increase in increments of 5nm. 
# linear interpolation to ensure that each spectrum has the same increments


# get a spectrum starting on 381, the target wavelengths
wl_target <- all_spectra %>% 
  dplyr::filter(individualID == "NEON.PLA.D16.ABBY.01037") %>% 
  dplyr::select(band_idx, wavelength_nm, reflectance)

# get a spectrum starting on 384, with values to be interpolated
wl_orig <- all_spectra %>% 
  dplyr::filter(individualID == "NEON.PLA.D01.BART.02912") %>% 
  dplyr::select(band_idx, wavelength_nm, reflectance) %>% 
  dplyr::mutate(spectra_type = "orig")

wl_interp <- approx(x = wl_orig$wavelength_nm,
                    y = wl_orig$reflectance,
                    xout = wl_target$wavelength_nm,
                    method = "linear")



# -- in progress -----

# rename the columns to plot interplated reflectance values 
wl_plot <- data.frame(band_idx = wl_orig$band_idx,
                      wavelength_nm = wl_interp$x,
                      reflectance = wl_interp$y,
                      spectra_type = "interpolated") %>% 
  cbind(wl_orig)

# plot original and interpolated spectra

interp_plt <- wl_orig %>% # original
  ggplot(aes(x = wavelength_nm, y = reflectance)) +
  geom_point(aes(color = spectra_type), 
            alpha = 0.5) +
  theme_bw()


interp_plt + # interpolated
  geom_point(wl_plot, aes(x = wavelength_nm, y = reflectance, color = spectra_type), 
            alpha = 0.5) +
  theme_bw()









# create train, evaluation, & test set ------------------------------------
# split multiple observations of individuals across sets.
# add a "set" column to the data ("train", "eval", "test")