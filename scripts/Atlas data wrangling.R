## DATA PREPARATION FOR ATLAS SUBMISSION ##
#' A requirement of the NSW scientific licence for the fieldschool is that we submit 
#' records of species sightings to the NSW BioNet atlas portal. Here I get our data
#' into the correct format for submission.

# Packages
pacman::p_load(tidyverse, readxl, janitor)

# Sites
gps <- read_xlsx("data/KCC new and old gps.xlsx") %>%
  clean_names() %>%
  select(c(name, new_name, lon, lat)) %>%
  mutate(new_name = as.character(new_name)) %>%
  # keep both sets of names
  pivot_longer(1:2) %>%
  rename(site = value) %>%
  select(-c(name)) %>%
  relocate(lat, .before = lon) %>%
  # add frog pond coords and general KCC coords
  rbind(
    data.frame(
      site = c("Veg_pond", "Saline_pond", "Quarry_pond", "KCC", "Beach_pond"),
      lat = c(-35.5438969, -35.5443686, -35.5458526, -35.5449546, -35.5440779),
      lon = c(150.3627293, 150.3708081, 150.3661521, 150.3752011, 150.3841933)
    )
  )

# Bird Data
bird <- read_xlsx("data/ENVS2018 kioloa bird data 2023.xlsx") %>%
  rbind(read_xlsx("data/ENVS2018 kioloa bird data 2024.xlsx")) %>%
  clean_names() 
  # remove records outside of sites, cannot be pinned to GPS locations
  select(-c(overhead)) %>%
  # tally counts
  mutate(count = rowSums(.[8:11], na.rm = TRUE)) %>%
  # keep only needed columns
  select(c(date, site, scientific_name, count, observer)) %>%
  # format date as date
  mutate(date = as_date(date)) %>%
  # add columns
  mutate(date2 = "",
         datum = "GDA94",
         GPS = "Yes") %>%
  # add gps data
  left_join(gps) %>%
  select(-c(site)) %>%
  # add additional columns
  mutate(accuracy = 100.0,
         location = "Kioloa Coastal Campus, 496 Murramarang Rd, Kioloa NSW 2539",
         notes = "Diurnal bird survey",
         observation = "O") %>%
  # reorder columns
  relocate(scientific_name, .before = date) %>%
  relocate(observer, .after = location) %>%
  relocate(count, .after = date2) %>%
  # rename observers
  mutate(observer = ifelse(observer == "Julian", "Julian Reid",
                           ifelse(observer == "Sho", "Shoshana Rapley",
                                  ifelse(observer %in% c("Bel", "Belinda"), "Belinda Wilson",
                                         observer))))

# Acoustic Data
acoustic <- read_xlsx("data/ENVS2018 kioloa acoustic data 2023.xlsx") %>%
  rbind(read_xlsx("data/ENVS2018 kioloa acoustic data 2024.xlsx")) %>%
  clean_names() %>%
  # keep only needed columns
  select(date, site, scientific_name) %>%
  # format date as date
  mutate(date = as_date(date)) %>%
  # add columns
  mutate(date2 = "",
         datum = "GDA94",
         GPS = "Yes") %>%
  # add gps data
  left_join(gps) %>%
  select(-c(site)) %>%
  # add additional columns
  mutate(accuracy = 100.0,
       location = "Kioloa Coastal Campus, 496 Murramarang Rd, Kioloa NSW 2539",
       notes = "",
       observer = "Students of ENVS2018",
       observation = "AR",
       count = "") %>%
  # reorder columns
  relocate(scientific_name, .before = date) %>%
  relocate(observer, .after = location) %>%
  relocate(count, .after = date2)

# Camera data
camera <- read_xlsx("data/ENVS2018 kioloa camera data 2023.xlsx") %>%
  rbind(read_xlsx("data/ENVS2018 kioloa camera data 2024.xlsx")) %>%
  clean_names() %>%
  # keep only needed columns
  select(date, site, scientific_name) %>%
  # format date as date
  mutate(date = as_date(date)) %>%
  # add columns
  mutate(date2 = "",
         datum = "GDA94",
         GPS = "Yes") %>%
  # add gps data
  left_join(gps) %>%
  select(-c(site)) %>%
  # add additional columns
  mutate(accuracy = 5.0,
         location = "Kioloa Coastal Campus, 496 Murramarang Rd, Kioloa NSW 2539",
         notes = "",
         observer = "Students of ENVS2018",
         observation = "Q",
         count = "") %>%
  # reorder columns
  relocate(scientific_name, .before = date) %>%
  relocate(observer, .after = location) %>%
  relocate(count, .after = date2)

# Herpetofauna data
herps <- read_xlsx("data/ENVS2018 kioloa data 2017-2024.xlsx", sheet = "Herpetofauna") %>%
  clean_names() %>%
  filter(year >= 2023) %>%
  # keep only needed columns
  mutate(count = as.numeric(abundance)) %>%
  select(year, site, scientific_name, count) %>%
  # add dates
  mutate(date = as_date(ifelse(year == 2023, "2023-09-15", "2024-09-12"))) %>%
  select(-c(year)) %>%
  # add columns
  mutate(date2 = "",
         datum = "GDA94",
         GPS = "Yes") %>%
  # add gps data
  left_join(gps) %>%
  select(-c(site)) %>%
  # add additional columns
  mutate(accuracy = 20.0,
         location = "Kioloa Coastal Campus, 496 Murramarang Rd, Kioloa NSW 2539",
         notes = "Diurnal herpetofauna survey",
         observer = "Shoshana Rapley",
         observation = "O") %>%
  # reorder columns
  relocate(scientific_name, .before = date) %>%
  relocate(observer, .after = location) %>%
  relocate(count, .after = date2)

# Combine data
atlas <- rbind(bird, acoustic, camera, herps)

write.csv(atlas, "data/atlas_submission_2023-2024.csv", row.names = FALSE)
