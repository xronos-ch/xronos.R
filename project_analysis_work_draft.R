library(magrittr)

#### basic data preparation ####

# get first radiocarbon date selection
raw_data <- c14bazAAR::get_all_dates() %>%
  dplyr::filter(
    sourcedb %in% c("CALPAL", "CONTEXT", "EUBAR", "EUROEVOL", "Palmisano", "RADON", "RADON-B") 
  )

# remove dates without coordinates
raw_data_with_coords <- raw_data %>% dplyr::filter(
  !is.na(lat) & !is.na(lon) 
)

# fix country names
raw_data_fixed_countries <- raw_data_with_coords %>% c14bazAAR::finalize_country_name()

# filter dates to only include dates from Italy and Switzerland
raw_data_italy_switzerland <- raw_data_fixed_countries %>% dplyr::filter(country_final %in% c("Switzerland", "Italy"))

# remove duplicates
raw_data_without_duplicates <- raw_data_italy_switzerland %>% c14bazAAR::remove_duplicates(
  preferences = c("Palmisano", "RADON", "RADON-B", "EUBAR", "CONTEXT", "EUROEVOL", "CALPAL")
)

# remove dates without age information
raw_data_with_age <- raw_data_without_duplicates %>%
  # remove dates without age
  dplyr::filter(!is.na(c14age) & !is.na(c14std)) %>%
  # remove dates outside of theoretical calibration range
  dplyr::filter(!(c14age < 71) & !(c14age > 46401))

# calibrate dates
# Calibration with Bchron could be broken: https://github.com/andrewcparnell/Bchron/pull/12
threshold <- (1 - 0.9545) / 2
raw_data_calibrated <- raw_data_with_age %>%
  dplyr::mutate(
    # add list column with the age density distribution for every date
    calage_density_distribution = Bchron::BchronCalibrate(
      ages      = raw_data_with_age$c14age,
      ageSds    = raw_data_with_age$c14std,
      calCurves = rep("intcal13", nrow(raw_data_with_age)),
      eps       = 1e-06
    ) %>%
      # transform BchronCalibrate result to a informative tibble
      # this tibble includes the years, the density per year,
      # the normalized density per year and the information,
      # if this year is in the two_sigma range for the current date
      pbapply::pblapply(
        function(x) {
          x$densities %>% cumsum -> a      # cumulated density
          bottom <- x$ageGrid[which(a <= threshold) %>% max]
          top <- x$ageGrid[which(a > 1-threshold) %>% min]
          tibble::tibble(
            age = x$ageGrid,
            dens_dist = x$densities,
            norm_dens = x$densities/max(x$densities),
            two_sigma = x$ageGrid >= bottom & x$ageGrid <= top
          )
        }
      )
    )

# transform calBP age to calBC
bol <- 1950
raw_data_calibrated$calage_density_distribution %<>% lapply(
  function(x) {
    x$age = -x$age + bol
    return(x)
  }
)

# remove dates outsite of the timeframe of interest
raw_data_in_time_of_interest <- raw_data_calibrated %>%
  dplyr::mutate(
    in_time_of_interest =
      purrr::map(calage_density_distribution, function(x){
        any(
          x$age >= -10000 &
            x$age <= -200 &
            x$two_sigma
        )
      }
    )
  ) %>%
    dplyr::filter(
      in_time_of_interest == TRUE
    ) %>%
    dplyr::select(-in_time_of_interest)

# fixing encoding
raw_data_utf8 <- raw_data_in_time_of_interest %>%
  dplyr::mutate_if(is.character, function(x) {
    iconv(x, "UTF-8", "UTF-8", sub='')
  })

# store result dataset
c14dates <- raw_data_utf8
save(c14dates, file = "data/c14dates.RData")



#### get dates in 50km radius of pollen samples ####

# load c14 dates
load("data/c14dates.RData")

c14dates_sf <- c14dates %>% c14bazAAR::as.sf()
# c14dates_sf %>% mapview::mapview(zcol = "data.c14age")

# transform points to projected coordinate system
c14dates_sf_7794 <- c14dates_sf %>% st_transform(crs = 7794)

# generate random points for pollen samples
# can be replaced with the real data when available
coords <- dplyr::sample_n(as.data.frame(sf::st_coordinates(c14dates_sf_7794)), 5) %>%
  dplyr::mutate(pollen_region = LETTERS[1:5])
pollen_cores <- sf::st_as_sf(
  coords,
  coords = c("X", "Y"), 
  crs = 7794
)
# pollen_cores %>% mapview::mapview()

# generate buffers around pollen cores
pollen_areas <- sf::st_buffer(pollen_cores, 30000)
# pollen_areas %>% mapview::mapview()

# find dates in regions with an intersection operation
inter_dates_pollen_areas <- sf::st_intersection(c14dates_sf_7794, pollen_areas)

# transform sf object back to c14_date_list 
colnames(inter_dates_pollen_areas) <- gsub("data\\.", "", colnames(inter_dates_pollen_areas))
dates_per_pollen_area <- inter_dates_pollen_areas %>% 
  sf::st_set_geometry(NULL) %>%
  c14bazAAR::as.c14_date_list()

# preparing result dataset
save(dates_per_pollen_area, file = "data/dates_per_pollen_area.RData")






