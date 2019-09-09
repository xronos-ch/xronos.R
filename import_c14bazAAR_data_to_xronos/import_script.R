library(magrittr)
source("~/agora/xronos.rails/R_add_data_to_db/helper_functions.R")

#### prepare data #### 
#imp <- c14bazAAR::get_aDRAC()
imp <- c14bazAAR::get_RADON()
imp %<>% c14bazAAR::calibrate(choices = "calprobdistr")
imp %<>% add_simple_cal()
imp %<>% c14bazAAR::finalize_country_name()

#### read env variables ####
env_vars <- readLines("~/agora/xronos.rails/env_variables.env")
env_vars_vals <- env_vars %>% strsplit("=") %>% lapply(., function(x) x[[2]]) %>% unlist()
user <- env_vars_vals[1]
password <- env_vars_vals[2]
dbname <- env_vars_vals[3]

#### make connection to database ####
con <- DBI::dbConnect(
  RPostgres::Postgres(), 
  dbname = dbname, 
  host = '127.0.0.1',
  port = 5432,
  user = user,
  password = password
)

#### write to db loop ####
pbapply::pblapply(
  1:nrow(imp), function(i, imp, con) {
  
    # get one row of input table
    cur <- imp[i,]
  
    #### preparing variables ####
  
    # measurements
    measurements_cur <- get_table("measurements", con)
    measurements.labnr <- if ("labnr" %in% colnames(cur)) { cur$labnr } else { NA }
    if (is.na(measurements.labnr) | exists_in_db(measurements.labnr, measurements_cur$labnr)) {
      return(i)
    }
    measurements.id <- get_id(measurements.labnr, measurements_cur$labnr, measurements_cur$id)
      
    # arch_objects
    arch_objects_cur <- get_table("arch_objects", con)
    arch_objects.id <- get_new_id(arch_objects_cur$id)
  
    # c14_measurements
    c14_measurements_cur <- get_table("c14_measurements", con)
    c14_measurements.bp <- cur$c14age
    c14_measurements.std <- cur$c14std
    c14_measurements.cal_bp <- cur$cal_bp
    c14_measurements.cal_std <- cur$cal_std
    c14_measurements.delta_c13 <- if ("c13val" %in% colnames(cur)) { cur$c13val } else { NA }
    c14_measurements.delta_c13_std <- NA
    c14_measurements.method <- if ("method" %in% colnames(cur)) { cur$method } else { NA }
    c14_measurements.id <- get_new_id(c14_measurements_cur$id)
    
    # countries
    countries_cur <- get_table("countries", con)
    countries.name <- cur$country_final
    countries.id <- get_id(countries.name, countries_cur$name, countries_cur$id)
    
    # materials
    materials_cur <- get_table("materials", con)
    materials.name <- if ("material" %in% colnames(cur)) { cur$material } else { NA }
    materials.id <- get_id(materials.name, materials_cur$name, materials_cur$id)
    
    # measurements_references
    measurements_references_cur <- get_table("measurements_references", con)
    
    # on_site_object_positions
    on_site_object_positions_cur <- get_table("on_site_object_positions", con)
    on_site_object_positions.feature <- if ("feature" %in% colnames(cur)) { cur$feature } else { NA }
    on_site_object_positions.id <- get_new_id(on_site_object_positions_cur$id)
    
    # periods
    periods_cur <- get_table("periods", con)
    periods.name <- if ("period" %in% colnames(cur)) { cur$period } else { NA }
    periods.id <- get_id(periods.name, periods_cur$name, periods_cur$id)
  
    # periods_site_phases
    periods_site_phases_cur <- get_table("periods_site_phases", con)
    
    # physical_locations
    physical_locations_cur <- get_table("physical_locations", con)
    
    # references
    references_cur <- get_table("references", con)
    references.short_refs <- if ("shortref" %in% colnames(cur)) { 
      if (cur$sourcedb == "aDRAC") {
        cur$shortref %>% 
          gsub("\\:[^;]+(\\;|$)", ";", .) %>%
          gsub("\\;$", "", .) %>%
          strsplit(., ";") %>%
          unlist %>%
          trimws()
      } else if (cur$sourcedb == "RADON") {
        cur$shortref %>% 
          strsplit(., ";") %>%
          unlist %>%
          gsub(",.*", "", .) %>%
          trimws()
      } else {
        NA
      }
    } else { NA }
  
    refcurref <- references_cur$short_ref
    refcurid <- references_cur$id
    references.ids <- c()
    for (i in 1:length(references.short_refs)) {
      nid <- get_id(references.short_refs[i], refcurref, refcurid)
      references.ids <- append(references.ids, nid)
      if (!is.na(nid)) {
        refcurid <- append(refcurid, nid)
        refcurref <- append(refcurref, references.short_refs[i])
      }
    }
    
    # samples
    samples_cur <- get_table("samples", con)
    samples.id <- get_new_id(samples_cur$id)
    
    # site_phases
    site_phases_cur <- get_table("site_phases", con)
    site_phases.name <- if ("site" %in% colnames(cur) & !is.na(cur$site)) { cur$site } else { 
      paste0("unknown site ", random_alphanumeric_string())
    }
    site_phases.id <- get_id(site_phases.name, site_phases_cur$name, site_phases_cur$id)
    
    # site_phases_typochronological_units
    site_phases_typochronological_units_cur <- get_table("site_phases_typochronological_units", con)
    
    # site_types
    site_types_cur <- get_table("site_types", con)
    site_types.name <- if ("sitetype" %in% colnames(cur)) { cur$sitetype } else { NA }
    site_types.id <- get_id(site_types.name, site_types_cur$name, site_types_cur$id)
    
    # sites
    sites_cur <- get_table("sites", con)
    sites.name <- site_phases.name
    sites.lat <- if ("lat" %in% colnames(cur)) { cur$lat %>% round(4) } else { NA }
    sites.lng <- if ("lon" %in% colnames(cur)) { cur$lon %>% round(4) } else { NA }
    sites.id <- get_id(sites.name, sites_cur$name, sites_cur$id)
    
    # species
    species_cur <- get_table("species", con)
    species.name <- if ("species" %in% colnames(cur)) { cur$species } else { NA }
    species.id <- get_id(species.name, species_cur$name, species_cur$id)
    
    # typochronological_units
    typochronological_units_cur <- get_table("typochronological_units", con)
    typochronological_units.name <- if ("culture" %in% colnames(cur)) { cur$culture } else { NA }
    typochronological_units.id <- get_id(typochronological_units.name, typochronological_units_cur$name, typochronological_units_cur$id)
    
    #### writing tables ####
    
    # arch_objects
    DBI::dbWriteTable(
      con, "arch_objects", 
      tibble::tibble(
        id = arch_objects.id,
        material_id = materials.id,
        species_id = species.id,
        on_site_object_position_id = on_site_object_positions.id,
        site_phase_id = site_phases.id
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% arch_objects_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('arch_objects_id_seq', (SELECT MAX(id) FROM arch_objects));"
    )
  
    # c14_measurements
    DBI::dbWriteTable(
      con, "c14_measurements", 
      tibble::tibble(
        id = c14_measurements.id,
        bp = c14_measurements.bp,
        std = c14_measurements.std,
        cal_bp = c14_measurements.cal_bp,
        cal_std = c14_measurements.cal_std,
        delta_c13 = c14_measurements.delta_c13,
        delta_c13_std = c14_measurements.delta_c13_std,
        method = c14_measurements.method
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% c14_measurements_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('c14_measurements_id_seq', (SELECT MAX(id) FROM c14_measurements));"
    )
    
    # countries
    DBI::dbWriteTable(
      con, "countries", 
      tibble::tibble(
        id = countries.id,
        name = countries.name
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% countries_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('countries_id_seq', (SELECT MAX(id) FROM countries));"
    )
    
    # materials
    DBI::dbWriteTable(
      con, "materials", 
      tibble::tibble(
        id = materials.id,
        name = materials.name
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% materials_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('materials_id_seq', (SELECT MAX(id) FROM materials));"
    )
    
    # samples
    DBI::dbWriteTable(
      con, "samples", 
      tibble::tibble(
        id = samples.id,
        arch_object_id = arch_objects.id
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% samples_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('samples_id_seq', (SELECT MAX(id) FROM samples));"
    )
    
    # measurements
    DBI::dbWriteTable(
      con, "measurements", 
      tibble::tibble(
        id = measurements.id,
        labnr = measurements.labnr,
        sample_id = samples.id,
        lab_id = NA,
        c14_measurement_id = c14_measurements.id
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% measurements_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('measurements_id_seq', (SELECT MAX(id) FROM measurements));"
    )
    
    # measurements_references
    DBI::dbWriteTable(
      con, "measurements_references", 
      tibble::tibble(
        measurement_id = measurements.id,
        reference_id = references.ids
      ) %>%
        dplyr::filter(!is.na(measurement_id) & !is.na(reference_id)) %>%
        rbind(measurements_references_cur) %>%
        dplyr::distinct(),
      append = F,
      overwrite = T
    )
    
    # on_site_object_positions
    DBI::dbWriteTable(
      con, "on_site_object_positions", 
      tibble::tibble(
        id = on_site_object_positions.id,
        feature = on_site_object_positions.feature,
        coord_reference_system = NA,
        coord_X = NA,
        coord_Y = NA,
        coord_Z = NA,
        feature_type_id = NA
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% on_site_object_positions_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('on_site_object_positions_id_seq', (SELECT MAX(id) FROM on_site_object_positions));"
    )
    
    # periods
    DBI::dbWriteTable(
      con, "periods", 
      tibble::tibble(
        id = periods.id,
        name = periods.name,
        approx_start_time = NA,
        approx_end_time = NA,
        parent_id = NA 
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% periods_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('periods_id_seq', (SELECT MAX(id) FROM periods));"
    )
    
    # periods_site_phases
    DBI::dbWriteTable(
      con, "periods_site_phases", 
      tibble::tibble(
        site_phase_id = site_phases.id,
        period_id = periods.id
      ) %>% 
        dplyr::filter(!is.na(site_phase_id) & !is.na(period_id)) %>%
        rbind(periods_site_phases_cur) %>%
        dplyr::distinct(),
      append = F,
      overwrite = T
    )
    
    # physical_locations
    DBI::dbWriteTable(
      con, "physical_locations", 
      tibble::tibble(
        site_id = sites.id,
        country_id = countries.id
      ) %>% 
        add_time_columns() %>%
        dplyr::filter(!is.na(site_id) & !is.na(country_id)) %>%
        rbind(physical_locations_cur) %>%
        dplyr::distinct(),
      append = F,
      overwrite = T
    )
    
    # references
    DBI::dbWriteTable(
      con, "references", 
      tibble::tibble(
        id = references.ids,
        short_ref = references.short_refs,
        bibtex = NA
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% references_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('references_id_seq', (SELECT MAX(id) FROM \"references\"));"
    )
    
    # site_phases
    DBI::dbWriteTable(
      con, "site_phases", 
      tibble::tibble(
        id = site_phases.id,
        name = site_phases.name,
        approx_start_time = NA,
        approx_end_time = NA,
        site_id = sites.id,
        site_type_id = site_types.id
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% site_phases_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('site_phases_id_seq', (SELECT MAX(id) FROM site_phases));"
    )
    
    # site_phases_typochronological_units
    DBI::dbWriteTable(
      con, "site_phases_typochronological_units", 
      tibble::tibble(
        site_phase_id = site_phases.id,
        typochronological_unit_id = typochronological_units.id
      )  %>% 
        dplyr::filter(!is.na(site_phase_id) & !is.na(typochronological_unit_id)) %>%
        rbind(site_phases_typochronological_units_cur) %>%
        dplyr::distinct(),
      append = F,
      overwrite = T
    )
    
    # site_types
    DBI::dbWriteTable(
      con, "site_types", 
      tibble::tibble(
        id = site_types.id,
        name = site_types.name,
        description = NA
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% site_types_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('site_types_id_seq', (SELECT MAX(id) FROM site_types));"
    )
    
    # sites
    DBI::dbWriteTable(
      con, "sites", 
      tibble::tibble(
        id = sites.id,
        name = sites.name,
        lat = sites.lat,
        lng = sites.lng,
        country_id = countries.id
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% sites_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('sites_id_seq', (SELECT MAX(id) FROM sites));"
    )
    
    # species
    DBI::dbWriteTable(
      con, "species", 
      tibble::tibble(
        id = species.id,
        name = species.name
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% species_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('species_id_seq', (SELECT MAX(id) FROM species));"
    )
    
    # typochronological_units
    DBI::dbWriteTable(
      con, "typochronological_units", 
      tibble::tibble(
        id = typochronological_units.id,
        name = typochronological_units.name,
        approx_start_time = NA,
        approx_end_time = NA,
        parent_id = NA 
      ) %>% 
        add_time_columns() %>% 
        dplyr::filter(!is.na(id)) %>%
        dplyr::filter(!(id %in% typochronological_units_cur$id)),
      append = T
    )
    DBI::dbExecute(
      con,
      "SELECT setval('typochronological_units_id_seq', (SELECT MAX(id) FROM typochronological_units));"
    )
    
    return(i)
    
  },
  imp = imp,
  con = con
)

DBI::dbDisconnect(con)

