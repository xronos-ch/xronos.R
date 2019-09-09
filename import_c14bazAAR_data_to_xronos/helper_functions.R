add_time_columns <- function(x, time = get_time()) { x %>% dplyr::mutate(created_at = time, updated_at = time) }

get_time <- function() { format(Sys.time(), "%Y-%m-%d %H:%M:%OS3") }

get_table <- function(x, con) { 
  DBI::dbReadTable(con, x) %>% tibble::as_tibble() 
}

exists_in_db <- function(new_value, old_vector) {
  exists <- FALSE
  if ( new_value %in% old_vector ) {
    exists <- TRUE
  }
  return(exists)
}

get_new_id <- function(id_vector) {
  if ( length(id_vector) > 0 ) {
    id <- max(id_vector) + 1
  } else {
    id <- 1
  }
  return(id)
}

get_id <- function(new_value, old_vector, id_vector) {
  if ( is.na(new_value) ) {
    id <- NA
  } else if ( exists_in_db(new_value, old_vector) ) {
    id <- id_vector[new_value == old_vector][1]
  } else {
    id <- get_new_id(id_vector)
  }
  return(id)
}

add_simple_cal <- function(imp) {
  simple_cal_list <- pbapply::pblapply(imp$calprobdistr, function(x) {
    if (nrow(x) == 0) {
      return(data.frame(bp = NA, std = NA))
    }
    middle <- which.min(abs(cumsum(x$density) - 0.5))
    res_sum <- 0
    i <- 1
    while (res_sum < 0.95) {
      start <- middle - i
      stop <- middle + i
      if (start <= 0) {
        start <- 1
      }
      if (stop > nrow(x)) {
        stop <- nrow(x)
      }
      res_sum <- sum(x$density[start:stop], na.rm = T)  
      i <- i + 1
    }
    return(data.frame(bp = x$calage[middle], std = i))
  })
  simple_cal <- do.call(rbind, simple_cal_list)
  imp %<>% dplyr::mutate(
    cal_bp = simple_cal$bp,
    cal_std = simple_cal$std
  )
  return(imp)
}

random_alphanumeric_string <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}
