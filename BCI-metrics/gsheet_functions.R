# Update Google Sheet of BCI metrics


# params ----------------------------------------------------------------------------

our_gsheet <- "https://docs.google.com/spreadsheets/d/1ptmuVvQ7krxm8TlQQ7iz5Cr-YdngAGb53eewvhCvLcI/edit#gid=641296182"
# test "https://docs.google.com/spreadsheets/d/1-A8w6TXPJn4HDCozJHvENeww9Zx-vrFgba50Bjq-H5o/edit#gid=96990122"

sheet_prefix_prev <- glue('{PrevMonthLab}-{str_trunc(date_prevrel %>% year(), width = 2, side = "left", ellipsis = "")}')
sheet_prefix_cur <- glue('{CurMonthLab}-{str_trunc(CurYear, width = 2, side = "left", ellipsis = "")}')

# function -------------------------------------------------------------------------

# determine dimensions of range in GSheet based on data dimensions
get_gsheet_dim <- function(data, which_level, which_IN = "ebd") {
  
  if (!which_level %in% c("IN", "ST", "DT")) {
    stop("Invalid level!")
  }
  
  if (which_level == "IN") {
    
    if (which_IN == "ebd") {
      start_row <- 1
    } else if (which_IN == "website") {
      start_row <- 12
    }
    
    start_col <- 2 # row names
    
  } else if (which_level %in% c("ST", "DT")) {
    start_row <- 2
    start_col <- 1
  }
  
  n_rows <- (start_row) + dim(data)[1] # start row usually header
  n_cols <- (start_col - 1) + dim(data)[2]
  
  # in India sheet the website metrics are only being cleared (no input data) 
  # and also don't have header
  if (which_level == "IN") {
    if (which_IN == "website") n_rows = 14
  }
  
  first <- glue("{LETTERS[start_col]}{start_row}")
  last <- glue("{LETTERS[n_cols]}{n_rows}")
  return(glue("{first}:{last}"))
  
}


# write metrics to GSheet
write_metrics_sheet <- function(metric_data, which_level) {
  
  if (!which_level %in% c("IN", "ST", "DT")) {
    return('Please choose one of {"IN", "ST", "DT"} for which_level.')
  }

  sheet_suffix <- which_level
  
  sheet_name_prev <- glue("{sheet_prefix_prev}-{sheet_suffix}")
  sheet_name_cur <- glue("{sheet_prefix_cur}-{sheet_suffix}")
  
  # range of cells in GSheet to write to, based on which metric
  if (which_level == "IN") {
    sheet_range <- get_gsheet_dim(india_metrics, "IN", "ebd")
    website_range <- get_gsheet_dim(india_metrics, "IN", "website")
  } else if (which_level == "ST") {
    sheet_range <- get_gsheet_dim(state_metrics, "ST")
  } else if (which_level == "DT") {
    sheet_range <- get_gsheet_dim(district_metrics, "DT")
  }
  
  
  if (!sheet_name_cur %in% (gs4_get(our_gsheet) %>% pluck("sheets", "name"))) {
    
    # first, create a copy of appropriate old sheet
    # refer https://googlesheets4.tidyverse.org/reference/sheet_copy.html
    
    sheet_copy(from_ss = our_gsheet, .before = 1, # place as the first sheet
               from_sheet = sheet_name_prev, to_sheet = sheet_name_cur)
    
  } else {
    
    print(glue("Sheet of interest ({sheet_name_cur}) already exists. Rewriting range."))
    
  }

  # then write our current/new data to specific range 
  range_write(ss = our_gsheet, data = metric_data,
              sheet = sheet_name_cur, range = sheet_range, 
              col_names = TRUE, # we need col names (months) to be updated
              reformat = FALSE) # we want to retain existing formatting (conditional colours for YoY%)

  # on IN sheet, BCI website stats needs to be cleared cos PJ brings in separately
  if (which_level == "IN") {
    range_write(ss = our_gsheet, data = data.frame(matrix(NA, nrow = 3, ncol = 7)),
                sheet = sheet_name_cur, range = website_range, 
                col_names = FALSE, reformat = FALSE) 
  }
}
