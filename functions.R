
# get full path within directories ----------------------------------------

get_full_path <- function(file, dir = "EBD") {
  
  require(glue)
  
  full_path <- if (dir == "EBD") {
    glue("EBD/{file}")
  }
  
  return(full_path)
  
}


# unzip EBD ---------------------------------------------------------------

# unzip EBD download if not done already

unzip_ebd <- function(download_type = "all", dir_out = "EBD") {
  
  # download_type = c("all", "no_unvetted", "no_sed", "no_unvetted_se")
  # dir_out is folder name where output files should be stored
  
  if (download_type == "all") {
  
    if (file.exists(path_ebd_main) & file.exists(path_sed)) {
      print("Data download already unzipped.")
    } else if (!file.exists(path_ebd_main) & !file.exists(path_sed)) {
      
      if (!file.exists(path_zip)) {
        print("Latest data download does not exist!")
      } else {
        unzip(zipfile = path_zip, exdir = dir_out, # don't add trailing slash in path
              files = c(file_ebd_main, file_sed))
        print("Data download unzipped.")
      }
      
    }
    
  } else if (download_type == "no_sed") {

    if (file.exists(path_ebd_main)) {
      print("Data download already unzipped.")
    } else if (!file.exists(path_ebd_main)) {
      
      if (!file.exists(path_zip)) {
        print("Latest data download does not exist!")
      } else {
        unzip(zipfile = path_zip, exdir = dir_out, # don't add trailing slash in path
              files = file_ebd_main)
        print("Data download unzipped.")
      }
      
    }
    
  }
  
}

