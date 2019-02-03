#!/usr/bin/env Rscript
library(googledrive)
library(tidyverse)

# FIXME: Add instructions on how to authorize with Google Drive

# Helper functions --------------------------------------------------------

# Download a file from Google Drive
#
# @param x dribble row
# @param path character string local path where to save the file
#
download_file <- function(x, path) {
  
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  # Build target file name. The format is XXX-YYYYYY.ext, where XXX are three 
  # digits, YYYYYY is the species abbreviation and ext should be jpg, though 
  # few pngs are there as well. YYYYYY should be in upper, but is delivered 
  # in lower as well. Make it all upper.
  body <- unlist(strsplit(x$name, "\\."))[1]
  ext <- unlist(strsplit(x$name, "\\."))[2]
  path <- file.path(path, paste0(toupper(body), ".", tolower(ext)))
  
  # Download file
  googledrive::drive_download(googledrive::as_id(x$id), path = path, 
                              overwrite = TRUE)
  return(invisible(NULL))
}

# Process Drive content ---------------------------------------------------

# Set the URL
"https://drive.google.com/drive/folders/1dJx9KHHTB0SsWfNeIPYT3oOzr6TaXht9" %>% 
  # Transform URL to id
  googledrive::as_id() %>% 
  # Get Drive directory object
  googledrive::drive_get() %>% 
  # List photo files in the directory
  googledrive::drive_ls() %>% 
  # Process each row (i.e. file) in the dribble rowwise
  dplyr::rowwise() %>% 
  # Download files
  dplyr::do(download_file(., path = "www/img/sp_images/org"))

            
