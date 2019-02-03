#!/usr/bin/env Rscript
library(googledrive)
library(tidyverse)

# FIXME: Add instructions on how to authorize with Google Drive

# Download metadata spreadsheet from Google Drive and save as a CSV file
dst_csv <- "data/text_and_image_reference.csv"

dl <- googledrive::drive_download(
  as_id("1pkJtIYh_YL91hMS7QdWE7Di95XHY2n3q_NzxrUICbz0"), path = dst_csv, 
  overwrite = TRUE)
