library(assertthat)
library(magick)
library(progress)

# Helper functions --------------------------------------------------------

resize_all <- function(src_path, dst_path) {
  
  # List all files
  photo_files <- list.files(src_path, full.names = TRUE)
  
  # Create a progress bar
  pb <- progress::progress_bar$new(
    format = "  resizing [:bar] :percent in :elapsed",
    total = length(photo_files), clear = FALSE, width= 60)
  
  resize_photo <- function(x, path = ".", width = 900) {
    
    assertthat::assert_that(width > 0,
                            msg = "Width must be positive integer")
    assertthat::validate_that(width < 2000,
                              msg = "Rescaling to a large value, probably not a good idea")
    
    pb$tick()
    
    if (path != "." && !file.exists(path)) {
      dir.create(path)
    }
    
    target_file <- file.path(path, basename(x))
    
    x %>% 
      magick::image_read() %>% 
      magick::image_scale(as.character(width)) %>% 
      magick::image_write(path = target_file, format = "jpg")
    return(invisible(NULL))
  }
  
  purrr::walk(photo_files, resize_photo, path = dst_path)
  
  return(invisible(NULL))
}

# Where are the photos?
src_path <- "www/img/sp_images/org"
dst_path <- "www/img/sp_images/"

resize_all(src_path, dst_path)

# Delete original files
unlink(src_path)
