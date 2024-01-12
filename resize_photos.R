suppressPackageStartupMessages({

  library(magick, warn.conflicts = FALSE, quietly = TRUE)
  library(utils, warn.conflicts = FALSE, quietly = TRUE)

})

resize_photo <- function(x, path = ".") {

  if (path != "." && !file.exists(path)) {

    dir.create(path)

  }

  target_file <- file.path(path, basename(x))

  x <- magick::image_read(x)
  x <- magick::image_scale(x, "900")
  x <- magick::image_write(x, path = target_file, format = "jpg")

  magick::image_destroy(x)

}

resize_all <- function(src_path, dst_path) {

  photo_files <- list.files(src_path, full.names = TRUE)

  for (f in photo_files) {

    resize_photo(f, path = dst_path)

  }

}

src_path <- "var/data/sp_images/org/"
dst_path <- "var/data/sp_images/resized/"

resize_all(src_path, dst_path)

utils::zip(
  "var/data/sp_images.zip",
  list.files("var/data/sp_images/resized", full.names = TRUE),
  flags = "-j"
)
