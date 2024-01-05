library(assertthat, warn.conflicts = FALSE, quietly = TRUE)
library(here, warn.conflicts = FALSE, quietly = TRUE)
library(magick, warn.conflicts = FALSE, quietly = TRUE)
library(progress, warn.conflicts = FALSE, quietly = TRUE)
library(utils, warn.conflicts = FALSE, quietly = TRUE)

resize_photo <- function(x, path = ".", width = 900, pb) {

  assertthat::assert_that(
    width > 0, msg = "Width must be positive integer"
  )

  assertthat::validate_that(
    width < 2000, msg = "Rescaling to a large value, probably not a good idea"
  )

  pb[["tick"]]()

  if (path != "." && !file.exists(path)) {

    dir.create(path)

  }

  target_file <- file.path(path, basename(x))

  x <- magick::image_read(x)
  x <- magick::image_scale(x, as.character(width))
  x <- magick::image_write(x, path = target_file, format = "jpg")

  invisible(NULL)

}

resize_all <- function(src_path, dst_path) {

  photo_files <- list.files(src_path, full.names = TRUE)

  pb <- progress::progress_bar[["new"]](
    format = "  resizing [:bar] :percent in :elapsed",
    total = length(photo_files), clear = FALSE, width = 60
  )

  for (f in photo_files) {

    resize_photo(f, path = dst_path, pb = pb)

  }

  invisible(NULL)

}

src_path <- here::here("var/data/sp_images/org/")
dst_path <- here::here("var/data/sp_images/resized/")

resize_all(src_path, dst_path)

utils::zip(
  "var/data/sp_images.zip",
  list.files("var/data/sp_images/resized", full.names = TRUE),
  flags = "-j"
)
