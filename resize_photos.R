library(assertthat, warn.conflicts = FALSE, quietly = TRUE)
library(here, warn.conflicts = FALSE, quietly = TRUE)
library(magick, warn.conflicts = FALSE, quietly = TRUE)
library(progress, warn.conflicts = FALSE, quietly = TRUE)
library(purrr, warn.conflicts = FALSE, quietly = TRUE)
library(utils, warn.conflicts = FALSE, quietly = TRUE)

resize_all <- function(src_path, dst_path) {

  photo_files <- list.files(src_path, full.names = TRUE)

  pb <- progress::progress_bar$new(
    format = "  resizing [:bar] :percent in :elapsed",
    total = length(photo_files), clear = FALSE, width = 60
  )

  resize_photo <- function(x, path = ".", width = 900) {

    assertthat::assert_that(
      width > 0, msg = "Width must be positive integer"
    )

    assertthat::validate_that(
      width < 2000, msg = "Rescaling to a large value, probably not a good idea"
    )

    pb$tick()

    if (path != "." && !file.exists(path)) {

      dir.create(path)

    }

    target_file <- file.path(path, basename(x))

    magick::image_read(x) %>%
      magick::image_scale(as.character(width)) %>%
      magick::image_write(path = target_file, format = "jpg")

    invisible(NULL)

  }

  purrr::walk(photo_files, resize_photo, path = dst_path)

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
