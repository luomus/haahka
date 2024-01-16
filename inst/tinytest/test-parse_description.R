description <- readRDS("cygcyg-desc.rds")

description <- parse_description(description, "en")

expect_inherits(description, "character")
