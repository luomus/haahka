branch <- Sys.getenv("BRANCH")

Sys.setenv("BRANCH" = "dev")

expect_identical(haahka_taxa()[[1, 1]], "CYGOLO")

Sys.setenv("BRANCH" = "main")

expect_identical(haahka_taxa()[[11, 1]], "BRALEU")

Sys.setenv("BRANCH" = branch)
