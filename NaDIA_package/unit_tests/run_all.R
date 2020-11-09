#Single tests
tests <- list.files("EMMA_package/unit_tests/", full.names = TRUE)
tests <- tests[grep("run_all", tests, invert = TRUE)]

#RUN: no errors means everything is O.K
tests_results <- lapply(tests, source)
