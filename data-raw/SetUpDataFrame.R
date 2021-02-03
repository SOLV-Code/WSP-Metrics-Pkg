set.seed(1)
exampleData <- data.frame(Stock1 = sample(100,40),Stock2 = sample(2000,40),Stock3 = sample(500,40))
exampleData[29,3] <- NA
usethis::use_data(exampleData, overwrite = TRUE)
