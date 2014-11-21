sf12sample <- read.table(file = "sf12sample.txt",  as.is = TRUE, header = TRUE)
names(sf12sample) <- tolower(names(sf12sample))
sf12sample[,1] <- NULL
save("sf12sample", file = "../data/sf12sample.rda")
