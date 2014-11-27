sf36sample <- read.table(file = "sf36sample.txt",
                         stringsAsFactors = FALSE)[,-1]
sf36sample <- as.data.frame(apply(sf36sample, 2, as.integer))
names(sf36sample) <- c("GH1", "HT", "PF01", "PF02", "PF03", "PF04", "PF05",
                       "PF06", "PF07", "PF08", "PF09", "PF10", "RP1",
                       "RP2", "RP3", "RP4", "RE1", "RE2", "RE3", "SF1",
                       "BP1", "BP2", "VT1", "MH1", "MH2", "MH3", "VT2",
                       "MH4", "VT3", "MH5", "VT4", "SF2", "GH2", "GH3",
                       "GH4", "GH5") 
save("sf36sample", file = "../data/sf36sample.rda")
