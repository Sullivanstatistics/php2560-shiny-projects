setwd("/home/oking/R/libraries")

pupld_2014 <- read.csv("pupld2014.csv")
pusum_2014 <- read.csv("pusum2014.csv")
pupld_2013 <- read.csv("pupld2013.csv")
pusum_2013 <- read.csv("pusum2013.csv")
pupld_2012 <- read.csv("pupld2012.csv")
pusum_2012 <- read.csv("pusum2012.csv")

pupld_2014 <- pupld_2014[order(pupld_2014$LIBNAME),]
pupld_2013 <- pupld_2013[order(pupld_2013$LIBNAME),]
pupld_2012 <- pupld_2012[order(pupld_2012$LIBNAME),]

save(pupld_2014,pupld_2013,pupld_2012,pusum_2014,pusum_2013,pusum_2012, file = "libraryData.rda")
