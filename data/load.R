library(readxl)
dat <- read_excel("mymerge135.xlsx")

## remove NA columns
dat <- dat[,!(colSums(is.na(dat)) == nrow(dat))]
