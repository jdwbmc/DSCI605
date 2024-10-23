###ts() object
setwd("~/GitHub/DSCI605/Mod11")
sstoi.pa <- read.table("sstoi_pa.txt", header=T)
mo.sstoi <- length(sstoi.pa$YR) - 11
yr.sstoi <- mo.sstoi/12
sstoi.pa34 <- ts(sstoi.pa[,3], start = c(1950,1), end = c(1996,12), frequency = 12)
class(sstoi.pa34)

ts.plot(sstoi.pa34)
plot(sstoi.pa34)
