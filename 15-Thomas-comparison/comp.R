# To look at precision in comparison of emission at two times
# For Thomas Kupper

library(ALFAM2)

dat <- data.frame(ct = 168, app.mthd = "bsth", air.temp = c(15, 14),
                  wind.2m = c(4, 3), wind.sqrt = sqrt(c(4, 3)),
                  apptime = c('A', 'B'))

ps2 <- alfam2(dat, pars = alfam2pars02, group = 'apptime')
ps3 <- alfam2(dat, pars = alfam2pars03, group = 'apptime', conf.int = 0.9)

diff(ps2$er)
diff(ps3$er)

ps3

# CI on the difference. . . 
library(data.table)
library(ALFAM2)

dat <- data.table(ct = 168, app.mthd = "bsth", air.temp = c(15, 14),
                  wind.2m = c(4, 3), wind.sqrt = sqrt(c(4, 3)),
                  apptime = c('A', 'B'))

summ <- alfam2(dat, pars = alfam2pars03, group = 'apptime', conf.int = 0.9)
summ

preds <- alfam2(dat, pars = alfam2pars03, group = 'apptime', conf.int = 'all')
setDT(preds)

pw <- dcast(preds, par.id ~ apptime, value.var = 'er')
pw[, dd := A - B]
100 * quantile(pw$dd, c(0.025, 0.5, 0.975))
