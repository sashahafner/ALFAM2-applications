# To look at precision in comparison of emission at two times
# For Thomas Kupper
# This version uses hourly data

library(ALFAM2)
library(data.table)
library(ggplot2)

# Read input from Thomas
dat <- fread('input.csv')

# Add other variables
dat[, `:=` (app.rate.ni = 25, man.dm = 4.5, man.ph = 7.2)]
dat[, rain.rate := precipitation_mm / 1]
dat[, air.temp := temperature_celsius]
dat[, wind.2m := windspeed_m_s]

d1 <- dat[7:(7 + 48), ]
d2 <- dat[15:(15 + 48), ]

d1[, ct := as.numeric(difftime(time, min(time), units = 'hours'))]
d2[, ct := as.numeric(difftime(time, min(time), units = 'hours'))]

d1[, apptime := 'T12']
d2[, apptime := 'T20']

din <- rbind(d1, d2)

pred2 <- alfam2(din, pars = alfam2pars02, group = 'apptime')
pred3 <- alfam2(din, pars = alfam2pars03, group = 'apptime', conf.int = 0.9)

setDT(pred2)
setDT(pred3)

pred2[, parset := 'pars 2']
pred3[, parset := 'pars 3']

preds <- rbind(pred2, pred3, fill = TRUE)

pend <- preds[ct == 48, ]
pend <- dcast(pend, parset ~ apptime, value.var = 'er')
pend[, ediff := 100 * (T12 - T20)]

ggplot(preds, aes(ct, er, colour = apptime)) +
  geom_line() +
  geom_errorbar(aes(ymin = er.lwr, ymax = er.upr, width = 0), alpha = 0.2) +
  facet_wrap(~ parset) +
  theme_bw()
ggsave('emis.png', height = 4, width = 7)

# CI on the difference. . . 
pred3c <- alfam2(din, pars = alfam2pars03, group = 'apptime', conf.int = 'all')
setDT(pred3c)
pend3c <- pred3c[ct == 48, ]
pw <- dcast(pend3c, par.id ~ apptime, value.var = 'er')
pw[, dd := T12 - T20]
100 * quantile(pw$dd, c(0.025, 0.5, 0.975))
