
library(data.table)
library(ALFAM2)
source('dfsumm.R')
source('model_stats.R')

pmidcal <- fread('../data/S1_plot_codes_calibration.csv')
pmideval <- fread('../data/S2_plot_codes_evaluation.csv')

pmidcal[, datasub := 'cal']
pmideval[, datasub := 'eval']
pmid <- rbind(pmidcal, pmideval)

idat <- fread('../data/ALFAM2_interval.csv')
idat[, app.mthd := app.method]

idat <- merge(idat, pmid, by = 'pmid')

idat <- idat[ct > 0 & ct < 78, ]

x <- idat[, dfsumm(.SD[, .(app.mthd, app.rate, man.dm, man.source, air.temp, wind.2m, man.ph, rain.rate, incorp)]), by = datasub]

# Fill in missing wind speed and air temperature values
idat[, wind.2m.ave := mean(wind.2m, na.omit = TRUE), by = pmid]
idat[, air.temp.ave := mean(air.temp, na.omit = TRUE), by = pmid]

idat[is.na(wind.2m), wind.2m := wind.2m.ave]
idat[is.na(air.temp), air.temp := air.temp.ave]

idat[is.na(rain.rate), rain.rate := 0]
idat[is.na(rain.cum), rain.cum := 0]

x <- idat[, dfsumm(.SD[, .(app.mthd, app.rate, man.dm, man.source, air.temp, wind.2m, man.ph, rain.rate, incorp)]), by = datasub]

names(idat)
args(alfam2)
pars <- alfam2pars01
pars <- pars[!grepl('man.ph', names(pars))]
pars

pred <- alfam2(idat, pars = pars, app.name = 'tan.app', time.incorp = 'time.incorp', group = 'pmid')

idat[, e.rel.pred := pred$er]

idat[, ct.max := max(ct), by = pmid]
idat.final <- idat[ct == ct.max, ]

me()
args(me)
summ <- idat.final[, .(me = me(e.rel, e.rel.pred)), by = datasub]
summ



