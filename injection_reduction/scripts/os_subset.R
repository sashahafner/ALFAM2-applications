# Gets a subset of experiments where os and th were compared

library(ggplot2)
library(data.table)

pdat <- fread('../data-emission/data/ALFAM2_plot.csv')

# Add variables
pdat[, app.mthd := app.method]
pdat[, uexper := paste(inst, exper)]
pdat[, inst := factor(inst)]

# Get unique application methods by experiment
esumm <- pdat[, .(app.mthds = paste(unique(app.mthd), collapse = ' ')), by = c('inst', 'exper', 'uexper')]

# Find those experiments with both th and os
dexper <- esumm[grepl('bsth.*os|os.*bsth', app.mthds), ]
exper.sel <- dexper[, uexper]

# Subset to those experiments and only those application methods
psub <- pdat[uexper %in% exper.sel & app.mthd %in% c('bsth', 'os')]
psumm <- table(psub[, c('uexper', 'app.mthd')])
psumm

# Reshape (use data.table operation instead if more variables are needed).
psubw <- dcast(psub, institute + inst + exper + uexper ~ app.mthd, value.var = 'e.rel.final', fun.aggregate = mean)
psubw[, os.rr := 100 * (bsth - os) / bsth]

# Export
fwrite(psub, '../output/ALFAM2_plot_th_os_exper_subset.csv')
fwrite(psubw, '../output/ALFAM2_plot_th_os_exper_subset_wide.csv')

# Simple plot
ggplot(psubw, aes(bsth, os, colour = inst)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
ggsave('../plots/os_th.png')

# Reduction plot
ggplot(psubw, aes(bsth, os.rr, colour = inst)) +
  geom_point() 
ggsave('../plots/os_reducation.png')
