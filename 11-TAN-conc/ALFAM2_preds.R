# Generate some ALFAM2 model predictions for effect of loading rate

library(ALFAM2)
library(data.table)

dat <- data.table(id = 0:100, ct = 168, app.rate.ni = 0:100, TAN.app = 100)

preds2 <- alfam2(dat, pars = alfam2pars02, group = 'id')
preds3 <- alfam2(dat, group = 'id')

png('emis_v_app_rate.png', height = 500, width = 500)
  plot(er ~ id, data = preds2, type = 'l', col = 'red', ylim = c(0, 0.4), xlab = 'Slurry application rate (t/ha)', ylab = '168 hr emission (frac. applied TAN)')
  lines(er ~ id, data = preds3, col = 'blue')
dev.off()

