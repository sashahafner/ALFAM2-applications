# Some reaction-transport model predictions

source('NH3_mods.R')

# Create slurry
reftot <- c(H. = -0.02, NH3 = 0.1, H2CO3 = 0.1, 
            K. = 0.03, Na. = 0.02, Cl. = 0.03, 
            HAc = 0.0)

eq1 <- eqSpec(tot = reftot, temp.c = 20, pH = 7.5, of = 'all')

itot1 <- eq1$totk

# And triple the concentration of TAN
eq2 <- eqSpec(tot = 3 * reftot, temp.c = 20, pH = 7.5, of = 'all')

itot2 <- eq2$totk


# 1 cm thick layer
layers <- c(rep(1E-5, 5), rep(1E-4, 5), rep(9.45E-4, 10))

# Times in seconds
times <- c(0:5*600, 1:48*3600)
max(times) / 3600

pred1 <- kinEmisDiffMod(c.thk = layers, h.m = 1E-3, p.CO2.a = 4E-6, 
                        times = times, temp.c = 20, tot = itot1)

pred2 <- kinEmisDiffMod(c.thk = layers, h.m = 1E-3, p.CO2.a = 4E-6, 
                        times = times, temp.c = 20, tot = itot2)

pred1$emis
names(pred1)
str(pred1$tot.f)
str(pred1$tot.k)

# Plot NH3 emission
png('RT_emis1.png', height = 500, width = 500)
  plot(e.NH3.rel ~ I(t/3600), data = pred1$emis, type = 'l', col = 'blue', xlab = 'Time (h)', ylab = 'Relative emission (frac. TAN)')
  lines(e.NH3.rel ~ I(t/3600), data = pred2$emis, type = 'l', col = 'red')
dev.off()

# Surface pH
png('RT_pH1.png', height = 500, width = 500)
  plot(pred1$times, pred1$ph[, 1], type = 'l', col = 'blue')
  lines(pred2$times, pred2$ph[, 1], type = 'l', col = 'red')
dev.off()

# So, only small differences in relative losses, and the reason isn't obvious to me
# Maybe the pH difference is enough to explain it . . .

# How about with a thinner slurry layer?

pred3 <- kinEmisDiffMod(c.thk = layers / 3, h.m = 1E-3, p.CO2.a = 4E-6, 
                        times = times, temp.c = 20, tot = itot2)

png('RT_emis2.png', height = 500, width = 500)
  plot(e.NH3.rel ~ I(t/3600), data = pred1$emis, type = 'l', col = 'blue', 
       ylim = c(0, max(pred3$emis$e.NH3.rel)), xlab = 'Time (h)', ylab = 'Relative emission (frac. TAN)')
  lines(e.NH3.rel ~ I(t/3600), data = pred2$emis, type = 'l', col = 'red')
  lines(e.NH3.rel ~ I(t/3600), data = pred3$emis, type = 'l', col = 'orange')
dev.off()

# Big effect, by effectively reducing slurry-side resistance

# Plot TAN profile
png('RT_TAN_prof1.png', height = 500, width = 500)
  i <- 9
  plot(pred1$tot.k[i, , 'NH3'], I(-pred1$pos * 1000), type = 'l', col = 'blue', xlim = c(0, 0.3), 
       xlab = 'TAN (mol/kg)', ylab = 'Position (mm)',
       main = paste(pred1$times[i] / 3600, 'hours'), lwd = 2)
  lines(pred2$tot.k[i, , 'NH3'], I(-pred2$pos * 1000), type = 'l', col = 'red', lwd = 2)
  lines(pred3$tot.k[i, , 'NH3'], I(-pred3$pos * 1000), type = 'l', col = 'orange', lwd = 2)
  abline(h = 0, lty = 2)
dev.off()

png('RT_TAN_prof2.png', height = 500, width = 500)
  i <- 54
  plot(pred1$tot.k[i, , 'NH3'], I(-pred1$pos * 1000), type = 'l', col = 'blue', xlim = c(0, 0.3), 
       xlab = 'TAN (mol/kg)', ylab = 'Position (mm)',
       main = paste(pred1$times[i] / 3600, 'hours'), lwd = 2)
  lines(pred2$tot.k[i, , 'NH3'], I(-pred2$pos * 1000), type = 'l', col = 'red', lwd = 2)
  lines(pred3$tot.k[i, , 'NH3'], I(-pred3$pos * 1000), type = 'l', col = 'orange', lwd = 2)
  abline(h = 0, lty = 2)
dev.off()
