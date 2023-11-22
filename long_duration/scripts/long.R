# Look at emission after 48 hours

library(data.table)
library(ggplot2)

idat <- fread('../data/ALFAM2_interval.csv')
pdat <- fread('../data/ALFAM2_plot.csv')

cdat <- merge(idat, pdat, by = c('pid', 'pmid'))
cdat[, app.mthd := app.method]

cdat[, cta.max := max(cta[!is.na(e.rel)]), by = pmid]

cdat <- cdat[cta.max > 48 & app.mthd %in% c('bc', 'bsth', 'ts', 'os', 'cs') & meas.tech2 %in% c('micro met', 'wt'), ]

# Get increase after 48 hours
sdat <- cdat[, .(emax = max(e.rel), der = max(e.rel) - e.rel[cta == cta[which.min(abs(cta - 48))]]), by = .(pmid, app.mthd, man.source, meas.tech2)]
sdat[, rder := der / emax]

sdat[, app.mthd.nm := factor(app.mthd, levels = c('bc', 'bsth', 'ts', 'os', 'cs'), 
                               labels = c('Broadcast', 'Trailing hose', 'Trailing shoe', 'Open slot\ninjection', 'Closed slot\ninjection'))]
sdat[, meas.tech.nm := factor(meas.tech2, levels = c('micro met', 'wt'), labels = c('Micro met.', 'Wind tunnel'))]

cdat[, app.mthd.nm := factor(app.mthd, levels = c('bc', 'bsth', 'ts', 'os', 'cs'), 
                               labels = c('Broadcast', 'Trailing hose', 'Trailing shoe', 'Open slot\ninjection', 'Closed slot\ninjection'))]
cdat[, meas.tech.nm := factor(meas.tech2, levels = c('micro met', 'wt'), labels = c('Micro met.', 'Wind tunnel'))]



ggplot(cdat, aes(cta, e.rel, colour = app.mthd.nm, group = pmid)) +
  geom_line() +
  xlim(0, 200) +
  facet_wrap(~ meas.tech.nm) +
  labs(x = 'Time after application (h)', y = 'Relative emission (frac. applied TAN)', colour = 'Application method') +
  theme_bw() +
  theme(legend.position = 'none')
ggsave('curves.png', height = 6.2, width = 11.02)

ggplot(sdat, aes(app.mthd.nm, der, fill = app.mthd.nm)) +
  geom_boxplot() +
  facet_wrap(~ meas.tech.nm) +
  labs(x = 'Application method', y = '> 48 h relative emission (frac. applied TAN)', fill = 'Application method') +
  coord_cartesian(ylim = c(0, 0.3)) +
  theme_bw() +
  theme(legend.position = 'none')
ggsave('box1.png', height = 6.2, width = 11.02)

ggplot(sdat, aes(app.mthd.nm, rder, fill = app.mthd.nm)) +
  geom_boxplot() +
  facet_wrap(~ meas.tech.nm) +
  labs(x = 'Application method', y = '> 48 h relative emission (frac. total emis.)', fill = 'Application method') +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme_bw() +
  theme(legend.position = 'none')
ggsave('box2.png', height = 6.2, width = 11.02)
