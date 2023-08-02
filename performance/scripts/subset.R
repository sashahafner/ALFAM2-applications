# Prepare input data for ALFAM2 model

ds$app.mthd <- ds$app.method

d <- merge(d, ds, by = c('pmid', 'pid'))



#-------------------------------------------------------------------------------------------------
# Subset 
# Manure pH required
names(ds)
ds2 <- ds[!is.na(ds$e.48) &
          !is.na(ds$app.mthd) &
          !is.na(ds$man.dm) &
          !is.na(ds$man.source) & 
          !is.na(ds$air.temp.48) & 
          !is.na(ds$wind.2m.48) & 
          !is.na(ds$till) & 
          !is.na(ds$incorp) & 
          !is.na(ds$crop) & 
          !is.na(ds$man.ph) & 
          !ds$acid &
          ds$e.48 > 0 & 
          ds$e.rel.48 < 1.0 &
          ds$man.source %in% c('cat', 'pig') &
          ds$man.dm <= 15 &
          ds$app.mthd != 'pi' &
          ds$app.mthd != 'cs' &
          ds$app.mthd != 'bss' 
          , ]

# These pmid will be retained (more trimming below)
pmid.keep <- ds2$pmid

# Drop obs with high 168 h emis (thinking of 1184 e.rel.72 1.10 for bsth!)
# More than 105% at 168 hr is too much
pmid.keep <- pmid.keep[!pmid.keep %in% unique(d[d$e.rel > 1.05 & d$ct == d$ct.168, 'pmid'])]

# Keep only those with > 10 plots (drop CA, DE, IT)
table(ds2$country)

# Main subset (trimmed below also)
d$pmid.d2 <- d$pmid %in% pmid.keep
d2 <- droplevels(d[d$pmid %in% pmid.keep & d$ct <= 48 & d$ct > 0, ])
ds2 <- droplevels(ds[ds$pmid %in% pmid.keep, ])

# How many dropped?
dim(d)
dim(d2)

dim(ds)
dim(ds2)

# Check number of plots per country
table(ds$country)
table(ds2$country)
table(ds2$country, ds2$app.mthd)
table(ds2$country, ds2$incorp)
length(pmid.keep)

# Look for missing values
dfsumm(d2[, c('e.int', 'app.mthd', 'man.dm', 'man.source', 'air.temp', 
              'wind.2m', 'rain.rate', 'rain.cum', 'till', 'incorp', 'crop')])

# Fill in rain
for (v in c('rain.rate', 'rain.cum')) {
  d2[is.na(d2[, v]), v] <- 0
}

dfsumm(d2[, c('e.int', 'app.mthd', 'man.dm', 'man.source', 'air.temp', 
              'wind.2m', 'rain.rate', 'rain.cum', 'till', 'incorp', 'crop')])

# None missing


