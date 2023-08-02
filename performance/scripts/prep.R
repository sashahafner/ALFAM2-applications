# Prepare input data for ALFAM2 model

# Application rate x NOT injection
d2$app.rate.ni <- d2$app.rate * !(d2$app.mthd %in% c('os', 'cs'))


