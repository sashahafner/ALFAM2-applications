# Gets ALFAM2 data in DATAMAN structure

# Set ALFAM2 database version
ver <- 'v1.3'

library(lubridate)

options(width = 65)

dat <- read.csv('../ALFAM2 file/ALFAM2_plot.csv', as.is = TRUE)

# Export publication info for checking for overlap with other sources
pp <- sort(unique(dat$pub.info))
write.csv(pp, '../output/pubs.csv')

# Drop 107 (old Swiss data)
dat <- dat[dat$inst != 107, ]

# Drop non cattle or pig
dat <- dat[dat$man.source %in% c('cat', 'pig'), ]

dat$meas.end <- as.character(ymd_hms(dat$app.start) + dat$ct.max * 3600, format = '%Y-%m-%d')
dat$app.date <- as.character(ymd_hms(dat$app.start), format = '%Y-%m-%d')

dat$gas <- 'NH3'
dat$app.rate.unit <- 't/ha' # Assumed
dat$tn.app <- dat$man.tkn * dat$app.rate
dat$ef <- dat$e.final/dat$tn.app
dat$man.bed <- tolower(dat$man.bed)
dat$entryperson <- 'Sasha D. Hafner'

dat$paperlink <- NA
dat$paperlink[is.na(dat$pub.info)] <- 'https://www.sciencedirect.com/science/article/pii/S0168192317304008'

dat$paperref <- dat$pub.info
dat$paperref[is.na(dat$pub.info)] <- 'Hafner, S.D., Pacholski, A., Bittman, S., Burchill, W., Bussink, W., Chantigny, M., Carozzi, M., Génermont, S., Häni, C., Hansen, M.N., Huijsmans, J., Hunt, D., Kupper, T., Lanigan, G., Loubet, B., Misselbrook, T., Meisinger, J.J., Neftel, A., Nyord, T., Pedersen, S.V., Sintermann, J., Thompson, R.B., Vermeulen, B., Vestergaard, A.V., Voylokov, P., Williams, J.R., Sommer, S.G., 2018. The ALFAM2 database on ammonia emission from field-applied manure: Description and illustrative analysis. Agricultural and Forest Meteorology, Greenhouse gas and ammonia emissions from livestock production 258, 66–79. https://doi.org/10.1016/j.agrformet.2017.11.027'
dat$n.reps <- 1

dat$source <- 'dataset+publication'

dat$app.method <- factor(dat$app.method, levels = c('bc', 'bss', 'bsth', 'cs', 'os', 'pi', 'ts'),
                         labels = c('broadcast', 'bandspread on slots', 'trailing hose', 
                                    'closed slot', 'open slot', 'pressurized injection', 
                                    'trailing shoe'))

dat$meas.tech3 <- factor(dat$meas.tech2, levels = c('wt', 'micro met', 'chamber', 'cps'), 
                         labels = c('windtunnel', 'micro met', 'dynamic enclosure', 'calibrated passive'))

dat$ct.max <- dat$ct.max/24
dat$wind.z <- 2
dat$entrydate <- as.character(as.Date(Sys.time()), format = '%Y-%m-%d')
dat$man.source <- factor(dat$man.source, levels = c('cat', 'pig'), labels = c('Cattle', 'Swine'))

# Only case of 2 treatments was floculation after anaerobic digestion. Will be ignored.
# See table(dat$man.trt1, dat$man.trt2)
dat$man.trt <- tolower(dat$man.trt1)
dat$man.trt[grepl('acid', dat$man.trt)] <- 'acidification'
dat$man.trt[grepl('separation', dat$man.trt)] <- 'separation'
dat$man.trt[dat$man.trt == 'floculation'] <- 'separation'
dat$man.trt[dat$man.trt == 'filtration'] <- 'separation'
dat$man.trt[dat$man.trt == 'low dm'] <- 'other'
dat$man.trt[dat$man.trt == 'low protein diet'] <- 'other'
dat$man.trt[dat$man.trt == 'surface'] <- 'other'

# Append cattle issue note to notes
dat$notes[dat$man.source == 'Cattle'] <- paste(dat$notes[dat$man.source == 'Cattle'], 'Cattle level for AnimalCategory variable may include dairy and beef.', sep = ' ')

# Add note on ALFAM2 database version
dat$notes <- paste(dat$notes, paste0('Data from ALFAM2 database ', ver, '.'), sep = ' ')
dat$notes <- gsub('^ ', '', dat$notes)

# Match ALFAM2 variables with DATAMAN variables
# Unnamed vars have no match in ALFAM2
dvars <- c(exper = 'TrialDescription', gas = 'GasMeasured', meas.tech3 = 'EmissionMeasTechnique', country = 'Country', 
           institute = 'InstituteName', man.source = 'AnimalCategory', man.con = 'ManureType', 
           man.trt = 'ManureTreatment', app.date = 'ApplicStartDate', rep = 'ReplicateNumber', 
           lat = 'Latitude', long = 'Longitude', 
           'InhibitorType', 
           man.dm = 'ManureDM', man.tkn = 'ManureTotalN', 
           man.tan = 'ManureTAN', 
           'ManureRAN', 'ManureOrgC', 'ManureCNRatio', 
           man.ph = 'ManurePH', app.rate = 'ManureApplicRate', app.rate.unit = 'ManureApplicRateUnit', 
           tn.app = 'ManureTNApplicRate', tan.app = 'ManureTANApplicRate', 
           'ManureRANApplicRate', 'ManureApplicSeason', 
           app.method = 'ManureApplicMethod', incorp = 'ManureSoilIncorporation', time.incorp = 'NH3TimeOfIncorporation', 
           ct.max = 'DurationOfEmissionMeasurement', e.final = 'CumulatEmission', 
           ef = 'EmissionFactor', 
           'SoilNH4', 'SoilNO3', 'SoilTotalNitrogen', 
           oc = 'SoilOrganicCarbon', 
           soil.ph = 'SoilPH', soil.dens = 'SoilBulkDensity', clay = 'SoilClay', soil.type = 'SoilTexture', 
           'SoilWaterUnit', 'SoilWaterDay1Avg', 'SoilWaterFirst30dAvg', 'SoilWaterEntireExptAvg', 
           air.temp.24 = 'NH3AirTemperature24hAvg', 
           'AirTemperatureFirst30dAvg', 
           air.temp.mn = 'AirTemperatureEntireExptAvg', 
           'RainfallIrrigation30d', 
           rain.tot = 'TotalRainfallIrrigationEntireExpt', 
           e.72 = 'NH3CumulEmission72h', crop.z = 'NH3CropHeight', wind.2m.12 = 'NH3WindSpeed12h', 
           wind.2m.72 = 'NH3WindSpeed3d', wind.z = 'NH3WindSpeedHeight', rain.1 = 'NH3RainIrrigation1h', 
           rain.6 = 'NH3RainIrrigation6h', 
           'AnimalSubCategory', 'DietDMContent', 'DietLipidContent', 'DietAshContent', 'DryMatterIntake', 'CrudeProtein', 
           'CrudeFibre', 'NDFContent', 'OMDigestibilityCoefficient', 'MultiphaseFeeding', 'Diet', 'SwineFeedType', 
           'ForageSpecies', 'ForageDMContent', 'ConcentrateType', 'ConcentrateDM', 'ProportionOfConcentrateInDiet', 
           'NitrogenDigestibilityCoefficient', 'NDFDigestibilityCoefficient', 'CH4Enteric', 
           pmid = 'DatabaseId', 
           'ClimateZone', 'AnimalBreed', 
           man.bed = 'TypeOfBeddingMaterial', 
           'RateOfInhibitorApplicUnit', 'RateOfInhibitorApplic', 
           'ManureDurationOfStorage', 
           'SoilDepthMineralN', 'SoilDepthOther', 'CropType', 'CropYieldDryMatter', 'CropTotalYieldUnit', 
           'CropTotalYield', 'CropTotalN', 
           entryperson = 'NameEntryPerson', entrydate = 'DateEntryCompleted', 
           'NameQCPersonIfRelevant', 
           paperlink = 'OnlineLinkPaper', paperref = 'Reference', 
           'QCCommentsIfRelevant', 'QCDateCompleted', 
           # Should be able to get some of these
           # Start and end are approximate, assumed based on application time
           app.date = 'StartGasMeasurements', meas.end = 'EndGasMeasurements', 
           n.ints = 'NumberOfMeasurements', 
           'N2OChambersPerPlot ', 'N2OChamberArea', 'N2OGasSamplesPerChamber', 
           n.reps = 'NumberOfReplicates', 
           'IfMeanWhatVariationUnit', 'DegreeOfVariation', 'DescribeStatMethodUsed', 
           source = 'SourceOfData', 
           notes = 'Comments')

# Drop missing ones
allvars <- dvars
missingvars <- dvars[nchar(names(dvars)) == 0]
dvars <- dvars[nchar(names(dvars)) > 0]

# Rename columns that will go into DATAMAN
dat <- dat[, names(dvars)]
names(dat) <- dvars

# Add blank columns for spacing for copy/paste
dat[, missingvars] <- NA

# Sort columns
dat <- dat[, allvars]

# Export
write.csv(dat, '../output/ALFAM2_DATAMAN.csv', row.names = FALSE, na = '', quote = TRUE)
sink('../log/log.txt')
  print('Date/time:')
  print(Sys.time())
  print('Dimensions:')
  print(dim(dat))
sink()

# Export Bussink data
datb <- dat[grepl('^Bussink', dat$Reference), ]
write.csv(datb, '../output/ALFAM2_DATAMAN_Bussink.csv', row.names = FALSE, na = '', quote = TRUE)

