
library(data.table)
library(ggplot2)

idat <- fread('ALFAM2_interval.csv.gz')
pdat <- fread('ALFAM2_plot.csv.gz')

# 1. Date
dp <- pdat[pmid %in% c(989, 1001, 1015), ]
unique(dp$file)
unique(dp$first.row.in.file.int)
names(dp)
unique(dp$t.start.p)
dp$t.end.p

di <- idat[pmid %in% c(989, 1001, 1015), ]
di$t.start
di$t.end
di$row.in.file.int

ggplot(di, aes(cta, e.rel, colour = factor(pmid))) +
	geom_line() + geom_point()


# 2. Measurements start late
di <- idat[pmid %in% c(1517, 1519), ]
dp <- pdat[pmid %in% c(1517, 1519), ]
unique(dp$file)
unique(dp$first.row.in.file.int)

dd <- pdat[file == dp$file[1], ]
table(dd$pmid)
unique(dd$first.row.in.file.int)

ggplot(di, aes(cta, e.rel, colour = factor(pmid))) +
	geom_line() + geom_point()

# 3. No emission at start
di <- idat[pmid %in% c(1514, 1516, 1523, 1525), ]
dp <- pdat[pmid %in% c(1514, 1516, 1523, 1525), ]
unique(dp$file)
unique(dp$first.row.in.file.int)

ggplot(di, aes(cta, e.rel, colour = factor(pmid))) +
	geom_line() + geom_point()

ggplot(di, aes(cta, e.rel, colour = factor(pmid))) +
	geom_line() + geom_point() +
	xlim(0, 20)


pdat[pmid == 1514, .(file, first.row.in.file.int)]
idat[pmid == 1514, .(t.start, row.in.file.int)]

# 3. Atypical response
di <- idat[pmid %in% c(1183), ]
dp <- pdat[pmid %in% c(1183), ]
unique(dp$file)
unique(dp$first.row.in.file.int)

ggplot(di, aes(cta, e.rel, colour = factor(pmid))) +
	geom_line() + geom_point()


ggplot(di, aes(cta, j.NH3, colour = factor(pmid))) +
	geom_line() + geom_point()


