################################################################################
# LOADING DATA
################################################################################

load.from.disk <- function(
	filename,
	dir = '~/Documents/usp/research/currency-strength/data',
	period = 'days',
	period.k = 1,
	date.column = 1,
	date.format = '%Y-%m-%d %H:%M:%S'
	) {
	filepath <- file.path(dir, filename)
	
	# Read as data.frame and transform into an xts object ordered by the date column
	data <- read.table(filepath, header = TRUE, stringsAsFactors = FALSE, sep = ',')
	dates <- strptime(data[,date.column], '%Y-%m-%d %H:%M:%S', tz='GMT')
	data <- xts(data[,-1], order.by = dates)
	
	data <- switch(period,
		minutes = to.minutes(data, drop.time = TRUE, k = period.k),
		hours = to.hourly(data, drop.time = TRUE, k = period.k),
		days = to.daily(data, drop.time = TRUE),
		weeks = to.weekly(data, drop.time = TRUE),
		months = to.monthly(data, drop.time = TRUE, indexAt = 'yearmon'),
		quarters = to.quarterly(data, drop.time = TRUE, indexAt = 'yearqrt'),
		years = to.yearly(data, drop.time = TRUE),
		NULL)
	return(data)
}
