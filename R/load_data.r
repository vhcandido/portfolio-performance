source('load_packages.r')
load.packages()

################################################################################
# LOADING DATA
################################################################################

pair <- 'EURUSD'
strat.periodicity <- 'days'

img.path <- '~/Documents/usp/research/currency-strength/data'
filepath <- file.path(img.path, paste0(pair, '.csv'))

# Read as data.frame
data <- read.csv(filepath, header = T)

# Transform into an xts object ordered by the first column (date)
data <- xts(data[,-1], order.by = as.POSIXct(data[,1]))
data <- to_period(data, period = strat.periodicity)

# Check if there's any NA value
print(any(is.na(data)))
