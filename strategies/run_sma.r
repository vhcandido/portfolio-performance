args1 <- commandArgs(trailingOnly = TRUE)
if(length(args1) < 3) {
	cat("Expected 3 parameters. Exiting...\n")
	quit()
}

args <- list()
args$pair <- as.character(args1[1]) # EURUSD
args$time.period <- as.character(args1[2]) # 2014-07::
args$periodicity <- as.character(args1[3]) # days

################################################################################
# SETTING WORKING DIRECTORY
dirs <- list()
dirs$repo <- '~/Documents/usp/research/performance'
dirs$R <- file.path(dirs$repo, 'R')
dirs$data <- file.path(dirs$repo, 'results')
dirs$images <- file.path(dirs$repo, 'images')
setwd(dirs$R)

cat('Currently at\n', getwd(), '\n')

# Number of cores for mclapply
cores <- 7
################################################################################

source('load_packages.r')
load.packages()
source('strategy_generic.r')
source('load_data.r')

##########
# BUILDING METADATA ABOUT THE PARAMETERS
##########

# Choosing parameters' prefixes and values
params <- list(
	list('MA.', seq(1,250,1)),
	list('MA.', seq(1,250,1))
)

# Building metadata
metadata <- build.metadata(args$pair, params)

#########
# LOADING DATA
#########

ma.type <- 'SMA'
data <- load.from.disk(paste0(metadata$name, '.csv'), period = args$periodicity)

bt <- new.env()
bt$time.period <- args$time.period
bt$periodicity <- args$periodicity
bt$price.returns <- na.fill(Cl(data) / Op(data) - 1, 0)[bt$time.period]
bt$length <- length(bt$price.returns)
bt$dates <- time(bt$price.returns)

# Compute necessary technical indicators over the functions
all.ind <- lapply(
	params[[1]][[2]],
	function(size, prices, ma.fun)
		ma.fun(x = prices, n = size),
	prices = Cl(data),
	ma.fun = match.fun(ma.type)
)
all.ind <- do.call('merge.xts', all.ind)
all.ind <- lag(all.ind)[bt$time.period]
colnames(all.ind) <- metadata$params$names[[1]]

gen.positions <- function(fast.n, slow.n, allow.buy = TRUE, allow.sell = TRUE) {
	if(fast.n == slow.n) {
		return(rep(0, bt$length))
	} else if(fast.n < slow.n) {
		# Filter a region over the parameter space
		return(rep(NA, bt$length))
	} 
	fast <- all.ind[,fast.n]
	slow <- all.ind[,slow.n]
	
	# Create lagged signals (technical indicators are already lagged)
	signals <- as.numeric(ifelse(fast > slow, -1, 1))
	
	# Remove the first signal sequence
	# (keep the data after the first cross)
	first.non.NA <- min(which( !is.na(signals) ))
	first.change <- which( signals != signals[first.non.NA] )
	
	# if signals doesn't change then first.signal becomes integer(0)
	if(length(first.change) > 0) {
		idx <- min(first.change)
		signals[1:(idx - 1)] <- 0
	} else {
		signals <- rep(0, bt$length)
	}
	
	# If short sell or long buy is not allowed
	if(!allow.buy) signals[which(signals == 1)] <- 0
	if(!allow.sell) signals[which(signals == -1)] <- 0
	
	return(signals)
}

system.time(run.backtest(gen.positions, debug = FALSE))
################################################################################

# Writing strategy data to CSV files so it can be used later
rda.file.name <- paste(
	metadata$name,
	bt$periodicity,
	'.rda',
	sep = '_'
)

d <- file.path(dirs$data, 'SMA')
if(!file.exists(d)) {
	dir.create(d)
}
setwd(d)
# Saving RData
cat('Saving RData at', file.path(getwd(), rda.file.name), '\n')
save(metadata, bt, file = rda.file.name)
#jsonlite::write_json(metadata, path='filename.json')

setwd(dirs$R)

