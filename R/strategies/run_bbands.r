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
Rcpp::sourceCpp('bbands.cpp')

##########
# BUILDING METADATA ABOUT THE PARAMETERS
##########

# Choosing parameters' prefixes and values
params <- list(
	list('N.', seq(1, 50, 1)),
	list('SD1.', seq(0, 5, by = 0.1)[-1]),
	list('F.', seq(1.1, 4, by = 0.1))
)

# Building metadata
metadata <- build.metadata(args$pair, params)

#########
# LOADING DATA
#########

ma.type <- 'BBands'
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
	function(size, prices)
		reclass(runSD(rowSums(HLC(prices)), n = size), prices),
	prices = HLC(data)
)
all.ind <- do.call('merge.xts', all.ind)
all.ind <- lag(all.ind)[bt$time.period]
colnames(all.ind) <- metadata$params$names[[1]]

# SELL on PEAKS over 100-THRESHOLD
# BUY on BOTTOMS below THRESHOLD
gen.positions <- function(n, sd1, f, allow.buy = TRUE, allow.sell = TRUE) {
	mavg <- lag(SMA(Cl(data), n))[bt$time.period]
	sdev <- all.ind[,paste0(params[[1]],n)]
	dev <- sd1*sdev
	fdev <- dev*f
	up1 <- mavg + dev
	dn1 <- mavg - dev
	up2 <- mavg + fdev
	dn2 <- mavg - fdev
	
	signals <- bb_strat(lag(Cl(data))[bt$time.period], up1, dn1, up2, dn2)
	
	# If short sell or long buy is not allowed
	if(!allow.buy) signals[which(signals == 1)] <- 0
	if(!allow.sell) signals[which(signals == -1)] <- 0
	
	return(signals)
}

system.time(run.backtest(gen.positions, debug = T))
################################################################################

# Writing strategy data to CSV files so it can be used later
rda.file.name <- paste(
	metadata$name,
	bt$periodicity,
	'.rda',
	sep = '_'
)

d <- file.path(dirs$data, 'BBands')
if(!file.exists(d)) {
	dir.create(d)
}
setwd(d)
# Saving RData
cat('Saving RData at', file.path(getwd(), rda.file.name), '\n')
save(metadata, bt, file = rda.file.name)

setwd(dirs$R)




