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
dirs$strat <- file.path(dirs$repo, 'strategies')
dirs$utils <- file.path(dirs$repo, 'utils')
dirs$data <- file.path(dirs$repo, 'results')
dirs$images <- file.path(dirs$repo, 'images')
setwd(dirs$strat)

cat('Currently at\n', getwd(), '\n')

# Number of cores for mclapply
cores <- 7
################################################################################

source(file.path(dirs$utils, 'load_packages.r'))
load.packages()
source('strategy_generic.r')
source(file.path(dirs$utils, 'load_data.r'))

##########
# BUILDING METADATA ABOUT THE PARAMETERS
##########

# Choosing parameters' prefixes and values
params <- list(
	list('N.', seq(1,50,1)),
	list('OFF.', seq(1,50,1))
)

# Building metadata
metadata <- build.metadata(args$pair, params)

#########
# LOADING DATA
#########

ma.type <- 'RSI'
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
		RSI(price = prices, n = size),
	prices = Cl(data)
)
all.ind <- do.call('merge.xts', all.ind)
all.ind <- lag(all.ind)[bt$time.period]
colnames(all.ind) <- metadata$params$names[[1]]

# SELL on PEAKS over 100-THRESHOLD
# BUY on BOTTOMS below THRESHOLD
gen.positions <- function(n, thresh, pb=2, allow.buy = TRUE, allow.sell = TRUE) {
	rsi.s <- all.ind[, paste0(params[[1]][[1]],n) ]
	
	pb <- peaks.bottoms(rsi.s, n = pb)
	sell <- pb$peaks & rsi.s >= 100-thresh
	buy <- pb$bottoms & rsi.s <= thresh
	#peaks <- bottoms <- rsi.s
	#peaks[!sell] <- NA
	#bottoms[!buy] <- NA
	
	# RSI is between thresholds
	between.threshold <- rsi.s > thresh & rsi.s < 100-thresh
	
	# When it enters or leaves threshold zones
	crossed.threshold <- diff(between.threshold)
	entered.threshold <- crossed.threshold == -1
	left.threshold <- crossed.threshold == 1
	
	# STRATEGY TO ENTER A POSITION
	# Get signal to enter AND must be between thresholds
	# lag it because the peaks must be confirmed after 'n' periods
	signals <- lag(ifelse(buy, 1, ifelse(sell, -1, NA)), n)# & between.threshold #not for now
	
	# Quit position as soon as it enters a threshold zone
	# Suggestions: quit only when it enters the opposed zone,
	# 		or as soon as it leaves the current one
	signals[entered.threshold & is.na(signals)] <- 0 # do not overwrite an 'enter signal' with 0
	signals <- na.locf(signals) # carry positions
	#signals <- lag(signals) # take action only in the next day ##### already lagged when computing RSI
	signals[is.na(signals)] <- 0
	#plot.ts(cbind(rsi.s,na.locf(signals))[1:500,] %>% as.matrix())
	
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

d <- file.path(dirs$data, 'RSI')
if(!file.exists(d)) {
	dir.create(d)
}
setwd(d)
# Saving RData
cat('Saving RData at', file.path(getwd(), rda.file.name), '\n')
save(metadata, bt, file = rda.file.name)

setwd(dirs$strat)

