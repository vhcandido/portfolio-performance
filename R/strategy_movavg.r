# Loading required packages and data
source('load_data.r')

################################################################################
# MOVING AVERAGES
################################################################################
ma <- list()
ma$size.min <- 1
ma$size.max <- 250
ma$size.step <- 1
ma$sizes <- seq(ma$size.min, ma$size.max, ma$size.step)
ma$sizes.names <- paste0('MA_', ma$sizes)

ma$combn <- t(as.matrix(expand.grid(ma$sizes, ma$sizes)))[2:1,]
rownames(ma$combn) <- NULL
ma$combn.names <- apply(ma$combn, 2, paste0, collapse = '_')
ma$combn.len <- ncol(ma$combn)

ma$combn.upper <- combn(ma$sizes, 2)
ma$combn.upper.names <- apply(ma$combn.upper, 2, paste0, collapse = '_')
ma$combn.lower <- ma$combn.upper[2:1,]
ma$combn.lower.names <- apply(ma$combn.lower, 2, paste0, collapse = '_')

ma$type <- "SMA"

# Compute moving averages
all.ma <- lapply(
	ma$sizes,
	function(size, prices, ma.fun)
		ma.fun(x = prices, n = size),
	prices = Cl(data),
	ma.fun = match.fun(ma$type)
)
all.ma <- do.call('merge.xts', all.ma)
all.ma <- na.omit(all.ma)
colnames(all.ma) <- ma$sizes.names

################################################################################
# TRADING POSITIONS
################################################################################

gen.positions <- function(fast, slow, buy.only = FALSE) {
	# Create an array with crossing points (lagged )
	# up: 1
	# down: -1
	crossed <- lag(diff(ifelse(fast > slow, 1, 0)))
	crossed[which(crossed == 0)] <- NA
	
	# Repeat signals
	positions <- na.locf(crossed) # repeat positions until next change
	positions <- na.fill(positions, 0) # replace NAs by 0
	
	# If short sell is not allowed
	if(buy.only) positions[which(positions != 1)] <- 0
	
	return(positions)
}

gen.positions2 <- function(fast, slow, allow.buy = TRUE, allow.sell = TRUE) {
	# Create lagged signals
	signals <- ifelse(fast > slow, 1, -1)
	
	# Remove the first signal sequence
	# (keep the data after the first cross)
	first.signal <- which.min( signals == as.numeric(signals[2]) )
	signals[1:(first.signal - 1)] <- 0
	
	# If short sell or long buy is not allowed
	if(!allow.buy) signals[which(signals == 1)] <- 0
	if(!allow.sell) signals[which(signals == -1)] <- 0
	
	return(signals)
}


################################################################################
# RUNNING TESTS
################################################################################
time.period <- '2014-07::'

bt <- new.env()
bt$prices <- Cl(data)[time.period]
bt$price.returns <- Delt(bt$prices, type = 'arithmetic')
bt$price.returns <- na.omit(bt$price.returns)[time.period]

################################################################################


################################################################################
# Run backtest and create an xts (in bt environment) for
# -positions
# -returns
# -balance
# Each column refers to a combination of moving average sizes

run.backtest <- function(type = 'geometric') {
	# Subset of MAs used to compute positions
	all.ma.subset = lag(all.ma)[time.period]
	
	# Compute the strategy positions by running gen.positions on every combination of parameters
	#
	# Split combinations in blocks of 100 and cbind the resulting list at the end
	# Performance
	# 	-> ~20s for 62500 combinations
	# 	-> Better than apply (~80s)
	# 	-> A little slower than mclapply (~20s), however pure mclapply returns a list with 62500 elements and then cbind overflows the callstack
	cat('Computing positions\n')
	time.start <- proc.time()
	bt$strat.positions <- apply(
		ma$combn, 2,
		function(ma.combn) {
			return(gen.positions2(
				all.ma.subset[,ma.combn[1]], # FAST
				all.ma.subset[,ma.combn[2]] # SLOW
			))
		}
	)
	colnames(bt$strat.positions) <- ma$combn.names
	bt$strat.positions <- xts(bt$strat.positions, order.by = time(all.ma.subset))
	print(proc.time() - time.start)
	
	# Compute the strategy returns by multiplying the positions by the period returns
	cat('Computing returns\n')
	bt$strat.returns <- bt$strat.positions * c(0, as.numeric(bt$price.returns))
	
	cat('Computing balance evolution\n')
	if(type == 'arithmetic') {
		bt$strat.balance <- cumsum(bt$strat.returns) + 1
	} else if(type == 'geometric') {
		bt$strat.balance <- cumprod(bt$strat.returns + 1)
	}
	
	# Set names for each xts object in the environment
	# combn.names: 1_2, 1_3, 1_4, ...
	for(x in paste0( 'strat.', c('positions', 'returns', 'balance'))) {
		colnames(bt[[x]]) <- ma$combn.names
	}
}

system.time(run.backtest())
################################################################################

# Writing strategy data to CSV files so it can be used later
rda.file.name <- paste(
	pair,
	strat.periodicity,
	paste(ma$size.min, ma$size.max, ma$size.step, sep = '-'),
	'.rda',
	sep = '_'
)

setwd(file.path(dirs$data, ma$type))
	# Saving RData
	cat('Saving RData at', rda.file.name, '\n')
	save.image(file = rda.file.name)
	
	# Saving strategy data
	for(x in paste0( 'strat.', c('positions', 'returns', 'balance'))) {
		file.name <- paste0(x, '.csv')
		write.csv(bt[[x]], file = file.name, row.names = time(bt[[x]]))
	}
setwd(dirs$R)
