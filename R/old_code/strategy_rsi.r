# Loading required packages and data
source('load_data.r')

# Peaks and bottoms
source('peaks_bottoms.r')

################################################################################
# RSI LIMITS
################################################################################

rsi <- list()
rsi$size.min <- 1
rsi$size.max <- 50
rsi$size.step <- 1
rsi$sizes <- seq(rsi$size.min, rsi$size.max, rsi$size.step)
rsi$sizes.names <- paste0('N_', rsi$sizes)

rsi$threshold.min <- 1
rsi$threshold.max <- 50
rsi$threshold.step <- 1
rsi$thresholds <- seq(rsi$threshold.min, rsi$threshold.max, rsi$threshold.step)
rsi$thresholds.names <- paste0('OFF_', rsi$threshold)

rsi$combn <- t(as.matrix(expand.grid(rsi$sizes, rsi$thresholds)))[2:1,]
rownames(rsi$combn) <- NULL
rsi$combn.names <- apply(rsi$combn, 2, paste0, collapse = '_')
rsi$combn.len <- ncol(rsi$combn)

# Compute RSI
all.rsi <- lapply(
	rsi$sizes,
	function(size, prices)
		RSI(price = prices, n = size),
	prices = Cl(data)
)
all.rsi <- do.call('merge.xts', all.rsi)
all.rsi <- na.omit(all.rsi)
colnames(all.rsi) <- rsi$sizes.names


################################################################################
# TRADING POSITIONS
################################################################################

# SELL on PEAKS over 100-THRESHOLD
# BUY on BOTTOMS below THRESHOLD

# Generate peaks and bottoms
gen.positions <- function(rsi.s, thresh, n=2, allow.buy = TRUE, allow.sell = TRUE) {
	pb <- peaks.bottoms(rsi.s, n = n)
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
	signals <- lag(signals) # take action only in the next day
	signals[is.na(signals)] <- 0
	#plot.ts(cbind(rsi.s,na.locf(signals))[1:500,] %>% as.matrix())
	
	# If short sell or long buy is not allowed
	if(!allow.buy) signals[which(signals == 1)] <- 0
	if(!allow.sell) signals[which(signals == -1)] <- 0
	
	return(signals)
}


################################################################################
# RUNNING TESTS
################################################################################
time.period <- '::'

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
	all.rsi.subset = all.rsi[time.period]
	
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
		rsi$combn, 2,
		function(rsi.combn) {
			return(gen.positions(
				all.rsi.subset[,rsi.combn[1]], # RSI series
				rsi.combn[2] # threshold
			))
		}
	)
	colnames(bt$strat.positions) <- rsi$combn.names
	bt$strat.positions <- xts(bt$strat.positions, order.by = time(all.rsi.subset))
	print(proc.time() - time.start)
	
	# Compute the strategy returns by multiplying the positions by the period returns
	cat('Computing returns\n')
	bt$strat.returns <- bt$strat.positions * as.numeric(bt$price.returns[time(bt$strat.positions)])
	
	cat('Computing balance evolution\n')
	if(type == 'arithmetic') {
		bt$strat.balance <- cumsum(bt$strat.returns) + 1
	} else if(type == 'geometric') {
		bt$strat.balance <- cumprod(bt$strat.returns + 1)
	}
	
	# Set names for each xts object in the environment
	# combn.names: 1_2, 1_3, 1_4, ...
	for(x in paste0( 'strat.', c('positions', 'returns', 'balance'))) {
		colnames(bt[[x]]) <- rsi$combn.names
	}
}

system.time(run.backtest())
################################################################################

# Writing strategy data to CSV files so it can be used later
rda.file.name <- paste(
	pair,
	strat.periodicity,
	paste(rsi$size.min, rsi$size.max, rsi$size.step, sep = '-'),
	'.rda',
	sep = '_'
)

setwd(file.path(dirs$data, 'RSI'))
# Saving RData
cat('Saving RData at', rda.file.name, '\n')
save.image(file = rda.file.name)

## Saving strategy data
#for(x in paste0( 'strat.', c('positions', 'returns', 'balance'))) {
#	file.name <- paste0(x, '.csv')
#	write.csv(bt[[x]], file = file.name, row.names = time(bt[[x]]))
#}
setwd(dirs$R)



