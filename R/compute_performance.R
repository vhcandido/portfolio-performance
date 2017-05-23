source('load_packages.r')
load_packages()

ma.type <- 'EMA'
setwd(file.path(dirs$data, ma.type))
	load('EURUSD_days_1-250-1_.rda')
setwd(dirs$R)

################################################################################
# Computing metrics
# ################################################################################
ret <- lapply(ma$combn.names, function(x) bt$strat.returns[,x] )

cat('starndart metrics\n')
final.balance = as.numeric( tail(bt$strat.balance, 1) )
mean.return = apply(bt$strat.returns, 2, mean)
mean.profit = apply(bt$strat.returns, 2, function(x) mean(x[x>0]) )
mean.loss = apply(bt$strat.returns, 2, function(x) mean(x[x<0]) )
sd.return = apply(bt$strat.returns, 2, sd)
sd.loss = apply(bt$strat.returns, 2, DownsideDeviation)
#sharpe = t(apply(bt$strat.returns, 2, SharpeRatio)), # FUN = c("StdDev", "VaR", "ES")
cat('sharpe\n')
sharpe = t(do.call('cbind', mclapply(ret, SharpeRatio, mc.cores = cores) ))
cat('sortino\n')
sortino = unlist(mclapply(ret, SortinoRatio, mc.cores = cores))
cat('upside\n')
upside.potencial = unlist(mclapply(ret, UpsidePotentialRatio, mc.cores = cores))
#treynor = t(apply(bt$strat.returns, 2, TreynorRatio)), # missing Rb
#information = t(apply(bt$strat.returns, 2, InformationRatio)), # missing Rb
#calmar = apply(bt$strat.returns, 2, CalmarRatio), # periodicity too high,
#sterling = apply(bt$strat.returns, 2, SterlingRatio), # periodicity too high,
#burke = t(apply(bt$strat.returns, 2, BurkeRatio)) # periodicity too high,
cat('var\n')
var = unlist(mclapply(ret, 'VaR', mc.cores = cores))
cat('cvar\n')
cvar = unlist(mclapply(ret, 'CVaR', mc.cores = cores))
maxDD = unlist(mclapply(ret, maxDrawdown, mc.cores = cores))

################################################################################
cat('results\n')
results <- data.frame(
	final.balance = as.numeric( tail(bt$strat.balance, 1) ),
	
	mean.return = mean.return,
	mean.profit = mean.profit,
	mean.loss = mean.loss,
	
	sd.return = sd.return,
	sd.loss = sd.loss,
	
	sharpe.StdDev = sharpe[,1],
	sharpe.VaR = sharpe[,2],
	sharpe.ES = sharpe[,3],
	sortino = sortino,
	
	VaR = var,
	CVaR = cvar,
	maxDD = maxDD,
	upside.potencial = upside.potencial,
	row.names = ma$combn.names
)
results <- results * 100
################################################################################

perf.file.name <- gsub('.rda', 'results.rda', rda.file.name)

setwd(file.path(dirs$data, ma.type))
	# Saving RData
	cat('Saving RData at', perf.file.name, '\n')
	save(results, file = perf.file.name)
setwd(dirs$R)



