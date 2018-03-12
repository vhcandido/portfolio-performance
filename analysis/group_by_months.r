require(xts)

load('../results/SMA/EURUSD_days_.rda')
dates <- format(bt$dates, '%Y-%m') # %V is week of the year
dates.i <- lapply(unique(dates), function(d) which(dates == d))

compute.return <- function(x, type = 'prod') {
	if(type == 'prod') {
		prod(x + 1)
	} else if(type == 'sum') {
		sum(x) + 1
	}
}

#compute.window.return <- function(x, windows, type = 'prod') {
#    sapply(unique(windows), function(w, x, type = 'prod') {
#        #idx <- which(grepl(w, bt$dates))
#	idx <- which(windows == w)
#        compute.return(x[idx])
#    }, x = x, type = type)
#}

compute.window.return <- function(ret, windows.idx, type='prod') {
  sapply(windows.idx, function(idx, ret, type='prod') compute.return(ret[idx], type),
    ret=ret, type=type)
}

#grouped <- apply(bt$strat.returns, -1, compute.window.return, windows = dates.i)
grouped <- apply(bt$strat.returns[,1:10,1:10], -1, compute.window.return, windows = dates.i)
