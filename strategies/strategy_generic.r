source('../utils/perm.r')
source('../utils/peaks_bottoms.r')

build.metadata <- function(name, params, filter.FUN) {
	metadata <- list()
	
	metadata$name <- name
	
	metadata$params <- list()
	metadata$params$values <- lapply(params, function(x) x[[2]])
	metadata$params$names <- lapply(params, function(x) paste0(x[[1]], seq(x[[2]])))
	# Naming lists allows convertion to JSON and parsing it back to list
	# however simplifyVector=TRUE must be passed to jsonlite::read_json()
	# so it'll consider unnamed lists as vectors.
	# Other advantage: loading metadata from RDA file brings integers with 8bytes while
	#     loading it from JSON brings integers with 4bytes, however a simple as.numeric()
	#     is suficient to double its size back to 8bytes (c() and as.vector() keep the
	#     4bytes size.
	# Cons: it makes strat.balance (and others) to increase in size because the .Dim
	#     attribute turns into a named structure, instead of a unnamed one.
	names(metadata$params$values) <- names(metadata$params$names) <- seq(params)
	
	metadata$params$combn <- perm(metadata$params$values)
	if(!missing(filter.FUN)) {
		idx <- which(apply(metadata$params$combn, 1, filter.FUN))
		metadata$params$combn <- metadata$params$combn[idx,]
	}
	metadata$params$combn.len <- ncol(metadata$params$combn)
	metadata$params$combn.names <- apply(metadata$params$combn, 1, paste0, collapse = '_')
	
	return(metadata)
}

run.backtest <- function(gen.positions.FUN, type = 'prod', debug = TRUE) {
	# Compute the strategy positions by running gen.positions on every combination of parameters
	cat('Computing positions\n')
	param.list <- apply(metadata$params$combn, 1, as.list)
	bt$strat.positions <-
		if(!debug) {
			mclapply(
				param.list,
				FUN = do.call,
				what = gen.positions.FUN,
				mc.cores = cores
			)
	} else {
		# Below there's a way of doing the same thing with apply
		apply(
			metadata$params$combn, 1,
			function(param.combn) {
				do.call(gen.positions.FUN, as.list(param.combn))
			}
		)
	}
	
	# Transform into an array of n+1 dimensions (n is the # of params)
	bt$strat.positions <- array(
		data = unlist(bt$strat.positions),
		dim = c(bt$length, lengths(metadata$params$values))
	)

	# Compute the strategy returns by multiplying the positions by the period returns
	cat('Computing returns\n')
	bt$strat.returns <- bt$strat.positions * as.numeric(bt$price.returns)
	
	cat('Computing balance evolution\n')
	if(type == 'prod') {
		bt$strat.balance <- apply(bt$strat.returns, -1, function(x) cumprod(x + 1))
	} else if(type == 'sum') {
		bt$strat.balance <- apply(bt$strat.returns, -1, function(x) cumsum(x) + 1)
	}

	# Removing names given by metadata
	names(dim(bt$strat.balance)) <- NULL
	names(dim(bt$strat.returns)) <- NULL
	names(dim(bt$strat.positions)) <- NULL
}
