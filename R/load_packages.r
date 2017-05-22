################################################################################
# LOADING PACKAGES
################################################################################
load_packages <- function(extra = NULL) {
	packages <- c(
		'xts',
		'TTR',
		'quantmod',
		'PerformanceAnalytics',
		'parallel',
		extra
	)
	
	for(pkg in packages) {
		cat('Loading', pkg, '\n')
		suppressMessages( require(pkg, character.only = T, quietly = T)	)
	}
}

################################################################################
# SETTING WORKING DIRECTORY
################################################################################
original.wd <- '~/Documents/usp/research/performance/R'
setwd(original.wd)

cat('Currently at\n', getwd(), '\n')
