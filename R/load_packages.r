################################################################################
# LOADING PACKAGES
################################################################################
load.packages <- function(extra = NULL) {
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

detach.all.packages <- function() {
	info <- sessionInfo()
	pkg.names <- names(info$otherPkgs)
	pkg.names <- paste0('package:', pkg.names)
	lapply(pkg.names, detach, character.only = TRUE, unload = TRUE)
}

################################################################################
# SETTING WORKING DIRECTORY
################################################################################
dirs <- list()
dirs$repo <- '~/Documents/usp/research/performance'
dirs$R <- file.path(dirs$repo, 'R')
dirs$data <- file.path(dirs$repo, 'data')
dirs$images <- file.path(dirs$repo, 'images')
setwd(dirs$R)

cat('Currently at\n', getwd(), '\n')

# Number of cores for mclapply
cores <- 7
