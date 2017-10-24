source('load_packages.r')
load.packages(c('plot3D', 'ggplot2', 'reshape2'))

ma.type <- 'RSI'

setwd(file.path(dirs$data, ma.type))
load('EURUSD_days_1-50-1_.rda')
load('EURUSD_days_1-50-1_results.rda')

dir.create(file.path(dirs$images, ma.type), showWarnings = FALSE)
setwd(file.path(dirs$images, ma.type))
################################################################################

################################################################################
# PLOTTING
################################################################################
obj <- 'final.balance'

n.rsi <- length(rsi$sizes)
n.thresh <- length(rsi$thresholds)
m <- matrix( rep.int(NA, n.rsi*n.thresh), nrow = n.rsi ) # fill matrix
m[ t(rsi$combn) ] <- results[rsi$combn.names, obj]
#m[which(lower.tri(m))] <- NA # get upper triangle


z.range <- range(bt$strat.balance)*10
offset <- ceiling(max(abs(z.range - 10))) * 10
z.range <- 100 + c(-offset, offset)

dates <- if(strat.periodicity == 'days') {
	as.Date(time(bt$strat.balance))
} else {
	as.POSIXct(time(bt$strat.balance))
}
wday <- weekdays(dates)
dates <- format(dates, '%d %b %Y')

################################################################################

aux.fun <- function(trans, m, MARGIN, FUN, col = 'grey') {
	FFUN <- match.fun(FUN)
	rm <- apply(m, MARGIN, function(x) FFUN(x, na.rm = TRUE))
	if(MARGIN == 1) {
		XY <- trans3D(x = rsi$sizes, y = rep(rsi$size.min, rsi$size.max), z = rm, pmat = trans)
		lines(XY$x[-n.rsi], XY$y[-n.rsi], col = col, lwd = 2)
	} else {
		XY <- trans3D(x = rep(rsi$size.max, rsi$size.max), y = rsi$sizes, z = rm, pmat = trans)
		lines(XY$x[-1], XY$y[-1], col = col, lwd = 2)
	}
}

my.panel <- function(trans) {
	M <- m
	#M[-which(upper.tri(M))] <- NA
	aux.fun(trans, M, 1, 'max')
	aux.fun(trans, M, 2, 'max')
	
	aux.fun(trans, M, 1, 'min')
	aux.fun(trans, M, 2, 'min')
	
	aux.fun(trans, M, 1, 'mean', col = 'black')
	aux.fun(trans, M, 2, 'mean', col = 'black')
}

png(paste0('final_persp3D_', pair, '.png'), width = 2000, height = 2000)
persp3D(
	z = m,
	x = 1:50,
	y = 1:50,
	theta = 150,
	scale = TRUE,
	contour = TRUE,
	panel.first = my.panel,
	zlim = c(0,120),
	colkey = list(length = 0.5),
	facets = TRUE
)
dev.off()

png(paste0('final_image2D_', pair, '.png'), width = 1000, height = 1000)
image2D(z = m, clab = 'm', zlim = z.range, x = rsi$sizes, y = rsi$thresholds)
dev.off()

################################################################################
bt$strat.balance <- bt$strat.balance * 100

cat('Generating persp3D images\n')
dir.create(paste0('persp3D_', pair), showWarnings = FALSE)
for(i in seq(0,360,5)) {
	cat('persp3D:', i, '\n')
	png(paste0('persp3D_', pair, '/persp3D_', sprintf('%03d', i), '.png'))
	persp3D(z = m, theta = i, zlim = z.range)
	dev.off()
}

cat('Generating image2D images\n')
dir.create(paste0('image2D_', pair), showWarnings = FALSE)
system.time(
	for(i in 1:nrow(bt$strat.balance)) {
		m[ t(rsi$combn) ] <- bt$strat.balance[i, rsi$combn.names]
		png(paste0('image2D_', pair, '/image2D_', sprintf('%03d', i), '.png'))
		image2D(z = m, clab = 'm', zlim = z.range, main = paste0(dates[i], '\n', wday[i]))
		dev.off()
	}
)

quit('no')
cat('Generating image2D images with array\n')
m <- array(bt$strat.balance, c(nrow(bt$strat.balance), 250, 250))
m <- aperm(m, c(1,3,2))
system.time(
	dir.create('image2D_teste', showWarnings = FALSE),
	for(i in 1:nrow(bt$strat.balance)) {
		png(paste0('image2D/image2D_', sprintf('%03d', i), '.png'))
		image2D(z = m[i,,], clab = 'm', zlim = z.range, main = paste0(dates[i], '\n', wday[i]))
		dev.off()
	}
)

################################################################################


##### IMAGE2D
# col = ramp.col(c('darkred', 'white', 'darkblue'))
# contour = list(col = "white", labcex = 0.8, alpha = 0.5)

##### PERSP3D
# colkey = list(length = 0.5)
# contour = list(nlevels = 20, col = "red"), 
# image = list(col = grey (seq(0, 1, length.out = 100))))

#persp3D(z = m, 
#	contour = list(side = c("zmin", "z", "250")),
#	zlim = c(-100, 350),
#	image = list(side = 250),
#	phi = 20, , theta = -135)



