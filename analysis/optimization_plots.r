require(magrittr)
require(reshape2)
require(ggplot2)
require(grid)
require(gridExtra)

setwd('~/Documents/usp/research/performance/R/')
source('perm.r')
source('ggplot_PositionSorted.r')

params <- list()
params$param1 <- c(10, 20, 30)
params$param2 <- letters[1:5]
params$param3 <- (1:10)/10

n = 100
balance <- lengths(params) %>% { array(rnorm(n * prod(.), sd = .01)+1, c(n, .)) }
balance <- apply(balance, seq(params)+1, cumprod)

# load balance
#balance <- bt$strat.balance; params <- metadata$params$values
dims <- dim(balance)
r <- range(balance)

#p.grid <- lengths(params) %>% lapply(seq) %>% perm()
plot(1:100, seq(r[1]-.1, r[2]+.1, len = 100), pch = -1)
apply(balance, seq(dims)[-1], function(x) lines(x))

# Same as
# mat <- balance[dims[1], , , ...]
margin.idx <- balance %>% slice.index(MARGIN=1)
mat <- balance[which(margin.idx == dims[1])] %>% array(dims[-1])

################################################################################
# Plot pure R
max.len <- max(lengths(params))
box = FALSE
opar = par()
par(mfrow = c(length(params), max.len))
for(i in seq(params)) {
	i.idx <- slice.index(mat, i)
	x <- lengths(params)[-i] %>% prod() %>% seq()
	for(j in seq(max.len)) {
		if(j > length(params[[i]])) {
			#plot(1:100, 1:100, pch=-1)
			plot.new()
		        next
		}
		j.idx <- which(i.idx == j)
		if(box == FALSE) {
			plot(x, sort(mat[j.idx]),
				ylim = r, ylab = 'balance', xlab = '', xaxt = 'n',
				main = sprintf('param %d = %s', i, params[[i]][j])
			)
			abline(h = 1, lty = 2)
		} else {
			boxplot(mat[j.idx], ylim = r)
		}
		grid()
	}
}

################################################################################
# Plot reshape2 + ggplot2
# http://seananderson.ca/2013/10/19/reshape.html
mm <- melt(mat)

# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
nas <- rep(NA, max(lengths(params)))
hlay <- lapply(seq(params), function(i) {v <- nas; v[seq(params[[i]])] <- i; v })
hlay <- do.call('rbind', hlay)
select_grobs <- function(lay) {
  id <- unique(c(t(lay)))
  id[!is.na(id)]
}

alpha = .6
pl = 'sorted'
plots <- list()
for(i in seq(params)) {
	var <- paste0('Var', i)
	p <- ggplot(mm, aes_string(x = sprintf('factor(%s)', var), y = 'value')) + ylim(r)
	p <- p + geom_hline(yintercept = 1, linetype = 2)
	p <- p + geom_violin() #draw_quantiles = c(.5))
	p <- p + geom_boxplot(width=0.1)

	# Points organization
	p <- if(pl == 'point') { p + geom_point(alpha = alpha) }
	else if(pl == 'jitter') { p + geom_jitter(alpha = alpha) }
	else if(pl == 'sorted') { p + geom_sorted(alpha = alpha) }
	else { p }

	# Text stuff
	# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
	p <- p + scale_x_discrete(labels = params[[i]])
	p <- p + theme_bw()

	plots[[i]] <- p
}
#do.call('grid.arrange', c(plots, ncol = 1))
grid.arrange(grobs=plots[select_grobs(hlay)], layout_matrix=hlay, top = '1D optimization results')

pls <- list()
for(i in seq(params)) {
	var <- paste0('Var', i)
	stats <- function(x, ...) quantile(x, ...)

	df <- aggregate(mm$value, list(mm[,var]), stats, na.rm = TRUE, na.action = NULL)
	df <- cbind(df[[1]], df[[2]])
	df <- as.data.frame(df)
	colnames(df) <- c('x', paste0('q' , 0:4*25))

	p <- ggplot(df, aes(x)) + ylim(r) + theme_bw()
	#p <- p + geom_ribbon(aes(ymin=q0, ymax=q100), fill='grey90')
	#p <- p + geom_ribbon(aes(ymin=q25, ymax=q75), fill='grey70')

	# coloring between 0 and 100
	p <- p + geom_ribbon(aes(ymin=pmax(1,q0), ymax=pmax(1,q100)), fill='green', alpha=.3)
	p <- p + geom_ribbon(aes(ymin=pmin(1,q0), ymax=pmin(1,q100)), fill='red', alpha=.3)

	# coloring between 25 and 75
	p <- p + geom_ribbon(aes(ymin=pmax(1,q25), ymax=pmax(1,q75)), fill='green', alpha=.5)
	p <- p + geom_ribbon(aes(ymin=pmin(1,q25), ymax=pmin(1,q75)), fill='red', alpha=.5)

	p <- p + geom_line(aes(y=q50))
	p <- p + geom_hline(yintercept=1, color = 'grey30')
	pls[[i]] <- p
}
do.call('grid.arrange', pls)


#plots <- list()
#for(i in seq(params)) {
#	var <- paste0('Var', i)
#	idx <- order(mm[,i], mm$value)
#	p <- ggplot(mm[idx,]) + ylim(r)
#	p <- p + geom_point(aes_(x = ~seq(value)-1, y = ~value, color = as.name(var)))
#	p <- p + geom_vline(xintercept = cumsum(c(0,table(mm[idx,i]))))
#	p <- p + geom_hline(yintercept = 1, linetype = 2)
#	plots[[i]] <- p
#}
##https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#do.call('grid.arrange', c(plots, ncol = 1))

################################################################################
ggplot2:::plot.ggplot -> ggplot_build (contains the true data)
geom_jitter -> position_jitter -> ggproto (getting where I should extend to create new positions)
showMethods(class='PositionJitter') # doesn't work
ls(PositionJitter) # shows attributes of this class, which are accessed with '$'

