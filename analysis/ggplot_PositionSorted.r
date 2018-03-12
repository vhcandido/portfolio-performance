require(ggplot2)

# Writing a new Position class
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
# https://github.com/tidyverse/ggplot2/blob/master/R/geom-jitter.r #position-jitter.r
foo <- function(x, amount) { x + seq(-amount, amount, len = length(x)) }
position_sorted <- function(width = NULL) { ggplot2::ggproto(NULL, PositionSorted, width = width) }

`%||%` <- ggplot2:::`%||%`
PositionSorted <- ggproto("PositionSorted", Position,
  compute_layer = function(data, params, panel) {
    foo.wrap <- function(x) foo(x, amount = params$width)
    idx <- order(data$x, data$y)
    df <- data[idx,]
    # replace df$group by df$x (inside factor) if 'x' in aes is not categorical (factors)
    df$x <- ave(df$x, factor(df$group), FUN = foo.wrap)
    df

    #trans_x <- function(x) foo(x, params$width)
    #trans_y <- function(x) x[order(x)]
    #ggplot2::transform_position(data, trans_x, trans_y)
  },

  setup_params = function(self, data) {
    list(width = self$width %||% ggplot2::resolution(data$x, zero = FALSE) * 0.4)
  },


  required_aes = c("x", "y")
)

geom_sorted <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = PositionSorted,
                        ...,
                        width = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  if (!missing(width)) {
    if (!missing(position)) {
      stop("Specify either `position` or `width`", call. = FALSE)
    }

    position <- position_sorted(width = width)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
