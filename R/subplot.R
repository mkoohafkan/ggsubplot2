#' Geom Subplot
#'
#' Plot subplot elements in a plot.
#'
#' @param mapping requires aesthetics x, y, width, height, plot.
#' @inheritParams ggplot2::geom_rect
#' @return a ggplot2 layer.
#'
#' @import ggplot2
#' @export
geom_subplot = function(mapping = NULL,
  stat = StatIdentity,
  position = PositionIdentity,
  data = NULL, ...,
  na.rm = FALSE, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSubplot,
    position = position,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @import grid
GeomSubplot = ggproto("GeomSubplot", Geom,
  required_aes = c("x", "y", "width", "height", "plot"),

non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

	setup_params = function(data, params) {
		params$flipped_aes <- has_flipped_aes(data, params)
		params
	},

	extra_params = c("na.rm", "orientation"),

	handle_na = function(data, params) {
		data
	},

	setup_data = function(data, params) {
		data$flipped_aes <- params$flipped_aes
		data <- flip_data(data, params$flipped_aes)
		data$width <- data$width %||% params$width
		data$height <- data$height %||% params$height 
		data <- transform(data,
											ymin = y - height / 2,
											ymax = y + height / 2,
											xmin = x - width / 2,
											xmax = x + width / 2,
											width = NULL,
											height = NULL
		)
		flip_data(data, params$flipped_aes)
	},

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    n = nrow(data)
    if (n == 1) return(zeroGrob())
    if (!coord$is_linear()) {
      abort("geom_subplot only works with linear coordinates")
    }
    #coordinate transformation for viewports
		coords <- coord$transform(data, panel_params)
    # plot viewports
    newGrobs = vector("list", nrow(data))
    for (i in seq_along(newGrobs)) {
      vp <- viewport(x = coords$x[i], y = coords$y[i],
				width = coords$xmax[i] - coords$xmin[i],
				height = coords$ymax[i] - coords$ymin[i],
        just = c("center", "center"))
      grob = ggplotGrob(data$plot[[i]])
      newGrobs[[i]] = editGrob(grob,
        vp = vp, name = paste(grob$name, i))
    }
		do.call(grobTree, newGrobs)
  }
)