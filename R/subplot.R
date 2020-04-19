#' Geom Subplot
#'
#' Plot subplot elements in a plot.
#'
#' @param mapping requires aesthetics x, y, width, height, plot.
#' @inheritParams ggplot2::geom_rect
#' @param nudge_x,nudge_y Horizontal and vertical adjustment 
#'   to nudge subplots by. Useful for offsetting plots from 
#'   their center coordinates, particularly on discrete scales.
#' @return a ggplot2 layer.
#'
#' @import ggplot2
#' @export
geom_subplot = function(mapping = NULL,
	stat = StatIdentity,
	position = PositionIdentity,
	data = NULL, ...,
	subplot.aes = aes(),
	subplot.geom = geom_blank(),
	scales = "free",
	nudge_x = 0, nudge_y = 0,
	subplot.theme = theme_void(),
	na.rm = FALSE, inherit.aes = TRUE) {

	layer(
		data = data,
		mapping = mapping,
		stat = stat,
		geom = GeomSubplot,
		position = position,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			subplot.geom = subplot.geom,
			subplot.theme = subplot.theme,
			scales = scales,
			nudge_x = nudge_x,
			nudge_y = nudge_y,
			...
		)
	)
}

#' @import grid
GeomSubplot = ggproto("GeomSubplot", Geom,
	required_aes = c("x", "y", "group", "width", "height"),
	non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

	setup_params = function(data, params) {
		params$flipped_aes <- has_flipped_aes(data, params)
		params
	},

	extra_params = c("na.rm", "orientation",
	  "subplot.geom", "subplot.aes",
	  "subplot.theme",
		"nudge_x", "nudge_y", "scales"),

		handle_na = function(data, params) {
			data
		},

		setup_data = function(data, params) {
			data$flipped_aes = params$flipped_aes
			data = flip_data(data, params$flipped_aes)
			data$y = data$y + params$nudge_y
			data$x = data$x + params$nudge_x
			data$width = data$width %||% params$width
			data$height = data$height %||% params$height
			data = transform(data,
											ymin = y - height / 2,
											ymax = y + height / 2,
											xmin = x - width / 2,
											xmax = x + width / 2,
											width = NULL,
											height = NULL
		)
			flip_data(data, params$flipped_aes)
		},
		draw_key = function(data, params) {
			subplot.data = subplot_data(data)
			subplot.gp = subplot.data[names(subplot.data) %in% names(get.gpar())]
			gp = do.call(gpar, subplot.gp)
		},
		draw_group = function(self, data, panel_params, coord, na.rm = FALSE,
		  subplot.geom = geom_blank(),
			subplot.theme = NULL) {
			n = nrow(data)
			if (n < 1) return(zeroGrob())
			if (!coord$is_linear()) {
				abort("geom_subplot only works with linear coordinates")
			}
			#coordinate transformation for viewports
			coords <- coord$transform(data, panel_params)
			# construct plot
#			browser()
			subplot.data = subplot_data(data)
			subplot.aes = subplot_aes(subplot.data)
			plot = ggplot(subplot.data, subplot.aes) +
				subplot.geom +
				subplot.theme +
				theme(legend.position = "none")
			# plot viewport      
			vp <- viewport(x = coords$x[1], y = coords$y[1],
			width = coords$xmax[1] - coords$xmin[1],
			height = coords$ymax[1] - coords$ymin[1],
			just = c("center", "center"))
			grob = ggplotGrob(plot)
			newGrob = editGrob(grob, vp = vp, name = grob$name)
			grobTree(newGrob)
		}
)

subplot_data = function(data) {
	subplot.data = data[, grep("^subplot.", names(data))]
	names(subplot.data) = gsub("^subplot.", "", names(subplot.data))
	subplot.data
}

#' @importFrom rlang sym
#' @import ggplot2
subplot_aes = function(subplot.data) {
	subplot.aes.names = lapply(names(subplot.data), sym)
	names(subplot.aes.names) = names(subplot.data)
	subplot.aes = do.call(aes, subplot.aes.names)
}