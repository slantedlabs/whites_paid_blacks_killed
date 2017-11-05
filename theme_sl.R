library(ggplot2)
library(ggthemes)
library(grid)

theme_sl_data <- list("colors"=list("background"="white",
                                    "strip.bg"="gray90",
                                    "text"="gray48",
                                    "title"="gray26",
                                    "line"="gray26",
                                    "axis"="black",
                                    "grid"="gray80"))



theme_sl <- function(base_size = 12,
                     color = "brown",
                     base_family = "sans",
                     title_family = "Palatino") {
  (theme_foundation(base_size = base_size,
                    base_family = base_family) +
   theme(# bases
         line = element_line(linetype = 1,
                             colour = theme_sl_data$colors$line),
         rect = element_rect(fill = theme_sl_data$colors$background,
                             linetype = 0,
                             colour=NA),
         text = element_text(colour = theme_sl_data$colors$text),

         # text titles
         title = element_text(family = title_family,
                              size = rel(2),
                              colour = theme_sl_data$colors$title),
         axis.title = element_blank(),

         # axes
         axis.text = element_text(face = "bold",size = rel(1)),
         axis.text.x = element_text(colour=NULL),
         axis.text.y = element_text(colour = NULL,
                                    hjust = 1,
                                    vjust = 0.5),

         axis.ticks = element_line(colour = NULL),
         axis.ticks.x = element_line(colour = NULL),
         axis.ticks.y = element_blank(),
         axis.line = element_line(),
         axis.line.y = element_blank(),

         # legend
         legend.title = element_text(size = rel(0.6)),
         legend.background = element_rect(),
         legend.position = "right",
         legend.direction = "vertical",
         legend.box = "vertical",

         # grid
         panel.grid = element_line(colour = NULL, linetype = 3),
         panel.grid.major = element_line(colour = theme_sl_data$colors$grid),
         panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank(),

         # plot, strip
         plot.title = element_text(hjust = 0.5, face = "bold",
                                   margin = margin(0, 0, 1, 0, "lines")),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0,
                                     size = rel(0.5),
                                     colour = theme_sl_data$colors$text,
                                     margin = margin(3, 0, 0, 0, "lines")),
         plot.margin = unit(c(1, 1, 1, 1), "lines"),
         strip.background = element_rect(fill = theme_sl_data$colors$strip.bg),

         complete = T,
         validate = T
         ))
}



geom_nytlab <- function(mapping=NULL, data=NULL,
                        stat="identity", position="identity",
                        ...,
                        parse = FALSE,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = NA,
                        inherit.aes = FALSE) {
  layer(data = data.frame(),
        mapping = mapping,
        stat = stat,
        geom = GeomNYTLab,
        position = position,
        inherit.aes = inherit.aes,
        params = list(
          parse = parse,
          label.padding = label.padding,
          label.r = label.r,
          label.size = label.size,
          ...
        )
      )
}

default_text_col <- theme_sl_data$colors$text

GeomNYTLab <- ggproto("GeomNYTLab", Geom,
    required_aes = c("label"),
    optional_aes = c("y"),

    default_aes = aes(colour = "gray48", fill = "white",
      size = 3.88, angle = 0, hjust=0, vjust=0.5,
      alpha = NA, family = "", fontface = 1, lineheight = 1.2),

    draw_panel = function(data, panel_params, coord, parse, label.padding, label.r, label.size) {
      lab <- data$label
      if (parse) {
        lab <- parse(text = as.character(lab))
      }

      data <- coord$transform(data, panel_params)
      if (is.character(data$vjust)) {
        data$vjust <- compute_just(data$vjust, data$y)
      }
      if (is.character(data$hjust)) {
        data$hjust <- compute_just(data$hjust, data$x)
      }

      if (is.null(data$y)) {
        data$y <- panel_params$y.major[length(panel_params$y.major)]
      }

      #textGrob(
      #  lab,
      #  0, data$y, default.units="native",
      #  hjust = data$hjust, vjust = data$vjust,
      #  rot = data$angle,
      #  gp = gpar(
      #    col = alpha(data$colour, data$alpha),
      #    fontsize = data$size * .pt,
      #    fontfamily = data$family,
      #    fontface = data$fontface,
      #    lineheight = data$lineheight
      #  )
      #)

			grobs <- lapply(1:nrow(data), function(i) {
				row <- data[i, , drop = FALSE]
				NYTLabelGrob(lab[i],
					#x = unit(row$x, "native"),
          x = 0,
					y = unit(row$y, "native"),
					just = c(row$hjust, row$vjust),
					padding = label.padding,
					r = label.r,
					text.gp = gpar(
						col = row$colour,
						fontsize = row$size * .pt,
						fontfamily = row$family,
						fontface = row$fontface,
						lineheight = row$lineheight
					),
					rect.gp = gpar(
						col = row$colour,
						fill = alpha(row$fill, row$alpha),
						lwd = label.size * .pt
					)
				)
			})
			class(grobs) <- "gList"

      ggname <- function(prefix, grob) {
        grob$name <- grobName(grob, prefix)
        grob
      }
			ggname("geom_nytlab", grobTree(children = grobs))
    },
    
    draw_key = draw_key_text
  )

NYTLabelGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {

  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
    name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob")
}

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
