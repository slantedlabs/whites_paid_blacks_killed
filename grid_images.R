library(tidyverse)
library(cowplot)
library(grid)
library(extrafont)
library(Cairo)

source("analysis.R")
source("theme_sl.R")

text.fontfam <- "Palatino" 
fontfam <- "sans"
title.col <- theme_sl_data$colors$title
text.col <- theme_sl_data$colors$text


### Grid layouts
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid_text_box <- function(lines, vp=vplayout(2, 12),
                          gp=gpar(fontfamily=text.fontfam,
                                  col=text.col, cex=1.3),
                          x=0.5, y=0.5, line.gap=0.33, hjust=0.5) {
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }

  print_line <- function(line, line.num) {
    grid.text(line,
              vp=vp,
              x=unit(x, "npc"),
              y=unit(newline(y, line.num), "npc"),
              hjust=hjust,
              gp=gp)
  }
  sapply(1:length(lines), function(i) print_line(lines[i], i))
}

grid_death_disparity <- function() {

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(death.disparity.plot, vp = vplayout(2:9, 1:11))

  # Title
  grid.text(expression("Black People are " *
                       phantom("Killed")),
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))
  grid.text(expression(phantom("Black People are ") *
                       "Killed"),
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=black.col, cex=2))


  ## MAIN TEXT
  xval <- 0.8
  yval <- 0.4
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.vp <- vplayout(3, 3)
  block.hjust <- 0

  grid.text(expression("Black people make up much higher percentages of police killing victims"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("than they do the general population. They are over-represented"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("in the populations of police killing victims in 42 states, with"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("an average " *
                       phantom("disparity") *
                       " of 16 points. The eight remaining"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 3), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("an average ") *
                       "disparity" *
                       phantom(" of 16 points. The eight remaining")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 3), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=black.col, cex=block.cex))

  grid.text(expression("states have some of the lowest proportions of"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 4), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("black people in their populations."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 5), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))


  ## SPECIAL TEXT
  grid_text_box(c("Roughly",
                  "50% of",
                  "the DC",
                  "population",
                  "is black,",
                  "but 95%",
                  "of people",
                  "killed by",
                  "police in",
                  "DC are"),
                x=-0.15, y=0.72, hjust=0,
                line.gap=0.26,
                gp=gpar(fontfamily=text.fontfam, col=black.col,
                        cex=1.2))
}

grid_income_disparity <- function() {

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(income.disparity.plot, vp = vplayout(2:9, 1:11))

  # Title
  grid.text(expression("White People are " *
                       phantom("Paid")),
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))
  grid.text(expression(phantom("White People are ") *
                       "Paid"),
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=white.col, cex=2))


  ## MAIN TEXT
  xval <- 0.8
  yval <- 0.8
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.vp <- vplayout(3, 3)
  block.hjust <- 0

  grid.text(expression(phantom("White people") *
                       " receive a higher percentage of income than they make up"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression("White people" *
                       phantom(" receive a higher percentage of income than they make up")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=white.col, cex=block.cex))

  grid.text(expression("in population in all but one state, with an average"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression(phantom("disparity") *
                       " of 5 points."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression("disparity" *
                       phantom(" of 5 points.")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=white.col, cex=block.cex))



  xval <- 0.8
  yval <- 0.5
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.vp <- vplayout(7, 3)
  block.hjust <- 0

  grid.text(expression(phantom("Black people") *
                       ", on the other"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression("Black people" *
                       phantom(", on the other")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=black.col, cex=block.cex))

  grid.text(expression("hand, receive less than their population's share"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("of income in every state, with an average " *
                       phantom("disparity") *
                       " of -4 points."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("of income in every state, with an average ") *
                       "disparity" *
                       phantom(" of")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=black.col, cex=block.cex))


  ## SPECIAL TEXT
  grid_text_box(c("Roughly",
                  "40% of",
                  "the DC",
                  "population",
                  "is white,",
                  "but they",
                  "make 60%",
                  "of the",
                  "income"),
                x=-0.15, y=0.72, hjust=0,
                line.gap=0.26,
                gp=gpar(fontfamily=text.fontfam, col=white.col,
                        cex=1.2))
  grid_text_box(c("Roughly",
                  "48% of",
                  "the DC",
                  "population",
                  "is black,",
                  "but they",
                  "only make",
                  "30% of the",
                  "income"),
                x=-0.15, y=-3.75, hjust=0,
                line.gap=0.26,
                gp=gpar(fontfamily=text.fontfam, col=black.col,
                        cex=1.2))
}

grid_income_disparity_map <- function() {

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(income.disparity.map, vp = vplayout(1:9, 1:12))

  # Title
  grid.text(expression("Geography of Disparity between"),
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))
  grid.text(expression("White Income and White Population"),
            vp=vplayout(1, 1:12),
            y=unit(-0.1, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))

}

grid_death_disparity_map <- function() {

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(death.disparity.map, vp = vplayout(1:9, 1:12))

  # Title
  grid.text(expression("Geography of Disparity between"),
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))
  grid.text(expression("Black Deaths by Police"),
            vp=vplayout(1, 1:12),
            y=unit(-0.1, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))
  grid.text(expression("and Black Population"),
            vp=vplayout(1, 1:12),
            y=unit(-0.6, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))

}



make_death_disparity_svg <- function() {
  fname <- "images/death_disparity.svg"
  svg(fname, width=12, height=8)
  grid_death_disparity()
  dev.off()
}
make_income_disparity_svg <- function() {
  fname <- "images/income_disparity.svg"
  svg(fname, width=12, height=8)
  grid_income_disparity()
  dev.off()
}

make_death_disparity_map_svg <- function() {
  fname <- "images/death_disparity_map.svg"
  svg(fname, width=12, height=8)
  grid_death_disparity_map()
  dev.off()
}
make_income_disparity_map_svg <- function() {
  fname <- "images/income_disparity_map.svg"
  svg(fname, width=12, height=8)
  grid_income_disparity_map()
  dev.off()
}
