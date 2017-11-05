library(tidyverse)
library(tilemapr)
library(viridis)
library(usmap)

source("theme_sl.R")

black.col <- "red"
white.col <- "darkgreen"

cat("Analyzing death disparity...\n")

deaths.data <- read.csv("KillingsByState2013to2016_clean.csv",
                        comment.char="#")
state.dc.name <- c(state.name, "District of Columbia")
state.dc.abb <- c(state.abb, "DC")
deaths.data$state.abb <- state.dc.abb[match(deaths.data$state, state.dc.name)]

death.disparity.plot <- ggplot(deaths.data,
                               aes(x=reorder(state.abb, disparity),
                               y=disparity)) +
    geom_segment(aes(xend=state.abb,
                     yend=0),
                 col=black.col, alpha=0.5) +
    geom_point(aes(size=percent.population.african.american),
               col=black.col, alpha=0.5) +
    scale_y_continuous(label=scales::unit_format("", scale=100),
                       breaks=c(0, 0.1, 0.21, 0.45)) +
    guides(colour=F) +
    scale_size_continuous("% pop.",
                          guide=guide_legend(override.aes=list(colour="gray48")),
                          breaks=c(0.1, 0.5),
                          labels=scales::percent) +
    scale_color_gradient2(low="black", mid="#990000", high="red",
                          midpoint=median(deaths.data$disparity)) +
    geom_nytlab(aes(label="points")) +
    theme_sl() +
    theme(axis.line.x=element_blank(),
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=rel(0.75)),
          legend.justification=c(0,0),
          legend.position=c(0,0.3),
          legend.direction="vertical",
          legend.key.size=unit(1, "lines"))


states_map <- square_usa(d=0.99, style="Wall Street Journal")
state.centers <- square_usa(style="Wall Street Journal", center=T)

death.disparity.map <- ggplot(states_map,
                        aes(map_id = region)) +
  geom_map(aes(), fill="gray",
           map=states_map) +
  geom_map(data=deaths.data,
           aes(fill=disparity,
               map_id=tolower(state)),
           map=states_map) +
  expand_limits(x=states_map$long, y=states_map$lat) +
  geom_text(data=state.centers,
            aes(x=long, y=lat, label=region_abr),
            col="white",
            inherit.aes=F) +
  coord_fixed() +
  scale_fill_viridis("Disparity",
                     guide="colourbar",
                     breaks=c(0, 0.1, 0.4),
                     labels=c(0, 10, "40 pts")) +
  theme_void() +
  theme(legend.position=c(0.935, 0.036),
        legend.justification=c(1, 0),
        legend.key.height=unit(2.2, "lines"),
        legend.key.width=unit(2, "lines"))

cat("Death disparity analysis complete.\n")


cat("Analyzing income disparity...\n")

cat("Loading income data...\n")
income.data <- read.csv("ipums_RaceStateIncome.csv")
cat("Income data loaded.\n")

income.sum <- function(ftotinc, perwt) {
  valids <- ftotinc != 9999999
  income <- sum(as.numeric(ftotinc[valids] * perwt[valids]))
  return(data.frame(income, n=sum(perwt)))
}
income.disparity <- function(income, n, race) {
  white.code <- 1
  black.code <- 2
  total.income <- sum(income)
  total.pop <- sum(n)
  black.income.pct <- sum(income[race == black.code]) / total.income
  white.income.pct <- sum(income[race == white.code]) / total.income
  black.pop.pct <- sum(n[race == black.code]) / total.pop
  white.pop.pct <- sum(n[race == white.code]) / total.pop

  black.disparity <- black.income.pct - black.pop.pct
  white.disparity <- white.income.pct - white.pop.pct
  return(data.frame(black.income.pct,
                    black.pop.pct,
                    black.disparity,
                    white.income.pct,
                    white.pop.pct,
                    white.disparity))
}

cat("Summarising income data...\n")
inc.disp <- income.data %>%
  group_by(STATEFIP, RACE) %>%
  do(income.sum(.$FTOTINC, .$PERWT)) %>%
  group_by(STATEFIP) %>%
  do(income.disparity(.$income, .$n, .$RACE))
cat("Summary complete.\n")

inc.disp$state.abb <- sapply(inc.disp$STATEFIP, function(f) fips_info(f)$abbr)
inc.disp$state <- sapply(inc.disp$STATEFIP, function(f) fips_info(f)$full)

max.data = subset(inc.disp,
                  white.disparity == max(inc.disp$white.disparity))
nstates <- length(inc.disp[[1]])
income.disparity.plot <- ggplot(inc.disp,
                                aes(x=reorder(state.abb, white.disparity))) +
    geom_hline(aes(yintercept=0), col="gray48") +
    geom_segment(aes(xend=state.abb,
                     y=white.disparity,
                     yend=0),
                 col=white.col, alpha=0.5) +
    geom_segment(aes(xend=state.abb,
                     y=black.disparity,
                     yend=0),
                 col=black.col, alpha=0.5) +
    geom_point(aes(y=white.disparity,
                   size=white.pop.pct),
               col=white.col, alpha=0.5,
               show.legend=F) +
    geom_point(aes(y=black.disparity,
                   size=black.pop.pct),
               col=black.col, alpha=0.5) +
    scale_y_continuous(label=scales::unit_format("", scale=100),
                       breaks=c(-0.18, -0.05, -0.02, 0, 0.04, 0.09, 0.19)) +
    scale_size_continuous("% pop.",
                          guide=guide_legend(override.aes=list(colour="gray48")),
                          breaks=c(0, 0.25, 0.75),
                          labels=scales::percent) +
    geom_nytlab(aes(label="points")) +
    theme_sl() +
    theme(axis.line.x=element_blank(),
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=rel(0.75)),
          legend.justification=c(0,0.5),
          legend.position=c(0,0.2),
          legend.direction="vertical",
          legend.key.size=unit(1, "lines"))


income.disparity.map <- ggplot(states_map,
                               aes(map_id = region)) +
  geom_map(aes(), fill="gray",
           map=states_map) +
  geom_map(data=inc.disp,
           aes(fill=white.disparity,
               map_id=tolower(state)),
           map=states_map) +
  expand_limits(x=states_map$long, y=states_map$lat) +
  geom_text(data=state.centers,
            aes(x=long, y=lat, label=region_abr),
            col="white",
            inherit.aes=F) +
  coord_fixed() +
  scale_fill_viridis("Disparity",
                     guide="colourbar",
                     breaks=c(0, 0.05, 0.18),
                     labels=c(0, 5, "18 pts")) +
  theme_void() +
  theme(legend.position=c(0.935, 0.036),
        legend.justification=c(1, 0),
        legend.key.height=unit(2.2, "lines"),
        legend.key.width=unit(2, "lines"))
