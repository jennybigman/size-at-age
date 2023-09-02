# pcod example under Basic Use

library(dplyr)
library(ggplot2)
library(sdmTMB)
head(pcod)

mesh <- make_mesh(pcod, xy_cols = c("X", "Y"), cutoff = 10)

fit <- sdmTMB(
	density ~ s(depth),
	data = pcod,
	mesh = mesh,
	family = tweedie(link = "log"),
	spatial = "on"
)


pcod$fyear <- as.factor(pcod$year)

fit_pcod <- sdmTMB(
	density ~ s(depth),
							data = pcod,
							mesh = mesh,
							family = tweedie(link = "log"),
							spatial = "on")




