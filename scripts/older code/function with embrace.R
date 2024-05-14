mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)
plot(mesh)

m3 <- sdmTMB(
  data = pcod,
  formula = density ~ poly(log(depth), 2),
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)

yrs <-sort(unique(pcod$year))

add_yr_fun <- function(x){
	
	df <- qcs_grid %>%
		mutate(year = x)
}

qcs_grid <- map(yrs, add_yr_fun) %>% bind_rows()

predictions <- predict(m3, newdata = qcs_grid)

plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, fill = {{ column }})) +
    geom_raster() +
    coord_fixed()
}

# not in a function
plot_map(predictions, exp(est)) +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "yellow", limits = c(0, quantile(exp(predictions$est), 0.995))
  ) +
  facet_wrap(~year) +
  ggtitle("Prediction (fixed effects + all random effects)",
    subtitle = paste("maximum estimated biomass density =", round(max(exp(predictions$est))))
  )

# in a function
cols <- names(predictions %>% dplyr::select(contains("est")))

plot_fun <- function(col){
	
	p <- plot_map(predictions, col)
	
}

plots <- purrr::map(cols, plot_fun)

# now try with two vars

new_df_fun <- function(sp){
	
	df <- predictions %>%
		mutate(species = sp)
}

sp <- c("pol", "pcod", "yfin")

dfs <- map(sp, new_df_fun)

# run fun
cols <- names(predictions %>% dplyr::select(contains("est")))

plot_fun <- function(df, col){
	
	p <- plot_map(df, col)
	
}

fdf <- crossing(
	df = dfs,
	col = cols
)

plots <- purrr::map2(fdf$df, fdf$col, plot_fun)
