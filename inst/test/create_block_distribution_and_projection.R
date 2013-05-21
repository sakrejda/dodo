library(dodo)

o <- within(data = list(), expr = {
	stages = letters
	n_bins = rbinom(n=length(letters), size=5, prob=0.3) + 1
	minima = rep(0,length(letters))
	maxima = rep(100, length(letters))
	distr <- new('block_distribution', stages=stages, n_bins=n_bins,
						minima=minima, maxima=maxima)
	distr_summary <- distr$summary
	proje <- new('block_projection', stages=stages, bins=n_bins)
	proje_plot <- proje$plot

	new_distr <- proje %*% distr	
})


