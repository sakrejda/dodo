library(dodo)

o <- within(data = list(), expr = {
	stages = letters
	n_bins = rbinom(n=length(letters), size=5, prob=0.3) + 1
	minima = rep(0,length(letters))
	maxima = rep(100, length(letters))
	allowed_projections = c('survive', 'age')

	pop <- new('population', 
							stages = stages,
							bins = n_bins,
							minima = minima,
							maxima = maxima)



})


