library(dodo)

o <- within(data = list(), expr = {
	stages = letters
	n_bins = rbinom(n=length(letters), size=5, prob=0.3) + 1
	minima = rep(0,length(letters))
	maxima = rep(100, length(letters))
	traits = list()
	obj <- new('staged_block_distribution', stages=stages, n_bins=n_bins,
						minima=minima, maxima=maxima)
})

obj_summary <- o$obj$summary
print(obj_summary)

