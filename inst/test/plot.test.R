library(dodo)
library(reshape2)
library(ggplot2)

stgs <- letters[1:4]

o <- within(data = list(), expr = {
	stages = stgs
	n_bins = rbinom(n=length(stgs), size=5, prob=0.4) + 1
	obj <- new('block_projection', stages=stages, bins=n_bins)
})


pl <- visualizeBlockMatrix(M=o$obj)



