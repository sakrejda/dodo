library(dodo)

o <- within(data = list(), expr = {
	stages = letters
	allowed_projections = c('survive', 'age')
	lc <- new('life_cycle', stages=stages, allowed_projections = allowed_projections)

})


