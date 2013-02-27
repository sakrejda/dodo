library(dodo)
data('estimates-ATS-smolt-M2'); s <- s2
data('environment-ATS-smolt')
data('life_cycle-ATS-smolt'); rm(pop, stages, transformations)
 

## Data:
stages <- structure(
	list(
	stage_name = c(
		"stock", 												"eggs", 												"fry", 
		"zero_summer_parr", 						"zero_autumn_parr", 						"zero_winter_parr",
		"one_spring_parr", 							"one_summer_parr", 							"one_autumn_parr", 
		"one_winter_parr", 							"two_spring_parr", 							"two_summer_parr", 
		"two_autumn_parr", 							"two_winter_parr", 							"three_spring_parr", 
		"three_summer_parr",	 					"three_autumn_parr", 						"three_winter_parr",
		"four_spring_parr", 						"four_summer_parr",	 						"four_autumn_parr", 
		"four_winter_parr", 						"two_spring_riverine", 					"three_spring_riverine", 
		"four_spring_riverine", 				"dead_stock", 									"dead_eggs", 
		"dead_fry", 										"dead_zero_summer_parr", 				"dead_zero_autumn_parr", 
		"dead_zero_winter_parr",	 			"dead_one_spring_parr", 				"dead_one_summer_parr", 
		"dead_one_autumn_parr", 				"dead_one_winter_parr", 				"dead_two_spring_parr", 
		"dead_two_summer_parr", 				"dead_two_autumn_parr", 				"dead_two_winter_parr", 
		"dead_three_spring_parr",				"dead_three_summer_parr", 			"dead_three_autumn_parr", 
		"dead_three_winter_parr",				"dead_four_spring_parr", 				"dead_four_summer_parr", 
		"dead_four_autumn_parr",				"dead_four_winter_parr", 				"dead_two_spring_riverine", 
		"dead_three_spring_riverine", 	"dead_four_spring_riverine",		"two_spring_riverine_total",
		"three_spring_riverine_total",	"four_spring_riverine_total"),

	age_in_months = c(
		-1L, 0L, 7L, 9L, 12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L, 
		42L, 45L, 48L, 51L, 54L, 57L, 60L, 63L, 31L, 43L, 55L, -1L, 0L, 
		 7L, 9L, 12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L, 42L, 
	 45L, 48L, 51L, 54L, 57L, 60L, 63L, 31L, 43L, 55L, 31L, 43L, 55L), 
			 
	duration = c(
		0L, 7L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
	 	3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 0L, 7L, 2L, 3L, 3L, 3L, 3L, 3L, 
	 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L,
		0L, 0L, 0L), 

	moy = c(
		9L, 9L, 4L, 6L, 9L, 12L, 3L, 6L, 9L, 12L, 3L, 6L, 9L, 12L, 3L, 6L, 9L, 
		12L, 3L, 6L, 9L, 12L, 4L, 4L, 4L, 9L, 9L, 4L, 6L, 9L, 12L, 3L, 6L, 9L, 
		12L, 3L, 6L, 9L, 12L, 3L, 6L, 9L, 12L, 3L, 6L, 9L, 12L, 4L, 4L, 4L,
		4L, 4L, 4L),

	ageInSamples = structure(c(
		NA, NA, NA, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 
		14L, 11L, 12L, 13L, 14L, 15L, NA, NA, NA, NA, NA, NA, 1L, 2L, 3L, 
		4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 11L, 12L, 13L, 14L, 
		15L, NA, NA, NA, NA, NA, NA), .Label = c("1", "2", "3", "4", "5", "6", "7", "8", 
		"9", "10", "11", "12", "13", "14", "15"), class = "factor"),

	sizeAtAge = c(
		NA, 			NA, 		NA, 
						61.9, 	69.0,		71.8,
		74.5,	 106.8,  114.7,  122.8,
	 121.4,	 141.7,  144.8,  149.3,
	 150.6,  163.1,  162.9,  167.9,
	 168.9,  181.9,	 182.9,	 183.0,
	 rep(NA,31)
	), 
	sdAtAge = c(
		 NA,		 NA,				NA,
	 					3.0,			 5.6,			 6.6,
		7.7,    9.9,      12.4,     12.3,
	 14.5,	 10.8,		  11.8,     11.9,
	 12.1,	 11.4,			12.9,     11.9,
	 12.1,   11.4,      12.9,     12.9,
	 rep(NA,31))
	),

	.Names = c("stage_name", "age_in_months", "duration", "moy",
						 "ageInSamples", "sizeAtAge", "sdAtAge"), 
	row.names = c(
		"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
		"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "36", 
		"49", "110", "24", "31", "41", "51", "61", "71", "81", "91", 
		"101", "111", "121", "131", "141", "151", "161", "171", "181", 
		"191", "201", "211", "221", "231", "361", "491", "500", "501", "502"), 

class = "data.frame")


transformations <- structure(
	list(order = 1:6, model = c("clear_smolts", "smolt", "die", "grow", "age", "stock")), 
	.Names = c("order", "model"), 
	row.names = c(1L, 2L, 3L, 4L, 5L, 6L), class = "data.frame")




## Aging:
pa <- new('pGLM',
	formula = ~ 1,
	family = gaussian(link="identity"),
	coefficients = list(
		"(Intercept)" = 1
	),
	epsilon = function(n=1) {return(rep(0,n))},
	samp = FALSE
)


## Stocking:
sp <- new('pGLM', 
	formula = ~ 1 + season,
	family = binomial(link=rlogit),
	coefficients = list(
		"(Intercept)" = -100, 
		season2 = -100,
		season3 = -100,
		season4 = 100+1.3
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)
sf <- new('pGLM',
	formula = ~ 1 + season, 
	family = poisson(link=log),
	coefficients = list(
		"(Intercept)"=-100,
		season2 = -100,
		season3 = -100,
		season4 = 100+2
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.01) },
	samp = FALSE
)
ss <- new('pGLM',
	formula = ~ 1,
	family = gaussian(),
	coefficients = list("(Intercept)" = 5),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.5) },
	samp = FALSE
)

## Fry growth:
fg <- new('pGLM',
	formula = ~ 1,
	family = gaussian(),
	coefficients = list(
		"(Intercept)"  = 55
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=1) },
	samp = FALSE
)

## Fry survival:
fm <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=rlogit),
	coefficients = list(
		"(Intercept)"  = -0.9
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.2) },
	samp = FALSE
)


## Growth of juveniles:
jg <- new('pGLM',
	formula = ~ 1 + ageInSamples + ageInSamples:WB + ageInSamples:flow + ageInSamples:sizes + offset(sizes),
	family = gaussian(link="log"),
	coefficients = list(
		"(Intercept)"  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 1],
		ageInSamples2  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 2],
		ageInSamples3  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 3],
		ageInSamples4  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 4],
		ageInSamples5  = s[['gr_beta_0']],
		ageInSamples6  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 6],
		ageInSamples7  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 7],
		ageInSamples8  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 8],
		ageInSamples9  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 9],
		ageInSamples10 = s[['gr_beta_0']] +  s[['gr_beta_ais']][10],
		ageInSamples11 = s[['gr_beta_0']] +  s[['gr_beta_ais']][11],
		ageInSamples12 = s[['gr_beta_0']] +  s[['gr_beta_ais']][12],
		ageInSamples13 = s[['gr_beta_0']] +  s[['gr_beta_ais']][13],
		ageInSamples14 = s[['gr_beta_0']] +  s[['gr_beta_ais']][14],
		ageInSamples15 = rep(0, length(s[['gr_beta_0']])),

	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=1) },
	samp = FALSE
)

## Mortality of juveniles:
jm <- new('pGLM', 
	formula = ~ 1 + ageInSamples,
	family = binomial(link=rlogit),
	coefficients = list(
		"(Intercept)"  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 1],
		ageInSamples2  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 2],
		ageInSamples3  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 3],
		ageInSamples4  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 4],
		ageInSamples5  = s[['phi_beta_0']],
		ageInSamples6  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 6],
		ageInSamples7  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 7],
		ageInSamples8  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 8],
		ageInSamples9  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 9],
		ageInSamples10 = s[['phi_beta_0']] +  s[['phi_beta_ais']][10],
		ageInSamples11 = s[['phi_beta_0']] +  s[['phi_beta_ais']][11],
		ageInSamples12 = s[['phi_beta_0']] +  s[['phi_beta_ais']][12],
		ageInSamples13 = s[['phi_beta_0']] +  s[['phi_beta_ais']][13],
		ageInSamples14 = s[['phi_beta_0']] +  s[['phi_beta_ais']][14],
		ageInSamples15 = rep(-100,length(s[['phi_beta_0']]))
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)


## Smolting of juveniles:
js1 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = -2.5
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

js2 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = 0.5
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

js3 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = 1.5
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

js4 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = 1.0
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)


## Make a blank population, this has to happen once:
pop <- population$new(
	stages = stages, transformations = transformations)

## Build/Add the models:

## Stock:
pop$add_model( 
	stage						=	'stock',							
	transformation	=	'stock',
	model						=	staged_reproduction_factory('stock','fry', sp, sf, ss))

## Fry:
pop$add_model(
	stage						=	'fry',							
	transformation	=	'grow',
	model						=	staged_growth_factory('fry', fg))
pop$add_model(
	stage						=	'fry',
	transformation	=	'die',
	model						= staged_transition_factory('fry', 'dead_fry', fm))
pop$add_model(
	stage						= 'fry',
	transformation 	= 'age',
	model						= staged_transition_factory('fry','zero_summer_parr', pa))

## Zero plus.
pop$add_model(
	stage						=	'zero_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('zero_summer_parr', jg))
pop$add_model(
	stage						=	'zero_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('zero_summer_parr', 'dead_zero_summer_parr', jm))
pop$add_model(
	stage						=	'zero_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('zero_summer_parr', 'zero_autumn_parr', pa))

pop$add_model(
	stage						=	'zero_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('zero_autumn_parr', jg))
pop$add_model(
	stage						=	'zero_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('zero_autumn_parr', 'dead_zero_autumn_parr', jm))
pop$add_model(
	stage						=	'zero_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('zero_autumn_parr', 'zero_winter_parr', pa))


pop$add_model(
	stage						=	'zero_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('zero_winter_parr', jg))
pop$add_model(
	stage						=	'zero_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('zero_winter_parr', 'dead_zero_winter_parr', jm))
pop$add_model(
	stage						=	'zero_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('zero_winter_parr', 'one_spring_parr', pa))

## One plus:
pop$add_model(
	stage						=	'one_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_spring_parr', jg))
pop$add_model(
	stage						=	'one_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_spring_parr', 'dead_one_summer_parr', jm))
pop$add_model(
	stage						=	'one_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_spring_parr', 'one_summer_parr', pa))

pop$add_model(
	stage						=	'one_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_summer_parr', jg))
pop$add_model(
	stage						=	'one_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_summer_parr', 'dead_one_summer_parr', jm))
pop$add_model(
	stage						=	'one_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_summer_parr', 'one_autumn_parr', pa))

pop$add_model(
	stage						=	'one_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_autumn_parr', jg))
pop$add_model(
	stage						=	'one_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_autumn_parr', 'dead_one_autumn_parr', jm))
pop$add_model(
	stage						=	'one_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_autumn_parr', 'one_winter_parr', pa))


pop$add_model(
	stage						=	'one_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_winter_parr', jg))
pop$add_model(
	stage						=	'one_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_winter_parr', 'dead_one_winter_parr', jm))
pop$add_model(
	stage						=	'one_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_winter_parr', 'two_spring_parr', pa))

## Two plus:
pop$add_model(
	stage						=	'two_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_spring_parr', jg))
pop$add_model(
	stage						=	'two_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_spring_parr', 'dead_two_spring_parr', jm))
pop$add_model(
	stage						=	'two_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_spring_parr', 'two_summer_parr', pa))

pop$add_model(
	stage						=	'two_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_summer_parr', jg))
pop$add_model(
	stage						=	'two_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_summer_parr', 'dead_two_summer_parr', jm))
pop$add_model(
	stage						=	'two_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_summer_parr', 'two_autumn_parr', pa))

pop$add_model(
	stage						=	'two_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_autumn_parr', jg))
pop$add_model(
	stage						=	'two_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_autumn_parr', 'dead_two_autumn_parr', jm))
pop$add_model(
	stage						=	'two_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_autumn_parr', 'two_winter_parr', pa))

pop$add_model(
	stage						=	'two_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_winter_parr', jg))
pop$add_model(
	stage						=	'two_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_winter_parr', 'dead_two_winter_parr', jm))
pop$add_model(
	stage						=	'two_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_winter_parr', 'three_spring_parr', pa))

## Three plus:
pop$add_model(
	stage						=	'three_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_spring_parr', jg))
pop$add_model(
	stage						=	'three_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_spring_parr', 'dead_three_spring_parr', jm))
pop$add_model(
	stage						=	'three_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_spring_parr', 'three_summer_parr', pa))

pop$add_model(
	stage						=	'three_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_summer_parr', jg))
pop$add_model(
	stage						=	'three_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_summer_parr', 'dead_three_summer_parr', jm))
pop$add_model(
	stage						=	'three_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_summer_parr', 'three_autumn_parr', pa))

pop$add_model(
	stage						=	'three_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_autumn_parr', jg))
pop$add_model(
	stage						=	'three_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_autumn_parr', 'dead_three_autumn_parr', jm))
pop$add_model(
	stage						=	'three_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_autumn_parr', 'three_winter_parr', pa))

pop$add_model(
	stage						=	'three_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_winter_parr', jg))
pop$add_model(
	stage						=	'three_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_winter_parr', 'dead_three_winter_parr', jm))
pop$add_model(
	stage						=	'three_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_winter_parr', 'four_spring_parr', pa))

## Four plus:
pop$add_model(
	stage						=	'four_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_spring_parr', jg))
pop$add_model(
	stage						=	'four_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_spring_parr', 'dead_four_spring_parr', jm))
pop$add_model(
	stage						=	'four_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('four_spring_parr', 'four_summer_parr', pa))

pop$add_model(
	stage						=	'four_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_summer_parr', jg))
pop$add_model(
	stage						=	'four_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_summer_parr', 'dead_four_summer_parr', jm))
pop$add_model(
	stage						=	'four_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('four_summer_parr', 'four_autumn_parr', pa))

pop$add_model(
	stage						=	'four_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_autumn_parr', jg))
pop$add_model(
	stage						=	'four_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_autumn_parr', 'dead_four_autumn_parr', jm))
pop$add_model(
	stage						=	'four_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('four_autumn_parr', 'four_winter_parr', pa))

pop$add_model(
	stage						=	'four_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_winter_parr', jg))
pop$add_model(
	stage						=	'four_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_winter_parr', 'dead_four_winter_parr', jm))

## Smolting:
pop$add_model(
	stage						= 'two_spring_parr',
	transformation	= 'smolt',
	model						=	staged_transition_factory('two_spring_parr', 'two_spring_riverine', js2))
pop$add_model(
	stage						= 'three_spring_parr',
	transformation	= 'smolt',
	model						=	staged_transition_factory('three_spring_parr', 'three_spring_riverine', js3))
pop$add_model(
	stage						= 'four_spring_parr',
	transformation	= 'smolt',
	model						=	staged_transition_factory('four_spring_parr', 'four_spring_riverine', js4))
pop$add_model(
	stage						= 'two_spring_riverine',
	transformation  = 'clear_smolts',
	model						=	staged_transition_factory(
											'two_spring_riverine', 'two_spring_riverine_total', pa))
pop$add_model(
	stage						= 'three_spring_riverine',
	transformation  = 'clear_smolts',
	model						=	staged_transition_factory(
											'three_spring_riverine', 'three_spring_riverine_total', pa))
pop$add_model(
	stage						= 'four_spring_riverine',
	transformation  = 'clear_smolts',
	model						=	staged_transition_factory(
											'four_spring_riverine', 'four_spring_riverine_total', pa))

################################################################################
################################################################################
## To get everything above this line, one would do:
##		data("life_cycle-ATS-smolt")
##
################################################################################
################################################################################

pop$clear()
#save(s2, file='../../data/estimates-ATS-smolt-M1.RData')
#save(envir, file='../../data/environment-ATS-smolt.RData')
save(stages, transformations, envir, pop, file='../../data/life_cycle-ATS-smolt.RData')

