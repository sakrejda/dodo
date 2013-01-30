library(dodo)
data('life_cycle-ATS-smolt')

pop$add(
  stage="stock",
  args=list(
    n_bins=350,
    limits=c(min=0, max=350),
    density=function(y) dnorm(x=y, mean=15, sd=1)
  )
)
pop$run(n=4, e=envir, o='/tmp')
pop$clear()

# Alternatively, if you want to see what it looks like before sync:
#pop$step(synchronize=FALSE)
#pop$sync()

# Save the whole population object to a file, or into a directory (under a
# safe/random name:
#pop$save(path="/tmp")

# Save the "sub-populations" to a file, or into a directory (under a
# safe/random name:
#pop$save_populations(path="/tmp")

# Load environmental data, run a hundred iterations.
#env_file='~/env.csv'
#library(dodo); data('life_cycle-TSD')
#pop$run(n=100, e=read.csv(env_file), o='/tmp')

# Clear the model to restart from a fresh population (blank, need to add
# some individuals to start it again:
#pop$clear()



