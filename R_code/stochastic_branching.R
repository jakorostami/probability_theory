# ---- This is a part of probability theory regarding extinction probabilities



# Branching processes serves well to model reproduction, population growth etcetera
# If you know about random forests and tree based algorithms, then this should be intuitive to you



# - If we have a population of individuals where each produces
# - a random number of children (independently from eachother) which follows some probability distribution
# - Such that an individual has m children with probability q_m for m >= 0, independent of other individuals.
# - Now call m the offspring distribution
# - For each generation, the population grows or declines
# - Have D to be the number of individuals of the z:th generation for z >= 0
# - Declare D_0 = 1 such that the population starts with 1 individual
# - Now we have a branching process for the sequence D_0, D_1, ..., D_z


# Since the number of individuals only depends on the previous generation
# and the number of their offspring this makes a branching process a Markov chain


# Start with a Uniform offspring U~[0, 6]

stochastic_branching = function(z){
  
  r = c(1, rep(0, z))
  
  for(j in 2:(z+1)){
    
    r[j] = sum(sample(0:6, r[j-1], replace = TRUE))
    
  }
  
  return(r)
  
}

n_sim = 50000

trace1 = rep(0, n_sim)
trace2 = rep(0, n_sim)

for(u in 1:n_sim){
  limbs = stochastic_branching(9)
  
  trace1[u] = if(limbs[7] == 0){   # Probability that the process goes extinct by the 6th generation
    1
  } else{
    0
  }
  
  trace2[u] = if(limbs[10] == 0){   # Used for later, extinction probability by 9th generation
    1
  } else{
    0
  }
}



mean(trace1) 
mean(trace2)


prob_gen_func = function(v){
  (1/7) * (1 + v + v**2 + v**3 + v**4 + v**5 + v**6)
}

prob_gen_func(    # Numerical solver via probability generating function
  prob_gen_func(
    prob_gen_func(
      prob_gen_func(
        prob_gen_func(
          prob_gen_func(0)
        )
      )
    )
  )
) -> nsolver


# Should be approx. 0  between simulating a branching process vs. numerical solution
mean(nsolver - trace1)




# - What if we had a Poisson offspring with lambda = 0.7, what would the total descendants be
# - and how would the first and second moments look like from its distribution?

stochastic_poisson_branch = function(r, lambda){
  u = c(1, rep(0, r))
  
  for(k in 2:(r+1)){
    
    u[k] = sum(rpois(u[k-1], lambda))
    
    
  }
  
  return(u)
  
}


n_gen = 5000

# Let us say that extinction happens by 30th gen

tracer = replicate(n_gen, sum(stochastic_poisson_branch(30, 0.7)))


mean(tracer); var(tracer); sd(tracer)

# Moments of the total progeny distribution 
paste("Average descendants:", round(mean(tracer), 2), "--", "Variance descendants:", 
      round(var(tracer), 2), "--", "Standard deviation descendants:", round(sd(tracer), 2))

hist(tracer, main = "Distribution of total descendants from a Poisson offspring")

