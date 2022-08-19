# Example excercise from the book "Introduction to Stochastic processes with R"



# --- Exercise 5.5: Exhibit a Metropolis-Hastings algorithm to sample from the distribution
#      ----------------------------------------------------------------
#      1            2           3          4            5            6
#      ----------------------------------------------------------------
#      0.01         0.39        0.11      0.18          0.26       0.05
#      ----------------------------------------------------------------

# We can start by declaring them below

x = 1:6
p = c(0.01, 0.39, 0.11, 0.18, 0.2, 0.05)

plot(x, p, type="h", col="blue", main="Distribution to sample from")



# Start by building the sampler

sampler = function(historical, novel){
  
  p = p
  
  a = p[novel] / p[historical]
  
  if(a >= 1){
    return(novel)
  } else {
    if(runif(1) < a){
      return(novel)
    } else {
      return(historical)
    }
  }
}

# Define number of trials

n = 10000


# Create an empty vector to put in the sampled values in
simulations = rep(0, n)

for(j in 1:n){
  
  state = 1
  
  for(u in 1:100){
    state <- sampler(state, sample(1:length(p), 1))
  }
  
  simulations[j] = state
}


# Put the sampled distribution in a table

table(simulations)/n -> sampled_p

# Plot them next to eachother

par(mfrow=c(1,2))

plot(x, p, type="h", col="blue", main="Distribution to sample from", ylim=c(0, 0.417));abline(h=max(p), col="red");abline(h=mean(p), lty=2, col="green")
plot(sampled_p, main="Sample distribution with MH algorithm");abline(h=max(p), col="red");abline(h=mean(p), lty=2, col="green")
