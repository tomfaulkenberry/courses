# lecture 6 - characteristics of response time distributions

# build a random walk accumulator model
reps = 10000
samples = 5000 # think of these as milliseconds (1 sample per millisecond)

# parameters of model
driftRate = 0.0
threshold = 3

# build empty structures for storing RTs, responses, and accumulated evidence
RTs = numeric(reps)
responses = numeric(reps)
evidence = matrix(0, reps, samples + 1)

# run the simulation
for (i in 1:reps){
  evidence[i, ] = cumsum(c(0, rnorm(n = samples, mean = driftRate, sd = 0.3)))
  p = which(abs(evidence[i,]) > threshold)[1]
  responses[i] = sign(evidence[i,p])
  RTs[i] = p
}

# plot some of the random walks and a histogram of correct RTs
par(mfrow = c(2,1), cex.main = 0.9)

howmany = 5
plot(1:max(RTs[1:howmany])+10, 
     type = "n",
     las = 1, bty="n",
     ylim = c(-threshold-0.5, threshold+0.5), 
     xlab = "Response time (ms)",
     ylab = "Evidence"
     )
for (i in 1:howmany){
  lines(evidence[i, 1:(RTs[i])])
}
abline(h = c(threshold, -threshold), lty=2)


# plot histograms of RTs
topRT = RTs[responses > 0]
topErr = 1- length(topRT)/reps
hist(topRT, breaks=30,
     xlim = c(0, max(RTs)),
     xlab = "Response time (ms)",
     main = sprintf("Correct responses: M = %.1f ms, err = %.3f, ", mean(topRT), topErr)
     )

