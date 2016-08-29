#
#         Theory of Statistics - CQAS 721
#
#            Probability as Frequency
#
#      source('stat-721-prob-as-frequency.R')
#
#            Fall 2013, Ernest Fokoue
#

# Note: Click in the plot and Press ESC to stop before the end

# The event A occurs with probability p

p = 0.75
stop = FALSE
n = 0
xord = yord = vx = NULL

while (!stop) {
  x  = rbinom(1, 1, p)
  n  = n + 1
  vx = c(vx, x)
  prob = sum(vx) / n
  xord = c(xord, n)
  yord = c(yord, prob)

  plot(xord, yord, type = 'l', col = 'red', xlab = 'n', ylab = 'Pr[A] = n(A) / n')
  abline(h = p)

  stop = (n == 100000)
}

abline(h = p)

# Exercise: Change the probability of the event and see what you get
