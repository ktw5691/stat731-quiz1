STAT-731 Recitation 1
========================================================
author: K. Tyler Wilcox
date: August 29, 2016
autosize: true
font-family: 'Lucida Console'


Welcome
========================================================

Goals:
- Install R and RStudio (highly recommended)
  - cran.r-project.org
  - www.rstudio.com
- Install R Packages
  - IPSUR
  - prob
  - distrEx
  - distr
- Why Use Computer Simulation?

Tossing Coins
========================================================

- First, let's write a simple R function that flips a coin
- We will set the number of coin flips with the `nflips` argument
- We can even make the coin fair or unfair with the `weights` argument



```r
flip_coin = function(nflips = 1,
                     weights = c(0.5, 0.5)) {

  sequence = sample(x       = c("H", "T"),
                    size    = nflips,
                    replace = TRUE,
                    prob    = weights)
}
```

Tossing Coins: What Does a Fair Coin Look Like?
========================================================

- Let's flip a fair two-sided coin (Heads or Tails) once
- We will use our `flip_coin` function
- `set.seed()` ensures that we can rerun the same code and get the same result
even though we are using a pseudo-random number generator


```r
set.seed(333)
sequence = flip_coin(nflips = 1)
print(sequence)
```

```
[1] "T"
```

Tossing Coins: Two Tosses
========================================================

- Do we have a fair coin based on what we saw?
- Let's flip a fair two-sided coin (Heads or Tails) twice


```r
sequence = flip_coin(nflips = 2)
print(sequence)
```

```
[1] "T" "H"
```

- Is this a fair sequence?

Tossing Coins: Three Tosses
========================================================

- What about 3 tosses?


```r
sequence = flip_coin(nflips = 3)
print(sequence)
```

```
[1] "H" "T" "H"
```

Tossing Coins: 10,000 Tosses
========================================================
- What does 3 tosses look like?

![plot of chunk unnamed-chunk-5](stat731-recitation1-figure/unnamed-chunk-5-1.png)

***

- What about 10,000 tosses?

![plot of chunk unnamed-chunk-6](stat731-recitation1-figure/unnamed-chunk-6-1.png)

Tossing Coins: Probability as Relative Frequency
========================================================

- Effectively, the probability of an event is defined by the relative frequency
of the event as the number of independent trials approaches infinity

- For our fair coin toss, the probability of "Heads" is:
$$Pr\left[ H \right] = \lim_{n \to \infty} \frac{1}{n}\sum_{i = 1} ^ n I(H)$$

- Let's use the function `sample()` to compute the probability of seeing "Heads"

sample()
========================================================

- `sample()` is a function that draws a random sample from a sample space
  - `x`: a vector containing the elements of the sample space
      - `sample(x = c("H", "T"))`
  - `size`: a non-negative integer specifying the sample size to draw
      - `sample(x = c("H", "T"), size = 10)`
  - `replace`: `TRUE` or `FALSE`; sample with or without replacement
      - default is `replace = FALSE`
      - `sample(x = c("H", "T"), size = 10, replace = TRUE)`
  - `prob`: a vector of probabilities for each element of the sample space `x`
      - default is `1 / length(x)`
      - `sample(x = c("H", "T"), size = 10, replace = TRUE, prob = c(0.75, 0.25))`

Tossing Coins: Probability of "Heads" by Simulation
========================================================

- Let $n$ be very large (approximately infinite)
- `f` will count the number of times a "Heads" is obtained


```r
n = 100000
nheads = 0
for (i in 1:n) {
  nheads = nheads + ifelse(
    test = sample(x = c("H", "T"))[1] == "H",
    yes  = 1,
    no   = 0)
}
```

- We obtained 5.0022 &times; 10<sup>4</sup> "Heads" out of 10<sup>5</sup> tosses of a fair coin
- Therefore, $Pr\left[H\right] = 0.5002$
- How close are we to the theoretical probability?

Tossing Coins: A Rigged Coin
========================================================

- Let's scam some tourists with a rigged coin toss game:
  - $Pr[H] = 0.75$
  - $Pr[T] = 1 - Pr[H] = 0.25$
- A crowd of 40 tourists approaches...


```r
scams = flip_coin(nflips = 40, weights = c(0.75, 0.25))
table(scams)
```

```
scams
 H  T 
32  8 
```

- We probably don't have a fair coin when
80% of the flips landed on
"Heads"

Tossing Coins: Probability of Rigged "Heads" by Simulation
========================================================

- Let $n$ be very large (approximately infinite)
- `f` will count the number of times a "Heads" is obtained


```r
n = 100000
nheads = 0
for (i in seq(1:n)) {
  nheads = nheads + ifelse(
    test = sample(x = c("H", "T"), prob = c(0.75, 0.25))[1] == "H",
    yes  = 1,
    no   = 0)
}
```

- We obtained 7.5165 &times; 10<sup>4</sup> "Heads" out of 10<sup>5</sup> tosses of a fair coin
- Therefore, $Pr\left[H\right] = 0.7516$
- How close are we to the theoretical probability?

Rolling Dice
========================================================

- We spent a good amount of time simulating binomial events
- Can we simulate probabilities for larger spaces?
- Let's roll two 6-sided fair dice:
$$\mathcal{S} = \{1, 2, 3, 4, 5, 6\} \times \{1, 2, 3, 4, 5, 6\}$$
- What is the probability of rolling the same number on both dice?

```r
n = 10000
outcome = 0
for (i in seq(1:n)) {
  outcome = outcome + ifelse(
    test = sample(x = 1:6)[1] == sample(x = 1:6)[1],
    yes = 1,
    no = 0)
}
```

- We empirically obtain 1640 out of 10<sup>4</sup> rolls
- Our empirical estimate of $Pr[f_1 = f_2] = 0.164$
- How does this compare to our theoretical expectation?

Empirical Exploration of an Arbitrary Sample Space
========================================================

- Let $\mathcal{S} = \{-2, 0, 1, 3\}$
- $Pr[X = -2] = Pr[X = 0] = Pr[X = 1] = Pr[X = 3] = 0.25$
- What is the size (cardinality) of $\mathcal{S}$?
- Let's compute $Pr[X = -2]$ empirically using relative frequencies


```r
n = 10000
outcome = 0
for (i in seq(1:n)) {
  outcome = outcome + ifelse(
    test = sample(x = c(-2, 0, 1, 3), size = 1, replace = TRUE) == -2,
    yes = 1,
    no = 0)
}
```

- We observed 2579 events where $X = -2$ out of 10<sup>4</sup> samples
- Our empirical estimate of $Pr[X = -2] = 0.2579$
- How does this compare to our theoretical expectation?

Empirical Exploration of an Arbitrary Sample Space
========================================================

- Let $\mathcal{S} = \{-2, 0, 1, 3\}$
- $Pr[X = -2] = Pr[X = 0] = Pr[X = 1] = Pr[X = 3] = 0.25$
- What is the probability that X is non-negative?


```r
n = 10000
outcome = 0
for (i in seq(1:n)) {
  outcome = outcome + ifelse(
    test = sample(x = c(-2, 0, 1, 3), size = 1, replace = TRUE) >= 0,
    yes = 1,
    no = 0)
}
```

- We observed 7453 events where $X \ge 0$ out of 10<sup>4</sup> samples
- Our empirical estimate of $Pr[X \ge 0] = 0.7453$
- How does this compare to our theoretical expectation?
