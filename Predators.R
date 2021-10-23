# -----------------------------
# MODEL ONE - PREY ONLY

A = 0.1
x1 = 100

preyGeneration = function(n) (1 + A)^(n - 1) * x1

plot(preyGeneration(1:100), main="Figure 1. Prey Only", pch=20, col = "green", xlab = "Generation", ylab = "Prey population")

preyGeneration(100)

# Alternatively
# x1 = 100 # Initial population
# N = 100 # Number of generations N
# n = c(1:N)
# r = (1 + A)
# prey = r^(n - 1) * x1
# plot(1:N, prey, main="Figure 1. Prey Only", pch=20, col = "green", xlab = "Generation", ylab = "Prey population")


# -----------------------------
# MODEL TWO - PREDATORS ONLY

C = 0.02
y1 = 100

predatorGeneration = function(n) (1 - C)^(n - 1) * y1

plot(predatorGeneration(1:100), col = "red", main="Figure 2. Predators Only", pch = 20, xlab = "Generation", ylab = "Predator population")

predatorGeneration(100)

# Alternatively
# y1 = 100 # Initial population
# N = 100 # Number of generations N
# n = c(1:N)
# r = (1 - C)
# prey = r^(n - 1) * x1
# plot(1:N, prey, col = "red", main="Figure 2. Predators Only", pch = 20, xlab = "Generation", ylab = "Predator population")


# -----------------------------
# MODEL THREE - COMPLETE MODEL

# Given the values
A = 0.01
B = 0.0001
C = 0.02
D = 0.00002

N = 100 # Number of generations N
x = c(1:N) # Prey population
y = c(1:N) # Predator population

# Assign initial population to the first element in each vector
x[1] = C / D; # 1000
y[1] = A / B; # 100

# Loop through the range of wanted generations with a for loop
for(n in 1:(N - 1)) {
  x[n + 1] = (1 + A) * x[n] - B * x[n] * y[n]
  y[n + 1] = (1 - C) * y[n] + D * x[n] * y[n]
}

# x; y

plot(x, col = "green", type="l", main="Figure 3. Complete Model", lwd=2, ylim = c(-50,max(x + 200)), xlab = "Generation", ylab = "Population")
points(y, col = "red", type="l", lwd=2)

legend("bottomleft", 
       legend = c("Prey", "Predators"),
       col = c("green", "red"),
       lty = 1,
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       text.col = "black",
       horiz = F,
       inset = c(0.03, 0.4))

# -----------------------------
# OTHER TESTS

# Same initial population
# x[1] = 500;
# y[1] = 500;

# Abundance of prey
# x[1] = 5000;
# y[1] = 100;

# Abundance of predators
# x[1] = 1;
# y[1] = 1000;