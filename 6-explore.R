# explore.R

# testing

age <- analyze.demographics$age
los <- analyze.demographics$length.stay

age1 <- analyze.demographics[analyze.demographics$group == "moderate", ][["age"]]
age2 <- analyze.demographics[analyze.demographics$group == "normal", ][["age"]]

qqplot(age1, age2)
qqnorm(age)
qqline(age, col = "blue")
qqnorm(log10(age))
qqline(log10(age), col = "blue")

qqnorm(los)
hist(age)
hist(log10(age))
hist(los)

mu.age <- mean(age)
var.age <- var(age)
n.age <- length(age)
sd.age <- sd(age)
(mu.age + c(-1, 1) * qnorm(0.975) * sd.age / sqrt(n.age))
nrml.age <- (age - mu.age) / sd.age
qqnorm(nrml.age)

hist(nrml.age)
library(ggplot2)
graph <- ggplot(analyze.demographics, aes(x = age)) +
    geom_histogram(binwidth = 5, color = "black", fill = "light blue", aes(y = ..density..)) +
    geom_density() +
    geom_vline(aes(xintercept = mean(age)), color = "blue", linetype = "dashed")
print(graph)