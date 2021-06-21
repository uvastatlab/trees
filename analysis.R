# tree data analysis
# diameter, height and volume of timber in 31 felled black cherry trees.

# packages
library(ggplot2)

# volume v. girth
data("trees")
summary(trees)
pairs(trees)
# from trees help page
pairs(trees, panel = panel.smooth, main = "trees data")
plot(Volume ~ Girth, data = trees, log = "xy")

# height v. girth
ggplot(trees[order(trees$Girth), ], aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = 'loess', color = 'forestgreen') +
  ggtitle('Height versus Girth')

# bootstrap girth coefficient in volume ~ girth regression
mod <- lm(Volume ~ Girth, data = trees)
set.seed(12)
b <- 1000
coefs <- matrix(ncol = 2, nrow = b)
for (i in 1:b) {
  cat(i, '...')
  indices <- sample(nrow(trees), replace = T)
  temp <- trees[indices, ]
  temp_mod <- lm(Volume ~ Girth, data = temp)
  coefs[i, ] <- temp_mod$coefficients
}
colnames(coefs) <- c('Intercept', 'Girth')
# bootstrapped girth coefficient estimate
mean(coefs[, 2])
# SE of bootstrapped girth coefficient estimate
sd(coefs[, 2])
# 95% percentile CI for bootstrapped girth coefficient
quantile(coefs[, 2], c(.025, .975))
# estimate degree of bias in bootstrapped girth coefficients
abs((sum(coefs[, 2] > coef(mod)[2]) / b) - 0.5)

# histogram of bootstrapped coefficients
hist(coefs[,'Girth'], main = "Distribution of bootstrapped coefficients",
     xlab = "Bootstrap Girth")
