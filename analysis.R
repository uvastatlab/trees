# Tree data analysis
# `trees` contains the diameter, height and volume of 31 felled black cherry trees

# Packages
library(ggplot2)
library(rvest)
library(stringi)

# First: What are these things?
black_cherry_wiki <- read_html('https://en.wikipedia.org/wiki/Prunus_serotina') %>% 
  html_elements('p') %>%
  html_text() %>% 
  .[min(which(nchar(.) > 50))] %>% 
  gsub('(\\[)(\\d+)(\\])', '', .)
black_cherry_wiki

#####################
# Assorted analyses #
#####################

# Volume v. girth
data("trees")
summary(trees)
pairs(trees)
# from trees help page
pairs(trees, panel = panel.smooth, main = "trees data")
plot(Volume ~ Girth, data = trees, log = "xy")

# Height v. girth
ggplot(trees[order(trees$Girth), ], aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = 'loess', color = 'forestgreen') +
  ggtitle('Height versus Girth')

# Bootstrap girth coefficient in volume ~ girth regression
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
# Bootstrapped girth coefficient estimate
mean(coefs[, 2])
# SE of bootstrapped girth coefficient estimate
sd(coefs[, 2])
# 95% percentile CI for bootstrapped girth coefficient
quantile(coefs[, 2], c(.025, .975))
# Estimate degree of bias in bootstrapped girth coefficients
abs((sum(coefs[, 2] > coef(mod)[2]) / b) - 0.5)

# Histogram of bootstrapped coefficients
hist(coefs[,'Girth'], main = "Bootstrap-coefficient Distribution",
     xlab = "Bootstrap Girth")