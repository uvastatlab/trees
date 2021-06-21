# tree data analysis
# diameter, height and volume of timber in 31 felled black cherry trees.

# packages
library(grid)

# volume v. girth
data("trees")
summary(trees)
pairs(trees)
# from trees help page
pairs(trees, panel = panel.smooth, main = "trees data")
plot(Volume ~ Girth, data = trees, log = "xy")

# heigh v. girth
ggplot(trees[order(trees$Girth), ], aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = 'loess', color = 'forestgreen') +
  ggtitle('Height versus Girth')
