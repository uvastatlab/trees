# tree data analysis
# diameter, height and volume of timber in 31 felled black cherry trees.

data("trees")
summary(trees)
pairs(trees)
# from trees help page
pairs(trees, panel = panel.smooth, main = "trees data")
plot(Volume ~ Girth, data = trees, log = "xy")

