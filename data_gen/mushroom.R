color <- c('red', 'brown', 'brown', 'green', 'red')
size <- c('small', 'small', 'large', 'small', 'large')
points <- c('yes', 'no', 'yes', 'no', 'no')
edible <- c('toxic', 'edible', 'edible', 'edible', 'edible')
mushroom <- data.frame(color = color, size = size, points = points, edibility = edible)
save(mushroom, file = "data/mushroom.rda", compress = "xz")

