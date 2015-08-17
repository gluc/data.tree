library(data.tree)


# This demo generates 100 simulations of a 3 generation population inheriting and developing a certain feature 
# (e.g. colour blindness). It then plots the probability distribution of the feature in the last generation.


#' Generate a family tree of a population exhibiting a certain feature (e.g. colour blindness).
#' 
#' @param children the number of children each population member has
#' @param probSex the probability of the sex of a descendant
#' @param probInherit the probability the feature is inherited, depending on the sex of the descendant
#' @param probDevelop the probability the feature is developed (e.g. a gene defect), depending on the sex
#' of the descendant
#' @param generations the number of generations our population should have
#' @param parent for recursion
GenerateChildrenTree <- function(children = 2, 
                                 probSex = c(male = 0.52, female = 0.48), 
                                 probInherit = c(male = 0.8, female = 0.15),
                                 probDevelop = c(male = 0.05, female = 0.01),
                                 generations = 3, 
                                 parent = NULL) {
  
  if (is.null(parent)) {
    parent <- Node$new("1")
    parent$sex <- 1
    parent$feature <- TRUE
  }
  
  #sex of descendants
  #1 = male
  #2 = female
  sex <- sample.int(n = 2, size = children, replace = TRUE, prob = probSex)
  for (i in 1:children) child <- parent$AddChild(i)
  Set(parent$children, sex = sex)
  
  #inherit
  if (parent$feature == TRUE) {
    for (i in 1:2) {
      subPop <- Traverse(parent, filterFun = function(x) (x$isLeaf && x$sex == i))
      inherit <- sample.int(n = 2, size = length(subPop), replace = TRUE, prob = c(1 - probInherit[i], probInherit[i]))
      Set(subPop, feature = (inherit - 1))
    }
  } else {
    Set(parent$children, feature = FALSE)
  }
  
  #develop
  for (i in 1:2) {
    subPop <- Traverse(parent, filterFun = function(x) (x$isLeaf && x$sex == i && !x$feature))
    develop <- sample.int(n = 2, size = length(subPop), replace = TRUE, prob = c(1 - probDevelop[i], probDevelop[i]))
    Set(subPop, feature = (develop - 1))
  }
  
  
  if (generations > 0) for (i in 1:children) GenerateChildrenTree(children, probSex, probInherit, probDevelop, generations - 1, parent$children[[i]])
  
  return (parent)
}

#just for demonstration purpose, this is what a tree looks like
tree <- GenerateChildrenTree()
print(tree, "sex", "feature", limit = 20)
print(tree, "sex", "feature", pruneFun = function(x) x$feature)

#how many females have developed the feature?
length(Traverse(tree, filterFun = function(x) !x$isRoot && x$sex == 1 && x$feature && !x$parent$feature))

#what is the occurrence of the feature in the last generation?
freqLastGen <- function(tree) {
  l <- tree$leaves
  sum(sapply(l, function(x) x$feature))/length(l)
}

#generate 100 sample trees and get the frequency of the feature in the last generation
x <- sapply(1:100, function(x) freqLastGen(GenerateChildrenTree()))

#plot histogram
hist(x, probability = TRUE, main = "Frequency of feature in last generation")

#interesting side note: the frequency seems to be never between 0.25 and 0.3. Why?

