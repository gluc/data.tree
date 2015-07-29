### This demo calculates and plots a simple decision tree
### It requires the yaml and the ape packages to be installed


library(data.tree)
library(yaml)


#load from file
readline("First, let's read the decision tree from YAML (hit key):")
fileName <- 'inst/extdata/jennylind.yaml'
l <- yaml.load_file(fileName)
jl <- as.Node(l)
print(jl, "type", "payoff", "p")



#calculate decision tree
readline("Now we calculate the tree:")
payoff <- function(x) {
  if (x$type == 'chance') x$payoff <- Aggregate(x, function(node) node$payoff * node$p, sum)
  else if (x$type == 'decision') x$payoff <- Aggregate(x, "payoff", max)
}

jl$Do(payoff, traversal = "post-order", filterFun = isNotLeaf)

decision <- function(x) {
  po <- sapply(x$children, function(child) child$payoff)
  x$decision <- names(po[po == x$payoff])
}

jl$Do(decision, filterFun = function(x) x$type == 'decision')

#Plot the decision tree with ape
#(The rest of the demo is not very data.tree specific)

library(ape)
jl$Revert()
jlp <- as.phylo(jl)
par(mar=c(1,1,1,1))
plot(jlp, show.tip.label = FALSE, type = "cladogram")


nodelabel <- function(x) {
  po <- paste0( '$ ', format(x$payoff, scientific = FALSE, big.mark = "'"))
  if (x$type == 'terminal') return (po)
  return ( paste0('ER\n', po) )
}

for (node in jl$leaves) edges(GetPhyloNr(node$parent, "node"), GetPhyloNr(node, "node"), arrows = 2, type = "triangle", angle = 60)

for(node in jl$Get(function(x) x)) {
  if(node$type == 'decision') {
    nodelabels(nodelabel(node), GetPhyloNr(node, "node"), frame = 'none', adj = c(0.3, -0.5))
  } else if(node$type == 'chance') {
    if (node$name == node$parent$decision) edges(GetPhyloNr(node$parent, "node"), GetPhyloNr(node, "node"), col = "red")
    nodelabels(" ", GetPhyloNr(node, "node"), frame = "circle")
    nodelabels(nodelabel(node), GetPhyloNr(node, "node"), frame = 'none', adj = c(0.5, -0.5))
    edgelabels(node$name, GetPhyloNr(node, "edge"), bg = "white")
  } else if(node$type == 'terminal') {
    tiplabels(nodelabel(node), GetPhyloNr(node, "node"), frame = "none", adj = c(0.5, -0.6))
    edgelabels(paste0(node$name," (", node$p, ")"), GetPhyloNr(node, "edge"), bg = "white")
  }
}

nodelabels("   ", GetPhyloNr(jl, "node"), frame = "rect")

readline("Hit key to see the same plot with DiagrammeR / Mermaid (best viewed in browser)")

#plot the same with DiagrammeR / mermaid
library("DiagrammeR")

jl$Set(id = letters[1:(jl$totalCount)])

FromLabel <- function(node) {
  if(node$parent$isRoot) return (ToLabel(node$parent))
  return (as.character(node$parent$id))
}

EdgeLabel <- function(node) {
  if (node$type == "decision") {
    return ('')
  } else if (node$type == "chance") {
    lbl <- node$name
  } else if (node$type == "terminal") {
    lbl <- paste0(node$name,": ", node$p)
  }
  lbl <- paste0(" --> |", lbl, "|")
  return (lbl)
}

FormatPayoff <- function(payoff) {
  paste0("$", payoff/1000, "k")
}

ToLabel <- function(node) {
  if (node$type == "decision") {
    lbl <- paste0("[", FormatPayoff(node$payoff), "]")
  } else if (node$type == "chance") {
    lbl <- paste0("((", FormatPayoff(node$payoff), "))")
  } else if (node$type == "terminal") {
    lbl <- paste0("[", FormatPayoff(node$payoff), "]")
  }
  lbl <- paste0(" ", node$id, lbl)
  return (lbl)
}


format <- paste0(
  "classDef default fill:none, bg:none, stroke-width:0px;
classDef decision fill:#9f6,stroke:#333,stroke-width:1px;
classDef chance fill:red,stroke:#333,stroke-width:1px;
class ", paste(jl$Get("id", filterFun = function(x) x$type == "decision"), collapse = ","), " decision;
class ", paste(jl$Get("id", filterFun = function(x) x$type == "chance"), collapse = ","), " chance;")
format <- ""
t <- Traverse(jl, traversal = "level", filterFun = isNotRoot)
df <- data.frame(from = Get(t, FromLabel), edge = Get(t, EdgeLabel), to = Get(t, ToLabel))

diagram <- paste("graph LR", paste( paste0(df$from, df$edge, df$to), collapse = "\n"), format, sep = "\n")

DiagrammeR(diagram)
