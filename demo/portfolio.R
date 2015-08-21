library(data.tree)



#read from file
pfodf <- read.csv('inst/extdata/portfolio.csv', stringsAsFactors = FALSE)

#convert to data.tree
pfodf$pathString <- paste("portfolio", pfodf$AssetCategory, pfodf$AssetClass, pfodf$SubAssetClass, pfodf$ISIN, sep = "/")
pfo <- as.Node(pfodf)

#Calculate breakdown
t <- Traverse(pfo, traversal = "post-order")
Do(t, function(x) Aggregate(x, "Weight", sum, "Weight"))
Do(t, function(x) x$WeightOfParent <- x$Weight / x$parent$Weight)

pfo$Do(function(x) x$Duration <- ifelse(is.null(x$Duration), 0, x$Duration), filterFun = isLeaf)

Do(t, function(x) x$Duration <- Aggregate(x, function(x) x$WeightOfParent * x$Duration, sum))



#Formatters
SetFormat(pfo, "WeightOfParent", function(x) FormatPercent(x, digits = 1))
SetFormat(pfo, "Weight", FormatPercent)
FormatDuration <- function(x) {
  if (x != 0) res <- FormatFixedDecimal(x, digits = 1)
  else res <- ""
  return (res)
}
SetFormat(pfo, "Duration", FormatDuration)


#Print
print(pfo, 
      "Weight", 
      "WeightOfParent",
      "Duration",
      filterFun = function(x) !x$isLeaf)


