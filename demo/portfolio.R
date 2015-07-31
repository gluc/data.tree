library(data.tree)



#read from file
pfodf <- read.csv('inst/extdata/portfolio.csv', stringsAsFactors = FALSE)

#convert to data.tree
pfodf$pathString <- paste("portfolio", pfodf$AssetCategory, pfodf$AssetClass, pfodf$SubAssetClass, pfodf$ISIN, sep = "/")
pfo <- as.Node(pfodf)

#Calculate breakdown
t <- Traverse(pfo, traversal = "post-order")
Do(t, function(x) x$Weight <- Aggregate(x, "Weight", sum))
Do(t, function(x) x$WeightOfParent <- x$Weight / x$parent$Weight)


GetDuration <- function(x) {
  if (is.numeric(x$Duration)) return(x$Duration) #use cache
  else if( x$isLeaf) return (NA) #if leaf has no duration, return NA
  else Aggregate(node = x, 
                 attribute = function(x) x$Weight * x$Duration / x$parent$Weight, 
                 aggFun = function(x) sum(x, na.rm = TRUE))
}




Do(t, function(x) x$Duration <- GetDuration(x))

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


