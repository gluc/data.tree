#This is an example inspired by the treemap examples
#As there are many countries, the chart gets
#clustered with many very small countries.
#So we limit the number of countries and sum the 
#remainder in an "Other"

library(treemap)
data(GNI2010)
GNI2010$pathString <- paste("world", GNI2010$continent, GNI2010$country, sep = "/")
n <- as.Node(GNI2010[,])
n$Do(function(x) Aggregate(x, "population", sum, "population"), traversal = "post-order")
n$Sort(attribute = "population", decreasing = TRUE, recursive = TRUE)
n$Do(function(x) Cumulate(x, "population", sum, "cumPop"))


myPruneFun <- function(x, cutoff = 0.9, maxCountries = 7) {
  if (isNotLeaf(x)) return (TRUE)
  if (x$position > maxCountries) return (FALSE)
  return (x$cumPop < (x$parent$population * cutoff))
}

n2 <- n$clone()
n2$Prune(pruneFun = myPruneFun)

#sum countries that we pruned away into a new "Other" node
n2$Do(function(x) {
  missing <- x$population - sum(sapply(x$children, function(x) x$population))
  other <- x$AddChild("Other")
  other$iso3 <- "OTH"
  other$country <- "Other"
  other$continent <- x$name
  other$GNI <- 0
  other$population <- missing
},
filterFun = function(x) x$level == 2
)


plot(as.dendrogram(n2, heightAttribute = "population"))
              
#or, as classical treemap


df <- ToDataFrameTable(n2, "iso3", "country", "continent", "population", "GNI")

treemap(df,
        index=c("continent", "iso3"),
        vSize="population",
        vColor="GNI",
        type="value")

