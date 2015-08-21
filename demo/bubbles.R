#This is inspired by Mike Bostock: http://bl.ocks.org/mbostock/4063269 
#It uses Joe Chengs bubbles package

library(jsonlite)


flareLoL <- fromJSON("http://bl.ocks.org/mbostock/raw/4063269/flare.json",
                     simplifyDataFrame = FALSE
                     )

flareTree <- as.Node(flareLoL, mode = "explicit")
flareTree$fieldsAll

flare_df <- ToDataFrameTable(flareTree, className = function(x) x$parent$name, packageName = "name", "size")


#devtools::install_github("jcheng5/bubbles")

library("bubbles")
bubbles(
  flare_df$size
  ,flare_df$className
  ,color = col_factor(
    RColorBrewer::brewer.pal(9,"Set1")
    ,factor(flare_df$packageName)
  )(flare_df$packageName)
  ,height = 600
  ,width = 960
)