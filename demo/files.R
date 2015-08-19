
files <- list.files(recursive = TRUE,
                    include.dirs = FALSE) 

df <- data.frame(
      filename = sapply(files, 
                        function(fl) paste0(tail(strsplit(getwd(),"/")[[1]],1),"/",fl)
      ), 
      file.info(files),
      stringsAsFactors = FALSE
    )
  
  # convert to a data.tree
fileStructure <- as.Node(df, pathName = "filename")

print(fileStructure, "mode", limit = 25)

readline("The following is inspired by timelyportfolio. It requires to install devtools::install_github('timelyportfolio/listviewer')")


#This is inspired by @timelyportfolio, the fabulous "One-Widget-Per-Week" man
#See http://bl.ocks.org/timelyportfolio
#Thanks a lot for all the inspiration!

#This requires listviewer, which is available only on github

#devtools::install_github("timelyportfolio/listviewer")

library(listviewer)

l <- ToListSimple(fileStructure)
jsonedit(l)
