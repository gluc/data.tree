#Run this to generate data(acme)

#library(data.tree)ac
acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")


software$cost <- 1000000
standards$cost <- 500000
newProductLine$cost <- 2000000
newLabs$cost <- 750000
outsource$cost <- 400000
agile$cost <- 250000
goToR$cost <- 50000

software$p <- 0.5
standards$p <- 0.75
newProductLine$p <- 0.25
newLabs$p <- 0.9
outsource$p <- 0.2
agile$p <- 0.05
goToR$p <- 1

save(acme, file = "data/acme.rda", compress = "gzip")