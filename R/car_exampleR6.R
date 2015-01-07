

root <- Node$new("Chose the best car for the Jones family")
  root$AddChild("Cost"
        )$AddChild("Purchase Price"
              )$AddSibling("Fuel Costs"
              )$AddSibling("Maintenance Costs"
              )$AddSibling("Resale Value")
  root$AddChild("Safety")
  root$AddChild("Style")
  root$AddChild("Capacity"
        )$AddChild("Cargo Capacity"
        )$AddSibling("Passenger Capacity")

print(root)


accord_sedan <- Node$new("Accord Sedan")
accord_sedan$price <- 20360

accord_hybrid <- Node$new("Accord Hybrid")
accord_hybrid$price <- 31090

pilot <- Node$new("Pilot SUV")
pilot$price <- 27595

crv <- Node$new("CR-V SUV")
crv$price <- 20700

element <- Node$new("Element SUV")
element$price <- 18980

odyssey <- Node$new("Odyssey Minivan")
odyssey$price <- 25645


alternatives <- list(accord_sedan,
                     accord_hybrid,
                     pilot,
                     crv,
                     element,
                     odyssey
                     )


root$AddAlternatives(alternatives)

prefs <- root$preferenceCombinations
print(prefs)
ahp <- AhpMatrix(prefs[1,], prefs[2,], c(3, 7, 3, 9, 1, 1/7))
root$SetPreferences(ahp)

prefs <- root$Find("Cost")$preferenceCombinations
print(prefs)
root$Find("Cost")$SetPreferences(AhpMatrix(prefs[1,], prefs[2,], c(2, 5, 3, 2, 2, 1/2)))

root$Find("Cost")$priority
root$Find(c("Cost", "Purchase Price"))$priority
root$Find(c("Cost", "Purchase Price"))$globalPriority

prefs <- root$Find("Capacity")$preferenceCombinations
print(prefs)
root$Find("Capacity")$SetPreferences(AhpMatrix("Cargo Capacity", "Passenger Capacity", 1/5))

root$Find(c("Capacity", "Passenger Capacity"))$globalPriority
root$Find(c("Capacity", "Cargo Capacity"))$globalPriority


mn <- root$Find(c("Capacity", "Cargo Capacity", "Pilot SUV"))

