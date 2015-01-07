root <- Node$new("Chose the best car for the Jones family")

   
  root$AddChild(Node$new("Cost"))
  
    root$Find("Cost")$AddChild(Node$new("Purchase Price"))
    root$Find("Cost")$AddChild(Node$new("Fuel Costs"))
    root$Find("Cost")$AddChild(Node$new("Maintenance Costs"))
    root$Find("Cost")$AddChild(Node$new("Resale Value"))

  root$AddChild(Node$new("Safety")$AddChild(Node$new("Safety")))

  root$AddChild(Node$new("Style")$AddChild(Node$new("Style")))

  root$AddChild(Node$new("Capacity"))

    root$Find("Capacity")$AddChild(Node$new("Cargo Capacity"))
    root$Find("Capacity")$AddChild(Node$new("Passenger Capacity"))

print(root)


alternatives <- list(Node$new("Accord Sedan"),
                     Node$new("Accord Hybrid"),
                     Node$new("Pilot SUV"),
                     Node$new("CR-V SUV"),
                     Node$new("Element SUV"),
                     Node$new("Odyssey Minivan")
                     )


root$AddAlternatives(alternatives)

prefs <- root$GetPreferenceCombinations()
print(prefs)
ahp <- AhpMatrix(prefs[1,], prefs[2,], c(3, 7, 3, 9, 1, 1/7))
root$SetPreferences(ahp)

prefs <- root$Find("Cost")$GetPreferenceCombinations()
print(prefs)
root$Find("Cost")$SetPreferences(AhpMatrix(prefs[1,], prefs[2,], c(2, 5, 3, 2, 2, 1/2)))

root$Find("Cost")$priority
root$Find(c("Cost", "Purchase Price"))$priority
root$Find(c("Cost", "Purchase Price"))$GetGlobalPriority()

prefs <- root$Find("Capacity")$GetPreferenceCombinations()
print(prefs)
root$Find("Capacity")$SetPreferences(AhpMatrix("Cargo Capacity", "Passenger Capacity", 1/5))

root$Find(c("Capacity", "Passenger Capacity"))$GetGlobalPriority()
root$Find(c("Capacity", "Cargo Capacity"))$GetGlobalPriority()
