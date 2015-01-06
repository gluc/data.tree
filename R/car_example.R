root <- Node("Chose the best car for the Jones family")

  cost <- Node("Cost")
  root$AddChild(cost)
  
    purchasePrice <- Node("Purchase Price")
    cost$AddChild(purchasePrice)
    
    fuelCosts <- Node("Fuel Costs")
    cost$AddChild(fuelCosts)

    maintenanceCosts <- Node("Maintenance Costs")
    cost$AddChild(maintenanceCosts)

    cost_4 <- Node("Resale Value")
    cost$AddChild(cost_4)


  safety <- Node("Safety")
  root$AddChild(safety)

    safetyDummy <- Node("Safety")
    safety$AddChild(safetyDummy)
  
  style <- Node("Style")
  root$AddChild(style)

    styleDummy <- Node("Style")
    style$AddChild(styleDummy)

  
  capacity <- Node("Capacity")
  root$AddChild(capacity)

    cargoCapacity <- Node("Cargo Capacity")
    capacity$AddChild(cargoCapacity)

    passengerCapacity <- Node("Passenger Capacity")
    capacity$AddChild(passengerCapacity)



alternatives <- list(Node("Accord Sedan"),
                     Node("Accord Hybrid"),
                     Node("Pilot SUV"),
                     Node("CR-V SUV"),
                     Node("Element SUV"),
                     Node("Odyssey Minivan")
                     )


root$AddAlternatives(alternatives)

prefs <- root$GetPreferenceCombinations()
print(prefs)
root$SetPreferences(AhpMatrix(prefs[1,], prefs[2,], c(3, 7, 3, 9, 1, 1/7)))

prefs <- cost$GetPreferenceCombinations()
print(prefs)
cost$SetPreferences(AhpMatrix(prefs[1,], prefs[2,], c(2, 5, 3, 2, 2, 1/2)))

cost$priority
purchasePrice$priority
purchasePrice$GetGlobalPriority()
