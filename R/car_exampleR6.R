
root <- Node$new("Chose the best car for the Jones family")
  cost <- root$AddChild("Cost")
    purchasePrice <- cost$AddChild("Purchase Price")
    fuelCosts <- cost$AddChild("Fuel Costs")
    maintenanceCosts <- cost$AddChild("Maintenance Costs")
    resaleValue <- cost$AddChild("Resale Value")
  safety <- root$AddChild("Safety")
    curbWeight <- safety$AddChild("Curb Weight")
    safetyClass <- safety$AddChild("Safety Class", "safetyClass")
      midsizeCar <- safetyClass$AddChild("Midsize Car")
      midsizeSUV <- safetyClass$AddChild("Midsize SUV")
      smallSUV <- safetyClass$AddChild("Small SUV")
      minivan <- safetyClass$AddChild("Minivan")
    crashRating <- safety$AddChild("Crash Rating")
  style <- root$AddChild("Style")
  capacity <- root$AddChild("Capacity")
    cargoCapacity <- capacity$AddChild("Cargo Capacity")
    passengerCapacity <- capacity$AddChild("Passenger Capacity")



print(root)

# set weights of categories

prefs <- root$preferenceCombinations
print(prefs)
ahp <- AhpMatrix(prefs[1,], prefs[2,], c(3, 7, 3, 9, 1, 1/7))
root$SetPreferenceMatrix(ahp)

prefs <- root$Find("Cost")$preferenceCombinations
print(prefs)
ahp <- AhpMatrix(prefs[1,], prefs[2,], c(2, 5, 3, 2, 2, 1/2))
root$Find("Cost")$SetPreferenceMatrix(ahp)

root$Find("Cost")$priority
root$Find(c("Cost", "Purchase Price"))$priority
root$Find(c("Cost", "Purchase Price"))$globalPriority

prefs <- root$Find("Capacity")$preferenceCombinations
print(prefs)
root$Find("Capacity")$SetPreferenceMatrix(AhpMatrix("Cargo Capacity", "Passenger Capacity", 1/5))

root$Find(c("Capacity", "Passenger Capacity"))$globalPriority
root$Find(c("Capacity", "Cargo Capacity"))$globalPriority


# alternatives: we create a node for each car on the short list

accord_sedan <- Node$new("Accord Sedan")
accord_hybrid <- Node$new("Accord Hybrid")
pilot <- Node$new("Pilot SUV")
crv <- Node$new("CR-V SUV")
element <- Node$new("Element SUV")
odyssey <- Node$new("Odyssey Minivan")


accord_sedan$safetyClass <- "Midsize Car"
accord_hybrid$safetyClass <- "Midsize Car"
pilot$safetyClass <- "Midsize SUV"
crv$safetyClass <- "Small SUV"
element$safetyClass <- "Small SUV"
odyssey$safetyClass <- "Minivan"


# add alternatives to leaves

alternatives <- list(accord_sedan,
                     accord_hybrid,
                     pilot,
                     crv,
                     element,
                     odyssey
                     )


root$AddAlternatives(alternatives)


# NOTE: these are R6 reference objects, so they same accord_sedan instance is attached to all the leaves!
# Formally, we don't have a tree anymore, as the alternatives now have more than one parent.

names(pilot2$parents)

pilot2 <- root$Find(c("Capacity", "Cargo Capacity", "Pilot SUV"))
names(pilot2$parents)

# PURCHASE PRICE
# ==============

# Let's start setting preferences for our alternatives
# First, we do this for the purchase price.
# We start by adding a custom attribute, price

accord_sedan$price <- 20360
accord_hybrid$price <- 31090
pilot$price <- 27595
crv$price <- 20700
element$price <- 18980
odyssey$price <- 25645

# The pairwise preference with respect to the purchase price depends on
# the absolute prices, but also on the price difference.
# If the price is above 26000 USD, we shun it.
# If the price is above 25000 USD, we don't like it.
# Otherwise, if it's much cheaper then the other car, we like it.

# Our preference function takes two alternative nodes as input,
# and returns the pairwise preference in ahp manner (1/9, 1/8 ... 1/2, 1, 2, ... 9)

# To keep it simple, the function is slightly different from the tutorial

purchasePricePreference <- function(car1, car2) {
  
  kSoftBudget <- 25000
  kHardBudget <- 26000
  
  if (car1$price > kHardBudget && car2$price > kHardBudget) {
    return (1)
  }
  
  if (car1$price <= kSoftBudget && car2$price > kHardBudget) {
    return (9)
  }
  
  if (car1$price <= kSoftBudget && car2$price > kSoftBudget) {
    return (7)
  }
  
  
  if (car1$price < car2$price) {
    pref <- max(1, min(9, round((car2$price - car1$price)/1000)))
    return (pref)
  }
  
  return (1/purchasePricePreference(car2, car1))
  
}

# We pass the function to the CalculatePreferences method

root$Find(c("Cost", "Purchase Price"))$CalculatePreferences(purchasePricePreference)

# The node has automatically calculated the ahp preference matrix ...

root$Find(c("Cost", "Purchase Price"))$preferenceMatrix

# ... as well as the priorities

root$Find(c("Cost", "Purchase Price"))$childPriorities

# CURB WEIGHT
# ==============

# Let's move on to the next criteria: Safety.Curb Weight. 

accord_sedan$curb_weight <- 3289
accord_hybrid$curb_weight <- 3501
pilot$curb_weight <- 4264
crv$curb_weight <- 3389
element$curb_weight <- 3433
odyssey$curb_weight <- 4385

# The Jones' believe that a heavier car is safer. 
# We use the ahp function LinearComparison, assuming that the overall lightest existing car is 3000, and the heaviest ist 5000.
# Note that we pass along additional parameters to the LinearComparison function.


root$Find(c("Safety", "Curb Weight"))$CalculatePreferences(LinearComparison, fieldName = "curb_weight", min_x = 3000, max_x = 5000)

# SAFETY CLASS
# ============

safetyClass <- Node$new("Safety Class")
safetyClass$AddChild("Midsize Car")
safetyClass$AddChild("Midsize SUV")
safetyClass$AddChild("Small SUV")
safetyClass$AddChild("Minivan")

prefs <- safetyClass$preferenceCombinations
print(prefs)

prefs1 <- c("Midsize Car", "Midsize Car", "Midsize Car", "Midsize SUV", "Midsize SUV", "Small SUV")
prefs2 <- c("Midsize SUV", "Small SUV"  , "Minivan"    , "Small SUV"  , "Minivan"    , "Minivan")
prefs3 <- c( 1/4         ,  1/2         ,  1/2         ,  4           ,  3           ,  1)

ahp <- AhpMatrix(prefs1, prefs2, prefs3)

#Let's check the consistency of our choices:

Ahp(ahp)


accord_sedan$safetyClass <- "Midsize Car"
accord_hybrid$safetyClass <- "Midsize Car"
pilot$safetyClass <- "Midsize SUV"
crv$safetyClass <- "Small SUV"
element$safetyClass <- "Small SUV"
odyssey$safetyClass <- "Minivan"

root$Find(c("Safety", "Safety Class"))