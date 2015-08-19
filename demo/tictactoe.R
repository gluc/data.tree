#brute force solution of Tic Tac Toe
fields <- expand.grid(letters[1:3], 1:3)

ttt <- Node$new("ttt")

#consider rotation, so first move is explicit
ttt$AddChild("a3")
ttt$a3$f <- 7
ttt$AddChild("b3")
ttt$b3$f <- 8
ttt$AddChild("b2")
ttt$b2$f <- 5


ttt$Set(player = 1, filterFun = isLeaf)




AddPossibleMoves <- function(node) {
  t <- Traverse(node, traversal = "ancestor", filterFun = isNotRoot)
  
  available <- rownames(fields)[!rownames(fields) %in% Get(t, "f")]
  for (f in available) {
    child <- node$AddChild(paste0(fields[f, 1], fields[f, 2]))
    child$f <- as.numeric(f)
    child$player <- ifelse(node$player == 1, 2, 1)
    hasWon <- HasWon(child)
    if (!hasWon && child$level <= 10) AddPossibleMoves(child)
    if (hasWon) {
      child$result <- child$player
      print(paste("Player ", child$player, "wins!"))
    } else if(child$level == 10) {
      child$result <- 0
      print("Tie!")
    }
    
  }
  
}

HasWon <- function(node) {
  t <- Traverse(node, traversal = "ancestor", filterFun = function(x) !x$isRoot && x$player == node$player)
  mine <- Get(t, "f")
  mineV <- rep(0, 9)
  mineV[mine] <- 1
  mineM <- matrix(mineV, 3, 3, byrow = TRUE)
  result <- any(rowSums(mineM) == 3) ||
    any(colSums(mineM) == 3) ||
    sum(diag(mineM)) == 3 ||
    sum(diag(t(mineM))) == 3
  return (result)
}

PrintBoard <- function(node) {
  t <- Traverse(node, traversal = "ancestor", filterFun = function(x) !x$isRoot && x$player == 1)
  one <- Get(t, "f")
  mineV <- rep(0, 9)
  mineV[one] <- 1
  
  t <- Traverse(node, traversal = "ancestor", filterFun = function(x) !x$isRoot && x$player == 2)
  two <- Get(t, "f")
  
  mineV[two] <- 2
  
  
  mineM <- matrix(mineV, 3, 3, byrow = TRUE)
  mineM
}

for (child in ttt$children) AddPossibleMoves(child)


winnerOne <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 1)
winnerTwo <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 2)
ties <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 0)
