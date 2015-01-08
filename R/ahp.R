# Define methods to calculate and tweak AHP (Analytic Hierarchy Process)

RI <- function(n){ #see handout
  if (n==2) return (100)
  if (n==3) return (0.58)
  if (n==4) return (0.90)
  if (n==5) return (1.12)
  if (n==6) return (1.24)
  if (n==7) return (1.32)
  if (n==8) return (1.41)
  return (1)
}

#' Calculate the ahp weights from the symmetric AHP matrix
#' 
#' @param mat The symmetric AHP preference matrix
#' @return the ahp preference weights
#' 
#' @export Ahp
Ahp <- function(mat){ 
  # weigthing vector
  eig <- eigen(mat, symmetric=FALSE)
  
  #consistency
  M22 = mat/kronecker(matrix(1, dim(mat)[1], 1), t(apply(mat, 2, sum)))
  w = apply(M22, 1, sum) / dim(mat)[1]
  lambdaMax <- max(Re(eig$values))
  CI = (lambdaMax - dim(mat)[1]) / (dim(mat)[1]-1)
  CR = CI / RI(dim(mat)[1])
  
  #we accept a consistency up to 20% which is very borderline (10% is normal)
  if (is.nan(CI) || CR < 0.2) res <- (Re(eig$vectors[,1])/sum(Re(eig$vectors[,1]))) else res <- (matrix(1/dim(mat)[1],1,dim(mat)[1]))
  names(res) <- dimnames(mat)[[1]]
  list(ahp = res, consistency = CR)
}

#' Create the AHP pairwise preference matrix from vectors
#' 
#' @details Creates and fills a AHP conform matrix given the category vectors cat1, cat2, and the preference vector pref.
#' @param cat1 A vector of n category names
#' @param cat2 A vector of n category names
#' @param pref A vector of preferences
#' @examples
#' categories <- combn(c('red','blue','green'),2)
#' cat1 <- categories[1,]
#' cat2 <- categories[2,]
#' pref <- c(1,5,4)
#' AhpMatrix(cat1, cat2, pref)
#' @seealso \code{\link{ahp}}
#' @export AhpMatrix
AhpMatrix <- function(cat1, cat2, pref){
	cats <- unique(c(cat1, cat2))
	mat <- matrix(1, nrow = length(cats), length(cats), byrow = TRUE, dimnames = list(cats, cats))
	for (i in 1:length(cat1))
	{
		mat[cat1[i], cat2[i]] <- pref[i]
		mat[cat2[i], cat1[i]] <- 1/ pref[i]
	}
	
	return(mat)

}


#' Tweak (adjust) an AHP matrix using adjustments
#' 
#' @param adjustments A named vector of AHP preference shifts. For example: c(ai.CDT=0,ai.GLD=2,ai.RE=-9)
#' @param base.matrix The AHP preference matrix to adjust
#' @details A positive shift translates our preferences in favour of the asset class, and a negative vice versa.
#' These shifts apply directly to our AHP preferences. So, if initially (in base.matrix) we prefer commodities over gold
#' by 3/1, then having +1 as shift will lead to 4/1. In that sense, it is recommended to have shift values between -9 and +9.
#' Larger values are cut off.
#' @examples
#' #let's shift our preferences massively away from real estate, and slightly towards gold
#' library(SAA.CH.CHF)
#' AhpAdjust(c(ai.CDT=0,ai.GLD=2,ai.RE=-9), market.ai.base)
#' @export
AhpAdjust <- function(adjustments, base.matrix)
{
	if(FALSE) {
		#debug
		adjustments <- c(ai.CDT=-1, ai.GLD=-0.6742077, ai.RE=-9)
		base.matrix <- market.ai.base
		base.matrix
		#end debug
	}
  
	cats <- names(adjustments)
	for (i in 1:length(adjustments))
	{
		r <- sapply(base.matrix[cats[i], !rownames(base.matrix) %in% cats[i]],  function(x) GetAhpAdjustment(x, adjustments[cats[i]]) )
		base.matrix[cats[i], !rownames(base.matrix) %in% cats[i]] <- r
		base.matrix[!rownames(base.matrix) %in% cats[i], cats[i]] <- 1 / r
	}
	return(base.matrix)
}

#possible.values <- c(1/c(2:9), c(1:9))
GetAhpAdjustment <- function(oldValue, adjustment)
{
	if (FALSE)
	{
		#debug
		oldValue = 1
		adjustment = -0.5
	}
	#step 1: make > 1
	if (oldValue < 1) {
		v <- 1 / oldValue
		v <- v - adjustment
	} else {
		v <- oldValue + adjustment
	}
	
	#step 3: cut off if larger than 9
	v <- min(v, 9)
	
	#step 4: inverse if negative
	
	if (v < 1) {
		v <- 1 / (-v + 2)
	}

	#step 5: cut off if too small
  v <- max(1 / 9, v)
	
	#last step: inverse if necessary
	if (oldValue < 1) {
		v <- 1 / v
	}
	
	return (v)
	
}


LinearComparison <- function(a, b, min_x, max_x, preferSmall = FALSE, fieldName = NA) {
  #scale to 1...9
  if (!is.na(fieldName)) {
    a <- a[[fieldName]]
    b <- b[[fieldName]]
  }
  v <- 1 + 8 * abs(a - b) / (max_x - min_x)
  if (xor(a < b, preferSmall)) v <- 1 / v
  return (v)
}





