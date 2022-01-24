# table 7.2.1, page 219 from the book by Cohen
## tab <- as.data.frame(matrix(data=c(22, 35, 3, 23, 10, 7),
##                             nrow=2, ncol=3, byrow=T))
## colnames(tab) <- c("dem", "rep", "ind")
## rownames(tab) <- c("men", "women")

contingencyTabToProp <- function(contingencyTable) {
    total <- sum(contingencyTable)
    return(contingencyTable/total)
}

getExpVals <- function(contingencyTable) {
    resTable <- contingencyTable

    total <- sum(contingencyTable)
    sumOfRows <- rowSums(contingencyTable)
    sumOfCols <- colSums(contingencyTable)

    shape <- dim(contingencyTable)

    for(r in 1:shape[1]){
        for(c in 1:shape[2]){
            resTable[r, c] <- sumOfRows[r]*sumOfCols[c]/total
        }
    }

    return(resTable)
}

flattenByCol <- function(df) {
    shape <- dim(df)
    result <- numeric(shape[1] * shape[2])
    startInd <- 1
    endInd <- startInd + (shape[1] - 1)
    for(i in 1:shape[2]) {
        result[startInd:endInd] <- df[, i]
        startInd <- endInd + 1
        endInd <- startInd + (shape[1] - 1)
    }
    return(result)
}

## the effect size index (page 216)
## small: w = 0.1
## medium: w = 0.3
## large: w = 0.5
getW <- function(contingencyTable) {
    p0s <- contingencyTabToProp(getExpVals(contingencyTable))
    pAs <- contingencyTabToProp(contingencyTable)
    p0s <- flattenByCol(p0s)
    pAs <- flattenByCol(pAs)
    diffs <- pAs - p0s
    diffsSquared <- diffs^2
    fractions <- diffsSquared/p0s
    w <- sqrt(sum(fractions))
    return(w)
}

getDf <- function(contingencyTable) {
    nOfRows = nrow(contingencyTable)
    nOfCols = ncol(contingencyTable)
    # df = (nrows - 1) * (ncols - 1)
    return((nOfRows - 1) * (nOfCols - 1))
}
