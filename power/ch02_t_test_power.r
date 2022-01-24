# muA - population A mean expressed in raw (original measurement) unit
# muB - population B mean expressed in raw (original measurement) unit
# sigma - the std. dev. of either population (since they are assumed equal)
# effSize - is diff in the means expressed in sd units
getEffSize <- function(muA, muB, sigma, twoTailed=T) {
    if (twoTailed) {
        numerator <- abs(muA - muB)
    } else {
        numerator <- muA - muB
    }
    return(numerator/sigma)
}
