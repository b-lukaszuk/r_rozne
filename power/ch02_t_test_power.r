## based on the lecture of the book by Cohen
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

## based on the lecture of the book by Altman
####### getDelta for pwr.t.test #########
####### while using power.t.test/pwr.t.test -> apply this function #########
####### to calculate getDelta and leave sd value default #########
## I guess this function is slightly better (more precise) that the one above
# x and y are either vectors or averages
getDelta <- function(x,y, vectors=F, cv=c(0.15, 0.15)) {
  pooled_sd <- function(x,y) {
    wynik <- ((length(x)-1)*var(x)+(length(y)-1)*var(y));
    wynik <- wynik/(length(x)+length(y)-2);
    wynik <- sqrt(wynik); # liczac pooled_sd z wariancji
    return(wynik); # a pozniej sqrt(var) otrzymujemy troche inny
    # # wynik niz liczac z sd (roznica spowodowana zaokraglaniem)
  }
  if (vectors==F) { # calculate getDelta for a priori design of experiment
    sd1 <- x*cv[1]; # in this case x and y are averages
    sd2 <- y*cv[2]; # we assume CV (coefficient variation) in group is 0.15, this is the default value
    wynik <- sqrt((sd1^2+sd2^2)/2);
    rezultat <- (x-y)/wynik;
  } else {
    aver1 <- mean(x);
    aver2 <- mean(y);
    rezultat <- (aver1-aver2)/pooled_sd(x,y);
  }
  return(abs(rezultat));
}
