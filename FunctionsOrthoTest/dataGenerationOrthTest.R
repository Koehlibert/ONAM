purifyNonLinFunction <- function(idx)
{
  x <- X[,idx]
  lmod <- lm(y ~ 1 + x, data = data.frame(y = lotf[[nonLinFIdx[idx]]](x), 
                                          x = x))
  nonLinF <- function(x) lotf[[nonLinFIdx[idx]]](x) - lmod$coefficients[1] -
    x * lmod$coefficients[2]
  scaleFac <- max(abs(nonLinF(x)))
  return(function(x) nonLinF(x) / scaleFac)
}
purifyInterFunction <- function(idx)
{
  relData <- X[,combn(1:p_inf, 2)[,idx]]
  x1 <- relData[,1]
  x2 <- relData[,2]
  lmod <- lm(y ~ 1 + x1 + x2, 
             data = data.frame(y = interf[[interFIdx[idx]]](x1, x2), 
                               x1 = x1,
                               x2 = x2))
  interF <- function(x1, x2) interf[[interFIdx[idx]]](x1, x2) - 
    lmod$coefficients[1] - 
    x1 * lmod$coefficients[2] - x2 * lmod$coefficients[3]
  scaleFac <- max(abs(interF(x1, x2)))
  return(function(x1, x2) interF(x1, x2) / scaleFac)
}
stackedOrthFunction <- function(nonLinFPre, interFPre, r, originalData)
{
  DesMatList <- 
    lapply(seq_along(nonLinFPre),
           function(idx)
           {
             nonLinFPre[[idx]](originalData[,idx])
           })
  DesMat <- matrix(unlist(DesMatList), ncol = length(DesMatList))
  DesMat <- cbind(DesMat, originalData)
  IntMatList <- 
    lapply(seq_along(interFPre),
           function(idx)
           {
             relData <- originalData[,combn(1:p_inf, 2)[,idx]]
             x1 <- relData[,1]
             x2 <- relData[,2]
             return(interFPre[[idx]](x1, x2))
           })
  IntMat <- matrix(unlist(IntMatList), ncol = length(IntMatList))
  P <- DesMat %*% solve(t(DesMat)%*%DesMat) %*% t(DesMat)
  z1z2 <- solve(t(DesMat)%*%DesMat) %*% t(DesMat) %*% IntMat
  PHOIntMat <- IntMat - P %*% IntMat
  PHODesMat <- DesMat + sweep(DesMat, 2, rowSums(z1z2), "*")
  #Orthog Nonlin Vs Lin
  IntMat2 <- PHODesMat[,1:length(nonLinFPre)]
  DesMat2 <- PHODesMat[,(length(nonLinFPre) + 1):ncol(PHODesMat)]
  P2 <- DesMat2 %*% solve(t(DesMat2)%*%DesMat2) %*% t(DesMat2)
  z1z2_2 <- solve(t(DesMat2)%*%DesMat2) %*% t(DesMat2) %*% IntMat2
  PHOIntMat2 <- IntMat2 - P2 %*% IntMat2
  PHODesMat2 <- DesMat2 + sweep(DesMat2, 2, rowSums(z1z2_2), "*")
  smoothPHODesMat <-
    lapply(1:3,
           function(idx)
           {
             tmpData <- originalData[,idx]
             sapply(tmpData,
                    function(x)
                      mean(PHOIntMat2[,idx][which((x - tmpData)^2 < r)]))}
    ) %>% unlist() %>% matrix(ncol = 3)
  nonLinF <- 
    lapply(seq_along(nonLinFPre),
           function(idx) smoothPHODesMat[,idx])
  interF <- 
    lapply(seq_along(interFPre),
           function(idx) PHOIntMat[,idx])
  return(list(nonLinF = nonLinF,
              interF = interF,
              betaUpdates = rowSums(z1z2)[(length(nonLinF) + 1):
                                            nrow(z1z2)] +
                rowSums(z1z2_2)))
}
