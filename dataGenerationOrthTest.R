purifyNonLinFunction <- function(idx, lotf, nonLinFIdx, X)
{
  x <- X[,idx]
  lmod <- lm(y ~ 1 + x, data = data.frame(y = lotf[[nonLinFIdx[idx]]](x),
                                          x = x))
  nonLinF <- function(x) lotf[[nonLinFIdx[idx]]](x) - lmod$coefficients[1] -
    x * lmod$coefficients[2]
  scaleFac <- max(abs(nonLinF(x)))
  # x <- scale(x)
  y <- nonLinF(x)
  # scaleFac <- sum(diff(x) * (abs(y[-1]) + abs(y[-length(y)])) / 2)
  return(function(x) nonLinF(x) / scaleFac)
}
purifyInterFunction <- function(idx, interf, interFIdx, X)
{
  relData <- X[,combn(1:p_inf, 2)[,idx]]
  x1 <- relData[,1]
  x2 <- relData[,2]
  mod <- mgcv::gam(y ~ 1 + s(x1) + s(x2),
             data = data.frame(y = interf[[interFIdx[idx]]](x1, x2),
                               x1 = x1,
                               x2 = x2))
  interF <- function(x1, x2) interf[[interFIdx[idx]]](x1, x2) -
    mod$coefficients[1] -
    x1 * lmod$coefficients[2] - x2 * lmod$coefficients[3]
  scaleFac <- max(abs(interF(x1, x2)))
  return(function(x1, x2) interF(x1, x2) / scaleFac)
}
# stackedOrthFunction <- function(nonLinFPre, interFPre, r, originalData,
#                                 globalTerm)
# {
#   nonLinMatList <-
#     lapply(seq_along(nonLinFPre),
#            function(idx)
#            {
#              nonLinFPre[[idx]](originalData[,idx])
#            })
#   nonLinMat <- matrix(unlist(nonLinMatList), ncol = length(nonLinMatList))
#   IntMatList <-
#     lapply(seq_along(interFPre),
#            function(idx)
#            {
#              relData <- originalData[,combn(1:p_inf, 2)[,idx]]
#              x1 <- relData[,1]
#              x2 <- relData[,2]
#              return(interFPre[[idx]](x1, x2))
#            })
#   IntMat <- matrix(unlist(IntMatList), ncol = length(IntMatList))
#   globalTermValues <- apply(originalData, 1, globalTerm)
#   DesMat <- cbind(1, originalData, nonLinMat, IntMat)
#   P <- DesMat %*% solve(t(DesMat)%*%DesMat) %*% t(DesMat)
#   z1z2 <- solve(t(DesMat)%*%DesMat) %*% t(DesMat) %*% globalTermValues
#   PHOglobalTermValues <- globalTermValues - P %*% globalTermValues
#   PHODesMat <- DesMat + sweep(DesMat, 2, rowSums(z1z2), "*")
#   #Orthog Inter Vs NonLin
#   IntMat2 <- PHODesMat[,15:17]
#   DesMat2 <- PHODesMat[,1:14]
#   P2 <- DesMat2 %*% solve(t(DesMat2)%*%DesMat2) %*% t(DesMat2)
#   z1z2_2 <- solve(t(DesMat2)%*%DesMat2) %*% t(DesMat2) %*% IntMat2
#   PHOIntMat2 <- IntMat2 - P2 %*% IntMat2
#   PHODesMat2 <- DesMat2 + sweep(DesMat2, 2, rowSums(z1z2_2), "*")
#   #Orthog Nonlin Vs Lin
#   NonLinMat2 <- PHODesMat2[,12:14]
#   DesMat3 <- PHODesMat2[,1:11]
#   P3 <- DesMat3 %*% solve(t(DesMat3)%*%DesMat3) %*% t(DesMat3)
#   z1z2_3 <- solve(t(DesMat3)%*%DesMat3) %*% t(DesMat3) %*% NonLinMat2
#   PHONonLinMat <- NonLinMat2 - P3 %*% NonLinMat2
#   finalPHOMat <- cbind(PHONonLinMat, PHOIntMat2, PHOglobalTermValues)
#   # finalPHOMat <- scale(cbind(PHODesMat2, PHOIntMat2, PHOglobalTermValues))
#   # #Orthog Nonlin Vs Lin
#   # IntMat2 <- PHODesMat[,1:length(nonLinFPre)]
#   # DesMat2 <- PHODesMat[,(length(nonLinFPre) + 1):ncol(PHODesMat)]
#   # P2 <- DesMat2 %*% solve(t(DesMat2)%*%DesMat2) %*% t(DesMat2)
#   # z1z2_2 <- solve(t(DesMat2)%*%DesMat2) %*% t(DesMat2) %*% IntMat2
#   # PHOIntMat2 <- IntMat2 - P2 %*% IntMat2
#   # PHODesMat2 <- DesMat2 + sweep(DesMat2, 2, rowSums(z1z2_2), "*")
#   # smoothPHODesMat <-
#   #   lapply(1:3,
#   #          function(idx)
#   #          {
#   #            tmpData <- originalData[,idx]
#   #            sapply(tmpData,
#   #                   function(x)
#   #                     # mean(PHOIntMat2[,idx][which((x - tmpData)^2 < r)]))}
#   #                     mean(PHODesMat[,idx][which((x - tmpData)^2 < r)]))}
#   #   ) %>% unlist() %>% matrix(ncol = 3)
#   nonLinF <-
#     lapply(seq_along(nonLinFPre),
#            # function(idx) scale(smoothPHODesMat[,idx], scale = FALSE)
#            function(idx) finalPHOMat[,idx]
#            )
#   interF <-
#     lapply(seq_along(interFPre),
#            function(idx) finalPHOMat[,3+idx])
#   return(list(nonLinF = nonLinF,
#               interF = interF,
#               globalF = finalPHOMat[,7]#,
#               # betaUpdates = rowSums(z1z2)[(length(nonLinF) + 1):
#               #                               nrow(z1z2)] +
#               #   rowSums(z1z2_2)
#               ))
# }

