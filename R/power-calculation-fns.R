#' @description This function computes the probability that the none-individually-significant pre-test reject
#' @param betaPre. The true pre-treatment beta
#' @param SigmaPre. The covariance matrix
#' @param thresholdTstat.Pretest (optional). The threshold used for the t-test (default = 1.96)
rejectionProbability_NIS <-
  function(betaPre, SigmaPre, thresholdTstat.Pretest = 1.96){


    ub <- sqrt( diag( as.matrix(SigmaPre) ) ) * thresholdTstat.Pretest
    lb <- -ub

    power <- 1-
      mvtnorm::pmvnorm(mean = betaPre,
              sigma = SigmaPre,
              lower = lb,
              upper =  ub)

    return(power)
  }


#' @description This function computes the mean of betaPreHat conditional on no betaPreHat_j being significant
#' @param betaPre. The true pre-treatment beta
#' @param SigmaPre. The covariance matrix
#' @param thresholdTstat.Pretest (optional). The threshold used for the t-test (default = 1.96)
meanBetaPre_NIS <-
  function(betaPre, sigmaPre, thresholdTstat.Pretest = 1.96){

    ub <- sqrt( diag( as.matrix(sigmaPre) ) ) * thresholdTstat.Pretest
    lb <- -ub

    meanBetaPre <- tmvtnorm::mtmvnorm(mean = betaPre, sigma = sigmaPre,
                                      lower = lb, upper = ub,
                                      doComputeVariance = F)$tmean

    return(meanBetaPre)
  }


#' @description This function computes the mean of betaPostHat conditional on no betaPreHat_j being significant
#' @param beta. The true beta
#' @param sigma. The covariance matrix
#' @param prePeriodIndices. The indices of beta corresponding with pre-treatment periods
#' @param postPeriodIndices. The indices of beta corresponding with post-treatment periods
#' @param tVec. The vector of time periods corresponding with the coefficients in beta
#' @param referencePeriod. The omitted pre-treatment reference period normalized to 0
#' @param thresholdTstat.Pretest (optional). The threshold used for the t-test (default = 1.96)
#' @param eta. A vector so that the post-treatment target parameter is eta'*beta
#' @return df. A dataframe with columns betaPostConditional, betaPostUnconditional, relativeTime giving the conditional and unconditional means of betaPost for each relativeTime (relativeTime is tVec - referencePeriod)




meanBetaPost_NIS <- function(beta,
                             sigma,
                             prePeriodIndices = 1:(dim(as.matrix(sigma))[1] -1),
                             postPeriodIndices = dim(as.matrix(sigma))[1],
                             tVec = c( seq(-dim(as.matrix(sigma))[1] -1, -1), 1),
                             referencePeriod = 0,
                             thresholdTstat.Pretest = 1.96,
                             eta = NULL,
                             ...){

  betaPre <- beta[prePeriodIndices]
  sigmaPre <- sigma[prePeriodIndices, prePeriodIndices]

  betaPost <- beta[postPeriodIndices]
  tVecPost <- tVec[postPeriodIndices]

  sigma12 <- sigma[postPeriodIndices, prePeriodIndices]

  meanBetaPost <- betaPost + sigma12 %*% solve(sigmaPre) %*%
    (meanBetaPre_NIS(betaPre = betaPre,
                     sigmaPre = sigmaPre,
                     thresholdTstat.Pretest = thresholdTstat.Pretest) -
       betaPre)

  if(is.null(eta)){
    relativeT <- tVecPost - referencePeriod

    df <- data.frame(betaPostConditional = meanBetaPost,
                     betaPostUnconditional = betaPost,
                     relativeT = relativeT)
  }else{
    etaPost <- eta[postPeriodIndices]
    gammaPostConditional <- t(etaPost) %*% meanBetaPost
    gammaPostUnconditional <- t(etaPost) %*% betaPost

    df <- data.frame(betaPostConditional = gammaPostConditional,
                     betaPostUnconditional = gammaPostUnconditional,
                     relativeT = 1)
  }
  return(df)
}




#' @description This function computes the null rejection probability for betaPostHat conditional on no betaPreHat_j being significant
#' @param beta. The true beta
#' @param sigma. The covariance matrix
#' @param prePeriodIndices. The indices of beta corresponding with pre-treatment periods
#' @param postPeriodIndices. The indices of beta corresponding with post-treatment periods
#' @param tVec. The vector of time periods corresponding with the coefficients in beta
#' @param eta (optional). A vector so that the post-treatment target parameter is eta'*beta
#' @param thresholdT (optional). The threshold-t used for the post-treatment effect (default = 1.96)
#' @param thresholdTstat.Pretest (optional). The threshold used for the t-test (default = 1.96)
#' @param nullRejectionForZero (optional). If true, this is the null rejection probability assuming tau=0. If false, this is the null rejection probability for betaPost.
#' @return df. A dataframe with columns betaPostConditional, betaPostUnconditional, relativeTime giving the conditional and unconditional means of betaPost for each relativeTime (relativeTime is tVec - referencePeriod)


betaPostNullRejectionProbability_NIS <- function(beta,
                                                 sigma,
                                                 prePeriodIndices,
                                                 postPeriodIndices,
                                                 eta = NULL,
                                                 thresholdT = 1.96,
                                                 thresholdTstat.Pretest = 1.96,
                                                 nullRejectionForZero = T, #if T, give probability reject 0; otherwise, probability rejection eta * betapost
                                                 sigmaActual = sigma, #Allows for the true covariance to be sigmaActual, where sigma is used for pre-testing
                                                 ...){

  sigmaPreTest <- sigma


  if(!is.null(eta)){


    kTotal <- length(beta)
    kPre <- length(prePeriodIndices)

    #Create matrix m so that M %*% beta gives the pre-period coeffs and then eta'beta
    m <- matrix(0, nrow = kPre + 1, ncol = kTotal)
    m[1:kPre, prePeriodIndices] <- diag(kPre)
    m[kPre + 1, ] <- eta

    #Compute the actual sigma of M %*% beta, and the one used for the pre-test (possible differnet if sigmaActual provided)
    sigmaM_Actual <- m %*% sigmaActual %*% t(m)
    sigmaM_Pretest <- m %*% sigmaPreTest %*% t(m)

    sigmaPrePretest <- as.matrix( sigmaPreTest[prePeriodIndices, prePeriodIndices] )
    sigmaPreActual <- as.matrix( sigmaActual[prePeriodIndices, prePeriodIndices] )

    betaM <- as.vector( m %*% beta )

    ub_pre <- c( thresholdTstat.Pretest * sqrt( diag(sigmaPrePretest) ), Inf )
    lb_pre <- -ub_pre

    sigmaEtaPretest <- t(eta) %*% sigmaPreTest %*% eta
    betaEta <- t(eta) %*% beta

    ub_eta <- ifelse(nullRejectionForZero, 0, betaEta) + thresholdT * sqrt(sigmaEtaPretest)
    lb_eta <- ifelse(nullRejectionForZero, 0, betaEta) - thresholdT * sqrt(sigmaEtaPretest)

    acceptProbability <-
      tmvtnorm::ptmvnorm.marginal(xn = ub_eta, n = kPre + 1,
                                  lower = lb_pre, upper = ub_pre,
                                  sigma = sigmaM_Actual, mean = betaM) -
      tmvtnorm::ptmvnorm.marginal(xn = lb_eta, n = kPre + 1,
                                  lower = lb_pre, upper = ub_pre,
                                  sigma = sigmaM_Actual, mean = betaM)

    rejectProbability <- 1 - acceptProbability
    return( data.frame(rejectionProbability = rejectProbability) )
  }else{

    f <-
      function(index){
        betaPostNullRejectionProbability_NIS(
          beta = beta,
          sigma = sigma,
          prePeriodIndices = prePeriodIndices,
          postPeriodIndices = postPeriodIndices,
          eta = basisVector(index, length(beta)),
          thresholdT = thresholdT,
          thresholdTstat.Pretest = thresholdTstat.Pretest,
          nullRejectionForZero = nullRejectionForZero,
          sigmaActual = sigmaActual,
          ...)
      }

    return( rbindapply.allColumns(X = postPeriodIndices,
                                  FUN = f) )
  }
}


NullRejectionBetaPostAsAFunctionOfSlope_NIS <- function(
  slope,
  intercept = 0,
  tVec,
  referencePeriod,
  ...){
  argsList <- list(...)
  argsList$beta <- (tVec - referencePeriod) * slope + intercept
  argsList$tVec <- tVec
  argsList$referencePeriod <- referencePeriod

  ans <- do.call(betaPostNullRejectionProbability_NIS, argsList)

  return(ans)
}


meanBetaPostAsAFunctionOfSlope_NIS <- function(slope,
                                               intercept = 0,
                                               tVec,
                                               referencePeriod,
                                               ...){
  argsList <- list(...)
  argsList$beta <- (tVec - referencePeriod) * slope + intercept
  argsList$tVec <- tVec
  argsList$referencePeriod <- referencePeriod

  ans <- do.call(meanBetaPost_NIS, argsList)

  return(ans)
}


findSlopeForPower_NIS <- function(
  targetPower = 0.5,
  sigma,
  prePeriodIndices = 1:(dim(as.matrix(sigma))[1]),
  tVec = seq(-dim(as.matrix(sigma))[1], -1),
  referencePeriod = 0,
  maxiter = 1000,
  thresholdTstat.Pretest = 1.96,
  ...){

  sigmaPre <- as.matrix(sigma)[prePeriodIndices, prePeriodIndices]
  tVecPre <- tVec[prePeriodIndices]

  relativeTVec <- tVecPre - referencePeriod

  powerFn <- function(slope){
    power <- rejectionProbability_NIS(betaPre = relativeTVec * slope,
                                      SigmaPre = sigmaPre,
                                      thresholdTstat.Pretest = thresholdTstat.Pretest)

    return(power)
  }

  powerMinusTarget <- function(slope){return(powerFn(slope) - targetPower)}

  #slopeThatAchievesPower <- pracma::fsolve(powerMinusTarget, 0, maxiter = maxiter)$x
  slopeThatAchievesPower <- stats::uniroot(powerMinusTarget,
                                    lower = 0,
                                    upper =  8 * max(sqrt(diag(sigmaPre))),
                                    maxiter = maxiter)$root

  return(slopeThatAchievesPower)
}


