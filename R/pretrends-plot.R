
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_pointrange
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_shape_discrete
#' @title  Power calculations and visualization for pre-trends tests
#' @description Conducts power calculations for test of pre-treatment trends. Provides a plot for visualization, and also analyses of distortions from pre-testing.
#' @param betahat. The estimated event-plot coefficients
#' @param sigma. The covariance matrix for betahat
#' @param deltatrue. The hypothesized difference in trends.
#' @param tVec. The vector of time periods corresponding with the coefficients in beta
#' @param referencePeriod. (Optional) The omitted pre-treatment reference period normalized to 0. Default is t=0
#' @param prePeriodIndices. (Optional) The indices of beta corresponding with pre-treatment periods. Default is which(tVec<referencePeriod)
#' @param postPeriodIndices. (Optional) The indices of beta corresponding with post-treatment periods. Default is which(tVec > referencePeriod)
#' @return df_eventplot. A dataframe with columns t, bethat, deltatrue, se (the SEs of betahat), and meanAfterPretesting (expectation of betahat conditional on no significant pre-period coefficient)
#' @return df_power. A dataframe with the power of the pre-test (probability of finding no significant pre-period coefficient under deltatrue), Bayes Factor (ratio of probability of passing pre-test under delta true relative to under parallel trends), and Likelihood Ratio (likelihood of realized betahat under deltatrue relative to under parallel trends).
#' @return event_plot. An event plot with betahat, its standard errors, and the hypothesized trend (deltatrue)
#' @return event_plot_pretest. The same as event_plot, but with an added series that shows the expected value of betahatconditional on passing the pre-test under deltatrue.
#' @export
pretrends <- function(betahat,
                      sigma,
                      deltatrue,
                      tVec,
                      referencePeriod = 0,
                      prePeriodIndices = which(tVec < referencePeriod),
                      postPeriodIndices = which(tVec > referencePeriod)) {


  #Remove column/row names for sigma, sicne this yields problems with mtvmnorm
  colnames(sigma) <- NULL
  rownames(sigma) <- NULL

  sigma <- as.matrix(sigma) #convert sigma to a matrix if it's not already
  if( NROW(sigma) != NCOL(sigma) | !isSymmetric(sigma) ){stop("sigma must be a symmetric positive definite matrix")}

  if(length(betahat) != NROW(sigma)){stop("The dimension of sigma must correspond with the number of event-study coefficients")}
  if(length(tVec) != length(betahat)){stop("The dimension of tVec must correspond with the number of event-study coefficients")}
  if(length(betahat) != length(deltatrue)){stop("The dimension of the hypothesized difference in trends (deltatrue) must correspond with the number of event-study coefficients.")}

  se <- sqrt(diag(sigma))


  #Extract the objets corresponding with the pre-period
  betaPreActual <- betahat[prePeriodIndices]
  betaPreAlt <- deltatrue[prePeriodIndices]
  sigmaPre <- sigma[prePeriodIndices, prePeriodIndices]

  #Compute power against the alt trend and power against 0 (i.e. size of test)
  power_against_betatrue <- rejectionProbability_NIS(betaPre = betaPreAlt, SigmaPre = sigmaPre)
  power_against_0 <- rejectionProbability_NIS(betaPre = 0*betaPreAlt, SigmaPre = sigmaPre)

  #COmpute likelihoods under beta=betaPreAlt and beta=0
  if(length(prePeriodIndices) == 1){
    likelihood_betatrue <- stats::dnorm(x = betaPreActual, mean = betaPreAlt, sd = sqrt(sigmaPre))
    likelihood_0 <- stats::dnorm(x = betaPreActual, mean = 0*betaPreAlt, sd = sqrt(sigmaPre))
  }else{
    likelihood_betatrue <- mvtnorm::dmvnorm(x = betaPreActual, mean = betaPreAlt, sigma = sigmaPre)
    likelihood_0 <- mvtnorm::dmvnorm(x = betaPreActual, mean = 0*betaPreAlt, sigma = sigmaPre)
  }

  #Compute the means after pre-testing
  meanBetaPre <- meanBetaPre_NIS(betaPre = betaPreAlt,sigmaPre = sigmaPre)
  meanBetaPost <- meanBetaPost_NIS(beta = deltatrue,
                                   sigma = sigma,
                                   prePeriodIndices = prePeriodIndices,
                                   postPeriodIndices = postPeriodIndices,
                                   tVec = tVec)$betaPostConditional

  meanAfterPretesting_df <- data.frame(t= c(tVec[prePeriodIndices], tVec[postPeriodIndices], -1),
                                       meanAfterPretesting = c(meanBetaPre, meanBetaPost, 0 ))

  df_eventplot <- data.frame(t = tVec,
                             betahat = betahat,
                             deltatrue = deltatrue,
                             se = se)

  #Add the reference period to df_eventplot, if it is not contained in tVec / betahat
  if(!(referencePeriod %in% tVec)){
    df_eventplot <- rbind(df_eventplot,
                          data.frame(t = referencePeriod,
                                     deltatrue = 0,
                                     betahat = 0,
                                     se = 0)) %>%
          dplyr::arrange(t)
  }
  df_eventplot <- dplyr::left_join(df_eventplot, meanAfterPretesting_df, by = c("t"))
  #Create a data frame displaying the power, BF, and LR
  df_power <-
    data.frame(Power = power_against_betatrue,
               `Bayes Factor` = (1-power_against_betatrue) / (1-power_against_0),
               `Likelihood Ratio` = likelihood_betatrue / likelihood_0 )


  #Create a plot with the event coefficients and hypothesized trend
  event_plot<-
    ggplot(data = df_eventplot, aes(x = t, y = betahat, ymin = betahat - 1.96* se, ymax = betahat + 1.96*se)) +
    geom_point(aes(color = "Estimated Coefs", shape = "Estimated Coefs")) + geom_pointrange() +
    geom_point(aes(y=deltatrue, color = "Hypothesized Trend", shape = "Hypothesized Trend"), size = 2.5) +
    geom_line(aes(y=deltatrue, color = "Hypothesized Trend")) +
    scale_color_manual(values = c("black", "red", "blue"),
                       breaks = c("Estimated Coefs", "Hypothesized Trend", "Expectation After Pre-testing"),
                       name = "") +
    scale_shape_discrete(#values = c(1,2,3),
      breaks = c("Estimated Coefs", "Hypothesized Trend", "Expectation After Pre-testing"),
      name = "") +
    xlab("Relative Time") + ylab("") +
    ggtitle("Event Plot and Hypothesized Trends")

  event_plot_pretest <-
    event_plot +
    geom_point(aes(y=meanAfterPretesting, color = "Expectation After Pre-testing", shape = "Expectation After Pre-testing"), size = 2.5) +
    geom_line(aes(y=meanAfterPretesting, color = "Expectation After Pre-testing"), linetype = "dashed")

  #Return the dataframes in a list
  return(list(df_eventplot = df_eventplot,
              df_power = df_power,
              event_plot = event_plot,
              event_plot_pretest = event_plot_pretest))
}



#' @title  Linear trend for which pre-trends tests have a given power level
#' @description This function computes the linear violation of parallel trends for which convention pre-trends tests have a pre-specified power level
#' @param sigma. The covariance matrix for the event-plot coefficients.
#' @param targetPower. The desired power for the pre-test, which reject if any pre-treatment coefficient is significant at the 5% level
#' @param tVec. The vector of time periods corresponding with the coefficients
#' @param referencePeriod. (Optional) The omitted pre-treatment reference period normalized to 0. Default is t=0
#' @param prePeriodIndices. (Optional) The indices of the event-plot corresponding with pre-treatment periods. Default is which(tVec<referencePeriod)
#' @return slope. The slope of the trend for which targetPower is achieved.
#' @export
slope_for_power <- function(sigma,
                            targetPower = 0.5,
                            tVec,
                            referencePeriod = 0,
                            prePeriodIndices = which(tVec < referencePeriod)){


  #Remove column/row names for sigma, since this yields problems with mtvmnorm
  colnames(sigma) <- NULL
  rownames(sigma) <- NULL

  sigma <- as.matrix(sigma) #convert sigma to a matrix if it's not already
  if( NROW(sigma) != NROW(sigma) | !isSymmetric(sigma) ){stop("sigma must be a symmetric positive definite matrix")}
  if(length(tVec) != NCOL(sigma)){stop("The dimension of tVec must correspond with the number of event-study coefficients, i.e. the number of rows/cols in sigma")}
  if(length(prePeriodIndices) == 0){stop("There are no pre-treatment periods with tVec < referencePeriod. If referencePeriod is not the last, pre-treatment period, use prePeriodIndices to denote the indices of the coefficients that are in the pre-treatment period. E.g., if the first two elements of beta are pre-treatment, use prePeriodIndices = c(1,2).")}

  findSlopeForPower_NIS(targetPower = targetPower,
                        sigma = sigma,
                        tVec = tVec,
                        prePeriodIndices = prePeriodIndices,
                        referencePeriod = referencePeriod)

}

