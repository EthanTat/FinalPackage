#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
lm_hypothesis <- function(mydata){
  name <- names(mydata)
  print(glue::glue("Hypothesis test"))
  print(glue::glue("Testing the variables {name[3]} and {name[4]}"))
  print(glue::glue("Null Hypothesis: β = 0"))
  print(glue::glue("Alternative Hypothesis: β ≠ 0"))
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#' @import ggplot2
#' @import patchwork
#' @examples
lm_assumptions <- function(mydata){
  X <- mydata$height
  Y <- mydata$weight
  lmdata <- lm(Y~X, data= mydata)
  p1 <- ggplot2::ggplot(mydata, ggplot2::aes(x = X, y = Y)) +
    geom_point() +
    stat_smooth() +
    ggtitle("Y vs X")
  p2 <- ggplot2::ggplot(lmdata, ggplot2::aes(y=lmdata$residuals, x=lmdata$fitted.values))+
    geom_point() + ggtitle("Residual vs fitted")
  p3 <- ggplot2::ggplot(lmdata, ggplot2::aes(lmdata$residual))+
    geom_histogram() + ggtitle("Distribution of residual values")
  patchwork::wrap_plots(p1, p2, p3, ncol = 1)
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
lm_fit <- function(mydata){
  lmdata <- lm(mydata$height~mydata$weight, data= mydata)
  lowerCI <- confint(lmdata, level=0.95)[2,1]
  upperCI <- confint(lmdata, level=0.95)[2,2]
  t <- summary(lmdata)[["coefficients"]][, "t value"][2]
  degfre <- lmdata$df.residual
  pvalue <- summary(lmdata)[["coefficients"]][, "Pr(>|t|)"][2]
  print(glue::glue("Test Statistics for Linear Model"))
  print(glue::glue("Confidance Interval = ({lowerCI}, {upperCI})"))
  print(glue::glue("t-value = {t}"))
  print(glue::glue("Degree of freedom = {degfre}"))
  print(glue::glue("p-value = {pvalue}"))
  x <- c(pvalue,lowerCI, upperCI, t, degfre)
  return(x)
}

#' hypothesis function
#'
#' @param fit1 ...
#'
#' @return
#' @export
#'
#' @examples
lm_decision <- function(fit1){
  if(fit1[1] < 0.05){
    print(glue::glue("Decision"))
    print(glue::glue("At 5% significance level, {fit1[1]} < 0.05."))
    print(glue::glue("So we reject the null hypothesis in favour of the alternative hypothesis."))
    print(glue::glue(""))
  } else {
    print(glue::glue("Decision"))
    print(glue::glue("At 5% significance level, {fit1[1]} > 0.05."))
    print(glue::glue("So we accept the null hypothesis."))
    print(glue::glue(""))
  }
}

#' hypothesis function
#'
#' @param fit1 ...
#'
#' @return
#' @export
#'
#' @examples
lm_conclusion <- function(fit1){
  if(fit1[1] < 0.05){
    print(glue::glue("Conclusion"))
    print(glue::glue("Since, we rejected the null hypothesis in favour of the alternative hypothesis, we can conclude that there is not a linear relationship between height and weight."))
  } else {
    print(glue::glue("Conclusion"))
    print(glue::glue("Since, we accepted the null hypothesis, we can conclude that there is a linear relationship between height and weight."))
  }
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
lm_wrap <- function(mydata){
  lm_hypothesis(mydata)
  fit1 <- lm_fit(mydata)
  lm_decision(fit1)
  lm_conclusion(fit1)
  lm_assumptions(mydata)
}
