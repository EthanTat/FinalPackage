#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
chi_hypothesis <- function(mydata){
  name <- names(mydata)
  print(glue::glue("Hypothesis test"))
  print(glue::glue("Testing the variables {name[2]} and {name[4]}"))
  print(glue::glue("Null Hypothesis: two variables are independent"))
  print(glue::glue("Alternative Hypothesis: not Null Hypothesis"))
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
chi_assumptions <- function(mydata){
  carrange <- table(mydata$phys, mydata$gender)
  p1 <- barplot(carrange,
                beside = TRUE,
                legend = rownames(carrange),
                main = "Bar Plot of Physical vs Gender")
  p1
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
chi_fit <- function(mydata){
  carrange <- table(mydata$phys, mydata$gender)
  cdata <- chisq.test(carrange)
  t <- cdata$statistic
  pvalue <- cdata$p.value
  degfre <- cdata$parameter
  print(glue::glue("Test Statistics for Chi Test"))
  print(glue::glue("t-value = {t}"))
  print(glue::glue("Degree of freedom = {degfre}"))
  print(glue::glue("p-value = {pvalue}"))
  x <- c(pvalue, t, degfre)
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
chi_decision <- function(fit1){
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
chi_conclusion <- function(fit1){
  if(fit1[1] < 0.05){
    print(glue::glue("Conclusion"))
    print(glue::glue("Since, we rejected the null hypothesis in favour of the alternative hypothesis, we can conclude that there is an association between gender and the amount of physical activity."))
    print(glue::glue("Hence, the variables are not independant."))
  } else {
    print(glue::glue("Conclusion"))
    print(glue::glue("Since, we accepted the null hypothesis, we can conclude that there is no association between gender and the amount of physical activity."))
    print(glue::glue("Hence, the variables are independant."))
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
chi_wrap <- function(mydata){
  chi_hypothesis(mydata)
  fit1 <- chi_fit(mydata)
  chi_decision(fit1)
  chi_conclusion(fit1)
  chi_assumptions(mydata)
}

