#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
t_hypothesis <- function(mydata){
  name <- names(mydata)
  print(glue::glue("Hypothesis test"))
  print(glue::glue("Testing the variables {name[2]} and {name[3]}."))
  print(glue::glue("Null Hypothesis: μ1 = μ2"))
  print(glue::glue("Alternative Hypothesis: μ1 ≠ μ2"))
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#' @import ggplot2
#' @import patchwork
#'
#' @examples
t_assumptions <- function(mydata){
  mdata <- mydata%>%
    filter(gender == "Male")%>%
    select(height)
  fdata <- mydata%>%
    filter(gender == "Female")%>%
    select(height)
  p1 <- ggplot2::ggplot(mdata, aes(sample = height))+
    geom_qq()+
    geom_qq_line(col="red")+
    ggtitle("QQ-plot for Male's Height")
  p2 <- ggplot2::ggplot(fdata, aes(sample = height))+
    geom_qq()+
    geom_qq_line(col="red")+
    ggtitle("QQ-plot for Female's Height")
  return(p1 + p2)
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
t_fit <- function(mydata){
  mdata <- mydata%>%
    dplyr::filter(gender == "Male")%>%
    dplyr::select(height)
  fdata <- mydata%>%
    dplyr::filter(gender == "Female")%>%
    dplyr::select(height)
  tdata <- t.test(fdata, mdata,alternative = "two.sided", var.equal = TRUE)
  t <- tdata$statistic
  degfre <- tdata$parameter
  lowerCI <- tdata$conf.int[1]
  upperCI <- tdata$conf.int[2]
  pvalue <- tdata$p.value
  print(glue::glue("Test Statistics for t Test"))
  print(glue::glue("Confidance Interval = ({lowerCI}, {upperCI})"))
  print(glue::glue("t-value = {t}"))
  print(glue::glue("Degree of freedom = {degfre}"))
  print(glue::glue("p-value = {pvalue}"))
  x <- c(pvalue, lowerCI, upperCI, t, degfre)
  return(x)
}

#' hypothesis function
#'
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
t_decision <- function(fit1){
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
#' @param mydata ...
#'
#' @return
#' @export
#'
#' @examples
t_conclusion <- function(fit1){
  if(fit1[1] < 0.05){
    print(glue::glue("Conclusion"))
    print(glue::glue("Since, we rejected the null hypothesis in favour of the alternative hypothesis, we can conclude that the mean height of males and females is not the same."))
  } else {
    print(glue::glue("Conclusion"))
    print(glue::glue("Since, we accepted the null hypothesis, we can conclude that the mean height of males and females is the same."))
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
t_wrap <- function(mydata){
  t_hypothesis(mydata)
  fit1 <- t_fit(mydata)
  t_decision(fit1)
  t_conclusion(fit1)
  t_assumptions(mydata)
}
