#######################################
### PEARSON CORRELATION COEFFICIENT ###
#######################################

#' Pearson Correlation Function
#'
#' This function calculates the Pearson correlation coefficient between two vectors of numbers.
#'
#' @param x Optional first numeric vector (not needed for interactive mode)
#' @param y Optional second numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The Pearson correlation coefficient between the two vectors (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#'
#' @examples
#' data <- c(10,4,5,7,3,4,1)
#' data2 <- c(1,8,3,4,4,5,7)
#'
#' # Simple calculation
#' pearson(data, data2)
#'
#' # Learning mode
#' pearson(data, data2, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' pearson(interactive = TRUE)
#' }
#'
#' @export
pearson <- function(x = NULL, y = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(y))) {
    stop("both data vectors are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("pearson.png")
    cont_aux <- 0

    cat("\nInsert your first data set:\n")
    x = getUserAction()
    cat("\nInsert your second data set:\n")
    y = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the pearson's correlation calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(pearson(x,y),3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        cat("Ups, that might not be correct...")
        if(cont_aux == 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }else if(cont_aux == 2) {
          cat(yellow("\nHint -> It has a value between +1 and -1 -->\n\n"))
        }
        else if(cont_aux > 2 ) {
          cat(yellow("\nHint 2 -> Pearson's correlation coefficient is the covariance of the two variables divided by the product of their standard deviations.It has a value between +1 and -1. A value of +1 is total positive linear correlation, 0 is no linear correlation, and -1 is total negative linear correlation.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    cat(bold("\n__PEARSON CORRELATION COEFFICIENT__ \n"))
    cat("\nPearson's correlation coefficient is the covariance of the two variables divided by the product of their standard deviations.It has a value between +1 and -1. A value of +1 is total positive linear correlation, 0 is no linear correlation, and -1 is total negative linear correlation.\n")
    cat(green("\nFormula ->  (covariance(x,y) / (standardDeviation(x) * standardDeviation(y))\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
    cat("\nThe contents of the vectors are: ")

    drawVector(x)
    drawVector(y)

    covar <- covariance(x,y)
    sDX <- standard_deviation(x)
    sDY <- standard_deviation(y)

    res <- (covar/(sDX*sDY))
    cat("The value of covariance: ", blue(covar), "\n")
    cat("The standard deviation of the elements of x is: ", blue(sDX), "\n")
    cat("The standard deviation of the elements of y is: ", blue(sDY), "\n")
    cat("\nFormula applied -> (", covar, "/ (",sDX , " * ", sDY, ") = ", bold(res))

    cat("\nNow try by your own! :D\n")
    cat("\nUse pearson(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)
  y <- as.vector(y)

  res <- (covariance(x,y)/(standard_deviation(x)*standard_deviation(y)))
  return(res)
}
