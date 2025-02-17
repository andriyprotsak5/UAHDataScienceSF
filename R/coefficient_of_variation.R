################################
### COEFFICIENT OF VARIATION ###
################################

#' Coefficient of Variation Function
#'
#' This function calculates the coefficient of variation of a numbers vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The coefficient of variation of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#' @examples
#' data <- c(10,4,5,7,3,4,1)
#'
#' # Simple calculation
#' cv(data)
#'
#' # Learning mode
#' cv(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' cv(interactive = TRUE)
#' }
#'
#' @export
cv <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("cv.jpg")
    cont_aux <- 0

    cat("\nInsert your data set:\n")
    x = getUserAction()
    buffer = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the coefficient of variation calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(cv(buffer),3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        cat("Ups, that might not be correct...")
        if(cont_aux == 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          cat(yellow("\nHint 2 -> The coefficient of variation (CV) is defined as the ratio of the standard deviation to the mean.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    cat(bold("\n__COEFFICIENT OF VARIATION__ \n"))
    cat("\nThe coefficient of variation (CV) is defined as the ratio of the standard deviation to the mean.\n")
    cat(green("\nFormula ->  (standardDeviation(x) / mean(x))\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
    cat("\nThe contents of the vector is: ")

    drawVector(x)

    sDX <- standard_deviation(x)
    meanx <- mean_(x)
    res <- (sDX/(meanx))

    cat("The standard deviation of the elements of x is: ", blue(sDX), "\n")
    cat("The value of mean: ", blue(meanx), "\n")
    cat("\nFormula applied -> (", sDX, "/ ",meanx , " = ", bold(res))

    cat("\nNow try by your own! :D\n")
    cat("\nUse cv(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)
  res <- (standard_deviation(x)/mean_(x))
  return(res)
}
