################################
###      LAPLACE`S RULE      ###
################################

#' Laplace's Rule Function
#'
#' This function calculates Laplace's Rule for a probability experiment.
#'
#' @param x Optional first vector (not needed for interactive mode)
#' @param y Optional second vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The probability according to Laplace's Rule (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#'
#' @examples
#' data <- 3
#' data2 <- c(1,2,3,4,5,6)
#'
#' # Simple calculation
#' laplace(data, data2)
#'
#' # Learning mode
#' laplace(data, data2, learn = TRUE)
#'
#' # Interactive mode
#' \dontrun{
#' laplace(interactive = TRUE)
#' }
#'
#' @export
laplace <- function(x = NULL, y = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(y))) {
    stop("both vectors are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("laplace.jpg")
    cont_aux <- 0

    cat("\nInsert your first data set:\n")
    x = getUserAction()
    cat("\nInsert your second data set:\n")
    y = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the laplace's rule calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(laplace(x,y),3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        cat("Ups, that might not be correct...")
        if(cont_aux == 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          cat(yellow("\nHint 2 -> Laplace's rule as the quotient between the number of favorable cases to A, and that of all possible results of the experiment.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    cat(bold("\n__LAPLACE`S RULE __ \n"))
    cat("\nLaplace's rule as the quotient between the number of favorable cases to A, and that of all possible results of the experiment.\n")
    cat(green("\nFormula ->  (Cases favorable to A / All possible results)\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
    cat("\nThe contents of the vector is: ")

    drawVector(x)

    casesF <- length(x)
    casesT <- length(y)
    res <- (casesF/casesT)

    cat("Favorables cases: ", blue(casesF), "\n")
    cat("All possible results: ", blue(casesT), "\n")
    cat("\nFormula applied -> (", casesF, "/ ",casesT , " = ", bold(res))

    cat("\nNow try by your own! :D\n")
    cat("\nUse laplace(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  res <- (length(x)/length(y))
  return(res)
}
