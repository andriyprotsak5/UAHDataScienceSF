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
#' if(interactive()){
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

    message("\nInsert your first data set:\n")
    x = getUserAction()
    message("\nInsert your second data set:\n")
    y = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the laplace's rule calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(laplace(x,y),3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          message(yellow("\nHint 2 -> Laplace's rule as the quotient between the number of favorable cases to A, and that of all possible results of the experiment.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    message(bold("\n__LAPLACE`S RULE __ \n"))
    message("\nLaplace's rule as the quotient between the number of favorable cases to A, and that of all possible results of the experiment.\n")
    message(green("\nFormula ->  (Cases favorable to A / All possible results)\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
    message("\nThe contents of the vector is: ")

    drawVector(x)

    casesF <- length(x)
    casesT <- length(y)
    res <- (casesF/casesT)

    message("Favorables cases: ", blue(casesF), "\n")
    message("All possible results: ", blue(casesT), "\n")
    message("\nFormula applied -> (", casesF, "/ ",casesT , " = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse laplace(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  res <- (length(x)/length(y))
  return(res)
}
