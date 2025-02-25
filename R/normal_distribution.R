################################
###   NORMAL DISTRIBUTION   ###
################################

#' Normal Distribution Function
#'
#' This function calculates the normal distribution probability density.
#'
#' @param x Optional numeric value (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The normal probability density (for non-interactive mode)
#' @importFrom crayon bold green yellow
#'
#' @examples
#' x <- 0.1
#'
#' # Simple calculation
#' normal(x)
#'
#' # Learning mode
#' normal(x, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' normal(interactive = TRUE)
#' }
#'
#' @export
normal <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("x is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("normal.png")
    cont_aux <- 0

    message("\nInsert your data set:\n")
    x = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the normal distribution calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(normal(x), 3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          message(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    pi <- 3.14159265358979323846
    e <- 2.718281828459045235360
    message(bold("\n__NORMAL DISTRIBUTION__ \n"))
    message("\n The standard normal distribution is one that has the mean value of zero, M = 0, and the standard deviation of unity, Sigma = 1.
Its density function is:\n")
    message(green("\nFormula ->  (1/(2pi)^(1/2)) * (e)^((-x^2)/2)\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the e, the s Euler's number\n")
    message("In this case e=",e," \n")
    message("\nFinally, we need to know pi, the number pi.\n")
    message("In this case pi=",pi,"\n")

    res <- normal(x)

    message("\nFormula applied -> (1/(2*",pi,")^(1/2)) * (",e,")^((-",x,"^2)/2) = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse normal(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  e <- 2.718281828459045235360
  pi <- 3.14159265358979323846
  res <- (1/(2*pi)^(1/2)) * (e)^((-x^2)/2)
  return(res)
}
