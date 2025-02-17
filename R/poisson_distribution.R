################################
###   POISSON DISTRIBUTION   ###
################################

#' Poisson Distribution Function
#'
#' This function calculates the Poisson distribution probability.
#'
#' @param k Optional number of occurrences (not needed for interactive mode)
#' @param lam Optional expected value lambda (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The Poisson probability (for non-interactive mode)
#' @importFrom crayon bold green yellow
#'
#' @examples
#' lam <- 2
#' k <- 3
#'
#' # Simple calculation
#' poisson_(k, lam)
#'
#' # Learning mode
#' poisson_(k, lam, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' poisson_(interactive = TRUE)
#' }
#'
#' @export
poisson_ <- function(k = NULL, lam = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(k) || is.null(lam))) {
    stop("k and lambda are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("poisson.png")
    cont_aux <- 0

    message("\nInsert the lam, parameter that represents the number of times.\n")
    lam = getUserAction()
    message("\nInsert k, the number of occurrences.\n")
    k = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the poisson calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(poisson_(k,lam),3)) {
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
    e <- 2.718281828459045235360
    message(bold("\n__POISSON DISTRIBUTION__ \n"))
    message("\nPoisson distribution that expresses the probability of a given number of events occurring in a fixed interval of time or space if these events occur with a known constant mean rate and independently of the time since the last event\n")
    message(green("\nFormula ->  ((e ^ (- lam)) * (lam ^ k)) / factorial(k)\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the e, the s Euler's number\n")
    message("In this case e=",e," \n")
    message("\nSecond, we need to know the lam, it is a positive parameter that represents the number of times the phenomenon is expected to occur during a given interval.\n")
    message("In this case lam=",lam," \n")
    message("\nFinally, we need to know the k, the number of occurrences.\n")
    message("In this case k=",k,"\n")

    res <- poisson_(k,lam)

    message("\nFormula applied -> ((",e,"  ^ (- ",lam,")) * (",lam," ^ ",k,")) / factorial(",k,") = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse poisson_(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  e <- 2.718281828459045235360
  res <- ((e ^ (- lam)) * (lam ^ k )) / (factorial(k))
  return(res)
}
