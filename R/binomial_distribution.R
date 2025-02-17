################################
###  BIONOMIAL DISTRIBUTION  ###
################################

#' Binomial Distribution Function
#'
#' This function calculates the binomial distribution probability.
#'
#' @param n Optional number of trials (not needed for interactive mode)
#' @param x Optional number of successes (not needed for interactive mode)
#' @param p Optional probability of success (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The binomial probability (for non-interactive mode)
#' @importFrom crayon bold green yellow
#'
#' @examples
#' n <- 3
#' x <- 2
#' p <- 0.7
#'
#' # Simple calculation
#' binomial_(n, x, p)
#'
#' # Learning mode
#' binomial_(n, x, p, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' binomial_(interactive = TRUE)
#' }
#'
#' @export
binomial_ <- function(n = NULL, x = NULL, p = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(n) || is.null(x) || is.null(p))) {
    stop("n, x, and p are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("binomial.png")
    cont_aux <- 0

    message("\nInsert the n, the number of trials\n")
    n = getUserAction()
    message("\nInsert p, probability of success.\n")
    p = getUserAction()
    message("\nInsert the x, binomial random variable\n")
    x = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the binomial distribution calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == binomial_(n,x,p)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          message(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    message(bold("\n__BIONOMIAL DISTRIBUTION__ \n"))
    message("\nBinomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments, each asking a yes or no question, and each with its
           own Boolean-valued outcome: success (with probability p) or failure (with probability q = 1 - p)\n")
    message(green("\nFormula ->  ((factorial(n) / (factorial(x) * factorial(n-x))) * (p ^ x) * (1 - p)^(n - x))\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the n, the number of trials\n")
    message("In this case n=",n,"\n")
    message("\nSecond, we need to know the p, probability of success.\n")
    message("In this case p=",p,"\n")
    message("\nFinally, we need to know the x, binomial random variable\n")
    message("In this case x=",x,"\n")

    res <- binomial_(n,x,p)

    message("\nFormula applied -> (factorial(",n,") / (factorial(",x,") * factorial(",n,"-",x,"))) * (",p," ^ ",x,") * (1 - ",p,")^(",n," - ",x,") = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse binomial_(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  res <- (factorial(n) / (factorial(x) * factorial(n-x))) * (p ^ x) * (1 - p)^(n - x)
  return(res)
}
