################################
###  T-STUDENT DISTRIBUTION  ###
################################

#' Student's t Distribution Function
#'
#' This function calculates the t-statistic for sample data.
#'
#' @param x Optional sample mean (not needed for interactive mode)
#' @param u Optional population mean (not needed for interactive mode)
#' @param s Optional standard deviation (not needed for interactive mode)
#' @param n Optional sample size (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The t-statistic (for non-interactive mode)
#' @importFrom crayon bold green yellow
#'
#' @examples
#' x <- 52.9
#' u <- 50
#' s <- 3
#' n <- 10
#'
#' # Simple calculation
#' tstudent(x, u, s, n)
#'
#' # Learning mode
#' tstudent(x, u, s, n, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' tstudent(interactive = TRUE)
#' }
#'
#' @export
tstudent <- function(x = NULL, u = NULL, s = NULL, n = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(u) || is.null(s) || is.null(n))) {
    stop("x, u, s, and n are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("t-student.jpg")
    cont_aux <- 0

    message("\nInsert the the x (sample mean)\n")
    x = getUserAction()
    message("\nInsert the the u (population mean)\n")
    u = getUserAction()
    message("\nInsert the the  s (population standard deviation)\n")
    s = getUserAction()
    message("\nInsert the the  n (is sample size)\n")
    n = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the t-student distribution calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(tstudent(x,u,s,n),3)) {
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
    message(bold("\n__T-STUDENT DISTRIBUTION__ \n"))
    message("\nT-student is a probability distribution that arises from the problem of estimating the mean of a normally distributed population when the sample size is small.\n")
    message(green("\nFormula ->  (x-u)/(s/(n)^(1/2))"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the x, is sample mean\n")
    message("In this case x=",x," \n")
    message("\nSecond, we need to know the u, is population mean\n")
    message("In this case u=",u," \n")
    message("\nNext, we need to know the s, is population standard deviation\n")
    message("In this case s=",s," \n")
    message("\nFinally, we need to know the n, is sample size.\n")
    message("In this case n=",n,"\n")

    res <- tstudent(x,u,s,n)

    message("\nFormula applied -> (",x," - ",u,")/(",s,"/(",n,")^(1/2)) = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse tstudent(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  res <- (x-u)/(s/(n)^(1/2))
  return(res)
}
