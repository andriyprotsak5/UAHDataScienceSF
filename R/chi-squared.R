############################################
###  CALCUATED CHI-SQUARED DISTRIBUTION  ###
############################################

#' Chi-squared Distribution Function
#'
#' This function calculates the chi-squared statistic between two groups.
#'
#' @param x Optional first vector (not needed for interactive mode)
#' @param y Optional second vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The chi-squared statistic (for non-interactive mode)
#' @importFrom crayon bold green red yellow
#'
#' @examples
#' x <- c(70,75,74,72,68,59)
#' y <- c(74,77,70,80,72,76)
#'
#' # Simple calculation
#' chisquared(x, y)
#'
#' # Learning mode
#' chisquared(x, y, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' chisquared(interactive = TRUE)
#' }
#'
#' @export
chisquared <- function(x = NULL, y = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(y))) {
    stop("x and y are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("chisquared.png")
    cont_aux <- 0

    cat("\nInsert the dataset x:\n")
    x = getUserAction()
    cat("\nInsert the dataset y:\n")
    y = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the chi-squared calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(chisquared(x,y),3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        cat("Ups, that might not be correct...")
        if(cont_aux == 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    cat(bold("\n__CALCUATED CHI-SQUARED DISTRIBUTION__ \n"))
    cat("\nCalculated chi-squared is a probability distribution that serves to manifest tests in hypothesis of frequencies, this test compares observed frequencies with those expected frequencies.\n")
    cat(green("\nFormula ->  ((x[1]-y[1])^2)/y[1] + ((x[2]-y[2])^2)/y[2] + ... + ((x[n]-y[n])^2)/y[n]"))

    sizex <- length(x)
    sizey <- length(y)

    if (sizex == sizey) {
      cat(bold("\n__Use Example__\n"))
      cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
      cat("\nThe contents of the vector x is: \n")
      drawVector(x)

      cat("\nThe contents of the vector y is: \n")
      drawVector(y)
      cat("\n")

      res <- chisquared(x,y)
      total = 0
      cat("\nFormula applied ->")
      for(i in 1:sizex) {
        chi <- ((x[i]-y[i])^2)/y[i]
        total <- total + chi
        if(i == sizex) {
          cat(red(chi))
        } else {
          cat(red(" ",chi,"+"))
        }
      }

      cat(red(" = "))
      cat(bold(total))

      cat("\nNow try by your own! :D\n")
      cat("\nUse chisquared(interactive=TRUE) function to practice.\n")
      return(res)

    } else {
      cat("Size of sample is not correct")
    }
  }

  # Simple calculation mode
  sizex <- length(x)
  sizey <- length(y)
  total = 0
  if (sizex == sizey) {
    for(i in 1:sizex) {
      total <- total + ((x[i]-y[i])^2)/y[i]
    }
    res <- total
    return(res)
  } else {
    cat("Size of sample is not correct")
  }
}
