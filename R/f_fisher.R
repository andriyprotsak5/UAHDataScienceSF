###############################
###  F FISHER DISTRIBUTION  ###
###############################

#' Fisher's F Distribution Function
#'
#' This function calculates the F statistic between two groups.
#'
#' @param x Optional first vector (not needed for interactive mode)
#' @param y Optional second vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The F statistic (for non-interactive mode)
#' @importFrom crayon bold green yellow
#'
#' @examples
#' x <- c(70,75,74,72,68,59)
#' y <- c(74,77,70,80,72,76)
#'
#' # Simple calculation
#' fisher(x, y)
#'
#' # Learning mode
#' fisher(x, y, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' fisher(interactive = TRUE)
#' }
#'
#' @export
fisher <- function(x = NULL, y = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(y))) {
    stop("x and y are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("f-fisher.png")
    cont_aux <- 0

    cat("\nInsert the first dataset:\n")
    x = getUserAction()
    cat("\nInsert the second dataset:\n")
    y = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the Fisher's f calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(fisher(x,y),3)) {
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
    cat(bold("\n__ F FISHER DISTRIBUTION__ \n"))
    cat("\nF-Fisher distribution is a continuous probability distribution that arises frequently as the null distribution of a test statistic.\n")
    cat(green("\nFormula -> sx2/sw2\n"))
    cat(green("\nsx2 <- 2 * (((mean_(x)-meant)^2) + ((mean_(y)-meant)^2))"))
    cat(green("\n(variance_(x) + variance_(y))/ 2"))

    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need two datasets.")
    cat("\n Dateset x: \n")
    drawVector(x)
    cat("\n Dateset x: \n")
    drawVector(y)

    meant <- (mean_(x) + mean_(y))/2
    sx2 <- 2 * (((mean_(x)-meant)^2) + ((mean_(y)-meant)^2))
    sw2 <- (variance(x) + variance(y))/ 2
    res <- fisher(x,y)

    cat("\nFormula applied -> (",sx2,"/",sw2,") = ", bold(res))

    cat("\nNow try by your own! :D\n")
    cat("\nUse fisher(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  meant <- (mean_(x) + mean_(y))/2
  sx2 <- 2 * (((mean_(x)-meant)^2) + ((mean_(y)-meant)^2))
  sw2 <- (variance(x) + variance(y))/ 2
  res <- sx2/sw2
  return(res)
}
