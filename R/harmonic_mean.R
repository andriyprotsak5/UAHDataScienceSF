#########################
### -HARMONIC MEAN- ###
#########################

#' Harmonic Mean Function
#'
#' This function calculates the harmonic mean of a numbers vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The harmonic mean of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#' @examples
#' data <- c(1,4,3,3,2,5,7,12,1,2,3,12)
#'
#' # Simple calculation
#' harmonic_mean(data)
#'
#' # Learning mode
#' harmonic_mean(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' harmonic_mean(interactive = TRUE)
#' }
#'
#' @export
harmonic_mean <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("harmonic-mean.jpg")
    cont <- 0

    cat("\nInsert your data set:\n")
    buffer = getUserAction()

    #mean result
    res <- harmonic_mean(buffer)
    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the harmonic mean calculus for your data : ")
      cat("\nIf the number has decimals, round to the 3rd\n")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(res,3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont <- cont + 1
        cat("Ups, that might not be correct...")
        if(cont == 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont > 2) {
          cat(yellow("\nHint 2 -> add each element of your vector and divide it by the length\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)
    x_sorted <- sort(x)
    denominator <- 0
    cat(bold("\n__HARMONIC MEAN CALCULUS__ \n"))
    cat("\nThe harmonic mean of a dataset is calculated by the number of values by divided the inverse sum of the values . We'll give the user an example for better comprension.\n")
    cat(green("\nFormula -> num_elements/ (1/x1 + 1/x2 +..+ 1/xn) \n"))
    cat(green("xn: valor of elements to dataset\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    cat("\nThe content of the vector is: ")
    for(i in x_sorted) {
      if( i == length(x_sorted)) {
        cat(x[i])
      } else {
        cat(x[i], ",")
      }
      denominator <- denominator + (1/i)
    }
    res <- (length(x)/denominator)
    cat("\n")
    cat("The invert sum of the elements is: ", blue(denominator), "\n")
    cat("\nNext step, get the number of elements that we've examined")
    cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
    cat("\nFormula applied -> ", blue(length(x)), "/", blue(denominator), " = ", bold(res))
    cat("\nNow try by your own! :D\n")
    cat("\nUse harmonic_mean(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)
  #sort the vector
  cat("\nSorted vector: ")
  x_sorted <- sort(x)
  cat(x_sorted, "\n")
  #function
  denominator = 0

  for(i in x_sorted) {
    denominator = denominator + (1 / i)
  }
  res <- length(x_sorted)/(denominator)
  cat("\n")
  return(res)
}
