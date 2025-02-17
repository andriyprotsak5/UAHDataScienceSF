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

    message("\nInsert your data set:\n")
    buffer = getUserAction()

    #mean result
    res <- harmonic_mean(buffer)
    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the harmonic mean calculus for your data : ")
      message("\nIf the number has decimals, round to the 3rd\n")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(res,3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont <- cont + 1
        message("Ups, that might not be correct...")
        if(cont == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont > 2) {
          message(yellow("\nHint 2 -> add each element of your vector and divide it by the length\n\n"))
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
    message(bold("\n__HARMONIC MEAN CALCULUS__ \n"))
    message("\nThe harmonic mean of a dataset is calculated by the number of values by divided the inverse sum of the values . We'll give the user an example for better comprension.\n")
    message(green("\nFormula -> num_elements/ (1/x1 + 1/x2 +..+ 1/xn) \n"))
    message(green("xn: valor of elements to dataset\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    message("\nThe content of the vector is: ")
    for(i in x_sorted) {
      if( i == length(x_sorted)) {
        message(x[i])
      } else {
        message(x[i], ",")
      }
      denominator <- denominator + (1/i)
    }
    res <- (length(x)/denominator)
    message("\n")
    message("The invert sum of the elements is: ", blue(denominator), "\n")
    message("\nNext step, get the number of elements that we've examined")
    message("\nThe length of the vector is ", blue(length(x)), "elements\n")
    message("\nFormula applied -> ", blue(length(x)), "/", blue(denominator), " = ", bold(res))
    message("\nNow try by your own! :D\n")
    message("\nUse harmonic_mean(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)
  #sort the vector
  message("\nSorted vector: ")
  x_sorted <- sort(x)
  message(x_sorted, "\n")
  #function
  denominator = 0

  for(i in x_sorted) {
    denominator = denominator + (1 / i)
  }
  res <- length(x_sorted)/(denominator)
  message("\n")
  return(res)
}
