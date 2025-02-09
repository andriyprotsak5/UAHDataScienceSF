#########################
###     VARIANCE      ###
#########################

#' Variance Function
#'
#' This function calculates the variance of a numbers vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The variance of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#'
#' @examples
#' data <- c(10,4,5,7,3,4,1)
#'
#' # Simple calculation
#' variance(data)
#'
#' # Learning mode
#' variance(data, learn = TRUE)
#'
#' # Interactive mode
#' \dontrun{
#' variance(interactive = TRUE)
#' }
#'
#' @export
variance <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("variance.png")
    cont_aux <- 0

    buffer = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(variance(buffer),3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        cat("Ups, that might not be correct...")
        if(cont_aux == 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          cat(yellow("\nHint 2 -> The variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    cat(bold("\n__VARIANCE CALCULUS__ \n"))
    cat("\nThe variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
    cat(green("\nFormula ->  (Summation(each_element - Mean)^2) / num_elements\n"))
    cat(green("\nMean -> (x1 + x2 +..+xn) / n\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    cat("\nThe content of the vector is: ")

    drawVector(x)

    mean <- mean_(x)
    cat("\nThe mean of dataset is...", blue(mean))
    cat("\nThe square of the diference between each number and the mean of dataset is: ")
    suma <- 0
    for(i in 1:length(x)) {
      square <- ((x[i] - mean) ^ 2)
      if( i == length(x)) {
        cat(square)
        suma <- suma + square
      } else {
        cat(square, ",")
        suma <- suma + square
      }
    }
    res <- (suma/(length(x)-1))
    cat("\nNow we need to add each element of the vector/dataset\n")
    cat("The sum of the squares is: ", blue(suma), "\n")
    cat("\nNext step, get the number of elements that we've examined")
    cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
    cat("\nFormula applied -> ", suma, "/", length(x)-1 , " = ", bold(res))

    cat("\nNow try by your own! :D\n")
    cat("\nUse variance(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)

  mean <- mean_(x)
  suma <- 0
  for(i in 1:length(x)) {
    suma <- suma + (x[i] - mean)^2
  }
  res <- (suma/(length(x)-1))
  return(res)
}
