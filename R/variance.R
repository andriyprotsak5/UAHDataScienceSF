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
#' if(interactive()){
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

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(variance(buffer),3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          message(yellow("\nHint 2 -> The variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    message(bold("\n__VARIANCE CALCULUS__ \n"))
    message("\nThe variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
    message(green("\nFormula ->  (Summation(each_element - Mean)^2) / num_elements\n"))
    message(green("\nMean -> (x1 + x2 +..+xn) / n\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    message("\nThe content of the vector is: ")

    drawVector(x)

    mean <- mean_(x)
    message("\nThe mean of dataset is...", blue(mean))
    message("\nThe square of the diference between each number and the mean of dataset is: ")
    suma <- 0
    for(i in 1:length(x)) {
      square <- ((x[i] - mean) ^ 2)
      if( i == length(x)) {
        message(square)
        suma <- suma + square
      } else {
        message(square, ",")
        suma <- suma + square
      }
    }
    res <- (suma/(length(x)-1))
    message("\nNow we need to add each element of the vector/dataset\n")
    message("The sum of the squares is: ", blue(suma), "\n")
    message("\nNext step, get the number of elements that we've examined")
    message("\nThe length of the vector is ", blue(length(x)), "elements\n")
    message("\nFormula applied -> ", suma, "/", length(x)-1 , " = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse variance(interactive = TRUE) function to practice.\n")
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
