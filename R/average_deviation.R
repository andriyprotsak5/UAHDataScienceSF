#########################
### AVERAGE DEVIATION ###
#########################

#' Average Absolute Deviation Function
#'
#' This function calculates the average absolute deviation of a numbers vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The average absolute deviation of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#'
#' @examples
#' data <- c(7,2,5,7,1,4,12)
#'
#' # Simple calculation
#' average_deviation(data)
#'
#' # Learning mode
#' average_deviation(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' average_deviation(interactive = TRUE)
#' }
#'
#' @export
average_deviation <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("average_deviation.jpg")
    cont_aux <- 0

    message("\nInsert your data set:\n")
    buffer = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the average deviation calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(average_deviation(buffer),3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          message(yellow("\nHint 2 -> Check the function 'explain.averageDeviation'\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    message(bold("\n__AVERAGE DEVIATION CALCULUS__ \n"))
    message("\nThe average deviation of a dataset is calculated by adding the absolute value of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
    message(green("\nFormula ->  (Summation(abs(each_element - mean))) / num_elements\n"))
    message(green("\nMean -> (x1 + x2 +..+xn) / num_elements\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    message("\nThe content of the vector is: ")

    drawVector(x)

    mean <- mean_(x)
    message("\nThe mean of dataset is...", blue(mean))
    message("\nThe absolute value of the diference between each number and the mean of dataset is: ")
    suma <- 0
    for(i in 1:length(x)) {
      absolute <- abs(x[i] - mean)
      if( i == length(x)) {
        message(absolute)
        suma <- suma + absolute
      } else {
        message(absolute, ",")
        suma <- suma + absolute
      }
    }
    res <- (suma/length(x))
    message("\nNow we need to add each element of the vector/dataset\n")
    message("The sum of the squares is: ", blue(suma), "\n")
    message("\nNext step, get the number of elements that we've examined")
    message("\nThe length of the vector is ", blue(length(x)), "elements\n")
    message("\nFormula applied -> ", suma, "/", length(x) , " = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse average_deviation(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)

  mean = mean_(x)
  suma <- 0
  for(i in 1:length(x)) {
    suma <- suma + abs(x[i] - mean)
  }
  res <- (suma/length(x))
  return(res)
}
