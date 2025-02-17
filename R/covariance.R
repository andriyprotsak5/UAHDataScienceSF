###########################
###     COVARIANCE      ###
###########################

#' Covariance Function
#'
#' This function calculates the covariance between two vectors of numbers.
#'
#' @param x Optional first numeric vector (not needed for interactive mode)
#' @param y Optional second numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The covariance between the two vectors (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#'
#' @examples
#' data <- c(10,4,5,7,3,4,1)
#' data2 <- c(1,8,3,4,4,5,7)
#'
#' # Simple calculation
#' covariance(data, data2)
#'
#' # Learning mode
#' covariance(data, data2, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' covariance(interactive = TRUE)
#' }
#'
#' @export
covariance <- function(x = NULL, y = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(y))) {
    stop("both data vectors are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("covariance.jpg")
    cont_aux <- 0

    message("\nInsert your first data set:\n")
    x = getUserAction()

    message("\nInsert your second data set:\n")
    y = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the covariance calculus for your data (if the result has decimal part, round to the 3rd): ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(covariance(x,y),3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 1 ) {
          message(yellow("\nHint 2 -> The covariance of a dataset is calculated by product of sum of elements of x minus the mean's x and sum elements of y minus the mean's y. All of then divide by size of anyone dataset.\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    x <- as.vector(x)

    message(bold("\n__COVARIANCE CALCULUS__ \n"))
    message("\nThe covariance of two datasets is calculated by multiplying the differences between each element and its mean, summing these products, and dividing by the number of elements.\n")
    message(green("\nFormula ->  Summation((x - mean_x)*(y - mean_y)) / n\n"))
    message(green("\nMean -> (x1 + x2 +..+xn) / n\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
    message("\nThe contents of the vectors are: ")

    drawVector(x)
    drawVector(y)

    meanx <- mean_(x)
    meany <- mean_(y)
    message("\nThe mean of x dataset is...", blue(meanx))
    message("\nThe mean of y dataset is...", blue(meany))
    message("\nThe products of differences from means: ")
    sum <- 0
    for(i in 1:length(x)) {
      prod <- (x[i] - meanx) * (y[i] - meany)
      if(i == length(x)) {
        message(prod)
        sum <- sum + prod
      } else {
        message(prod, ",")
        sum <- sum + prod
      }
    }

    res <- sum/length(x)
    message("\nNow we need to add all these products\n")
    message("The sum of the products is: ", blue(sum), "\n")
    message("\nNext step, get the number of elements that we've examined")
    message("\nThe length of the vectors is ", blue(length(x)), "elements\n")
    message("\nFormula applied -> ", sum, "/", length(x), " = ", bold(res))

    message("\nNow try by your own! :D\n")
    message("\nUse covariance(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)
  y <- as.vector(y)

  meanx <- mean_(x)
  meany <- mean_(y)

  sum <- 0

  for(i in 1:length(x)) {
    sum <- sum + ((x[i] - meanx)*(y[i] - meany))
  }

  res <- (sum/length(x))
  return(res)
}
