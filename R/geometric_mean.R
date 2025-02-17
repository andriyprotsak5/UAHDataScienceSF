######################
####GEOMETRIC MEAN-###
######################

#' Geometric Mean Function
#'
#' This function calculates the geometric mean of a numeric vector.
#' Can be used in three modes: simple calculation, learning mode with step-by-step
#' explanation, or interactive mode for practice.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The geometric mean of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#' @examples
#' data <- c(5,21,12,7,3,9,1)
#' # Simple calculation
#' geometric_mean(data)
#'
#' # Learning mode
#' geometric_mean(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' geometric_mean(interactive = TRUE)
#' }
#'
#' @export
geometric_mean <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  if (interactive) {
    initImages("geometric-mean.jpg")
    cont <- 0

    buffer = getUserAction()

    #mean result
    res <- geometric_mean(buffer)
    message("\nOK! Next Move !!\n")
    #flag for correct answer -> 1 = NO_OK, 0 = OK
    flag <- 1

    #checking loop
    while(flag == 1) {
      message("Please, insert the result of the geometric mean calculus for your data : ")
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

  if (learn) {
    x <- as.vector(x)
    producto <- 1
    message(bold("\n__GEOMETRIC MEAN CALCULUS__ \n"))
    message("\nThe geometric mean of a dataset is calculated by multiplying each element of the dataset and raising the result to 1 divided by the number of elements in the dataset (the nth root).
           We'll give the user an example for better comprension.\n")
    message(green("\nFormula -> (x1 * x2 *..* xn)^( 1 / num_elements)\n"))
    message(green("xn: valor of elements to dataset\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    message("\nThe content of the vector is: ")
    for(i in 1:length(x)) {
      if( i == length(x)) {
        message(x[i])
        producto <- producto * x[i]
      } else {
        message(x[i], ",")
        producto <- producto * x[i]
      }
    }
    res <- ((producto)^( 1 / length(x)))
    message("\n")
    message("Now we need to multiply each element of the vector/dataset\n")
    message("The product of the elements is: ", blue(producto), "\n")
    message("\nNext step, get the number of elements that we've examined")
    message("\nThe length of the vector is ", blue(length(x)), "elements\n")
    message("\nFormula applied -> (", blue(producto), ") ^ ( 1 /", blue(length(x)) , ") = ", bold(res))
    message("\nNow try by your own! :D\n")
    message("\nUse geometric_mean(interactive = TRUE) function to practice.\n")
    return(res)
  }

  x <- as.vector(x)
  producto <- 1
  for(i in 1:length(x)) {
    producto <- producto * x[i]
  }
  res <- (producto)^(1/length(x))
  return(res)
}
