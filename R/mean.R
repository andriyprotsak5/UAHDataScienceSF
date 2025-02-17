######################
########-MEAN-########
######################

#' Statistical Mean Function
#'
#' This function calculates the arithmetic mean of a numeric vector.
#' Can be used in three modes: simple calculation, learning mode with step-by-step
#' explanation, or interactive mode for practice.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The arithmetic mean of the vector
#' @importFrom crayon bold green blue yellow
#' @examples
#' # Simple calculation
#' data <- c(1,2,2,5,10,4,2)
#' mean_(data)
#'
#' # Learning mode
#' mean_(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' mean_(interactive = TRUE)
#' }
#'
#' @export
mean_ <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validar parámetros
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Modo interactivo
  if (interactive) {
    initImages("mean.png")
    cont <- 0
    message("\nInsert your data set:\n")
    buffer = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    result <- sum(buffer)/length(buffer)

    while(flag == 1){
      message("Please, insert the result of the mean calculus for your data : ")
      message("\nIf the number has decimals, round to the 3rd\n")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(result,3)){
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont <- cont + 1
        message("Ups, that might not be correct...")
        if(cont == 1){
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont > 2){
          message(yellow("\nHint 2 -> add each element of your vector and divide it by the length\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  if (learn) {
    suma <- 0
    message(bold("\n__MEAN CALCULUS__ \n"))
    message("\nThe mean of a dataset is calculated by the sum of the values divided by the number of values.\n")
    message(green("\nFormula -> (x1 + x2 +..+xn) / num_elements\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
    message("\nThe content of the vector is: ")
    for(i in 1:length(x)){
      if(i == length(x)){
        message(x[i])
        suma <- suma + x[i]
      } else {
        message(x[i], ",")
        suma <- suma + x[i]
      }
    }
    result <- suma/length(x)
    message("\n")
    message("Now we need to add each element of the vector/dataset\n")
    message("The sum of the elements is: ", blue(suma), "\n")
    message("\nNext step, get the number of elements that we've examined")
    message("\nThe length of the vector is ", blue(length(x)), "elements\n")
    message("\nFormula applied -> ", blue(suma), "/", blue(length(x)) , " = ", bold(result))
    return(result)
  }

  # Modo cálculo simple
  return(sum(x)/length(x))
}
