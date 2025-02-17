######################
########-MODE-########
######################

#' Mode Function
#'
#' This function calculates the mode of a numbers vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The mode of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#' @examples
#' data <- c(5,21,12,7,3,9,1)
#'
#' # Simple calculation
#' mode_(data)
#'
#' # Learning mode
#' mode_(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' mode_(interactive = TRUE)
#' }
#'
#' @export
mode_ <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    cont_aux <- 0

    cat("\nInsert your data set:\n")
    buffer = getUserAction()

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the mode calculus for your data : ")
      cat("\nIf the number has decimals, round to the 3rd\n")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(mode_(buffer),3)) {
        flag <- 0
        cat(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        cat("Ups, that might not be correct...")
        cat(yellow("\nHint -> Psst!... Take a closer look at the value most often\n\n"))
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    v <- as.vector(x)
    cat(bold("\n__MODE CALCULUS__ \n"))
    cat("\nThe mode of a dataset is calculated by looking for the most repeated value in the dataset. If in a group there are two or several scores with the same frequency and that frequency is the maximum, the distribution is bimodal or multimodal, that is, it has several modes.\n")
    cat(green("\nFormula -> Most repeated value of [Data]\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst step : search the most repeated value\n")
    cat("\nThe content of the vector is: ")

    drawVector(v)

    cat("\n")
    max <- 1
    mode <- v[0]
    for(i in 1:length(v)) {
      aux <- v[i]
      cont <- 0
      for(j in 1:length(v)) {
        if(aux == v[j]) {
          cont <- cont + 1
        }
      }
      if(cont > max) {
        mode <- aux
        max <- cont
      }
    }
    cat("Factor " , bold(mode) , " appears ", blue(max)," times in the vector.\n")

    cat("\nSecond step : check the dataset looking for a value with the same maximum frequency\n")
    cat("\nIf there are only 1 unique most repeated value, it is the mode.\n")
    cat("If there are 2 values repeated with the same maximum frequency each value represents the mode. Bimodal dataset\n")
    cat("If there are more than 2 values repeated with the same maximum frequency, it is a Multimodal dataset\n")

    cat("\nNow try by your own! :D\n")
    cat("\nUse mode_(interactive = TRUE) function to practice.\n")
    return(mode)
  }

  # Simple calculation mode
  v <- sort(as.vector(x))
  poliMod <- numeric()
  flag <- 1
  max <- 1
  mode <- v[0]
  for(i in 1:length(v)) {
    aux <- v[i]
    cont <- 0
    for(j in 1:length(v)) {
      if(aux == v[j]) {
        cont <- cont + 1
      }
    }
    if(cont > max) {
      mode <- aux
      max <- cont
    }
  }

  vect_frec = as.vector(table(v))
  v_aux = unique(sort(v))
  for(i in 1:length(vect_frec)) {
    if(vect_frec[i] == max) {
      poliMod = append(poliMod, v_aux[i])
    }
  }

  cat("Factor appears ",max," times in the vector.\n")

  if(length(poliMod) == 1) {
    cat("Unique mode ")
    return(mode)
  } else {
    cat("Multiples modes ")
    return(poliMod)
  }
}
