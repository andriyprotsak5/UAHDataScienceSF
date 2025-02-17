######################
######-QUARTILES-#####
######################

#' Quartiles Function
#'
#' This function calculates the quartiles of a numeric vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The quartiles of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue red yellow italic
#'
#' @examples
#' data <- c(1,2,2,5,10,4,2)
#'
#' # Simple calculation
#' quartile(data)
#'
#' # Learning mode
#' quartile(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' quartile(interactive = TRUE)
#' }
#'
#' @export
quartile <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("quartile.png")
    cont_aux <- 0
    cont_aux_lim <- 0

    message("\nInsert your data set:\n")
    buffer = getUserAction()

    #show data sorted
    buffer = sort(buffer)
    message("\nData sorted : ")

    drawVector(buffer)

    vec <- sort(as.vector(buffer))
    size <- round((length(vec)+1)/2)
    mid1 <- vec[c(1:size)]
    mid2 <- vec[c(size:length(vec))]

    #quartiles calculus
    q1 <- median_(mid1)
    q2 <- median_(buffer)
    q3 <- median_(mid2)
    #element which represents the quartile limit
    q1_lim <- buffer[ceiling((1 * length(buffer)) / 4)]
    q2_lim <- buffer[ceiling((2 * length(buffer)) / 4)]
    q3_lim <- buffer[ceiling((3 * length(buffer)) / 4)]

    message("\nOK! Next Move !!\n")

    #QUARTILE 1
    flag_q1 <- 1
    while(flag_q1 == 1) {
      message("Please, insert the result of the Quartil 1 calculus for your data : ")
      message("\nIf the number has decimals, round to the 3rd\n")
      q1_resp <- as.numeric(readline(prompt = ""))
      if(q1_resp == q1) {
        flag_q1 <- 0
        message(italic("\nQuartile 1 correct!\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 2 ) {
          message(yellow("\nHint 2 -> (size/4)\n\n"))
        }
      }
    }

    #QUARTILE 2
    flag_q2 <- 1
    while(flag_q2 == 1) {
      message("Please, insert the result of the Quantil 2 calculus for your data : ")
      message("\nIf the number has decimals, round to the 3rd\n")
      q2_resp <- as.numeric(readline(prompt = ""))
      if(q2_resp == q2) {
        flag_q2 <- 0
        message(italic("\nQuartile 2 correct!\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 2 ) {
          message(yellow("\nHint 2 -> The 2th Quartile is the median\n\n"))
        }
      }
    }

    #QUARTILE 3
    flag_q3 <- 1
    while(flag_q3 == 1) {
      message("Please, insert the result of the Quantil 3 calculus for your data : ")
      message("\nIf the number has decimals, round to the 3rd\n")
      q3_resp <- as.numeric(readline(prompt = ""))
      if(q3_resp == q3) {
        flag_q3 <- 0
        message(italic("\nQuartile 3 correct!\n"))
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux > 2 ) {
          message(yellow("\nHint 2 -> (3 * size)/4\n\n"))
        }
      }
    }

    message("\n\nNow identify the number which represents the limit of each quartil\n\n")

    #QUARTILE 1 LIMIT
    flag_q1_lim <- 1
    while(flag_q1_lim == 1) {
      message("Please, insert the number which represents the Quartile 1 limit for your data : ")
      message("\n(remember your data) -> ", buffer , "\n")
      q1_resp <- as.numeric(readline(prompt = ""))
      if(q1_resp == q1_lim) {
        flag_q1_lim <- 0
        message(italic("\nQuartile 1 limit correct!\n"))
      } else {
        cont_aux_lim <- cont_aux_lim + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... round up the result of the 1st Quartile\n\n"))
        }
      }
    }

    #QUARTILE 2 LIMIT
    flag_q2_lim <- 1
    while(flag_q2_lim == 1) {
      message("Please, insert the number which represents the Quartile 2 limit for your data : ")
      message("\n(remember your data) -> ", buffer , "\n")
      q2_resp <- as.numeric(readline(prompt = ""))
      if(q2_resp == q2_lim) {
        flag_q2_lim <- 0
        message(italic("\nQuartile 2 limit correct!\n"))
      } else {
        cont_aux_lim <- cont_aux_lim + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... round up the result of the 2nd Quartile\n\n"))
        }
      }
    }

    #QUARTILE 3 LIMIT
    flag_q3_lim <- 1
    while(flag_q3_lim == 1) {
      message("Please, insert the number which represents the Quartile 3 limit for your data : ")
      message("\n(remember your data) -> ", buffer , "\n")
      q3_resp <- as.numeric(readline(prompt = ""))
      if(q3_resp == q3_lim) {
        flag_q3_lim <- 0
        message(italic("\nQuartile 3 limit correct!\n"))
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux_lim <- cont_aux_lim + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... round up the result of the 3rd Quartile\n\n"))
        }
      }
    }

    message(italic("\n\nWell done, you've got it!\n\n"))
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    message(bold("\n__QUARTILES CALCULUS__ \n"))
    message("\nThe quartile divides the dataset in 4 parts as equal as possible.\n")
    message(green("\nFormula -> First quartile (Q1) as the median of the first half of values. \n"))
    message(green("             Second quartile (Q2) as the median of the series itself.\n"))
    message(green("             Third quartile (Q3) as the median of the second half of values. \n"))
    message(bold("\n__Use Example__\n"))
    message("\nStep 1: The vector must be sorted.\n")

    drawVector(x)

    message("\n")
    message("\nStep 2: Calculated the quartiles \n")

    vec <- sort(as.vector(x))
    size <- round((length(vec)+1)/2)
    mid1 <- vec[c(1:size)]
    mid2 <- vec[c(size:length(vec))]

    #ceiling round up the value
    #Quartiles 1, 2 & 3
    q1 <- ceiling((1 * length(vec)) / 4)
    q2 <- ceiling((2 * length(vec)) / 4)
    q3 <- ceiling((3 * length(vec)) / 4)

    q1_ <- median_(mid1)
    message("\nQ1 -> (median ", mid1, ")  = ", q1_)
    q2_ <- median_(x)
    message("\nQ1 -> (median ", vec, ")  = ", q2_)
    q3_ <- median_(mid2)
    message("\nQ1 -> (median ", mid2, ")  = ", q3_)

    message("\n\nVisualization with colors:\n")

    #visualization with colors
    for(i in 1:q1) {
      message(vec[i], ",")
    }
    for(i in (q1+1):(q2)) {
      message(blue(vec[i], ","))
    }
    for(i in (q2+1):(q3)) {
      message(green(vec[i], ","))
    }
    for(i in (q3+1):length(vec)) {
      if(i == (length(vec))) {
        message(red(vec[i]), "\n")
      } else {
        message(red(vec[i], ","))
      }
    }
    #displaying results
    message("\nQ1 -> ", q1_)
    message(blue(" || Q2 -> ", q2_))
    message(green(" || Q3 -> ", q3_))
    message(red(" || Q4 -> onwards"))

    message("\nNow try by your own! :D\n")
    message("\nUse quartile(interactive = TRUE) function to practice.\n")

    res <- c(q1_,q2_,q3_)
    names(res) <- c("Q1","Q2","Q3")
    return(res)
  }

  # Simple calculation mode
  vec <- sort(as.vector(x))
  size <- round((length(vec)+1)/2)
  mid1 <- vec[c(1:size)]
  mid2 <- vec[c(size:length(vec))]

  q0 <- vec[c(1)]
  q1 <- median_(mid1)
  q2 <- median_(x)
  q3 <- median_(mid2)
  q4 <- vec[c(length(vec))]

  res <- c(q0,q1,q2,q3,q4)
  names(res) <- c("Q0","Q1","Q2","Q3","Q4")
  return(res)
}
