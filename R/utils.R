###############################
#####-AUXILIARY FUNCTIONS-#####
###############################

#' Draw Vector Function
#'
#' This function prints all the elements of a vector
#'
#' @param buffer A vector of elements to be printed
#' @return No return value, prints to screen
#' @examples
#' \dontrun{
#' data <- c(1:12)
#' drawVector(data)
#' }
#' @export
drawVector <- function(buffer){
  for(i in 1:length(buffer)){
    if(i == length(buffer)){
      cat(buffer[i])
    } else {
      cat(buffer[i], ",")
    }
  }
}

#' Get User Action Function
#'
#' This function gets the buffer introduced by the user. Typically a numerical vector.
#'
#' @return A vector entered by the user
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' vector <- getUserAction()
#' }
#' @export
getUserAction <- function(){
  #init loop variables
  resp <- "n"

  while(resp == "n"){
    cat("Enter natural numbers separated by a space and press enter at the end\n")
    buffer_aux <- readline(prompt = "")
    buffer <- suppressWarnings(
      na.omit(as.numeric(unlist(strsplit(buffer_aux, " "))))
    )
    if(length(buffer) == 0) {
      cat("\nNo valid numbers found. Please try again.\n")
      next
    }
    cat("\nYou summited the following : ")
    drawVector(buffer)
    cat("\nAre the data correct? any/n")
    resp <- readline(prompt = "")

  }
  return(buffer)
}


#' Initialize Images Function
#'
#' This function displays an image from the package resources.
#'
#' @param image_name Name of the image file in inst/images
#' @return No return value
#' @importFrom magick image_read
#' @export
initImages <- function(image_name) {
  # Get the path to the package's image directory
  img_path <- system.file("images", image_name, package = "UAHDataScienceSF")
  if (img_path == "") {
    warning("Image not found in package resources")
    return(invisible(NULL))
  }
  # Display the image
  img <- image_read(img_path)
  plot(img)
}
