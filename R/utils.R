# Load gradients and add metadata 
load_gradient <- function(file, gradient_name) {
  gradient <- fread(file) 
  setnames(gradient, glasser$V1)
  gradient$group <- group
  gradient$age <- age
  gradient$sex <- sex
  gradient[, subject := .I]
  gradient$gradient <- gradient_name 
  return(gradient)
}