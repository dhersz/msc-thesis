build_otp_graph <- function(n = 5) {
  
  valid_java_version <- opentripplanner::otp_check_java()
  
  if (valid_java_version) opentripplanner::otp_build_graph(otp = "./otp/otp.jar", dir = "./otp", router = "rio", memory = n * 1024)
  
  else message("Please update your version of Java")
  
}