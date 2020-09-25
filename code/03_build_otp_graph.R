build_otp_graph <- function(router = "rio", mem = 5, command = "java") {
  
  valid_java_version <- opentripplanner::otp_check_java()
  
  if (valid_java_version) opentripplanner::otp_build_graph(otp = "./otp/otp.jar", dir = "./otp", router = "rio", memory = n * 1024)
  
  else message("Please update your version of Java")
  
}