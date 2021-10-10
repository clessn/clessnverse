#################################################################################################################
#################################################################################################################
############################################# Analysis Functions ################################################
#################################################################################################################
#################################################################################################################

#### 1. Geometry ####
#### ~1.1 Eucledian distance ####
get_EuDistance <- function(point1,point2){
  if (length(point1) > 3 | length(point2) > 3) {
    stop("Points must come from a 2D or 3D space. \n Point 1 has ",length(point1),
         " dimensions and point 2 has ",length(point2)," dimensions.")
  }
  else if (length(point1) != length(point2)) {
    stop("Points must have the same number of dimensions. \n Point 1 has ",length(point1),
         " dimensions and point 2 has ",length(point2)," dimensions.")
  }
  else if (length(point1) == 0) {
    stop("Point 1 has length 0.")
  }
  else if (length(point2) == 0) {
    stop("Point 2 has length 0.")
  }
  else if (length(point1) == 3) {
    euDistance <- sqrt((point2[1]-point1[1])^2 + (point2[2]-point1[2])^2 + (point2[3]-point1[3])^2)
  }
  else if (length(point1) == 2) {
    euDistance <- sqrt((point2[1]-point1[1])^2 + (point2[2]-point1[2])^2)
  }
  return(euDistance)
}