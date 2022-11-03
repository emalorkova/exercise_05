dX_A <- c(2, 3, 5, 10)
dX_B <- c(3, 7, 10)
dX_AB <- c(1, 2, 2, 5, 5, 5)
molecule_size <- sum(dX_A)

map_of_pos_A <- c(2, 3, 10, 5) #sample(dX_A)
map_of_pos_B <- c(3, 7, 10) #sample(dX_B)
coordinates_A <- c(0, cumsum(map_of_pos_A))
coordinates_B <- c(0, cumsum(map_of_pos_B))
interleave_pos <- sort(union(coordinates_A, coordinates_B),decreasing=FALSE)
diff_between_pos <- diff(interleave_pos)
sorted_diff <- sort(diff_between_pos, decreasing=FALSE)
identical_map = identical(dX_AB, sorted_diff)
restriction_pos_A <- coordinates_A[2:(length(coordinates_A)-1)]
restriction_pos_B <- coordinates_B[2:(length(coordinates_B)-1)]


DDP <- function(dX_A, dX_B, dX_AB) {
  molecule_size <- sum(dX_A)
  restriction_pos_A <- c()
  restriction_pos_B <- c()
  # nahodne zoradenie fragment sizes - nie je brute force
  map_of_pos_A <- sample(dX_A)
  map_of_pos_B <- sample(dX_B)
  # coordinates - pozicie 
  coordinates_A <- c(0, cumsum(map_of_pos_A))
  coordinates_B <- c(0, cumsum(map_of_pos_B))
  # zmergovanie pozíc
  interleave_pos <- sort(union(coordinates_A, coordinates_B),decreasing=FALSE)
  # rozdiel medzi pozíciami
  diff_between_pos <- diff(interleave_pos)
  # zoradenie pozíc vzostupne
  sorted_diff <- sort(diff_between_pos, decreasing=FALSE)
  # porovnanie s dX_AB - ak sú zhodné, mám restriction pozicie
  # ak nie su zhodne, opakujem hladanie dalej s inymi map of pos
  identical_map = identical(dX_AB, sorted_diff)
  
  if (identical_map == TRUE) {
    restriction_pos_A <- coordinates_A[2:(length(coordinates_A)-1)]
    restriction_pos_B <- coordinates_B[2:(length(coordinates_B)-1)]
  } else {
    return(DDP(dX_A, dX_B, dX_AB))
  }
  print(restriction_pos_A)
  print(restriction_pos_B)
}

DDP(c(2, 3, 5, 10), c(3, 7, 10), c(1, 2, 2, 5, 5, 5))
