

## Where to place round objects to fit within a larger object


# y+ downward

generate_spaces <- function(size = 4.5,
  edge = 0.1,
  sm_size = 0.5,
  amt = 5,
  start_height = 2.09,
  round=TRUE)
  {

  fill_space <- size - 2*edge - sm_size
  sieve <- fill_space - (amt*sm_size)
  print(paste("sieve is: ", sieve)) #
  inc <- sieve / (amt-1)
  print(paste("inc is: ", inc)) #

  j <- start_height+edge
  res_j <- c(j)

  for (i in c(1:(amt-1))){
    # print(i) #
    print(paste("J_before is: ",j)) #
    j <- j + 0.5 + inc
    print(paste("J_after is: ",j)) #
    res_j <- c(res_j,j)
  }

  if (round==TRUE){
    res_j <- round(res_j,2)
  }
  return(res_j)
}

