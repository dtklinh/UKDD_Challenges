Check_maximal_repeat <- function(S, t){
  t_idx <- gregexpr2(pattern = t, text = S)[[1]]
  if(length(t_idx)<=1){
    return(FALSE)
  }
  if(!1 %in% t_idx){
    ## expand to the left
    t_left_expand <- substr(S, t_idx[1]-1, t_idx[1]-1 + nchar(t))
    tmp_idx <- gregexpr2(t_left_expand, S)[[1]]
    tmp_idx <- tmp_idx +1
    if(length(setdiff(t_idx, tmp_idx))==0){
      return(FALSE)
    }
  }
  ## can only expand to the right
  t_right_expand <- substr(S, t_idx[1], t_idx[1] + nchar(t))
  tmp_idx <- gregexpr2(t_right_expand, S)[[1]]
  if(length(setdiff(t_idx, tmp_idx))==0){
    return(FALSE)
  }else{return(TRUE)}
}

# debug(Check_maximal_repeat)
# gregexpr2("CG", "ACGTAAATCGTAT")

Diff_Multiset_generate <- function(X_input){
  X <- X_input[!duplicated(X_input)]
  if(length(X) != length(X_input)){
    print("coercing into a set")
  }
  N <- length(X)
  L <- c()
  for(i in 1:(N-1)){
    for (j in (i+1):N) {
      L <- c(L, abs(X[i]-X[j]))
    }
  }
  L <- sort(L)
  return(L)
}
##---------
## estimate original set from its corresponding difference multiset
Estimate_orignal <- function(L){
  ## L is multiset containing positive elements
  L <- sort(L, decreasing = T)
  N <- (1 + sqrt(8*length(L)+1))/2
  if(N != round(N)){
    stop("Number of elements in difference delta multiset is wrong!!!")
  }
  
  A <- zeros(n=N-1, m = N)
  A[,N] <- 1
  A[,1:(N-1)] <- -1*diag(N-1)
  B <- c(L[1])
  L_reduced <- L[2:length(L)]
  L_reduced <- L_reduced[!duplicated(L_reduced)]
  All_cmbn <- combn(L_reduced, N-2)
  Solutions <- c()
  iter_num <- 0
  max_iter_num <- 100
  for(i in 1:ncol(All_cmbn)){
    if(iter_num > max_iter_num){break}
    B <- c(L[1])
    B <- c(B, All_cmbn[,i]) %>% sort(decreasing = T)
    rest <- L[-match(B,L)] %>% sort(decreasing = T)
    B_delta <- Diff_Multiset_generate(B) %>% sort(decreasing = T)
    if(!all(rest == B_delta)){
      iter_num <- iter_num +1
      next
    }
    asvd = svd(A)
    adiag = diag(1/asvd$d)
    #adiag <- adiag *1
    solution = asvd$v %*% adiag %*% t(asvd$u) %*% B
    ## shift solution
    min_val <- min(solution)
    solution <- solution + (1-min_val)
    Solutions <- cbind(Solutions, solution)
    print(Solutions)
    iter_num <- iter_num +1
    ##
  }
  return(Solutions)
}
#---
X <- c(1,3,6,7,10,11)
L <- c(2,2,3,3,4,5,6,7,8,10)
solutions <- Estimate_orignal(L)
