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
###---------
Estimate_orignal_v02 <- function(L){
  L <- sort(L, decreasing = T)
  N <- (1 + sqrt(8*length(L)+1))/2
  if(N != round(N)){
    stop("Number of elements in difference delta multiset is wrong!!!")
  }
  
  A <- zeros(n=N-1, m = N)
  A[,N] <- 1
  A[,1:(N-1)] <- -1*diag(N-1)
  B <- c(L[1])
  All_pos <- list(L[1])
  ## instead of list of all combinations, add one at a time
  L_reduced <- L[2:length(L)]
  for(i in 2:(N-1)){
    ## rank of ith element 
    candidates <- L[2:((i-1)*N-i*(i-1)/2 +1)]
    for(c in candidates){
      for(item in All_pos){
        if(min(item) > c){
          
        }
      }
    }
    
  }
  
}

##### test------------------
# Compute the upper bound index for each pick i (1-indexed)
upper_bound <- function(i, N) {
  (i - 1) * N - i * (i - 1) / 2 + 1
}
# check if a vec is fitted with L
check_validity <- function(L, vec){
  #vec <- vec %>% sort(decreasing = T)
  #return(grepl(paste(Diff_Multiset_generate(vec) %>% sort(decreasing = T),collapse=";"),paste(L_reduced,collapse=";"))>0)
  A <- Diff_Multiset_generate(vec) %>% sort(decreasing = T)
  B <- L[-match(vec,L)]
  freq_A <- table(A)
  freq_B <- table(B)
  #print(freq_A)
  #print(freq_B)
  
  # All elements of A must exist in B with sufficient count
  all(
    sapply(names(freq_A), function(el) {
      (freq_B[el] %in% NA) == FALSE &&   # element exists in B
        freq_A[el] <= freq_B[el]            # B has at least as many copies
    })
  )
}
pick_elements <- function(L, N) {
  
  # Recursive backtracking
  # i       : current pick (1 to N-1)
  # min_idx : picked index must be > last picked index (to stay strictly decreasing in value)
  backtrack <- function(i, min_idx, picked_vals) {
    
    if (i > N - 1) return(list())  # All N-1 elements picked successfully
    #if (i > N - 1) return(picked_vals)
    
    ub <- upper_bound(i,N)           # Allowed range: L[1:ub], but must be > min_idx
    
    # Valid indices for this pick: must be in [1, ub] AND > min_idx
    valid_indices <- seq(min_idx + 1, ub)
    
    if (length(valid_indices) == 0) return(NULL)  # Dead end
    
    # Randomly shuffle to get a random valid solution (not always the first)
    for (idx in sample(valid_indices)) {
      ## check 
      candidate_vals <- c(picked_vals, L[idx])
      if(length(candidate_vals) >1){
        if(length(candidate_vals) != length(candidate_vals[!duplicated(candidate_vals)])) next
        if (!check_validity(L, candidate_vals)) next
        
      }
      
      result <- backtrack(i + 1, idx, candidate_vals)
      if (!is.null(result)) {
        return(c(list(list(value = L[idx], index = idx)), result))
      }
    }
    
    return(NULL)  # No valid pick found
  }
  
  solution <- backtrack(1, 0, c())
  
  if (is.null(solution)) {
    message("No valid selection found.")
    return(NULL)
  }
  
  # Extract values and indices
  values  <- sapply(solution, `[[`, "value")
  indices <- sapply(solution, `[[`, "index")
  
  list(values = values, indices = indices)
}
###---------------------------
pick_all_elements <- function(L, N) {
  
  all_solutions <- list()
  # Recursive backtracking
  # i       : current pick (1 to N-1)
  # min_idx : picked index must be > last picked index (to stay strictly decreasing in value)
  backtrack <- function(i, min_idx, picked_vals) {
    
    if (i > N - 1){
      all_solutions[[length(all_solutions) + 1]] <<- picked_vals
      return(invisible(NULL))
    } ##return(list())  # All N-1 elements picked successfully
    #if (i > N - 1) return(picked_vals)
    
    ub <- upper_bound(i,N)           # Allowed range: L[1:ub], but must be > min_idx
    
    # Valid indices for this pick: must be in [1, ub] AND > min_idx
    valid_indices <- seq(min_idx + 1, ub)
    
    if (length(valid_indices) == 0) return(NULL)  # Dead end
    
    # Randomly shuffle to get a random valid solution (not always the first)
    for (idx in seq(min_idx + 1, ub)) {
      ## check 
      candidate_vals <- c(picked_vals, L[idx])
      if(length(candidate_vals) >1){
        if(length(candidate_vals) != length(candidate_vals[!duplicated(candidate_vals)])) next
        if (!check_validity(L, candidate_vals)) next
        
      }
      
      backtrack(i + 1, idx, candidate_vals)
      # if (length(all_solutions) == 0) {
      #   message("No valid selection found.")
      #   return(NULL)
      # }
      # if (!is.null(result)) {
      #   return(c(list(list(value = L[idx], index = idx)), result))
      # }
    }
    
    #return(NULL)  # No valid pick found
  }
  
  backtrack(1, 0, c())
  
  if (length(all_solutions) == 0) {
    message("No valid selection found.")
    return(NULL)
  }
  
  # Return as a tidy list of named vectors
  lapply(all_solutions, function(vals) {
    setNames(vals, paste0("x_", seq_along(vals)))
  })
}
##----------------------------

pick_elements_V00 <- function(L, N) {
  
  # Compute the upper bound index for each pick i (1-indexed)
  # upper_bound <- function(i) {
  #   (i - 1) * N - i * (i - 1) / 2 + 1
  # }
  
  # Recursive backtracking
  # i       : current pick (1 to N-1)
  # min_idx : picked index must be > last picked index (to stay strictly decreasing in value)
  backtrack <- function(i, min_idx) {
    
    if (i > N - 1) return(list())  # All N-1 elements picked successfully
    
    ub <- upper_bound(i)           # Allowed range: L[1:ub], but must be > min_idx
    
    # Valid indices for this pick: must be in [1, ub] AND > min_idx
    valid_indices <- seq(min_idx + 1, ub)
    
    if (length(valid_indices) == 0) return(NULL)  # Dead end
    
    # Randomly shuffle to get a random valid solution (not always the first)
    for (idx in sample(valid_indices)) {
      result <- backtrack(i + 1, idx)
      if (!is.null(result)) {
        return(c(list(list(value = L[idx], index = idx)), result))
      }
    }
    
    return(NULL)  # No valid pick found
  }
  
  solution <- backtrack(1, 0)
  
  if (is.null(solution)) {
    message("No valid selection found.")
    return(NULL)
  }
  
  # Extract values and indices
  values  <- sapply(solution, `[[`, "value")
  indices <- sapply(solution, `[[`, "index")
  
  list(values = values, indices = indices)
}

###-----------


# set.seed(42)
# N <- 5
# L_length <- N * (N - 1) / 2    # = 15
# L <- sort(sample(100, L_length), decreasing = TRUE)
X <- c(1,4,5,14,19,20,25)
L <- Diff_Multiset_generate(X)
L <- sort(L, decreasing = T)
# cat("L =", L, "\n\n")
# 
result <- pick_elements(L, N = 7)
# ##### end test -------------
# #---
# X <- c(1,3,6,7,10,11)
# L <- c(2,2,3,3,4,5,6,7,8,10)
# solutions <- Estimate_orignal(L)

## Position(function(x) identical(x, c(2,3,1) %>% sort()), xxx, nomatch = 0)
## grepl(paste(x,collapse=";"),paste(y2,collapse=";"))
