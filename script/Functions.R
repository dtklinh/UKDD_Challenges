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