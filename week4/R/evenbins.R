evenbins <- function(x, bin.count=10, order=T) {
  bin.size <- rep(length(x) %/% bin.count, bin.count)
  bin.size <- bin.size + ifelse(1:bin.count <= length(x) %% bin.count, 1, 0)
  bin <- rep(1:bin.count, bin.size)
  if(order) {    
    bin <- bin[rank(x,ties.method="random")]
  }
  return(factor(bin, levels=1:bin.count, ordered=order))
}