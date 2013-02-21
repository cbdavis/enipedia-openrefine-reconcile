########## String Matching ##########

#Look at length of intersection and union of tokens in two strings to determine similarity
#http://en.wikipedia.org/wiki/Jaccard_index
jaccard_index <- function(text1, text2){
  set1 = tokenize(text1)
  set2 = tokenize(text2)
  jaccard_index = length(intersect(set1, set2)) / length(union(set1, set2))
  return(jaccard_index)
}
