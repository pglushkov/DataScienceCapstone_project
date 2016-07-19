corpus_to_vector <- function(le_corpus) {

  res = vector();

  for (k in 1:length(le_corpus)){
      res[k] = le_corpus[[k]]$content;
  }

  return(res);
}
