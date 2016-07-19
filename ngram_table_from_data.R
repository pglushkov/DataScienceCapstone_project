ngram_table_from_data <- function(vector_data, ngram_order = 3) {

  library(ngram);

  le_string = concatenate(vector_data, collapse = ' ', rm.space = FALSE);
  strlen = nchar(le_string);
  le_bigram = ngram(le_string, n = ngram_order, sep = ' ');
  
  # le_grams = get.ngrams(le_bigram);
  le_table = get.phrasetable(le_bigram);
  thes =  grepl('the', le_table$ngrams);
  thes = grepl('^the', le_table$ngrams);

  return(le_table);
}
