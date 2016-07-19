ngram_tst <- function(file_name, num_lines) {

  library(ngram);
  library(tm);

  source('my_read_lines_cold.R');


  le_lines = my_read_lines_cold(file_name, num_lines);
  le_string = concatenate(le_lines, collapse = ' ', rm.space = FALSE);
  #
  strlen = nchar(le_string);
  #
  le_bigram = ngram(le_string, n = 2, sep = ' ');
  #
  # # ngram-print(lebigram, output='summary')
  # # ngram-print(lebigram, output='full')
  # # ngram-print(lebigram, output='truncated')
  #
  
  # le_grams = get.ngrams(le_bigram);
  le_table = get.phrasetable(le_bigram);
  #
  # # print(names(le_table));
  # # le_table$ngrams[1:100];
  # # le_table$freq[1:100];
  #
  thes =  grepl('the', le_table$ngrams);
  thes = grepl('^the', le_table$ngrams);
  #
  # return(strlen);
  # return(le_table);

  # return(thes);
  return(le_table);
}
