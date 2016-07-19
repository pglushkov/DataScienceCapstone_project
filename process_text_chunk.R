process_text_chunk <- function(input) {

  library(tm);

  source('my_read_lines_cold.R');
  source('corpus_to_vector.R');

  profanity_list = my_read_lines_cold('profanity.txt');
  profanity_list = gsub('[^A-Za-z0-9 ]+', '', profanity_list)
  
  stop_words = stopwords('english');
  stop_words = gsub('[^A-Za-z0-9 ]+', '', stop_words)

  le_source = VectorSource(input);
  le_corpus <- Corpus(le_source);

  #le_corpus <- tm_map(le_corpus, content_transformer(tolower))
  le_corpus <- tm_map(le_corpus, removeNumbers);
  #le_corpus <- tm_map(le_corpus, removePunctuation);
  le_corpus <- tm_map(le_corpus, stripWhitespace);
  le_corpus <- tm_map(le_corpus, removeWords, stopwords('english'));
  le_corpus <- tm_map(le_corpus, removeWords, profanity_list);

  le_result = corpus_to_vector(le_corpus);
  return(le_result);
}
