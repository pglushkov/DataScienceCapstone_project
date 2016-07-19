process_text_chunk2 <- function(input) {

  library(tm);
  library(stringr)

  source('my_read_lines_cold.R');
  source('corpus_to_vector.R');

  STOP_WORDS = stopwords('english');
  PROFANITY = my_read_lines_cold('profanity.txt');

  # le_source = VectorSource(input);
  # le_corpus <- Corpus(le_source);
  #
  # le_corpus <- tm_map(le_corpus, content_transformer(tolower))
  # le_corpus <- tm_map(le_corpus, removeNumbers);
  # le_corpus <- tm_map(le_corpus, removePunctuation);
  # le_corpus <- tm_map(le_corpus, stripWhitespace);
  # le_corpus <- tm_map(le_corpus, removeWords, stopwords('english'));
  # le_corpus <- tm_map(le_corpus, removeWords, profanity_list);
  #
  # le_result = corpus_to_vector(le_corpus);

  result = vector();
  for (k in 1 : length(input)) {
      result[k] = clean_the_string(input[k], STOP_WORDS, PROFANITY);
  }

  return(input);
}

clean_the_string <- function(input, STOP_WORDS, PROFANITY) {
    # convert to lower-case
    input = tolower(input);

    # removing stop words
    input = removeWords(input, STOP_WORDS);
    input = removeWords(input, PROFANITY);

    # removing all numbers and punctuation characters
    punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]1234567890”“’«»…';
    input <- gsub(punct, "", input);

    # removing spaces
    input = gsub('[ ]{2,}', " ", input);
    input = str_trim(input);

    return(input);
}
