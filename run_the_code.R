run_the_code <- function (filename, MAXSIZE = 100000)
{
    library(tm);
    library(LaF);

    # MAXSIZE = 100000;

    # raw_data = readLines(filename, MAXSIZE);
    raw_data = sample_lines(filename, MAXSIZE);

    le_source = VectorSource(raw_data);
    le_corpus <- Corpus(le_source);

    le_corpus <- tm_map(le_corpus, content_transformer(tolower))
    le_corpus <- tm_map(le_corpus, removeNumbers);
    le_corpus <- tm_map(le_corpus, removePunctuation);
    le_corpus <- tm_map(le_corpus, stripWhitespace);
    le_corpus <- tm_map(le_corpus, removeWords, stopwords('english'));

    le_dtm = DocumentTermMatrix(le_corpus);

    num_exclam = count_marks(raw_data, '!');
    num_quest = count_marks(raw_data, '\\?');
    num_elipsis = count_marks(raw_data, '\\.\\.\\.');

    lens = sapply(raw_data, nchar);
    names(lens) <- NULL;
    lens = sort(lens, decreasing = TRUE);
    print(lens[1:10]);
    print(sprintf('Mean length of an entry: %f',mean(lens)));

    print(sprintf('Number of terms with exclamation mark : %d', num_exclam));
    print(sprintf('Number of terms with question mark : %d', num_quest));
    print(sprintf('Number of terms with elipsis : %d', num_elipsis));

    PARAMS = data.frame(Num_elipsis = num_elipsis, Num_exclam = num_exclam, Num_quest = num_quest,
      Data_len = length(lens));

    # le_matrix = as.matrix(le_dtm);
   # freqs = colSums(le_matrix);

    # freqs = vector();
    # for ( c in 1 : ncol(le_dtm))
    # {
    #     capture.output(tmp <- (inspect(RES[,c]))) -> .null;
    #     freqs[c] =  sum(tmp);
    # }

    freqs = as.vector(slam::col_sums(le_dtm));
    names(freqs) <- as.vector(le_dtm$dimnames[[2]]);

    freqs <- sort(freqs, decreasing=TRUE);
    print(freqs[1:10]);

    # return(le_dtm);
    # return(termFreq(le_corpus));
    return(list(le_corpus, le_dtm, freqs, lens, PARAMS));
}

count_marks <- function (input, mark)
{
    num = sum(grepl(mark, input));
    return(num)
}
