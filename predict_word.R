predict_word <- function(word, NUM_OUT = 5) {

    library(stringr)

    database = read.table('./BIGRAMS/final_merged.csv', sep = ';', stringsAsFactors = FALSE);

    rexp_exact = sprintf('^%s ', word);
    rexp_likely = sprintf('^%s', word);

    candidates = database[which(grepl(rexp_exact, database$ngrams)),];
    res_len = nrow(candidates);

    if (res_len == 0) {

        # could not find exact match - lets search for likely candidates
        candidates = database[which(grepl(rexp_likely, database$ngrams)),];
        res_len = nrow(candidates);

        return(form_output(candidates, NUM_OUT, FALSE));

    } else {
        return(form_output(candidates, NUM_OUT, TRUE));
    }

}

form_output <- function( cands, NUM_OUT, exact) {

    res_len = nrow(cands);

    if (res_len == 0) {
        return( list("", vector()) );
    } else {

        num_out = min(NUM_OUT, res_len);

        cands$freq <- as.numeric(cands$freq);
        cands = cbind(cands, strlen = 1/nchar(cands$ngrams));

        cands$ngrams = str_trim(cands$ngrams);

        cands = cands[ order(cands$freq, cands$strlen, decreasing = c(TRUE)) ,];
        cands = cands[1:num_out,];

        tmp1 = cands$ngrams[1:num_out];
        tmp2 = tmp1;

        for (k in 1:num_out) {
            tmp = strsplit(tmp1[k], ' ');
            tmp1[k] = tmp[[1]][1];
            tmp2[k] = tmp[[1]][2];
        }

        if (exact) {
            if (num_out == 1) {
                return( list(tmp2[1], "" ) );
            } else {
                return( list(tmp2[1], tmp2[2:num_out]) );
            }

        } else {
            return( list(" ", tmp1[1:num_out]) );
        }

    }
}
