merge_sorted_ngram_table <- function(filename) {

    le_table = read.table(filename, sep = ';');

    cur_idx = 1;
    numrows = nrow(le_table);

    result = data.frame();

    #names(le_table) <- c('IDX', 'ngrams', 'freq', 'SOME_VALUE');

    while(cur_idx < numrows) {

        if (le_table$ngrams[cur_idx] == le_table$ngrams[cur_idx + 1]) {
            tmp_idx = cur_idx + 1;

            DO = TRUE;
            while(DO) {
                if ((le_table$ngrams[tmp_idx] != le_table$ngrams[tmp_idx + 1]) || tmp_idx == numrows)
                {
                    DO = FALSE;
                    break;
                }
                tmp_idx = tmp_idx + 1;
            }

            tmp_freq = sum(le_table$freq[cur_idx:tmp_idx]);
            tmp_entry = le_table[cur_idx,];
            tmp_entry$freq = tmp_freq;

            result = rbind(result, tmp_entry);

            # !!!!! VERY IMPORTANT !!!!!
            cur_idx = tmp_idx;
        } else {

            tmp_entry = le_table[cur_idx,];
            result = rbind(result, tmp_entry);
        }

        cur_idx = cur_idx + 1;
    }

    return(result);

}
