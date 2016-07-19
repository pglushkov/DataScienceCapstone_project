proc_piece_of_table <- function(le_table) {

    cur_idx = 1;
    numrows = nrow(le_table);

    result = data.frame();

    #names(le_table) <- c('IDX', 'ngrams', 'freq', 'SOME_VALUE');

    DUPL = !duplicated(le_table$ngrams);
    IDXS = which(DUPL);

    idxslen = length(IDXS);

    for ( k in  1:(idxslen - 1) ) {

        st_idx = IDXS[k];
        end_idx = IDXS[k + 1];

        num = end_idx - st_idx - 1;

        tmp_entry = le_table[st_idx,];
        tmp_freq = sum(le_table$freq[st_idx:st_idx + num]);
        tmp_entry$freq = tmp_freq;

        result = rbind(result, tmp_entry);
    }

    if (IDXS[idxslen] < numrows) {

        st_idx = IDXS[idxslen];
        end_idx = numrows;

        num = end_idx - st_idx - 1;

        tmp_entry = le_table[st_idx,];
        tmp_freq = sum(le_table$freq[st_idx:st_idx + num]);
        tmp_entry$freq = tmp_freq;

        result = rbind(result, tmp_entry);
    }

    return(result);
}
