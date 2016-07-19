merge_sorted_ngram_by_parts <- function(filename, part_size = 100000) {

    source('proc_piece_of_table.R')

    le_table = read.table(filename, sep = ';', stringsAsFactors = FALSE);
    #le_table$ngrams = as.character(le_table$ngrams);
    numrows = nrow(le_table);

    iter_num = 1;
    DO = TRUE;

    result = data.frame();

    while (DO) {
        st_idx = (iter_num - 1)*part_size + 1;
        end_idx = iter_num * part_size;

        if (end_idx > numrows) {
            DO = FALSE;
            end_idx = numrows;
            print("FINAL ITERATION!")
        }

        print(sprintf("Iteration %d, rows from %d to %d ", iter_num, st_idx, end_idx));

        piece = le_table[ st_idx : end_idx, ];
        piece1 = proc_piece_of_table(piece);
        print(sprintf("       done sorting ... "));

        result = rbind(result, piece1);
        print(sprintf("       done merging ... "));

        iter_num = iter_num + 1;
    }

    return(result);
}
