create_ngram_model <- function(grams = 2, in_freq_thr = 5, files) {

    library(stringr);

    source('make_tables_from_data.R');
    source('combine_matrices.R');
    source('merge_sorted_ngram_by_parts.R');

    COUNT1 = 50000;
    COUNT2 = 30000;

    resulting_files = str_replace(files, '.txt', '.csv');

    if(grams != 2 && grams != 3) {
        print("ERROR! For now only 2- and 3-gram models are supported");
        return(-1);
    }

    print(" STARTING ... First thing - accumulate all grams in one massive data.frame")
    ordered_grams_final = data.frame();
    for (k in 1:length(files)) {

        print(sprintf('WORKING ON INPUT DATA : %s ', files[k]));
        print('Creating n-gram tables from input data ...');
        make_tables_from_data(files[k], COUNT1, grams);

        print('Filtering n-gram tables and combining them into one ordred subset ...');
        tmp = combine_matrices(num_iters = 999, freq_thr = in_freq_thr, out_file_name = resulting_files[k]);

        file.remove(resulting_files[k]);

        print('Accumulating subsets into one uber-data.frame ...');
        ordered_grams_final = rbind(ordered_grams_final, tmp);
    }

    print('ordering uber-data.frame ...')
    ordered_grams_final = ordered_grams_final[order(ordered_grams_final$ngrams, decreasing = FALSE),];
    print('saving ordered uber-data.frame')
    write.table(ordered_grams_final, 'ordered_uber_frame.csv', sep = ';')

    print('Removing repetitions from the ordred ordered uber-data.frame ...');
    ordered_ngrams_final = merge_sorted_ngram_by_parts('ordered_uber_frame.csv', part_size = COUNT2);
    file.remove('ordered_uber_frame.csv');

    print('finally, saving ordered uber-frame with all repititions removed ...');
    final_name = sprintf('merged_%dgram.csv', grams);
    write.table(ordered_ngrams_final, final_name, sep = ';')

    return(0);
}
