combine_matrices <- function (num_iters = 999, freq_thr = 5, out_file_name = 'ordered_bigrams.csv') {

    result = data.frame();

    for (k in 1: num_iters) {

        print(sprintf("ITERATION : %d ", k));

        fname = sprintf('ngram_matrix_iter_%d.data', k);
        if (file.exists(fname)) {
            load(fname);

            filtered = filter_matrix(proc_result, freq_thr);
            result = rbind(result, filtered);

            rm(proc_result);
            file.remove(fname);
        } else {
            print("ALL DONE, RAN OUT OF FILES!");

            result = finalize_and_save(result, out_file_name);
            return(result);
        }
    }

    print("ALL DONE, RAN OUT OF ITERATIONS!")

    result = finalize_and_save(result, out_file_name);
    return(result);
}


filter_matrix <- function (input, freq_thr) {
    result = input[ which(input$freq >= freq_thr), ];
    return(result);
}


finalize_and_save <- function(input, out_name) {
    result = input[ order(input$ngrams, decreasing = FALSE) ,];

    write.table(result, out_name, sep = ';');

    return(result);
}
