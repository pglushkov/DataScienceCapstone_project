make_tables_from_data <- function(file_name, str_per_iter = 100000, grams = 2) {

  source('ngram_table_from_data.R');

  con = file(file_name, 'rt');

  DO = TRUE;
  iter_num = 1;
  while (DO) {
    result = readLines(con, str_per_iter)

    if(length(result) == 0) {
      print("ALL SEEMS DONE!");
      DO = FALSE;
      break;
    }

    print(sprintf("WORKING ON ITERATION : %d", iter_num));
    proc_result = ngram_table_from_data(result, grams);

    out_file_name = sprintf('ngram_matrix_iter_%d.data', iter_num);
    save(proc_result, file = out_file_name);

    iter_num = iter_num +1;
  }


  close(con);
  return(0);
}
