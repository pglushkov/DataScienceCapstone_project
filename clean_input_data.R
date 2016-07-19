clean_input_data <- function(file_name, str_per_iter = 100000) {

  source('process_text_chunk.R')
  source('process_text_chunk2.R')

  con = file(file_name, 'rt');
  out_con = file('tmp_output.txt', 'wt');

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

    result = tolower(result);
    result = gsub('[^A-Za-z0-9 ]+', '', result)

    print('    basic filtering done ...');

    proc_result = process_text_chunk(result);
    # proc_result = process_text_chunk2(result);

    print('    filtering with tm done ...');

    writeLines(proc_result, out_con)

    iter_num = iter_num +1;
  }


  close(con);
  close(out_con);

  return(0);
}
