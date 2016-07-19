tst_hot_read <- function(file_name, str_per_iter = 2) {

  source('process_text_chunk.R')

  con = file(file_name, 'rt');

  str_per_iter = 100;

  DO = TRUE;
  while (DO) {
    result = readLines(con, str_per_iter)
    print(result);

    if (length(result) == 0) {
      print('ALL SEEMS DONE!');
      DO = FALSE;
    }

  }


  close(con);

  return(0);
}
