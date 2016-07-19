proc_ze_data <- function(data_filename, prefix = 'unknown')
{
  WRDS_NUM = 10;

  load(data_filename);

  print(sprintf('SOME STATS OF SET : %s ', prefix));

  hist(RES[[4]], xlab = 'number of characters', ylab ='number of entries',
    main = sprintf('Histogram of entries lengths of %s', prefix));

  print(sprintf('%d of mostly used words : ', WRDS_NUM));
  print(RES[[3]][1:WRDS_NUM]);

  print(sprintf('Number of entries with exclamation mark : %d', RES[[5]]$Num_exclam));
  print(sprintf('Number of entries with question mark : %d', RES[[5]]$Num_quest));
  print(sprintf('Number of entries with elipsis : %d', RES[[5]]$Num_elipsis));

}
