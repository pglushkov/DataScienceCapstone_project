my_read_lines_cold <- function(file_name, num_lines = 10)
{
  con = file(file_name, 'rt');

  result = readLines(con, num_lines)

  close(con);

  return(result);
}
