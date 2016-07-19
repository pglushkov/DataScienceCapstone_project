my_read_lines_hot <- function(file_handle, num_lines = 10)
{
  result = readLines(file_handle, num_lines);

  return(result);
}
