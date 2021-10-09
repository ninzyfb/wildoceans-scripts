

getwd()
list.files(pattern = 'suitablehabitat.tif', full.names = TRUE, recursive=TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("FileName.gif") # write to current dir
