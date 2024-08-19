
# comparing our read vs auk read_ebd()

library(tictoc)
library(auk)

tic.clearlog()

tic("BCI current method of reading")
data_bci <- read.delim(path_ebd_main, 
                       sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ", NA))
toc(log = TRUE, quiet = TRUE)
rm(data_bci)

tic("auk method of reading")
data_auk <- read_ebd(path_ebd_main)
toc(log = TRUE, quiet = TRUE)
rm(data_auk)

tic("Trying auk's pre-import filtering prowess")
path_ebd_main %>% 
  auk_ebd() %>% 
  auk_state("IN-KL") %>% 
  auk_protocol(c("Stationary", "Traveling")) %>% 
  auk_filter(file = "test_auk.txt")

toc()

tic.log()

