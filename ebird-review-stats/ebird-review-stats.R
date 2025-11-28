
data_unv <- data |> 
  filter(REVIEWED == "0", APPROVED == "0")
data_unv <- data |> 
  filter(REVIEWED == "1", APPROVED == "0")
