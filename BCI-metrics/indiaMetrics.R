#################################################################
############# Preparing the India (Country) level metrics########
#################################################################


####Function that gets the metric for a month for the source dataframe
getMetric <- function (source_df, month)
{
  # Years of last six months. This assumes number of months < 12 
  year <- ifelse(month > CurMonth, PrevYear, CurYear)
  
  # Pick the value for the year/month from source df
  return ( source_df %>% 
            filter ( YEAR == year, MONTH == month) %>%
            select (count) %>% 
            pull ())
}

####Function that gets YoY metric for a month for the source dataframe
getYoY <- function (source_df, month)
{
  curYearVal <- source_df %>% 
                  filter ( YEAR == CurYear, MONTH == month) %>%
                  select (count) %>% 
                  pull ()
  preYearVal <- source_df %>% 
                  filter ( YEAR == PrevYear, MONTH == month) %>%
                  select (count) %>% 
                  pull ()
  
  YoY <- round (100 * (curYearVal - preYearVal)/preYearVal,0) 
  return ( accounting(YoY))
}
  
####Function that adds one row (for all months) of a metric to dataframe
addMetricRow <- function (dest_df, source_df)
{
  # Create a row of metrics for each month of source_df
  row <- c ( mapply ( getMetric, 
                          Months, 
                          MoreArgs = list (source_df = source_df)) %>% 
              unlist(), 
             getYoY(source_df, CurMonth)) 
  # Append the row to dest_df
  dest_df <- rbind (dest_df, row)

  return (dest_df)
}

genIndiaMetrics <- function ()
{
  # Create an empty dataframe with months as columns
  fields <- data.frame() 
  for (k in month.abb[Months]) fields[[k]]<-as.double()
  fields[["YoY%"]]<-as.double()
  
  india_metrics <<- fields %>%
    addMetricRow(india_obsv_stats) %>%
    addMetricRow(india_list_stats) %>%
    addMetricRow(india_user_stats) %>%
    addMetricRow(india_new_users_stats) %>%
    addMetricRow(photo_stats) %>% 
    addMetricRow(sound_stats)
    
  colnames(india_metrics) <- c (month.abb[Months], "YoY%")
  
  rownames(india_metrics) <- c ("Observations",
                                "Lists",
                                "Users",
                                "New Users",
                                "Photos",
                                "Sounds")
  return (india_metrics)
}
