library (jsonlite)
library (curl)
library (tidyverse)
source("BCI-metrics/token.R")

searchURL <- "https://search.macaulaylibrary.org/api/v1/stats/media-count?"
genSearchString <-"bmo=START_MONTH&yr=YCUSTOM&mr=MCUSTOM&ey=END_YEAR&region=India%20(IN)&emo=END_MONTH&regionCode=IN&by=START_YEAR&includeUnconfirmed=T"

getMonthSummary <- function (month)
{
  # Years of last six months. This assumes number of months < 12 
  year <- ifelse(month > CurMonth, PrevYear, CurYear)
  monthSummary <- getMediaSummary (month, month, year, year)
  monthSummary ['MONTH'] <- month
  monthSummary ['YEAR']  <- year
  
  return (monthSummary)
}

verifyYearMonth <- function (startMonth, endMonth, startYear, endYear)
{
   if ( (startMonth < 1) || 
        (endMonth < 1) ||
        (startMonth > 12) ||
        (endMonth > 12) ||
        (startMonth > endMonth) ||
        (startYear < 1929) ||
        (endYear < 1929) ||
        (startYear > CurYear) ||
        (endYear > CurYear) ||
        (startYear > endYear))
   {
     return (FALSE)
   }
  else
  {
    return (TRUE)
  }
}

replaceSearchString <- function (startMonth, endMonth, startYear, endYear)
{
  searchString <<- genSearchString
  
  if (verifyYearMonth (startMonth, endMonth, startYear, endYear))
  {
    searchString <<- gsub ("START_MONTH", startMonth, searchString)    
    searchString <<- gsub ("END_MONTH", endMonth, searchString)    
    searchString <<- gsub ("START_YEAR", startYear, searchString)    
    searchString <<- gsub ("END_YEAR", endYear, searchString)    
  }
  return (searchString)
}

getMediaSummary <- function (startMonth, endMonth, startYear, endYear)
{
  if (verifyYearMonth (startMonth, endMonth, startYear, endYear))
  {
    h <- new_handle()
    handle_setheaders(h,
                      "X-eBirdApiToken" = myEBirdToken)
    searchString <- paste0(searchURL, replaceSearchString (startMonth, endMonth, startYear, endYear)) 
    retries <- 0
    while (retries < 3)
    {
      req <- curl_fetch_memory(searchString, h)
      Sys.sleep(3)
      print(searchString)
      
      if (req$status_code == 200)
      {
        mediaSummary <- jsonlite::prettify(rawToChar(req$content)) %>% 
          fromJSON(flatten=FALSE) %>%
          as.data.frame()
        return (mediaSummary)
      }
      else
      {
        print("HTTP GET returned error. Retrying...")
        retries <- retries + 1
      }
    }
  }
  else
  {
    print("Invalid month or year")
    return (0)
  }
  return (0)
}

pullMediaStats <- function()
{
  mediastat <- getMediaSummary (CurMonth, CurMonth, PrevYear, PrevYear)
  mediastat ['MONTH'] <- CurMonth
  mediastat ['YEAR']  <- PrevYear

  monthstat <- mapply ( getMonthSummary, 
                            Months) %>% 
                           as.data.frame() 
     
  mediastat <- rbind (mediastat, t(monthstat))
   
  photo_stats <<-  mediastat %>% 
                    select ("photo", "MONTH", "YEAR") %>%
                    lapply(as.integer) %>%
                    as.data.frame()
  photo_stats <<-  photo_stats [, c(3, 2, 1)] 
       
  sound_stats <<-  mediastat %>% 
                    select ("audio", "MONTH", "YEAR") %>%
                    lapply(as.integer) %>%
                    as.data.frame()
  sound_stats <<-  sound_stats [, c(3, 2, 1)]
           
  video_stats <<-  mediastat %>% 
                    select ("video", "MONTH", "YEAR")  %>%
                    lapply(as.integer) %>%
                    as.data.frame()
  video_stats <<-  video_stats [, c(3, 2, 1)]
}        