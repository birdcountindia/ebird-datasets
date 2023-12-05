#################################################################
############# Script to create monthly metrics out of ebd########
#################################################################

##################################################################
#####Preparing the state metrics in the required format########### 
##################################################################

genDistrictMetrics <- function ()
{
  PrevMonth <- (CurMonth - 2) %% 12 + 1
  YearOfPrevMonth <- ifelse (CurMonth > PrevMonth, CurYear, PrevYear) 
  
  district_metrics <- districts %>% 
                      left_join ( district_obsv_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_obsv_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_obsv_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_list_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_list_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_list_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_user_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_user_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "COUNTY.CODE") %>%  
                      left_join ( district_user_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "COUNTY.CODE") %>% 
                      arrange(COUNTY.CODE)

# Bad code. Move to just numbering of the columns  
  district_metrics <- district_metrics %>%
                      select ("COUNTY.CODE", 
                              "COUNTY",
                              "count.x",
                              "count.y",
                              "count.x.x",
                              "count.y.y",
                              "count.x.x.x",
                              "count.y.y.y",
                              "count.x.x.x.x",
                              "count.y.y.y.y",
                              "count.x.x.x.x",
                              "count.y.y.y.y",
                              "count"
                              )
  colnames(district_metrics) <- c("COUNTY.CODE",
                                  "COUNTY",
                                  "Obsv_CurMCurY",
                                  "Obsv_PreMCurY",
                                  "Obsv_CurMPreY",
                                  "Lists_CurMCurY",
                                  "Lists_PreMCurY",
                                  "Lists_CurMPreY",
                                  "Users_CurMCurY",
                                  "Users_PreMCurY",
                                  "Users_CurMPreY")
  
  district_metrics[is.na(district_metrics)] = 0
                                  
  district_metrics <- district_metrics %>% 
    mutate(Obsv_Trend   = round (100 * (Obsv_CurMCurY - Obsv_CurMPreY)/Obsv_CurMPreY,0) %>% accounting(),
           Lists_Trend  = round (100 * (Lists_CurMCurY - Lists_CurMPreY)/Lists_CurMPreY,0) %>% accounting(),
           Users_Trend  = round (100 * (Users_CurMCurY - Users_CurMPreY)/Users_CurMPreY,0) %>% accounting())
  
  district_metrics <- district_metrics %>% 
    select(COUNTY.CODE,
           COUNTY,
           Obsv_PreMCurY,
           Obsv_CurMCurY,
           Obsv_Trend,
           Lists_PreMCurY,
           Lists_CurMCurY,
           Lists_Trend,
           Users_PreMCurY,
           Users_CurMCurY,
           Users_Trend) %>% 
    mutate(across(c(everything(), -COUNTY, -COUNTY.CODE), ~ as.numeric(.))) %>% 
    magrittr::set_colnames(c("",
                             month.abb[PrevMonth], month.abb[CurMonth], "YoY%",
                             month.abb[PrevMonth], month.abb[CurMonth], "YoY%",
                             month.abb[PrevMonth], month.abb[CurMonth], "YoY%"))

  return (district_metrics)
}
#####################################################################
