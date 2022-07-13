#################################################################
############# Script to create monthly metrics out of ebd########
#################################################################

##################################################################
#####Preparing the state metrics in the required format########### 
##################################################################

genStateMetrics <- function ()
{
  PrevMonth <- (CurMonth - 2) %% 12 + 1
  YearOfPrevMonth <- ifelse (CurMonth > PrevMonth, CurYear, PrevYear) 
  
  state_metrics <- states %>% 
                      left_join ( state_obsv_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_obsv_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_obsv_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_list_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_list_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_list_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_user_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_user_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( state_user_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>% 
                      left_join ( county_coverage_stats %>% 
                                    filter ( YEAR == CurYear, MONTH == CurMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( county_coverage_stats %>% 
                                    filter ( YEAR == YearOfPrevMonth, MONTH == PrevMonth),
                                  by = "STATE.CODE") %>%  
                      left_join ( county_coverage_stats %>% 
                                    filter ( YEAR == PrevYear, MONTH == CurMonth),
                                  by = "STATE.CODE")

# Bad code. Move to just numbering of the columns  
  state_metrics <- state_metrics %>%
                      select ("STATE.CODE", 
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
                              "count",
                              "coverage.x",
                              "coverage.y",
                              "coverage"
                              )
  colnames (state_metrics) <- c ( "STATE.CODE",
                                  "Obsv_CurMCurY",
                                  "Obsv_PreMCurY",
                                  "Obsv_CurMPreY",
                                  "Lists_CurMCurY",
                                  "Lists_PreMCurY",
                                  "Lists_CurMPreY",
                                  "Users_CurMCurY",
                                  "Users_PreMCurY",
                                  "Users_CurMPreY",
                                  "County_CurMCurY",
                                  "County_PreMCurY",
                                  "County_CurMPreY")
                                  
  state_metrics <- state_metrics %>% 
                    mutate (STATE.CODE   = substr(STATE.CODE,4,5),
                            Obsv_Trend   = round (100 * (Obsv_CurMCurY - Obsv_CurMPreY)/Obsv_CurMPreY,0) %>% accounting(),
                            Lists_Trend  = round (100 * (Lists_CurMCurY - Lists_CurMPreY)/Lists_CurMPreY,0) %>% accounting(),
                            Users_Trend  = round (100 * (Users_CurMCurY - Users_CurMPreY)/Users_CurMPreY,0) %>% accounting(),
                            County_Trend = round (100 * (County_CurMCurY - County_CurMPreY)/County_CurMPreY,0) %>% accounting())
  
  state_metrics <- state_metrics %>% select (
                          STATE.CODE,
                          Obsv_PreMCurY,
                          Obsv_CurMCurY,
                          Obsv_Trend,
                          Lists_PreMCurY,
                          Lists_CurMCurY,
                          Lists_Trend,
                          Users_PreMCurY,
                          Users_CurMCurY,
                          Users_Trend,
                          County_PreMCurY,
                          County_CurMCurY,
                          County_Trend
  )
  
  colnames (state_metrics) <- c (
                                 "",
                                 month.abb[PrevMonth],
                                 month.abb[CurMonth],
                                 "YoY%",
                                 month.abb[PrevMonth],
                                 month.abb[CurMonth],
                                 "YoY%",
                                 month.abb[PrevMonth],
                                 month.abb[CurMonth],
                                 "YoY%",
                                 month.abb[PrevMonth],
                                 month.abb[CurMonth],
                                 "YoY%")
  state_metrics[is.na(state_metrics)] = 0
  return (state_metrics)
}
#####################################################################
