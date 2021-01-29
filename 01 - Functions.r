## Extract CMS Drugs Advice by quarter using HTML Scraping
##
## This script was written to be run on the RStudio Desktop App
##
## Original Date: 15/01/2021
## Last Update: 29/01/2021
##
## Required Packages:
##  tidyverse (for basic data manipulation and pipe %>%)
##  rvest (for HTML Scraping. The XML dependency must be as up to date as possible)


library(tidyverse)
library(rvest)

get_data <- function(FY, FQ){
  
  ## first get urls for each status
  data<- tibble(quarter = paste0("Q", seq(1,4)),
                month_from = c("04", "07", "10", "01"),
                month_to = c("06", "09", "12", "03"),
                day_to = c("30", "30", "31", "31")) %>%
    filter(quarter == FQ)
  
  year = if_else(grepl(FQ, 1), paste0("20", substr(FY, 3,4)),
                 paste0("20", substr(FY, 1,2)))
  from_to <- paste0("from=01%2F", data$month_from, "%2F", year,
                    "&to=", data$day_to, "%2F", data$month_to, "%2F", year)
  
  ACCEPTED <- paste0("https://www.scottishmedicines.org.uk/medicines-advice/?active-tab=0&",
                     "node-id=6990&keywords=&filter-3561=&filter-3567=&filter-3803=&",
                     from_to,"&filter-3552=3553&total-results-0=5&total-results-1=0")
  RESTRICTED <- paste0("https://www.scottishmedicines.org.uk/medicines-advice/?active-tab=0&",
                       "node-id=6990&keywords=&filter-3561=&filter-3567=&filter-3803=&",
                       from_to, "&filter-3552=3556&total-results-0=5&total-results-1=0")
  NOT_RECOMMENDED <- paste0("https://www.scottishmedicines.org.uk/medicines-advice/?active-tab=0&",
                            "node-id=6990&keywords=&filter-3561=&filter-3567=&filter-3803=&",
                            from_to,"&filter-3552=3555&total-results-0=5&total-results-1=0")
  
  urls <- list("accepted" = ACCEPTED,
               "restricted" = RESTRICTED,
               "not_recommended" = NOT_RECOMMENDED)

  ## download the data for each status
  temp <- tempfile()
  download.file(urls$`accepted`, destfile = temp, quiet=TRUE)
  
  read_html(temp)
  
  if(read_html(temp) %>% html_nodes("div.no-results") %>% length() == 1){
    ACCEPTED <- (read_html(temp) %>%
                   html_nodes("div.table-holder") %>%
                   html_children() %>%
                   html_table() %>% as.data.frame())[,1:5]  
    ACCEPTED <- ACCEPTED %>% select(1:5) %>% mutate(Status = "Accepted")
  } else {
    ACCEPTED <- data.frame(SMC.ID = NULL,
                           Date = NULL,
                           Medicine = NULL,
                           Submission.Type = NULL,
                           Indication = NULL,
                           Status = NULL)
  }
  
  ## make sure all accepted drugs have been picked up
  if(read_html(temp) %>%
     html_node("span.list-filters__load-more-wrapper") %>% 
     length() > 0){
    print(paste("List of Accepted Drugs has exceeded the max downloadable length of 19.",
          "Please visit", urls$accepted, "to view the full list"))
  }
  
  file.remove(temp)
  
  temp <- tempfile()
  download.file(urls$restricted, destfile = temp, quiet=TRUE)
  
  if(read_html(temp) %>% html_nodes("div.no-results") %>% length() == 1){
    RESTRICTED <- (read_html(temp) %>%
                     html_nodes("div.table-holder") %>%
                     html_children() %>%
                     html_table() %>% as.data.frame())[,1:5]  
    RESTRICTED <- RESTRICTED %>% select(1:5) %>% mutate(Status = "Restricted")
  } else {
    RESTRICTED <- data.frame(SMC.ID = NULL,
                             Date = NULL,
                             Medicine = NULL,
                             Submission.Type = NULL,
                             Indication = NULL,
                             Status = NULL)
  }
  
  ## make sure all restricted drugs have been picked up
  if(read_html(temp) %>%
     html_node("span.list-filters__load-more-wrapper") %>% 
     length() > 0){
    print(paste("List of Restricted Drugs has exceeded the max downloadable length of 19.",
                "Please visit", urls$restricted, "to view the full list"))
  }
  
  file.remove(temp)
  
  temp <- tempfile()
  download.file(urls$not_recommended, destfile = temp, quiet=TRUE)
  
  if(read_html(temp) %>% html_nodes("div.no-results") %>% length() == 1){
    NOT_RECOMMENDED <- (read_html(temp) %>%
                          html_nodes("div.table-holder") %>%
                          html_children() %>%
                          html_table() %>% as.data.frame())[,1:5]  
    NOT_RECOMMENDED <- NOT_RECOMMENDED %>% select(1:5) %>% mutate(Status = "Not Recommended")
  } else {
    NOT_RECOMMENDED <- data.frame(SMC.ID = NULL,
                                  Date = NULL,
                                  Medicine = NULL,
                                  Submission.Type = NULL,
                                  Indication = NULL,
                                  Status = NULL)
  }
  
  ## make sure all not recommended drugs have been picked up
  if(read_html(temp) %>%
     html_node("span.list-filters__load-more-wrapper") %>% 
     length() > 0){
    print(paste("List of Not Recommended Drugs has exceeded the max downloadable length of 19.",
                "Please visit", urls$not_recommended, "to view the full list"))
  }
  
  file.remove(temp)
  
  bind_rows(ACCEPTED, RESTRICTED, NOT_RECOMMENDED)
  
}

## function to save the data
save_data <- function(FY,FQ, data){
  fp <- "//freddy/DEPT/PHIBCS/PHI/Prescribing/Topics/Controlled Drugs/Not Recommended Drugs/SMC Data Extract"
  folder <- paste0("/", FY)
  filename <- paste0(fp, folder, "/", "SMC_Extract_", FQ, ".csv")
  if(!dir.exists(paste0(fp, folder))){
    Sys.umask(000)
    dir.create(paste0(fp, folder))
  }
  
  write.csv(data, filename, row.names = FALSE)
}
