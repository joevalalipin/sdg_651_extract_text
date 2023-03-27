# this code reads .docx copy of a country report for sdg651 survey
# open text responses for status description and way forward for each question are extracted
# table is produced in csv format
 
library(officer)
library(stringr)
library(dplyr)

# get a list of all files
files <- list.files("word_files/")

# load hard coded vectors
# question numbers
load("questions.rdata")
# row numbers for status description
load("row_num_sd.rdata")
# row numbers for way forward
load("row_num_wf.rdata")


# initialise full data frame
txt_full <- tibble()
row_num_sd_full <- tibble(index = c(1:33))


# incrementally build full data frame
for (m in 1:length(files)) {
  
  # track progress
  print(files[m])
  
  # read docx files and filter contents in the tables
  table_values <- read_docx(paste0("word_files/", files[m])) %>% 
    docx_summary() %>% 
    as_tibble() %>% 
    filter(content_type == "table cell")
  
  # extract table cells for status description
  status_description <- table_values %>% 
    slice(row_num_sd) %>% 
    select(status_description = text)
  
  # extract table cells for way forward
  way_forward <- table_values %>% 
    slice(row_num_wf) %>% 
    select(way_forward = text)
  
  # build the dtaset
  txt <- bind_cols(q_num = questions,
                   status_description,
                   way_forward) %>% 
    mutate(file = files[m])
  
  txt_full <- bind_rows(txt_full, txt)
  
  # check countries that meet expectation for table cell ids or number
  row_num_sd_check <- as_tibble(which(grepl("Status description.*", table_values$text))) %>% 
    mutate(index = as.numeric(row.names(.)))
  
  colnames(row_num_sd_check) <- c(paste0("country_", m), "index")
  
  row_num_sd_full <- left_join(row_num_sd_full, row_num_sd_check, by = "index")
  
  # clean this iteration
  rm(txt)
  rm(table_values)
  rm(status_description)
  rm(way_forward)
  rm(row_num_sd_check)
}


# save
save(txt_full, file = "txt_full.rdata")
write.csv(txt_full, "txt_full.csv")


# Split file into separate csvs per question
for (n in 1:length(questions)) {
  
  print(paste(n, questions[n]))
  
  txt_question <- txt_full %>% 
    filter(q_num == questions[n])
  
  write.csv(txt_question, paste0("extract_by_question/txt_", str_replace(questions[n], "\\.", "_"), ".csv"))
  
  rm(txt_question)
}


