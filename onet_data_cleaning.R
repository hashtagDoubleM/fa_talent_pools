# subject: financial advisor talent pools ----
# author: matt milunski
# date: 2017-09-21
# tl;dr: this code is the ETL script for the o*net data.

# variable name cleaning function ----
clean.names <- function(df) {
    names(df) <- tolower(gsub("\\s|[[:punct:]]", "_", names(df)))
    return(df)
}

# install and load libraries ----
lapply(c("tidyverse", "data.table"), function(package.name) {
    if (!is.element(package.name, installed.packages()[, 1])) {
        show(paste0("installing...", package.name))
        install.packages(package.name, dependencies = TRUE)
        library(package.name, character.only = TRUE)
    } else {
        library(package.name, character.only = TRUE)
    }
})

# read data into memory ----
data_file_list <- list.files(paste0(getwd(), "/db_22_0_text"), pattern = ".txt")
for (i in seq_along(data_file_list)) {
    require(data.table)
    ## fread data
    onet_data <-
        fread(paste0(getwd(), "/db_22_0_text/", data_file_list[i]), 
              header = TRUE, stringsAsFactors = FALSE, sep = "\t", 
              na.strings = c("n/a", "\\s"))
    ## clean up variable names in each dataset
    names(onet_data) <- tolower(gsub("\\s|[[:punct:]]", "_",
                                     names(onet_data)))
    ## name each dataset and assign it to the global environment
    assign(x = tolower(gsub("\\s|.txt|[[:punct:]]", "_", data_file_list[i])),
           value = onet_data)
    ## clean up
    rm(onet_data)
}
gc() ##recover memory

# o*net top 10 matches to personal financial advisor ----
changer_fa <- career_changers_matrix_ %>%
    filter(o_net_soc_code == "13-2052.00") %>%
    left_join(occupation_data_,
              by = c("related_o_net_soc_code" = "o_net_soc_code"))

# create the dataframes with importance ratings ----
## knowledge
knowledge2 <- knowledge_ %>%
    ## unsuppressed and relevant obs retained
    filter(recommend_suppress %in% c("N", NA),
           not_relevant %in% c("N", NA),
           scale_id == "IM") %>%
    select(o_net_soc_code, element_name , data_value) %>%
    spread(key = element_name, value = data_value) %>%
    clean.names()
## skills
skills2 <- skills_ %>%
    filter(recommend_suppress %in% c("N", NA),
           not_relevant %in% c("N", NA),
           scale_id == "IM") %>%
    select(o_net_soc_code, element_name , data_value) %>%
    spread(key = element_name, value = data_value) %>%
    clean.names()
## abilities
abilities2 <- abilities_ %>%
    filter(recommend_suppress %in% c("N", NA),
           not_relevant %in% c("N", NA),
           scale_id == "IM") %>%
    select(o_net_soc_code, element_name , data_value) %>%
    spread(key = element_name, value = data_value) %>%
    clean.names()
## work values
work_values2 <- work_values_ %>%
    select(o_net_soc_code, element_name, data_value) %>%
    filter(!grepl("First|Second|Third", element_name)) %>%
    spread(key = element_name, value = data_value) %>%
    clean.names()
## work styles
work_styles2 <- work_styles_ %>%
    filter(recommend_suppress %in% c("N", NA),
           scale_id == "IM") %>%
    select(o_net_soc_code, element_name, data_value) %>%
    spread(key = element_name, value = data_value) %>%
    clean.names()
## interests
interests2 <- interests_ %>%
    ## only use OI/actual RIASEC ratings
    filter(scale_id == "OI") %>%
    select(o_net_soc_code, element_name, data_value) %>%
    spread(key = element_name, value = data_value) %>%
    clean.names()
## create single KSA dataframe
ksa_df <- occupation_data_ %>%
    left_join(knowledge2, by = "o_net_soc_code") %>%
    left_join(., skills2, by = "o_net_soc_code") %>%
    left_join(., abilities2, by = "o_net_soc_code") %>%
    left_join(., work_values2, by = "o_net_soc_code") %>%
    left_join(., work_styles2, by = "o_net_soc_code") %>%
    left_join(., interests2, by = "o_net_soc_code") %>%
    ## remove observations w/missing data
    na.omit()

# write to file ----
fwrite(ksa_df, paste0(getwd(), "/ksa_df.csv"), row.names = FALSE, na = "")
