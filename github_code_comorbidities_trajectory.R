##EHR analysis to demonstrate the lifetime comorbidities and disease trajectory

#Step 1. process a list of longitudianl encounter data mapped to ICD10/9 codes and generate age of onset
#Step 2. illustrate the comorbidities of Uveitis in the Upset plot using a mapped phecodes matrix file  
#Step 3. create files to determine each comorbidity before or after Uveitis
#Step 4. create a combined long-format file with diagonstic order
#Step 5. demonstrate the disease trajectory of patients with Uveitis using Sankey plot with or without plotly 


#Step 1.
#upload a list of .RData files which represent a list of longitudinal encounter data mapped to ICD10 or ICD9 codes
#each loaded object name is "dat_RECUR_ICD109"
data_dir <- "/to_your_directory_which_held_a_list_of_longitudinal_encounter_data/" #for all or unrelated individuals 

#load case_id for Patient ID with Uveitis
case_id  #with Uveitis (X371.1)  # ICD10/9 (360.*, 363.*, 364.*, H30.*, H20.*, H44.*)

#this case_id is the same as the following UVeitis_id created in Step 2.
#UVeitis_id <- paste0("PT", combined_data[combined_data$X371.1 == 1, ]$id)

# List all .RData files in the directory
rdata_files <- list.files(data_dir, pattern = "\\.RData$", full.names = TRUE)

# Placeholder for combined data
all_combined_data <- data.frame()

# Loop over each file
for (file in rdata_files) {
  # Load each .RData file
  load(file)
  
  # Check if `dat_RECUR_ICD109` exists in the loaded data
  if (exists("dat_RECUR_ICD109")) {
    
    # Subset cases and controls based on case_id
    dat_RECUR_ICD109_cases <- dat_RECUR_ICD109[dat_RECUR_ICD109$PT_ID %in% case_id, ]
    dat_RECUR_ICD109_controls <- dat_RECUR_ICD109[!(dat_RECUR_ICD109$PT_ID %in% case_id), ]
    
    dat_RECUR_ICD109_cases$group <- "case"
    dat_RECUR_ICD109_controls$group <- "control"
    
    #print(head(dat_RECUR_ICD109_cases, 5))
    # Filter for "L40" or "696" codes for cases and controls for psoriasis(X696)
    # Filter for "710" or "M32" codes for cases and controls for SLE (X695.42)
    # Filter for "714" or "M05.4" codes for cases and controls for RA(X714.1)
    # Filter for "340" or "G35" codes for cases and controls for MS(X335)
    # Filter for "135", "321.4", or "D86" codes for cases and controls for Sar(X697)
    # Filter for "720" or "M45" or "M08.1" for case and controls for AS(X715.2)
    # Filter for "556" or "K51" for case and controls for UC(X555.2)
    # Filter for "136.1" or "711.2" or "M35.2" for case and controls for Behcet's syndrome (X711.3)
    # Filter for "446.4" or "M31" for cases and controls for Wegener's granulomatosis (X446.4)
    # Filter for "M30.1" for cases and controls for Churg-Strauss Syndrome (X446.4)
    # Filter for "446.5" or "M31.5" or "M31.6" for Giant cell arteritis (X446.5)
    # Filter for "555" or "K50" for reginal enteritis (Crohn's Dis)  (X555.1)
    # Filter for "710.2" or "M35" for Sjogren's Syn (X709.2)
    dat_RECUR_ICD109_cases_filtered <- dat_RECUR_ICD109_cases %>%
      filter(str_detect(CS_CD, "710.2|M35")) %>%   #"L40|696", "M32|710", "M05.4|714", "G35|340", "D86|135|321.4", "720|M45|M08.1", "556|K51", "136.1|711.2|M35.2", "446.4|M31", "M30.1", "446.5|M31.5|M31.6", "555|K50", "710.2|M35"
      #filter(CS_CD != "H02.135" & CS_CD != "S52.135A") %>%
      #filter(CS_CD != "O99.340") %>%
      #filter(CS_CD != "Z90.710" & CS_CD != "K22.710" & CS_CD != "M1A.0710") %>%
      group_by(PT_ID) %>%
      arrange(desc(ENC_DT)) %>%
      slice(n()) %>%
      ungroup()
    
    dat_RECUR_ICD109_controls_filtered <- dat_RECUR_ICD109_controls %>%
      filter(str_detect(CS_CD, "710.2|M35")) %>%  #"L40|696", "M32|710", "M05.4|714", "G35|340", "D86|135|321.4", "720|M45|M08.1", "556|K51", "136.1|711.2|M35.2", "446.4|M31", "M30.1", "446.5|M31.5|M31.6", "555|K50", "710.2|M35"
      #filter(CS_CD != "H02.135" & CS_CD != "S52.135A") %>%
      #filter(CS_CD != "O99.340") %>%
      #filter(CS_CD != "Z90.710" & CS_CD != "K22.710" & CS_CD != "M1A.0710") %>%
      group_by(PT_ID) %>%
      arrange(desc(ENC_DT)) %>%
      slice(n()) %>%
      ungroup()
    
    # Combine cases and controls for this file
    dat_RECUR_ICD109_combined <- bind_rows(dat_RECUR_ICD109_cases_filtered, dat_RECUR_ICD109_controls_filtered)
    
    # Append to the main combined data
    all_combined_data <- rbind(all_combined_data, dat_RECUR_ICD109_combined)
  } else {
    warning(paste("dat_RECUR_ICD109 not found in", file))
  }
}

#save(all_combined_data, file = "Uveitis_AS_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_UC_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_Bechet_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_WGA_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_CS_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_GCA_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_crohn_age_of_onset.RData", version = 2)
#save(all_combined_data, file = "Uveitis_Sjo_age_of_onset.RData", version = 2)

#there is a cleaning step. when using "str_detect", be sure to filter (CS_CD != .....) in the above code.

# Loop over each file only for Uveitis
for (file in rdata_files) {
  # Load each .RData file
  load(file)
  
  # Check if `dat_RECUR_ICD109` exists in the loaded data
  if (exists("dat_RECUR_ICD109")) {
    
    # Subset cases and controls based on case_id
    dat_RECUR_ICD109_cases <- dat_RECUR_ICD109[dat_RECUR_ICD109$PT_ID %in% case_id, ]
    dat_RECUR_ICD109_controls <- dat_RECUR_ICD109[!(dat_RECUR_ICD109$PT_ID %in% case_id), ]
    
    # Filter for "360", "363", "364", "H20", "H30", "H44" codes for cases and controls for Uveitis(X371.1)
    dat_RECUR_ICD109_cases_filtered <- dat_RECUR_ICD109_cases %>%
      filter(str_detect(CS_CD, "H20|H30|H44|360|363.|364")) %>%
      group_by(PT_ID) %>%
      arrange(desc(ENC_DT)) %>%
      slice(n()) %>%
      ungroup()
    
    # only for control without Uveitis
    dat_RECUR_ICD109_controls_filtered <- dat_RECUR_ICD109_controls %>%
      group_by(PT_ID) %>%
      arrange(desc(ENC_DT)) %>%
      slice(1) %>%
      ungroup()
    
    # Change group label for controls
    dat_RECUR_ICD109_controls_filtered <- dat_RECUR_ICD109_controls_filtered %>%
      mutate(group = ifelse(group == 0, "case", 0))
    
    # Combine cases and controls for this file
    dat_RECUR_ICD109_combined <- bind_rows(dat_RECUR_ICD109_cases_filtered, dat_RECUR_ICD109_controls_filtered)
    
    # Append to the main combined data
    all_combined_data <- rbind(all_combined_data, dat_RECUR_ICD109_combined)
  } else {
    warning(paste("dat_RECUR_ICD109 not found in", file))
  }
}

save(all_combined_data, file = "Uveitis_Uveitis_age_of_onset.RData", version = 2)

# View combined data summary by group
summary_stats <- all_combined_data %>%
  group_by(group) %>%  #group is case or control for the particular disease such as MS, Crohn, ...
  summarise(
    count_group = n(),
    mean_age = mean(timediff_occur_censor, na.rm = TRUE),
    sd_age = sd(timediff_occur_censor, na.rm = TRUE)
  )

#############################################################
#Step 2.
#load the lifetime phecode matrix for Geisinger MyCode population
load("combined_short_data_unrelated_leftover_12122023.RData")
dim(combined_data)  #total sample size is 173514 and the first column is Patient ID; Totally 1857 phecodes mapped in binary (0/1) format.
#[1] 173514   1858
combined_data <- data.frame(combined_data)

WGA_id <- paste0("PT", combined_data[combined_data$X446.4 == 1, ]$id) 
PSO_id <- paste0("PT", combined_data[combined_data$X696 == 1, ]$id) 
SLE_id <- paste0("PT", combined_data[combined_data$X695.42 == 1, ]$id) 
RA_id <- paste0("PT", combined_data[combined_data$X714.1 == 1, ]$id) 
MS_id <- paste0("PT", combined_data[combined_data$X335 == 1, ]$id) 
SAR_id <- paste0("PT", combined_data[combined_data$X697 == 1, ]$id) 
AS_id <- paste0("PT", combined_data[combined_data$X715.2 == 1, ]$id) 
UC_id <- paste0("PT", combined_data[combined_data$X555.2 == 1, ]$id) 
Behcet_id <- paste0("PT", combined_data[combined_data$X711.3 == 1, ]$id) 
GCA_id <- paste0("PT", combined_data[combined_data$X446.5 == 1, ]$id) 
Crohn_id <- paste0("PT", combined_data[combined_data$X555.1 == 1, ]$id) 
SJO_id <- paste0("PT", combined_data[combined_data$X709.2 == 1, ]$id) 
UVeitis_id <- paste0("PT", combined_data[combined_data$X371.1 == 1, ]$id) 

#for CS_id, we used ICD10 code instead of phecode 

my_list <- list(WGA = WGA_id, PSO = PSO_id, SLE = SLE_id, RA = RA_id, MS = MS_id,
                SAR = SAR_id, AS = AS_id, UC = UC_id, Behcet = Behcet_id, GCA = GCA_id,
                Crohn = Crohn_id, SJO = SJO_id, CS = CS_id, UVeitis = UVeitis_id)

# Set the file path and dimensions for the TIFF file
tiff("upset_plot_comorbidity_uveitis_all.tiff", width = 12, height = 8, units = "in", res = 300)

# Generate the UpSet plot
upset(upset_data, sets = names(my_list), 
      order.by = "freq", 
      main.bar.color = "blue", 
      sets.bar.color = "gray", 
      text.scale = 1.2)
# Close the TIFF device
dev.off()


# Find items unique to each set
unique_items <- lapply(my_list, function(x) setdiff(x, unlist(my_list[names(my_list) != names(x)])))

# Step 2: Convert the unique items into a data frame
unique_items_table <- data.frame(
  Vector = names(unique_items),
  Unique_Items = sapply(unique_items, function(x) paste(x, collapse = ", "))
)

# Print the table
print(unique_items_table)


# Step 2: Convert the unique items into a data frame with a count of items
unique_items_table <- data.frame(
  Vector = names(unique_items),
  Unique_Items = sapply(unique_items, function(x) paste(x, collapse = ", ")),
  Count = sapply(unique_items, length)  # Count the number of unique items
)

write.table(unique_items_table, file = "upset_plot_comorbidity_uveitis_overlapped_unique_items_table.txt", sep = "|", quote = F, row.names = F, col.names = T)
save(unique_items_table, file = "upset_plot_comorbidity_uveitis_overlapped_unique_items_table.RData", version = 2)

#############################################################################################################
#Step 3.
load("Uveitis_Uveitis_age_of_onset.RData") #load an object named all_combined_data
all_combined_data_Uveitis <- all_combined_data

load("Uveitis_MS_age_of_onset.RData") #load an object named all_combined_data
all_combined_data_MS <- all_combined_data
colnames(all_combined_data_MS)
all_combined_data_Uveitis_MS <- merge(all_combined_data_Uveitis, all_combined_data_MS[, c("PT_ID", "timediff_occur_censor", "group")], by = "PT_ID", all.x = T)

# 
all_combined_data_Uveitis_MS <- all_combined_data_Uveitis_MS %>%
  dplyr::mutate(group.y = if_else(is.na(group.y), "nonMS", 
                                  if_else(group.y == "control", "case", group.y))) %>%
  dplyr::mutate(Uveitis_MS = case_when(
    group.x == "case" &  group.y == "case"  ~ "both",
    group.x == "case" &  group.y == "nonMS"  ~ "Uveitis_only",
    group.x == "control" &  group.y == "case"  ~ "MS_only",
    group.x == "control" &  group.y == "nonMS"  ~ "Neither"
  ))

#timediff_occur_censor.x is age of onset for uveitis; timediff_occur_censor.y is age of onset for MS
all_combined_data_Uveitis_MS_both <- all_combined_data_Uveitis_MS %>%
  filter(Uveitis_MS == "both") %>%
  mutate(history_MS = case_when(
    timediff_occur_censor.x >= timediff_occur_censor.y ~ 1, #Uveitis diagnosed same or after MS 
    timediff_occur_censor.x < timediff_occur_censor.y ~ 0   #Uveisits disgnosed before MS
  ))

table(all_combined_data_Uveitis_MS_both$history_MS)
# 0  1 
# 20 18
table(all_combined_data_Uveitis_MS_both$cohort, all_combined_data_Uveitis_MS_both$history_MS)
#            0  1
# leftover   8  7
# unrelated 12 11
save(all_combined_data_Uveitis_MS, all_combined_data_Uveitis_MS_both, file = "all_combined_data_Uveitis_MS_both.RData", version =2)


#apply the same process to other comorbidities. 
.................
#################################################################################################
#Step 4.
#create the function to do the work
process_condition_data <- function(condition_name, uveitis_data, both_conditions_data, history_col) {
  # Step 1: Process Uveitis-only patients
  uveitis_select <- uveitis_data %>%
    filter(!!sym(paste0("Uveitis_", condition_name)) == "Uveitis_only") %>%
    mutate(
      DiagnosisOrder = case_when(
        !!sym(paste0("Uveitis_", condition_name)) == "Uveitis_only" ~ 2,
        TRUE ~ NA_integer_
      ),
      Condition = "Uveitis"
    ) %>%
    dplyr::select(PT_ID, cohort, DiagnosisOrder, Condition)
  
  # Step 2: Process patients with both Uveitis and the specified condition
  both_select <- both_conditions_data %>%
    mutate(
      DiagnosisOrder = case_when(
        !!sym(history_col) == 1 ~ 1,  # Condition first
        !!sym(history_col) == 0 ~ 3   # Uveitis first
      ),
      Condition = condition_name
    ) %>%
    dplyr::select(PT_ID, cohort, DiagnosisOrder, Condition)
  
  # Step 3: Add Uveitis data to the combined data with both conditions
  both_select1 <- both_select %>%
    mutate(DiagnosisOrder = 2, Condition = "Uveitis")
  
  # Step 4: Combine all the datasets
  both_final <- rbind(both_select, both_select1)
  uveitis_condition_final <- rbind(uveitis_select, both_final)
  
  return(uveitis_condition_final)
}

result_ms <- process_condition_data(
  condition_name = "MS",
  uveitis_data = all_combined_data_Uveitis_MS,
  both_conditions_data = all_combined_data_Uveitis_MS_both,
  history_col = "history_MS"
)

result_crohn <- process_condition_data(
  condition_name = "crohn",
  uveitis_data = all_combined_data_Uveitis_crohn,
  both_conditions_data = all_combined_data_Uveitis_crohn_both,
  history_col = "history_crohn"
)

#apply the function to other comorbidities
.............

# Combine all results into a single dataframe
all_results_combined <- rbind(result_ms, result_psoriasis, result_AS, result_Bechet,  result_crohn, result_CS, result_RA, result_SAR, result_SLE, result_UC, result_WGA)
all_results_combined <- data.frame(all_results_combined)
all_results_combined <- rbind(all_results_combined, result_GCA, result_Sjo)
#save(all_results_combined, file = "all_results_combined_sankeyplot.RData", version =2)
load("all_results_combined_sankeyplot.RData")

head(all_results_combined)
dim(all_results_combined) #18821 4 where cohort represents unrelated or the rest in MyCode population
# PT_ID    cohort DiagnosisOrder Condition
# 1 PT1  leftover              2   Uveitis
# 2 PT2  leftover              2   Uveitis
# 3 PT3 unrelated              2   Uveitis
# 4 PT4  leftover              2   Uveitis
# 5 PT5  leftover              2   Uveitis
# 6 PT6 unrelated              2   Uveitis

unique(all_results_combined$Condition)
# [1] "Uveitis"   "MS"        "psoriasis" "AS"        "Bechet"    "crohn"     "CS"        "RA"        "SAR"       "SLE"      
# [11] "UC"        "WGA"       "GCA"       "Sjo"

all_results_combined_MS <- all_results_combined[(all_results_combined$Condition == "MS" & all_results_combined$PT_ID %in% MS_id), ]
all_results_combined_psoriasis <- all_results_combined[(all_results_combined$Condition == "psoriasis" & all_results_combined$PT_ID %in% PSO_id), ]
all_results_combined_AS <- all_results_combined[(all_results_combined$Condition == "AS" & all_results_combined$PT_ID %in% AS_id), ]
all_results_combined_Bechet <- all_results_combined[(all_results_combined$Condition == "Bechet" & all_results_combined$PT_ID %in% Behcet_id), ]
all_results_combined_crohn <- all_results_combined[(all_results_combined$Condition == "crohn" & all_results_combined$PT_ID %in% Crohn_id), ]
all_results_combined_CS <- all_results_combined[(all_results_combined$Condition == "CS" & all_results_combined$PT_ID %in% CS_id), ]
all_results_combined_RA <- all_results_combined[(all_results_combined$Condition == "RA" & all_results_combined$PT_ID %in% RA_id), ]
all_results_combined_SAR <- all_results_combined[(all_results_combined$Condition == "SAR" & all_results_combined$PT_ID %in% SAR_id), ]
all_results_combined_SLE <- all_results_combined[(all_results_combined$Condition == "SLE" & all_results_combined$PT_ID %in% SLE_id), ]
all_results_combined_UC <- all_results_combined[(all_results_combined$Condition == "UC" & all_results_combined$PT_ID %in% UC_id), ]
all_results_combined_WGA <- all_results_combined[(all_results_combined$Condition == "WGA" & all_results_combined$PT_ID %in% WGA_id), ]
all_results_combined_GCA <- all_results_combined[(all_results_combined$Condition == "GCA" & all_results_combined$PT_ID %in% GCA_id), ]
all_results_combined_Sjo <- all_results_combined[(all_results_combined$Condition == "Sjo" & all_results_combined$PT_ID %in% SJO_id), ]
all_results_combined_Uveitis <- all_results_combined[(all_results_combined$Condition == "Uveitis" & all_results_combined$PT_ID %in% UVeitis_id), ]

unique(all_results_combined_Uveitis$PT_ID) #1382

before_id <- unique(all_results_combined_filtered_comm[all_results_combined_filtered_comm$DiagnosisOrder == 1, ]$PT_ID) #240
after_id <- unique(all_results_combined_filtered_comm[all_results_combined_filtered_comm$DiagnosisOrder == 3, ]$PT_ID) #175

all_results_combined_filtered <- rbind(
  all_results_combined_MS,
  all_results_combined_psoriasis,
  all_results_combined_AS,
  all_results_combined_Bechet,
  all_results_combined_crohn,
  all_results_combined_CS,
  all_results_combined_RA,
  all_results_combined_SAR,
  all_results_combined_SLE,
  all_results_combined_UC,
  all_results_combined_WGA,
  all_results_combined_GCA,
  all_results_combined_Sjo,
  all_results_combined_Uveitis
  
  # Add other condition-specific filtered data frames here
)
unique(all_results_combined_filtered$Condition)
# [1] "MS"        "psoriasis" "AS"        "Bechet"    "crohn"     "CS"        "RA"        "SAR"       "SLE"       "UC"       
# [11] "WGA"       "GCA"       "Sjo"       "Uveitis"  

all_results_combined_filtered_before <- all_results_combined_filtered[all_results_combined_filtered$DiagnosisOrder == 1 | all_results_combined_filtered$DiagnosisOrder == 2, ]
all_results_combined_filtered_after <- all_results_combined_filtered[all_results_combined_filtered$DiagnosisOrder == 3 | all_results_combined_filtered$DiagnosisOrder == 2, ]


unique(all_results_combined_filtered[all_results_combined_filtered$DiagnosisOrder == 1, ]$PT_ID) #240
unique(all_results_combined_filtered[all_results_combined_filtered$DiagnosisOrder == 3, ]$PT_ID) #175

# Find overlapping elements
overlapping_cases <- intersect(before_id, after_id)  #38

all_results_combined_filtered_overlapped <- all_results_combined_filtered[all_results_combined_filtered$PT_ID %in% overlapping_cases, ]

######################################################

#Create Sankey plot with or without plotly 

# Load necessary libraries
library(dplyr)
library(networkD3)

# Assuming your data is stored in a data frame called 'all_results_combined_filtered'

# Step 1: Preprocess the data
# Find the diagnosis order of Uveitis for each patient

head(all_results_combined_filtered)
unique(all_results_combined_filtered[all_results_combined_filtered$Condition == "Uveitis", ]$PT_ID)
uveitis_order <- all_results_combined_filtered %>%
  #filter(cohort == "unrelated") %>% #only when apply to unrelated individuals
  filter(Condition == "Uveitis") %>%
  dplyr::select(PT_ID, DiagnosisOrder) %>%
  distinct()

head(uveitis_order)  #1382/1213  #724/618 for unrelated
# PT_ID DiagnosisOrder
# 1 PT1              2
# 2 PT2              2
# 3 PT3              2
# 4 PT4              2
# 5 PT5              2
# 6 PT6              2
unique(uveitis_order$PT_ID) #1382/1213 for all #724/618 for unrelated

all_results_combined_filtered_with_uveitis <- all_results_combined_filtered %>%
  left_join(uveitis_order, by = "PT_ID", suffix = c("", "_uveitis")) %>%
  mutate(Position = ifelse(DiagnosisOrder < DiagnosisOrder_uveitis, "Before Uveitis", 
                           ifelse(DiagnosisOrder > DiagnosisOrder_uveitis, "After Uveitis", NA))) %>%
  filter(!is.na(Position))

head(all_results_combined_filtered_with_uveitis) #855/747 for all/EUR; #479 for unrelated
# PT_ID    cohort DiagnosisOrder Condition DiagnosisOrder_uveitis       Position
# 1 PT1 unrelated              1        MS                      2 Before Uveitis
# 2 PT2  leftover              1        MS                      2 Before Uveitis
# 3 PT3 unrelated              1        MS                      2 Before Uveitis
# 4 PT4  leftover              1        MS                      2 Before Uveitis
# 5 PT5 unrelated              1        MS                      2 Before Uveitis
# 6 PT6  leftover              3        MS                      2  After Uveitis

# Step 2: Build the frequency table of conditions before and after Uveitis
condition_counts <- all_results_combined_filtered_with_uveitis %>%
  #filter(cohort == "unrelated") %>%  #apply to unrelated individual only
  group_by(Position, Condition) %>%
  summarise(Count = n(), .groups = "drop")
head(condition_counts)

# Step 3: Prepare data for Sankey plot
# Create nodes and links for the Sankey diagram
nodes <- data.frame(name = c("Before Uveitis", "After Uveitis", unique(condition_counts$Condition)))

# Create links (flow between Before/After Uveitis and conditions)
links <- condition_counts %>%
  mutate(Source = ifelse(Position == "Before Uveitis", 0, 1),  # 0 for Before Uveitis, 1 for After Uveitis
         Target = match(Condition, nodes$name) - 1) %>%  # Match to node indices (conditions start from index 2)
  dplyr::select(Source, Target, Value = Count)

# Step 4: Create Sankey plot
sankey_data <- list(nodes = nodes, links = links)

# Plot the Sankey diagram
sankey_plot <- sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, 
                             Source = "Source", Target = "Target", Value = "Value", NodeID = "name", 
                             units = "Conditions", fontSize = 12, nodeWidth = 30, nodePadding = 15)

# Show the plot
print(sankey_plot)

#########################
# Create more interactive plot using plotly
# Install and load plotly package if not already installed
if (!require("plotly")) install.packages("plotly")
library(plotly)

# Step 1: Create condition node labels
node_labels <- c("Before Uveitis", "After Uveitis", unique(condition_counts$Condition))

# Step 2: For each condition, attach counts for "Before Uveitis" and "After Uveitis"
for (i in 3:length(node_labels)) {
  condition_name <- node_labels[i]  # The condition name
  
  # Get the value of "Before Uveitis" (if exists)
  before_value <- condition_counts$Count[condition_counts$Position == "Before Uveitis" & condition_counts$Condition == condition_name]
  
  # Get the value of "After Uveitis" (if exists)
  after_value <- condition_counts$Count[condition_counts$Position == "After Uveitis" & condition_counts$Condition == condition_name]
  
  # Update the label to show the count for each position
  # If the "Before Uveitis" count exists, show it; otherwise, set it to 0
  if (length(before_value) > 0) {
    node_labels[i] <- paste0(condition_name, " (Before: ", before_value, ")")
  } else {
    node_labels[i] <- paste0(condition_name, " (Before: 0)")
  }
  
  # For the "After Uveitis" count, if it exists, append it to the label
  if (length(after_value) > 0) {
    node_labels[i] <- paste0(node_labels[i], " (After: ", after_value, ")")
  } else {
    node_labels[i] <- paste0(node_labels[i], " (After: 0)")
  }
}

# Step 3: Define a color palette for each node
colors <- c(
  "#1f77b4",  # Color for "Before Uveitis"
  "#ff7f0e",  # Color for "After Uveitis"
  "#2ca02c",  # Color for AS
  "#d62728",  # Color for Bechet
  "#9467bd",  # Color for CS
  "#8c564b",  # Color for MS
  "#e377c2",  # Color for RA
  "#7f7f7f",  # Color for SAR
  "#bcbd22",  # Color for SLE
  "#17becf",  # Color for UC
  "#aec7e8",  # Color for WGA
  "#ffbb78",  # Color for crohn
  "#98df8a"   # Color for psoriasis
)

# Step 4: Prepare the Sankey diagram
sankey_plot <- plot_ly(
  type = "sankey",
  domain = list(x = c(0, 1), y = c(0, 1)),
  orientation = "h",
  node = list(
    pad = 15,
    thickness = 20,
    line = list(color = "black", width = 0.5),
    label = node_labels,  # Use updated node labels with counts
    color = colors         # Assign each node a unique color
  ),
  link = list(
    source = links$Source,
    target = links$Target,
    value = links$Value
  )
) %>%
  layout(
    title = "Sankey Diagram with Before/After Uveitis Counts in All or Unrelated individuals",
    font = list(size = 12)
  )

# Display the plot
sankey_plot
