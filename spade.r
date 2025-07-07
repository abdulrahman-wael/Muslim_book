
##################################### (1) HELPER FUNCTIONS ########################################
graphing_themes <- function(counts) {
  library(tidyverse)
  library(patchwork)
  
  df <- group_by(counts, ChapterTitleAr) %>%
    mutate(
      ChapterTitleAr = if_else(
      str_detect(ChapterTitleAr, "^كتاب\\b"), 
      str_remove(ChapterTitleAr, "^كتاب\\s*"), 
      ChapterTitleAr                           
      ),
      Contribution = (Count / sum(Count)) * 100,
      IsTop = Contribution == max(Contribution),
      Label = ifelse(IsTop, paste0(Count, "*"), as.character(Count))
    ) %>%
    ungroup()
  
  # Heatmap
  p_heat <- ggplot(df, aes(ChapterTitleAr, NarratorNameAr, fill = Contribution, label = Label)) +
    geom_tile(color = "black", linewidth = 0.5) +
    geom_text(color = "white", fontface = ifelse(df$IsTop, "bold", "plain")) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "النسبة المئوية") +
    labs(
      x = "الموضوع", 
      y = "الراوي",
      title = "مساهمة الرواة في كل موضوع"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), 
    )
  
  p_top <- ggplot(df, aes(ChapterTitleAr, Count)) +
    stat_summary(fun = sum, geom = "bar", fill = "skyblue") +
    labs(y = "عدد الأسانيد", x="") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  final_plot <- p_top / p_heat + 
    plot_layout(
      heights = c(1, 4),         
      widths = c(1, 1)           
    ) & 
    theme(plot.margin = margin(5, 5, 5, 5)
    , aspect.ratio = NULL 
  )
  return(final_plot)
}

applying_spade <- function(sequence_id, event_id, item, input_support, input_confidence, input_lift) {
  library(arulesSequences)

  ready_data <- data.frame(
  item = item
  )

  trans <- as(ready_data, 'transactions')
  transactionInfo(trans)$sequenceID = sequence_id
  transactionInfo(trans)$eventID = event_id

  patterns <- cspade(
                    trans, 
                    parameter = list(support = input_support, maxgap = 1),
                    control = list(verbose=TRUE, tidLists= TRUE)                  
                  )
  rules <- ruleInduction(patterns, confidence = input_confidence, control = list(verbose = TRUE))
  # result <- sort(rules[quality(rules)$lift > input_lift], by = "lift")
  quality(rules)
  return(rules)
} 

generate_sanad_chains <- function(sequence_id, event_id, item) {
  ready_data <- data.frame(
    sequence_id,
    event_id,
    item
  )
  sanad_chains <- ready_data %>%
    arrange(sequence_id, event_id) %>%
    group_by(sequence_id) %>%
    summarise(
      sanad_chain = paste(item, collapse = " -> "),
      chain_length = n(),
      .groups = 'drop'
    ) %>%
    filter(chain_length > 1)
  
  return(sanad_chains)
}

choose <- function(prompt, choices, multiple_choices = TRUE) {
  
  print(prompt)
  print(paste(seq_along(choices), ": ", choices, sep = ""))
  
  instruction <- if (multiple_choices) {
    "Enter one or more numbers (e.g., '1,3'): "
  } else {
    "Enter one number: "
  }
  
  input <- readline(instruction)
  indices <- if (multiple_choices) {
    as.integer(unlist(strsplit(input, "\\s*,\\s*")))
  } else {
    as.integer(input)
  }
  
  # # Validate
  # if (any(is.na(indices)) stop("Invalid: Non-numeric input.")
  if (any(indices < 1 | indices > length(choices))) {
    stop("Invalid: Choices must be between 1 and ", length(choices))
  }
  
  return(indices)
}

##################################### (2) IMPORTING DATA ########################################

library(stringr)


IsnadData <- read.csv(file='/home/abdulrahman/code/Hadith project/Hadith project/Muslim/isnad_data.csv', stringsAsFactors=TRUE)
HadithContentData <- read.csv(file='/home/abdulrahman/code/Hadith project/Hadith project/Muslim/hadith_content_data.csv', stringsAsFactors=TRUE)
NarratorsData <- read.csv(file='/home/abdulrahman/code/Hadith project/Hadith project/Muslim/narrators_data.csv', stringsAsFactors=TRUE)


enter_ids <- "
  Enter the rawi IDs you want to work with (comma separated)
  (it's recommended to pass at most 10 IDs):
"
enter_support <- "
  Enter the minimum support value you want: 
"
enter_confidence <- "
  Enter the minimum confidence level you want: 
"
enter_lift <- "
  Enter the minimum lift value for the data you want: 
"
enter_sanads_filename <- "
  Enter the name of the path and name of (sanads) file
"
enter_rules_filename <- "
  Enter the name of the path and name of (spade rules) file
"
enter_themes_filename <- "
  Enter the name of the path and name of (themes graph) file
"

##################################### (3) APP'S LOGIC ########################################

while(TRUE) {
  # that's the first screen
  welcome_screen_choice = choose(prompt= "Welcome!, here are what you can do in this system:", 
      choices = list("Advanced analysis for some rawis", "General analysis for all", "Exit"), 
      multiple_choices = FALSE)


  if (welcome_screen_choice == 1){
    # (1) the advanced analysis screen
    advanced_analysis_screen_choice = choose(prompt= "For Narrators of your choice you can do the following: ",
      choices = list('Producing a topic graph', 'Producing a (spade) rules file', 'Producing a sanads file'),
      multiple_choices = TRUE)

        # preparing the needed data
        selected_ids = readline(enter_ids)
        selected_ids = as.factor(unlist(strsplit(selected_ids, "\\s*,\\s*")))


        FilteredNarratorsData <- filter(NarratorsData, NarratorID_Mapped %in% selected_ids)

        # relevant_hadiths <- IsnadData %>%
        #   filter(sourceNarratorID %in% selected_ids) %>%
        #   distinct(HadithNo) %>%
        #   pull(HadithNo)

        # merged_data <- HadithContentData %>%
        #   filter(HadithNo %in% relevant_hadiths) %>%
        #   inner_join(IsnadData, by = "HadithNo") %>%
        #   inner_join(NarratorsData %>% rename(sourceNarratorID = NarratorID_Mapped), 
        #             by = "sourceNarratorID") %>%
        #   distinct(HadithNo, SanadNo, NarratorNameAr, ChapterTitleAr, interactionLabel)

        merged_data <- HadithContentData %>%
        inner_join(IsnadData, by = "HadithNo") %>%
        inner_join(
          FilteredNarratorsData %>% rename(sourceNarratorID = NarratorID_Mapped),
          by = "sourceNarratorID"
        )


        sequence_id <- numeric(nrow(merged_data))
        sequence_id[1] <- 1
        for (i in 2:nrow(merged_data)) {
          if (merged_data$SanadNo[i] != merged_data$SanadNo[i-1] | merged_data$HadithNo[i] != merged_data$HadithNo[i-1]) {
            sequence_id[i] <- sequence_id[i-1] + 1
          } 
          
          else {
            sequence_id[i] <- sequence_id[i-1]
          }
        }
        event_id <- as.numeric(str_extract(merged_data$interactionLabel, "(?<=-i)\\d+$"))
        if (sum(is.na(event_id)) > 0) {
          stop("Error: Could not extract event IDs from interactionLabel")
        }
        item <- merged_data$NarratorNameAr


      if (1 %in% advanced_analysis_screen_choice){
        # first option: graph the narrator_theme relations

        # the data needs to be in this format to be graphed
        counts <- merged_data %>%
          count(NarratorNameAr, ChapterTitleAr, name = "Count") %>%
          as.data.frame()
        
        plot <- graphing_themes(counts)

        filename = readline(enter_themes_filename)
        ggsave(filename, plot, width = 16, height = 8, dpi = 300)
      } 
      if (2 %in% advanced_analysis_screen_choice) 
      {
        # second option: producing a rules file using spade
        support = as.numeric(readline(enter_support))
        confidence = as.numeric(readline(enter_confidence))
        lift = as.numeric(readline(enter_lift))

        rules <- applying_spade(sequence_id, event_id, item, support, confidence, lift)

        filename = readline(enter_rules_filename)
        write(rules, file = filename, sep = '\t', row.names=FALSE, quote = FALSE)


      } 
      if (3 %in% advanced_analysis_screen_choice) 
      {
        # third option: Producing a sanads file
        sanads <- generate_sanad_chains(sequence_id, event_id, item)
        filename = readline(enter_sanads_filename)
        write.csv(sanads, file = filename, row.names=FALSE, quote = FALSE)
      }

  } 
  else if (welcome_screen_choice == 2) 
  {
      # (2) the general analysis screen
      general_analysis_screen_choice = choose(prompt= "For All the Narrators you can do the following: ",
      choices = list('Producing a (spade) rules file', 'Producing a sanads file'),
      multiple_choices = TRUE)

      merged_data <- HadithContentData %>%
      inner_join(IsnadData, by = "HadithNo") %>%
      inner_join(
        NarratorsData %>% rename(sourceNarratorID = NarratorID_Mapped), # that's the only difference between the previous data and the current data (using NarratorsData instead of FilteredNarratorsData)
        by = "sourceNarratorID"
      )

      sequence_id <- numeric(nrow(merged_data))
      sequence_id[1] <- 1
      for (i in 2:nrow(merged_data)) {
        if (merged_data$SanadNo[i] != merged_data$SanadNo[i-1] | merged_data$HadithNo[i] != merged_data$HadithNo[i-1]) {
          sequence_id[i] <- sequence_id[i-1] + 1
        } 
        
        else {
          sequence_id[i] <- sequence_id[i-1]
        }
      }
      event_id <- as.numeric(str_extract(merged_data$interactionLabel, "(?<=-i)\\d+$"))
      if (sum(is.na(event_id)) > 0) {
        stop("Error: Could not extract event IDs from interactionLabel")
      }
      item <- merged_data$NarratorNameAr



      if (1 %in% general_analysis_screen_choice ) {
        # first option: producing a rules file using spade
        support = as.numeric(readline(enter_support))
        confidence = as.numeric(readline(enter_confidence))
        lift = as.numeric(readline(enter_lift))

        rules <- applying_spade(sequence_id, event_id, item, support, confidence, lift)

        filename = readline(enter_rules_filename)
        write(rules, file = filename, sep = '\t', row.names=FALSE, quote = FALSE)

      } 
      if (2 %in% general_analysis_screen_choice) 
      {
        # second option: Producing a sanads file
        sanads <- generate_sanad_chains(sequence_id, event_id, item)
        filename = readline(enter_sanads_filename)
        write.csv(sanads, file = filename, row.names=FALSE, quote = FALSE)
      }
  } 
  else if (welcome_screen_choice == 3) 
  {
    break
  }
}