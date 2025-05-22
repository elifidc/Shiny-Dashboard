

server <- function(input, output, session) {

  
  
  # Populate animalType dropdown after df_clean is available
  updateSelectInput(session, "animalType",
                    choices = c("All", sort(unique(df_clean$`Animal Type Grouped`))),
                    selected = "All"
  )
  
  
  
  
  
  
  # 
  default_end <- max(df_clean$`Intake Date`, na.rm = TRUE)
  default_start <- default_end %m-% months(6)  
  
  
  filtered_data <- reactive({
    req(input$animalType, input$dateRange)
    
    data <- df_clean
    
    if (input$animalType != "All") {
      data <- data %>% filter(`Animal Type` == input$animalType)
    }
    
    data <- data %>%
      filter(`Intake Date` >= input$dateRange[1],
             `Intake Date` <= input$dateRange[2])
    
    return(data)
  })
  
  
  
  
  full_latest_intake <- max(df_clean$`Intake Date`, na.rm = TRUE)
  
  output$totalAnimals <- renderValueBox({
    count <- nrow(df_clean)
    valueBox(
      value = scales::comma(count),
      subtitle = "Total Pets Cared For",
      icon = icon("paw"),
      color = "red",
      width = 3
    )
  })
  
  output$adoptionsInRange <- renderValueBox({
    count <- df_clean %>%
      filter(`Outcome Type` == "Adoption") %>%
      filter(`Outcome Date` >= max(df_clean$`Outcome Date`, na.rm = TRUE) - 30) %>%
      nrow()
    
    valueBox(
      value = count,
      subtitle = "Recent Adoptions",
      icon = icon("home"),
      color = "teal"
    )
  })
  
  
  
  output$recentAdoptionBox <- renderValueBox({
    recent_adoption <- df_clean %>%
      filter(`Outcome Type` == "Adoption", !is.na(`Outcome Date`), !is.na(`Intake Date`)) %>%
      arrange(desc(`Outcome Date`)) %>%
      mutate(`Length of Stay` = as.numeric(`Outcome Date` - `Intake Date`)) %>%
      slice(1)
    
    name <- recent_adoption$`Animal Name`
    type <- recent_adoption$`Animal Type`
    los <- recent_adoption$`Length of Stay`
    
    valueBox(
      value = ifelse(name == "", "(No Name)", name),
      subtitle = paste("Recently Adopted", type, "-", los, "days in shelter"),
      icon = icon("heart"),
      color = "yellow"
    )
  })
  
  
  output$catCountBox <- renderValueBox({
    cat_count <- df_clean %>%
      filter(`Animal Type` == "Cat", `Outcome Type` == "Adoption") %>%
      nrow()
    
    valueBox(
      value = scales::comma(cat_count),
      subtitle = "All-Time Cat Adoptions",
      icon = icon("cat"),
      color = "fuchsia"
    )
  })
  output$animalTypeBar <- renderPlotly({
    p <- df_clean %>%
      filter(is.na(`Outcome Date`)) %>%
      count(`Animal Type`) %>%
      ggplot(aes(
        x = n,
        y = reorder(`Animal Type`, n),
        fill = `Animal Type`,
        text = paste0("Animal Type: ", `Animal Type`, "<br>Count: ", n)
      )) +
      geom_col() +
      scale_fill_manual(values = c(
        "Cat" = "#C76387",       # Soft Navy
        "Dog" = "steelblue",       # Sage Green
        "Rabbit" = "#95cf92",    # Coral
        "Guinea Pig" = "#e6c994",# Rose Clay
        "Bird" = "#9656a2",      # Muted Purple
        "Other" = "#888888"      # Neutral Gray
      )) +
      labs(
        x = "Count", y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0))
    
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  
  output$outcomeLollipop <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(!is.na(`Outcome Date`), !is.na(`Outcome Type`)) %>%
      filter(`Outcome Date` >= Sys.Date() - 30) %>%
      count(`Outcome Type`) %>%
      arrange(desc(n)) %>%
      mutate(`Outcome Type` = forcats::fct_reorder(`Outcome Type`, n))
    
    p <- ggplot(plot_data, aes(
      x = n,
      y = `Outcome Type`,
      text = paste0("Outcome Type: ", `Outcome Type`, "<br>Count: ", n)
    )) +
      geom_segment(aes(x = 0, xend = n, yend = `Outcome Type`), color = "#B0C4DE") +
      geom_point(size = 6, color = "#4F9484") +
      theme_minimal(base_size = 13) +
      labs( x = "Number of Animals", y = "")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  output$intakeReasons <- renderPlotly({
    plot_data <- df_clean %>%
      filter(!is.na(`Reason for Intake`)) %>%
      count(`Reason for Intake`) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    p <- ggplot(plot_data, aes(x = n, y = reorder(`Reason for Intake`, n))) +
      geom_col(fill = "#FF6347") + 
      labs(title = "Top 10 Intake Reasons", x = "Count", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 12))
    
    ggplotly(p) %>% layout(margin = list(l = 70))
  })
  
  
  
  
  output$dogCountBox <- renderValueBox({
    dog_count <- df_clean %>%
      filter(`Animal Type` == "Dog", `Outcome Type` == "Adoption") %>%
      nrow()
    
    valueBox(
      value = scales::comma(dog_count),
      subtitle = "All-Time Dog Adoptions",
      icon = icon("dog"),
      color = "light-blue"
    )
  })
  
  
  
  output$avgStay <- renderValueBox({
    avg <- df_clean %>%
      filter(!is.na(`Outcome Date`)) %>%
      mutate(days = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days"))) %>%
      summarise(avg = mean(days, na.rm = TRUE)) %>%
      pull(avg) %>%
      round(1)
    valueBox(paste0(avg, " days"), "Avg Length of Stay", icon = icon("calendar"), color = "purple")
  })
  
  output$currentInShelter <- renderValueBox({subtitle = "Total Pets Cared For"
  count <- df_clean %>%
    filter(is.na(`Outcome Date`)) %>%
    nrow()
  valueBox(count, "Currently in Shelter", icon = icon("dog"), color = "olive")
  })
  
  output$overdueAnimals <- renderValueBox({
    count <- df_clean %>%
      filter(is.na(`Outcome Date`)) %>%
      mutate(days_in_shelter = as.numeric(full_latest_intake - `Intake Date`)) %>%
      filter(days_in_shelter > 30) %>%
      nrow()
    icon_tag <- icon("clock")
    colored_icon <- tagAppendAttributes(icon_tag, style = "color:#C0392B")
    valueBox(
      value = count,
      subtitle = "Currently In Shelter > 30 Days",
      icon = colored_icon,
      color = "navy"
    )
  })
  
  
  
  output$topIntakeType <- renderValueBox({
    top <- df_clean %>%
      filter(!is.na(`Intake Type`)) %>%
      count(`Intake Type`) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(`Intake Type`)
    valueBox(top, "Top Intake Type", icon = icon("sign-in-alt"), color = "navy")
  })
  
  
  
  
  
  
  # Overview tab plots
  
  
  output$intakeOutcomeTrend <- renderPlotly({
    intake_counts <- df_clean %>%
      mutate(month = floor_date(`Intake Date`, "month")) %>%
      count(month) %>%
      rename(Intake = n, date = month)
    
    outcome_counts <- df_clean %>%
      filter(!is.na(`Outcome Date`)) %>%
      mutate(month = floor_date(`Outcome Date`, "month")) %>%
      count(month) %>%
      rename(Outcome = n, date = month)
    
    combined <- full_join(intake_counts, outcome_counts, by = "date") %>%
      arrange(date) %>%
      mutate(
        Intake = replace_na(Intake, 0),
        Outcome = replace_na(Outcome, 0),
        CumulativeIntake = cumsum(Intake),
        CumulativeOutcome = cumsum(Outcome)
      )
    
    p <- ggplot(combined, aes(x = date)) +
      geom_line(aes(y = CumulativeIntake, color = "Cumulative Intake"), linewidth = 1) +
      geom_point(aes(y = CumulativeIntake, color = "Cumulative Intake"), size = 2) +
      geom_line(aes(y = CumulativeOutcome, color = "Cumulative Outcome"), linewidth = 1) +
      geom_point(aes(y = CumulativeOutcome, color = "Cumulative Outcome"), size = 2) +
      scale_color_manual(values = c(
        "Cumulative Intake" = "#3498db",
        "Cumulative Outcome" = "#2ecc71"
      )) +
      theme_minimal() +
      labs(x = "Month", y = "Cumulative Count", color = "") +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% layout(legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.2
    ))
  })
  
  
  output$animalTypePie <- renderPlotly({
    library(plotly)
    library(dplyr)
    
    animal_data <- df_clean %>%
      count(`Animal Type`) %>%
      mutate(
        percent = round(n / sum(n) * 100, 1),
        label = paste0(`Animal Type`, ": ", percent, "%")
      )
    
    plot_ly(
      animal_data,
      labels = ~ `Animal Type`,
      values = ~ n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      marker = list(line = list(
        color = '#FFFFFF', width = 1
      ))
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = 'v',
          x = 1,
          y = 0.5
        ),
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 0
        ),
        title = list(text = "Animal Type Distribution", x = 0.5)
      )
  })
  
  
  output$outcomeTypePie <- renderPlotly({
    outcome_counts <- filtered_data() %>%
      filter(!is.na(`Outcome Type`)) %>%
      count(`Outcome Type`) %>%
      arrange(desc(n)) %>%
      mutate(rank = row_number(),
             label = ifelse(rank <= 5, `Outcome Type`, "Other")) %>%
      group_by(label) %>%
      summarise(n = sum(n)) %>%
      arrange(desc(n)) %>%
      mutate(pct = n / sum(n))
    
    plot_ly(
      outcome_counts,
      labels = ~ label,
      values = ~ n,
      type = 'pie',
      textinfo = 'label+percent',
      marker = list(colors = RColorBrewer::brewer.pal(min(
        9, nrow(outcome_counts)
      ), "Set3"))
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center"
        ),
        margin = list(
          l = 20,
          r = 20,
          t = 20,
          b = 20
        )
      )
  })
  
  
  # Intake Analysis tab plots
  output$intakeReasons <- renderPlotly({
    reason_counts <- filtered_data() %>%
      filter(!is.na(`Reason for Intake`)) %>%
      mutate(`Reason for Intake` = stringr::str_to_upper(`Reason for Intake`)) %>%
      mutate(`Reason for Intake` = stringr::str_squish(`Reason for Intake`)) %>%
      mutate(`Reason for Intake` = case_when(
        `Reason for Intake` == "MOVE" ~ "Owner Moving",
        `Reason for Intake` == "OWNER PROB" ~ "Owner Problem",
        `Reason for Intake` == "ILL" ~ "Medical Issues",
        `Reason for Intake` == "POOR HELTH" ~ "Medical Issues",
        `Reason for Intake` == "TOO MANY" ~ "Too Many Animals",
        `Reason for Intake` == "AGG PEOPLE" ~ "Aggressive Toward People",
        `Reason for Intake` == "NO TIME" ~ "No Time",
        TRUE ~ str_to_title(`Reason for Intake`)
      )) %>%
      count(`Reason for Intake`) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10) %>%
      mutate(`Reason for Intake` = forcats::fct_reorder(`Reason for Intake`, n))
    
    p <- ggplot(reason_counts, aes(x = n, y = `Reason for Intake`, fill = `Reason for Intake`,
                                   text = paste0("Reason: ", `Reason for Intake`, "<br>Count: ", n))) +
      geom_col() +
      theme_minimal() +
      labs(title = "Top 10 Intake Reasons", x = "Count", y = "") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$intakeConditionsOverview <- renderPlotly({
    condition_counts <- df_clean %>%
      filter(!is.na(`Intake Condition`), !is.na(`Intake Date`)) %>%
      filter(`Intake Date` >= Sys.Date() - 30) %>%
      count(grouped_condition, name = "n") %>%
      arrange(desc(n)) %>%
      mutate(grouped_condition = factor(grouped_condition, levels = rev(unique(grouped_condition)))) 
    
    p <- ggplot(condition_counts, aes(
      x = n,
      y = grouped_condition,
      fill = grouped_condition,
      text = paste0("Condition: ", grouped_condition, "<br>Count: ", n)
    )) +
      geom_col() +
      theme_minimal() +
      labs(title = "Grouped Intake Conditions (Last 30 Days)", x = "Count", y = NULL) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
    
  
  
  output$intakeConditionsIntakeTab <- renderPlotly({
    condition_counts <- filtered_data() %>%
      filter(!is.na(`Intake Condition`)) %>%
      mutate(`Intake Condition` = stringr::str_to_upper(`Intake Condition`)) %>%
      mutate(`Intake Condition` = str_squish(`Intake Condition`)) %>%
      mutate(grouped_condition = case_when(
        # Illness
        `Intake Condition` %in% c("Ill Mild", "Ill Moderate", "Ill Moderatete") ~ "Illness (Mild/Moderate)",
        `Intake Condition` == "Ill Severe" ~ "Illness (Severe)",
        
        # Injury
        `Intake Condition` %in% c("Injured Mild", "Injured Moderate", "I/I Report") ~ "Injured (Mild/Moderate)",
        `Intake Condition` == "Injured Severe" ~ "Injured (Severe)",
        
        # Behavior
        `Intake Condition` %in% c("Behavior Mild", "Behavior Moderate", "Behavior Severe") ~ "Behavioral Issues",
        
        # Other medical / wellness
        `Intake Condition` == "Under Age/Weight" ~ "Underage/Underweight",
        `Intake Condition` == "Normal" ~ "Normal",
        `Intake Condition` %in% c("Fractious", "Aged", "Welfare Seizures", "Feral") ~ "Other",
        
        # Fallback
        TRUE ~ `Intake Condition`
      ))%>%
      count(`Intake Condition`) %>%
      arrange(desc(n)) %>%
      mutate(`Intake Condition` = forcats::fct_reorder(`Intake Condition`, n))
    
    p <- ggplot(condition_counts, aes(x = n, y = `Intake Condition`, fill = `Intake Condition`,
                                      text = paste0("Condition: ", `Intake Condition`, "<br>Count: ", n))) +
      geom_col() +
      theme_minimal() +
      labs(title = "Grouped Intake Conditions (Filtered)", x = "Count", y = "") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  monthly_intakes <- reactive({
    top_animals <- filtered_data() %>%
      count(`Animal Type`, sort = TRUE) %>%
      slice_head(n = 3) %>%
      pull(`Animal Type`)
    
    filtered_data() %>%
      filter(!is.na(`Intake Date`)) %>%
      filter(`Animal Type` %in% top_animals) %>%
      mutate(Month = lubridate::floor_date(`Intake Date`, "month")) %>%
      count(Month, `Animal Type`)
  })
  
  output$monthlyIntakePlot <- renderPlotly({
    req(monthly_intakes())
    
    p <- ggplot(monthly_intakes(), aes(x = Month, y = n, color = `Animal Type`)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      labs(
        x = NULL,
        y = "Monthly Intake Count"
      ) +
      scale_color_manual(values = c(
        "Cat" = "#EC4899",   
        "Dog" = "#3B82F6",   
        "Other" = "#15803D"  
      )) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
    
  })
  
  
  
  output$ageAtIntake <- renderPlotly({
    age_data <- filtered_data() %>%
      filter(!is.na(age_at_intake) &
               age_at_intake >= 0 & age_at_intake <= 20)
    
    p <- ggplot(age_data, aes(x = age_at_intake)) +
      geom_histogram(
        binwidth = 1,
        fill = "#3498db",
        color = "white"
      ) +
      theme_minimal() +
      labs(x = "Age (Years)", y = "Count")
    
    ggplotly(p)
  })
  
  output$genderDistribution <- renderPlotly({
    gender_data <- filtered_data() %>%
      count(Sex) %>%
      mutate(percentage = n / sum(n) * 100)
    
    plot_ly(
      gender_data,
      x = ~ Sex,
      y = ~ percentage,
      type = "bar",
      marker = list(color = "#9b59b6"),
      text = ~ paste0(round(percentage, 1), "%"),
      textposition = "auto"
    ) %>%
      layout(yaxis = list(title = "Percentage"),
             xaxis = list(title = ""))
  })
  
  
  outcome_colors <- c(
    "Adoption"               = "#5EC576",
    "Rescue Organization"    = "#74a892",
    "Transferred"            = "#8bc2e4",
    "Return To Owner"        = "#FFD966",
    "Return To Wild Habitat" = "#E69138",
    "Euthanasia"             = "#d46a6a",
    "Died (Not Euthanized)"  = "#CC4125",
    "Community Cat"          = "#9c78fe",
    "Foster"                 = "#F6B26B",
    "Other"                  = "#A0AEC0"
  )
  
  output$outcomeByAnimal <- renderPlotly({
    data <- filtered_data() %>%
      filter(!is.na(`Outcome Type`), `Outcome Type` != "DUPLICATE") %>%
      mutate(
        `Outcome Grouped` = case_when(
          toupper(`Outcome Type`) %in% c("DISPOSAL", "DIED") ~ "Died (Not Euthanized)",
          toupper(`Outcome Type`) == "RESCUE" ~ "Rescue Organization",
          toupper(`Outcome Type`) %in% c("TRANSFER", "TRANSPORT") ~ "Transferred",
          toupper(`Outcome Type`) == "RETURN TO OWNER" ~ "Return To Owner",
          toupper(`Outcome Type`) == "RETURN TO WILD HABITAT" ~ "Return To Wild Habitat",
          toupper(`Outcome Type`) == "COMMUNITY CAT" ~ "Community Cat",
          toupper(`Outcome Type`) == "FOSTER" ~ "Foster",
          toupper(`Outcome Type`) == "ADOPTION" ~ "Adoption",
          toupper(`Outcome Type`) == "EUTHANASIA" ~ "Euthanasia",
          TRUE ~ "Other"
        )
      ) %>%
      count(`Animal Type Grouped`, `Outcome Grouped`) %>%
      group_by(`Animal Type Grouped`) %>%
      mutate(percent = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(
        `Animal Type Grouped` = factor(
          `Animal Type Grouped`,
          levels = c("Dog", "Cat", "Rabbit", "Bird", "Reptile", "Wild", "Other")
        ),
        `Outcome Grouped` = factor(`Outcome Grouped`, levels = names(outcome_colors))
      )
    
    p <- ggplot(data, aes(
      x = `Animal Type Grouped`,
      y = percent,
      fill = `Outcome Grouped`,
      text = paste0(
  "Animal Type: ", `Animal Type Grouped`, "<br>",
  "Outcome: ", `Outcome Grouped`, "<br>",
  "Percent: ", round(percent, 1), "%"
)
    )) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = outcome_colors, drop = FALSE) +
      theme_minimal(base_size = 14) +
      labs(
        x = NULL,
        y = "Percentage of Outcomes",
        title = "Outcome Breakdown by Animal Type",
        fill = "Outcome Type"
      ) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$monthlyOutcomePlot <- renderPlotly({
    req(filtered_data())
    
    top_outcomes <- filtered_data() %>%
      count(`Outcome Type`, sort = TRUE) %>%
      slice_max(n, n = 5) %>%
      pull(`Outcome Type`)
    
    plot_data <- filtered_data() %>%
      filter(!is.na(`Outcome Date`)) %>%
      filter(`Outcome Type` %in% top_outcomes) %>%
      mutate(
        Outcome = case_when(
          `Outcome Type` == "RESCUE" ~ "Rescue Organization",
          `Outcome Type` %in% c("TRANSFER", "TRANSPORT") ~ "Transferred",
          TRUE ~ str_to_title(`Outcome Type`)
        ),
        Month = floor_date(`Outcome Date`, "month")
      ) %>%
      count(Month, Outcome, name = "n") %>%
      mutate(
        text = paste0(
          "Outcome Type: ", Outcome,
          "<br>Month: ", format(Month, "%b %Y"),
          "<br>Count: ", n
        )
      )
    
    p <- ggplot(plot_data, aes(
      x = Month,
      y = n,
      color = Outcome,
      group = Outcome,
      text = text
    )) +
      geom_line(size = 1.2) +
      scale_color_manual(values = outcome_colors) +
      labs(
        title = "Recent Monthly Outcome Trends",
        x = "Month", y = "Count", color = "Outcome"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  

  ## Trying to make a currently in shelter for how long distribution aka, for the animals still in the shelter, how long have they been in there?
  sheltered_now <- df_clean %>%
    filter(is.na(`Outcome Date`))
  sheltered_now <- df_clean %>%
    filter(is.na(`Outcome Date`)) %>%
    mutate(days_in_shelter = as.numeric(Sys.Date() - `Intake Date`))
  sheltered_now <- sheltered_now %>%
    mutate(stay_bucket = case_when(
      days_in_shelter <= 7 ~ "<1 week",
      days_in_shelter <= 14 ~ "1–2 weeks",
      days_in_shelter <= 30 ~ "2–4 weeks",
      days_in_shelter <= 60 ~ "1–2 months",
      days_in_shelter <= 90 ~ "2–3 months",
      TRUE ~ "3+ months"
    ))
  
  duration_summary <- sheltered_now %>%
    count(stay_bucket) %>%
    mutate(stay_bucket = factor(stay_bucket, levels = c(
      "<1 week", "1–2 weeks", "2–4 weeks", "1–2 months", "2–3 months", "3+ months"
    )))
  
  #plotting duration summary
  
  output$durationInShelterPlot <- renderPlotly({
    plot_data <- df_clean %>%
      filter(is.na(`Outcome Date`)) %>%
      mutate(days_in_shelter = as.numeric(Sys.Date() - `Intake Date`)) %>%
      mutate(stay_bucket = case_when(
        days_in_shelter < 7 ~ "<1 week",
        days_in_shelter < 14 ~ "1–2 weeks",
        days_in_shelter < 28 ~ "2–4 weeks",
        days_in_shelter < 60 ~ "1–2 months",
        days_in_shelter < 90 ~ "2–3 months",
        TRUE ~ "3+ months"
      )) %>%
      count(stay_bucket) %>%
      mutate(stay_bucket = factor(stay_bucket,
                                  levels = c("<1 week", "1–2 weeks", "2–4 weeks",
                                             "1–2 months", "2–3 months", "3+ months")))
    
    p <- ggplot(plot_data, aes(
      x = stay_bucket, y = n, fill = stay_bucket,
      text = paste0("Duration: ", stay_bucket, "<br>Count: ", n)
    )) +
      geom_col(color = "#f5f5f5", linewidth = 0.4) +
      scale_fill_manual(values = c(
        "<1 week"     = "#FFF3CD",   # pale gold
        "1–2 weeks"   = "#FFE699",   # light gold
        "2–4 weeks"   = "#FFD966",   # yellow-gold
        "1–2 months"  = "#F6B26B",   # soft orange
        "2–3 months"  = "#E69138",   # deeper orange
        "3+ months"   = "#CC4125"    # rich deep red
      )) +
      theme_minimal(base_size = 13) + scale_x_discrete(expand = c(0, 0)) +
      labs(
        x = "Time at Shelter", y = "Number of Animals"
      ) +
      theme(legend.position = "none") 
    
    ggplotly(p, tooltip = "text")
    
    
  })
  
  
  
  
  
  output$outcomeSuccess <- renderPlotly({
    # Define successful outcomes (e.g., Adoption, Return to Owner)
    success_data <- filtered_data() %>%
      filter(!is.na(`Outcome Type`)) %>%
      mutate(success = `Outcome Type` %in% c("Adoption", "Return to Owner", "Transfer")) %>%
      count(`Animal Type`, success) %>%
      group_by(`Animal Type`) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      filter(success == TRUE)
    
    p <- ggplot(success_data,
                aes(x = `Animal Type`, y = percentage, fill = `Animal Type`)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "", y = "Success Rate (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  
  
  
  
  
  
  
  # Length of Stay tab plots
  output$losDistribution <- renderPlotly({
    los_data <- filtered_data() %>%
      filter(!is.na(`Outcome Date`) & !is.na(`Intake Date`)) %>%
      mutate(los = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days"))) %>%
      filter(los >= 1 & los <= 60)
    
    p <- ggplot(los_data, aes(x = los)) +
      geom_histogram(binwidth = 1.5, fill = "#f1c40f", color = "white", boundary = 0) +
      geom_vline(aes(xintercept = median(los)), color = "red", linetype = "dashed", linewidth = 1) +
      theme_minimal() +
      labs(
        title = "Length of Stay (1–60 Days)",
        x = "Length of Stay (Days)", 
        y = "Animal Count"
      ) +
      scale_x_continuous(breaks = seq(0, 60, by = 5), expand = c(0.01, 0)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    
    ggplotly(p)
  })
  
  
  

  output$losByType <- renderPlotly({
    los_type <- filtered_data() %>%
      filter(!is.na(`Outcome Date`)) %>%
      mutate(los = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days"))) %>%
      filter(los >= 0 & los <= 120) %>%
      filter(!`Animal Type` %in% c("Other", "Wild")) %>%
      group_by(`Animal Type`) %>%
      summarise(avg_los = mean(los, na.rm = TRUE)) %>%
      mutate(
        `Animal Type` = forcats::fct_reorder(`Animal Type`, avg_los, .desc = TRUE),
        text = paste0("Animal Type: ", `Animal Type`, "<br>",
                      "Avg LOS: ", round(avg_los, 1), " days")
      )
    
    p <- ggplot(los_type, aes(x = `Animal Type`, y = avg_los, fill = `Animal Type`, text = text)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c(
        "Rabbit"     = "#D55E00",
        "Dog"        = "#F0E442",
        "Guinea Pig" = "#009E73",
        "Livestock"  = "#56B4E9",
        "Cat"        = "#0072B2",
        "Reptile"    = "#CC79A7",
        "Bird"       = "#999999"
      )) +
      theme_minimal() +
      labs(
        title = "Average Length of Stay by Animal Type",
        x = "", y = "Average Length of Stay (Days)"
      )
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  
  output$losByOutcome <- renderPlotly({
    los_outcome <- filtered_data() %>%
      filter(!is.na(`Outcome Date`)) %>%
      mutate(los = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days"))) %>%
      filter(los >= 0 & los <= 120) %>%  
      group_by(`Outcome Type`) %>%
      summarise(avg_los = mean(los, na.rm = TRUE))
    
    p <- ggplot(los_outcome,
                aes(x = `Outcome Type`, y = avg_los, fill = `Outcome Type`)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "", y = "Average Length of Stay (Days)") +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  output$losOverTime <- renderPlotly({
    los_time <- filtered_data() %>%
      filter(!is.na(`Outcome Date`)) %>%
      mutate(los = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days")),
             month = floor_date(`Outcome Date`, "month")) %>%
      filter(los >= 0 & los <= 120) %>%  
      group_by(month) %>%
      summarise(avg_los = mean(los, na.rm = TRUE))
    
    p <- ggplot(los_time, aes(x = month, y = avg_los)) +
      geom_line(color = "#e67e22") +
      geom_point(color = "#e67e22", size = 3) +
      theme_minimal() +
      labs(x = "Month", y = "Average Length of Stay (Days)")
    
    ggplotly(p)
  })
  
  # Geographic Analysis tab
  output$intakeMap <- renderLeaflet({
    # Filter out rows with missing coordinates
    map_data <- filtered_data() %>%
      filter(!is.na(latitude) & !is.na(longitude))
    
    # Create a color palette based on Intake Type
    pal <- colorFactor(palette = "Set1", domain = map_data$`Intake Type`)
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~ longitude,
        ~ latitude,
        color = ~ pal(`Intake Type`),
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~ paste(
          "<b>Animal Type:</b>",
          `Animal Type`,
          "<br>",
          "<b>Animal Name:</b>",
          `Animal Name`,
          "<br>",
          "<b>Intake Type:</b>",
          `Intake Type`,
          "<br>",
          "<b>Intake Date:</b>",
          `Intake Date`
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~ `Intake Type`,
        title = "Intake Type",
        opacity = 1
      )
  })
  
  output$jurisdictionHeat <- renderPlotly({
    juris_data <- filtered_data() %>%
      count(Jurisdiction) %>%
      arrange(desc(n)) %>%
      head(20) 
    
    p <- ggplot(juris_data, aes(
      x = reorder(Jurisdiction, n),
      y = n,
      fill = n
    )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      labs(x = "", y = "Number of Animals") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  output$successRateBox <- renderValueBox({
    df <- filtered_data() %>%
      filter(!is.na(`Outcome Type`)) %>%
      mutate(outcome_group = case_when(
        `Outcome Type` %in% c("Euthanasia", "Died", "Disposal", "Missing", "Duplicate") ~ "Failure",
        TRUE ~ "Success"
      ))
    
    pct_success <- round(mean(df$outcome_group == "Success", na.rm = TRUE) * 100, 1)
    
    valueBox(
      paste0(pct_success, "%"),
      "Successful Outcomes",
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$failureRateBox <- renderValueBox({
    df <- filtered_data() %>%
      filter(!is.na(`Outcome Type`)) %>%
      mutate(outcome_group = case_when(
        `Outcome Type` %in% c("Euthanasia", "Died", "Disposal", "Missing", "Duplicate") ~ "Failure",
        TRUE ~ "Success"
      ))
    
    pct_failure <- round(mean(df$outcome_group == "Failure", na.rm = TRUE) * 100, 1)
    
    valueBox(
      paste0(pct_failure, "%"),
      "Failed Outcomes",
      icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  
  output$successVsFailure <- renderPlotly({
    outcome_data <- filtered_data() %>%
      mutate(outcome_group = case_when(
        `Outcome Type` %in% c("Euthanasia", "Died", "Disposal", "Missing", "Duplicate") ~ "Failure",
        `Outcome Type` %in% c(
          "Adoption", "Return To Owner", "Transfer", "Rescue", 
          "Return To Wild Habitat", "Return To Rescue", "Transport", 
          "Community Cat", "Trap, Neuter, Release", "Homefirst", 
          "Foster To Adopt", "Shelter, Neuter, Return"
        ) ~ "Success",
        TRUE ~ NA_character_
      )) %>%
      count(outcome_group) %>%
      mutate(pct = round(100 * n / sum(n), 1))
    
    p <- ggplot(outcome_data, aes(x = outcome_group, y = pct, fill = outcome_group,
                                  text = paste0(outcome_group, ": ", pct, "%"))) +
      geom_col(width = 0.5) +
      labs(title = "Success vs Failure Outcomes", x = NULL, y = "Percentage") +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  output$totalIntakes <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Intakes",
      icon = icon("paw"),
      color = "olive"
    )
  })
  
  output$avgDailyIntakes <- renderValueBox({
    filtered <- filtered_data()
    
    total <- nrow(filtered)
    days <- as.numeric(difftime(max(filtered$`Intake Date`), min(filtered$`Intake Date`), units = "days")) + 1
    
    avg <- total / days
    
    valueBox(
      value = round(avg, 1),
      subtitle = "Avg Daily Intakes",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  
  
  output$mostCommonIntakeType <- renderValueBox({
    common <- filtered_data() %>%
      count(`Intake Type`, sort = TRUE) %>%
      slice(1) %>%
      pull(`Intake Type`)
    valueBox(
      value = common,
      subtitle = "Most Common Intake Type",
      icon = icon("sign-in-alt"),
      color = "navy"
    )
  })
  
  output$mostCommonCondition <- renderValueBox({
    common <- filtered_data() %>%
      count(`Intake Condition`, sort = TRUE) %>%
      slice(1) %>%
      pull(`Intake Condition`)
    valueBox(
      value = common,
      subtitle = "Most Common Intake Condition",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$intakeTypePlot <- renderPlotly({
    plot <- filtered_data() %>%
      count(`Intake Type`, sort = TRUE) %>%
      ggplot(aes(x = n, y = reorder(`Intake Type`, n), fill = `Intake Type`)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Count", y = NULL, title = "Intake Type Distribution") +
      scale_fill_manual(values = c(
        "Stray" = "#3B82F6",             # Deep blue
        "Return" = "#60A5FA",            # Mid blue
        "Wildlife" = "#BFDBFE",          # Pale blue
        "Owner Surrender" = "#EF9A9A",   # Muted coral
        "Welfare Seized" = "#D32F2F",    # Rich red
        "Quarantine" = "#FACC15",        # Gold amber
        "Other" = "#A0AEC0"              # Muted neutral gray-blue
      )) +  # <-- This closing paren was missing
      theme_minimal()
    
    ggplotly(plot)
  })
  
  
  output$intakeSubtypePlot <- renderPlotly({
    plot <- filtered_data() %>%
      count(`Intake Type`, `Intake Subtype`) %>%
      ggplot(aes(x = `Intake Type`, y = n, fill = `Intake Subtype`, text = paste(n))) +
      geom_col(position = "stack") +
      labs(x = NULL, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot, tooltip = "text")
  })
  
  output$groupedConditionsPlot <- renderPlotly({
    plot_data <- filtered_data() %>%
      mutate(grouped_condition = case_when(
        # Illness
        `Intake Condition` %in% c("Ill Mild", "Ill Moderate", "Ill Moderatete") ~ "Illness (Mild/Moderate)",
        `Intake Condition` == "Ill Severe" ~ "Illness (Severe)",
        
        # Injury
        `Intake Condition` %in% c("Injured Mild", "Injured Moderate", "I/I Report") ~ "Injured (Mild/Moderate)",
        `Intake Condition` == "Injured Severe" ~ "Injured (Severe)",
        
        # Behavior
        `Intake Condition` %in% c("Behavior Mild", "Behavior Moderate", "Behavior Severe") ~ "Behavioral Issues",
        
        # Other medical / wellness
        `Intake Condition` == "Under Age/Weight" ~ "Underage/Underweight",
        `Intake Condition` == "Normal" ~ "Normal",
        `Intake Condition` %in% c("Fractious", "Aged", "Welfare Seizures", "Feral") ~ "Other",
        
        # Fallback
        TRUE ~ `Intake Condition`
      )) %>%
      count(grouped_condition) %>%
      arrange(desc(n)) 
    
    color_palette <- c(
      "Underage/Underweight"    = "#E69F00",  # golden orange
      "Normal"                  = "#56B4E9",  # light blue
      "Illness (Mild/Moderate)" = "#009E73",  # teal
      "Injured (Mild/Moderate)" = "#F0E442",  # yellow
      "Injured (Severe)"        = "#D55E00",  # reddish orange
      "Illness (Severe)"        = "#CC79A7",  # pink
      "Other"                   = "#999999",  # gray
      "Behavioral Issues"       = "#0072B2"   # blue
    )
    
    
    
    p <- ggplot(plot_data, aes(x = reorder(grouped_condition, n), y = n, fill = grouped_condition)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = color_palette) +
      labs(
        x = NULL,
        y = "Count",
        title = "Top Intake Conditions"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  
  intake_type_colors <- c(
    "Stray"            = "#0072B2",  # blue
    "Wildlife"         = "#D55E00",  # reddish orange
    "Owner Surrender"  = "#009E73",  # teal
    "Welfare Seized"   = "#CC79A7",  # pink
    "Return"           = "#E69F00",  # golden orange
    "Quarantine"       = "#F0E442",  # yellow
    "Other"            = "#999999"   # gray (if applicable)
  )
  
  
  output$intakeTypePlot <- renderPlotly({
    plot_data <- filtered_data() %>%
      count(`Intake Type`, sort = TRUE)
    
    plot <- ggplot(plot_data, aes(x = n, y = reorder(`Intake Type`, n), fill = `Intake Type`)) +
      geom_col(show.legend = FALSE) +
      labs(
        x = "Count",
        y = NULL,
        title = "Most Common Intake Types"
      ) +
      scale_fill_manual(values = intake_type_colors)+  theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    ggplotly(plot)
  })
  
  
  
  
  
  output$quickOutcomes <- renderValueBox({
    data <- filtered_data() %>%
      filter(!is.na(`Intake Date`), !is.na(`Outcome Date`)) %>%
      mutate(los = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days")))
    
    pct_quick <- mean(data$los <= 3, na.rm = TRUE) * 100
    
    valueBox(
      paste0(round(pct_quick), "%"),
      "Outcomes within 3 Days",
      icon = icon("bolt"),
      color = "blue"
    )
  })
  
  output$longStays <- renderValueBox({
    data <- filtered_data() %>%
      filter(!is.na(`Intake Date`), !is.na(`Outcome Date`)) %>%
      mutate(los = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days")))
    
    pct_long <- mean(data$los > 30, na.rm = TRUE) * 100
    
    valueBox(
      paste0(round(pct_long), "%"),
      "Outcomes after 30 Days",
      icon = icon("hourglass-end"),
      color = "orange"
    )
  })
  
  output$avgOutcomeTime <- renderValueBox({
    df <- filtered_data() %>%
      filter(!is.na(`Intake Date`) & !is.na(`Outcome Date`)) %>%
      mutate(LOS = as.numeric(difftime(`Outcome Date`, `Intake Date`, units = "days"))) %>%
      filter(LOS >= 0) %>%
      filter(`Outcome Date` >= input$dateRange[1] & `Outcome Date` <= input$dateRange[2])
    
    avg_los <- round(mean(df$LOS, na.rm = TRUE), 1)
    
    valueBox(
      paste0(avg_los, " days"),
      "Average Time to Outcome",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  
  

}




