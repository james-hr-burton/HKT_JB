## Importation des librairies
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(plotly)
library(gt)
library(ggplot2)
  

## UI
ui <- dashboardPage(
  # Titre de la page
  dashboardHeader(title = "HKT dashboard"),
  
  # Menu interactif
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  
  # Contenu du dashboard
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              
              h1(strong("Description du jeu de données")),
              
              # Texte et tableau de description
              h3(textOutput("textdesc")),
              h3("Les validations se répartissent dans les types d'utilisateurs suivants :"),
              tableOutput('users_pct_table'),
              
              # Filtre
              h3("Afin d'afiner l'analyse du jeu, vous pouvez filtrer selon le type d'utilisateur :"),
              selectInput("user_t", "Type d'utilisateur :", 
                          choices=c("Tous","ADULT", "CHILD","SENIOR","STUDENT"), 
                          multiple=FALSE, 
                          selected = "Tous"),
              # Graphiques selon le filtre
              plotlyOutput("plot_one", height=600),
              
              br(),
              
              plotlyOutput("plot_two", height=600),
              
              br(),
              br(),
              # Tables Top 5
              fluidRow(column(6,h4(strong("Top 5 utilisateurs (semaine)"))),column(6,h4(strong("Top 5 Tramways (semaine)")))),
              fluidRow(column(6,div(tableOutput("table_user")), style="font-size:150%"),column(6,div(tableOutput("table_tramways"), style="font-size:150%"))),
              
              br(),
              
              #Graphiques Top 5
              plotOutput("user_hour"),
              
              br(),
              
              plotOutput("tram_hour")
              
      )
    )
  )
)


## Server
server <- function(input, output) {
  
  # Importation du jeu de données
  data = data.frame(read.csv2("www/TEST_DATA_HKT_A.csv", sep=",")[,-1])
  
  # Reformatage de la date en enlevant le fuseau horai"re
  data$OPERATION_DATE = ymd_hms(data$OPERATION_DATE)
  
  ## Création de nouvelles colonnes
  
  # Date sans l'heure
  data$Date_reformat = as.Date(data$OPERATION_DATE)
  # L'heure et la minute de la journée
  data$time <- format(data$OPERATION_DATE, format = "%H:%M")
  # L'heure de la journée
  data$hour <- format(data$OPERATION_DATE, format = "%H")
  
  
  # Données du texte de description
  nb_validation = length(data$OPERATION_DATE)
  nb_unique_users = length(unique(data$USER_ID))
  nb_unique_tramways = length(unique(data$DIM_VALIDATOR_ID))
  nb_missing = nrow(data[apply(data, 1, function(x) any(is.na(x))),])
  
  # Texte de sortie
  output$textdesc <- renderText({ 
    paste("Ce jeu de données contient un total de", nb_validation, "validations, avec", nb_unique_users, "utilisateurs uniques,", nb_unique_tramways, "tramways uniques et", nb_missing, "lignes avec des valeurs manquantes.")
  })
  
  # Création de la table de description des utilisateurs
  users_pct_table = as.data.frame(as.matrix(round(table(data$USER_TYPE)/length(data$USER_TYPE)*100,0)))
  users_pct_table$Type = row.names(users_pct_table)
  colnames(users_pct_table) = c("Fréquence (%)", "Type")
  users_pct_table = users_pct_table[, c("Type", "Fréquence (%)")]
  #Table de sortie
  output$users_pct_table <- renderTable(users_pct_table)
  
  # Filtre selon la séléction de l'utilisateur
  my_data = reactive({
    if (input$user_t != "Tous"){
      data %>% filter(USER_TYPE==input$user_t)
    } else {
      data
    } })
  
  # Graphique pour la fréquentation selon la date
  output$plot_one <- renderPlotly({
    
    data_count <- my_data() %>%
      group_by(Date_reformat) %>%
      summarise(count = n(), .groups = "drop_last")
    
    fig = plot_ly(data_count, x = ~Date_reformat, y=~count, type="bar", marker = list(color = "#00aa91")) %>%
      layout(title = "Nombre de validations selon la date", plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Date'), 
             yaxis = list(title = 'Nombre de validations'))

  })
  
  # Graphique pour la fréquentation selon l'horaire
  output$plot_two <- renderPlotly({
    
    data_time_count <- my_data() %>%
      group_by(time) %>%
      summarise(count = n(), .groups = "drop_last")
    
    fig = plot_ly(data_time_count, x = ~time, y=~count, type="bar", marker = list(color = "#00aa91")) %>%
      layout(title = "Nombre de validations selon l'horaire de la journée", plot_bgcolor = "#e5ecf6", xaxis = list(title = "Horaire", dtick = 100), 
             yaxis = list(title = 'Nombre de validations'))

  })
  
  ## Tableaux Top 5
  
  # Top 5 utilisateurs
  top_users = reactive({
    
    df1 = as.data.frame.table(head(sort(table(my_data()$USER_ID), decreasing = TRUE),5))
    colnames(df1) = c("ID de l'utilisateur", "Nombre de validations")
    return(df1)
    
  })
  # Table de sortie
  output$table_user <- renderTable({
    
    top_users()
    
    })

  
  # Top 5 tramways
  top_tramways = reactive({
    
    df2 = as.data.frame.table(head(sort(table(my_data()$DIM_VALIDATOR_ID), decreasing = TRUE),5))
    colnames(df2) = c("ID du valideur", "Nombre de validations")
    return(df2)
    
  })
  # Table de sortie
  output$table_tramways <- renderTable({
    
    top_tramways()
    
  }) 
  
  # Graphique validations top 5 des utilisateurs
  output$user_hour <- renderPlot({
    
    df2 = as.data.frame.table(head(sort(table(my_data()$USER_ID), decreasing = TRUE), 5))
    colnames(df2) = c("ID de l'utilisateur", "Nombre de validations")
    
    df3 = filter(my_data(), USER_ID %in% df2$`ID de l'utilisateur`)
    df3 = subset(df3, select = c("hour", "USER_ID"))
    df3$USER_ID = as.factor(df3$USER_ID)
    
    counts <- df3 %>%
      group_by(hour, USER_ID) %>%
      summarise(count = n(), .groups = "drop")
    
    my_colors = c("#182825", "#016FB9", "#22AED1", "#6D8EA0", "#AFA98D")
    
    ggplot(counts, aes(x = hour, y = count, group = USER_ID, color = USER_ID)) +
      geom_line(linewidth = 1.5) +
      labs(x = "Heure de la journée", y = "Nombre de validations", 
           title = "Nombre de validations par heure pour le top 5 des utilisateurs",
           color = "ID de l'utilisateur") +
      theme(plot.title = element_text(size = 22, face = "bold"),
            axis.title=element_text(size=15, face="bold"),
            legend.title = element_text(size=15), 
            legend.text = element_text(size=15)) +
      scale_color_manual(values = my_colors)
  })
  
  # Graphique validations top 5 Tramways
  output$tram_hour <- renderPlot({
    
    df2 = as.data.frame.table(head(sort(table(my_data()$DIM_VALIDATOR_ID), decreasing = TRUE), 5))
    colnames(df2) = c("ID du valideur", "Nombre de validations")
    
    df3 = filter(my_data(), DIM_VALIDATOR_ID %in% df2$`ID du valideur`)
    df3$DIM_VALIDATOR_ID = as.factor(df3$DIM_VALIDATOR_ID)
    
    counts <- df3 %>%
      group_by(hour, DIM_VALIDATOR_ID) %>%
      summarise(count = n(), .groups = "drop")
    
    my_colors = c("#FFCF00", "#187795", "#38686A", "#A3B4A2", "#CDC6AE")
    
    ggplot(counts, aes(x = hour, y = count, group = DIM_VALIDATOR_ID, color = DIM_VALIDATOR_ID)) +
      geom_line(linewidth = 1.5) +
      labs(x = "Heure de la journée", y = "Nombre de validations enregistrées", 
           title = "Nombre de validations par heure pour le top 5 des Tramways",
           color = "ID du Tramway") +
      theme(plot.title = element_text(size = 22, face = "bold"),
            axis.title=element_text(size=15, face="bold"),
            legend.title = element_text(size=15), 
            legend.text = element_text(size=15)) +
      scale_color_manual(values = my_colors)
    
  })
  
}

shinyApp(ui, server)