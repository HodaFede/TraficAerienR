
# nos librairies
library(shiny)
library(tidyverse)
library(nycflights13)
library(leaflet)
library(shinythemes)
library(DT)
library(viridis)
library(janitor)

# pre data


flights_clean <- flights %>%
  filter(
    !is.na(dep_delay), !is.na(arr_delay),
    !is.na(distance), !is.na(air_time)
  ) %>%
  mutate(
    month = as.integer(month),
    day = as.integer(day),
    month_int = as.integer(month)
  )

weather_clean <- weather %>%
  clean_names() %>%
  filter(!is.na(wind_speed), !is.na(humid), !is.na(precip)) %>%
  mutate(
    month = as.integer(month),
    day = as.integer(day)
  )

flights_weather <- flights_clean %>%
  left_join(weather_clean,
            by = c("origin","year","month_int"="month","day","hour"))

# données

# retard par compagnie
retard_compagnie <- flights_clean %>%
  group_by(carrier) %>%
  summarise(
    retard_depart_moy = mean(dep_delay, na.rm = TRUE),
    retard_arrivee_moy = mean(arr_delay, na.rm = TRUE)
  )

# retard par jour
retard_jour <- flights_clean %>%
  group_by(day) %>%
  summarise(retard_moy = mean(arr_delay, na.rm = TRUE))

# retard par aéroport
retard_origin <- flights_clean %>%
  group_by(origin) %>%
  summarise(retard_moy = mean(arr_delay, na.rm = TRUE))

# retard par heure
retard_heure <- flights_clean %>%
  group_by(hour) %>%
  summarise(retard_moy = mean(arr_delay, na.rm = TRUE)) %>%
  drop_na(hour)

# distribution des retards
dist_retards <- flights_clean$arr_delay

# vols par compagnie
vols_compagnie <- flights_clean %>% count(carrier)

# top destination
top_dest <- flights_clean %>% count(dest, sort = TRUE)

# mission 1 du prof


nb_airports_total <- nrow(airports)
nb_airports_origin <- flights %>% distinct(origin) %>% nrow()
nb_airports_dest <- flights %>% distinct(dest) %>% nrow()
nb_dst_N <- airports %>% filter(dst == "N") %>% nrow()
nb_tzones <- airports %>% distinct(tzone) %>% nrow()
nb_comp <- nrow(airlines)
nb_tailnum <- flights %>% distinct(tailnum) %>% nrow()

nb_cancelled <- flights %>% filter(is.na(dep_delay) & is.na(arr_delay)) %>% nrow()

top10_dest <- flights %>%
  count(dest) %>%
  left_join(airports, by=c("dest"="faa")) %>%
  mutate(pct = round(n/sum(n)*100,2)) %>%
  arrange(desc(n)) %>% head(10)

dest_by_carrier <- flights %>% group_by(carrier) %>% summarise(nb_dest=n_distinct(dest))
dest_by_carrier_origin <- flights %>%
  group_by(carrier, origin) %>% summarise(nb_dest=n_distinct(dest))

sea_flights <- flights %>% filter(dest=="SEA")
hou_flights <- flights %>% filter(dest %in% c("IAH","HOU"))

exclusive_dest <- flights %>%
  group_by(dest, carrier) %>% summarise(n=n()) %>%
  group_by(dest) %>% filter(n()==1)

UAD_flights <- flights %>% filter(carrier %in% c("UA","AA","DL"))

# heatmap meteo

heatmap_meteo <- flights_weather %>%
  group_by(month_int, origin) %>%
  summarise(
    retard = mean(dep_delay, na.rm=TRUE),
    vent   = mean(wind_speed, na.rm=TRUE),
    humid  = mean(humid, na.rm=TRUE),
    precip = mean(precip, na.rm=TRUE)
  ) %>%
  drop_na(retard) %>%
  mutate(month = month.abb[month_int])

# model
model_lm <- readRDS("model_lm.rds")

# UI dashb

ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Analyse des vols NYC 2013",

  # dashboard
  tabPanel("Dashboard",
           fluidRow(
             column(4, wellPanel(h3("Total vols"), h2(nrow(flights_clean)))),
             column(4, wellPanel(h3("Retard départ moyen"), h2(round(mean(flights_clean$dep_delay),2)))),
             column(4, wellPanel(h3("Retard arrivée moyen"), h2(round(mean(flights_clean$arr_delay),2))))
           ),
           hr(),
           h3("Retard moyen par mois"),
           plotOutput("plot_retard_mois")
  ),

  # graphiques
  tabPanel("Graphiques d'analyse",
           sidebarLayout(
             sidebarPanel(
               selectInput("plot_choice", "Choisir un graphique :",
                           c(
                             "Top 10 destinations",
                             "Retard moyen par compagnie",
                             "Retard moyen par jour",
                             "Retard moyen par aéroport d’origine",
                             "Retard moyen par heure",
                             "Répartition des retards",
                             "Nombre de vols par compagnie",
                             "Distribution des distances",
                             "Boxplot retard par compagnie",
                             "Heatmap retard journalier",
                             "Heatmap météo complète"
                           )),
               width = 3
             ),
             mainPanel(plotOutput("plot_dynamic", height = "550px"))
           )
  ),

  # mission 1
  tabPanel("Mission 1 – Prof",
           h2("Statistiques Globales"),
           uiOutput("stats_globales"),

           h2("Top 10 destinations"),
           plotOutput("plot_top10"),

           h2("Destinations par compagnie"),
           plotOutput("plot_dest_compagnie"),

           h2("Destinations par compagnie & origine"),
           plotOutput("plot_dest_compagnie_origin"),

           h2("Vols Houston & Seattle"),
           DTOutput("table_hou_sea"),

           h2("Tri complet avec noms"),
           DTOutput("table_trie"),

           h2("Destinations exclusives"),
           DTOutput("table_exclu"),

           h2("Vols UA / AA / DL"),
           DTOutput("table_uad")
  ),

  # carte
  tabPanel("Carte interactive",
           leafletOutput("map", height = "600px")
  ),

  # prediction
  tabPanel("Prédiction du retard",
           sidebarLayout(
             sidebarPanel(
               numericInput("dep", "Retard au départ (min)", 10),
               numericInput("dist", "Distance (miles)", 500),
               numericInput("air", "Temps de vol (min)", 100),
               actionButton("predict_btn", "Prédire")
             ),
             mainPanel(
               h3("Résultat :"),
               verbatimTextOutput("pred_result")
             )
           )
  ),

  # tableau
  tabPanel("Tableau complet",
           DTOutput("table")
  )
)

# server
server <- function(input, output, session) {

  # dashboard
  output$plot_retard_mois <- renderPlot({
    ggplot(flights_clean %>% group_by(month) %>%
             summarise(retard_moy = mean(arr_delay, na.rm = TRUE)),
           aes(month, retard_moy)) +
      geom_line(linewidth=1.5, color="#0088cc") +
      geom_point(size=3, color="#005f88") +
      scale_x_continuous(breaks = 1:12)
  })


  # graphiques analyse
  output$plot_dynamic <- renderPlot({
    p <- input$plot_choice

    # top destination
    if (p == "Top 10 destinations") {
      ggplot(head(top_dest, 10), aes(reorder(dest, n), n)) +
        geom_col(fill="steelblue") + coord_flip()

    # retard par compagnie
    } else if (p == "Retard moyen par compagnie") {
      ggplot(retard_compagnie, aes(reorder(carrier, retard_arrivee_moy), retard_arrivee_moy)) +
        geom_col(fill="red") + coord_flip()

    # retard par jour
    } else if (p == "Retard moyen par jour") {
      ggplot(retard_jour, aes(day, retard_moy)) +
        geom_line(linewidth=1.4, color="orange") +
        geom_point(size=3, color="darkorange")

    # retard par aéroport
    } else if (p == "Retard moyen par aéroport d’origine") {
      ggplot(retard_origin, aes(reorder(origin, retard_moy), retard_moy)) +
        geom_col(fill="steelblue") + coord_flip()

    # retard par heure
    } else if (p == "Retard moyen par heure") {
      ggplot(retard_heure, aes(hour, retard_moy)) +
        geom_line(linewidth=1.4, color="purple") +
        geom_point(size=3, color="darkmagenta")

    # distribution des retards
    } else if (p == "Répartition des retards") {
      ggplot(flights_clean, aes(arr_delay)) +
        geom_histogram(bins=60, fill="darkgreen")

    # vols par compagnie
    } else if (p == "Nombre de vols par compagnie") {
      ggplot(vols_compagnie, aes(reorder(carrier, n), n)) +
        geom_col(fill="darkcyan") + coord_flip()

    # distance
    } else if (p == "Distribution des distances") {
      ggplot(flights_clean, aes(distance)) +
        geom_histogram(bins=50, fill="purple")

    # boxplot
    } else if (p == "Boxplot retard par compagnie") {
      ggplot(flights_clean, aes(carrier, arr_delay)) +
        geom_boxplot(fill="skyblue") + coord_flip()

    # heatmap retard
    } else if (p == "Heatmap retard journalier") {
      d <- flights_clean %>% group_by(month, day) %>%
        summarise(retard = mean(arr_delay, na.rm=TRUE))
      ggplot(d, aes(month, day, fill = retard)) +
        geom_tile() + scale_fill_viridis_c()

    # heatmap météo
    } else if (p == "Heatmap météo complète") {
      ggplot(heatmap_meteo, aes(month, origin, fill = retard)) +
        geom_tile() + scale_fill_viridis(option = "magma")
    }
  })


  # mission 1
  output$stats_globales <- renderUI({
    tagList(
      fluidRow(
        column(3, wellPanel(h4("Aéroports total"), h2(nb_airports_total))),
        column(3, wellPanel(h4("Aéroports origine"), h2(nb_airports_origin))),
        column(3, wellPanel(h4("Aéroports destination"), h2(nb_airports_dest))),
        column(3, wellPanel(h4("Sans DST"), h2(nb_dst_N)))
      ),
      fluidRow(
        column(3, wellPanel(h4("Fuseaux horaires"), h2(nb_tzones))),
        column(3, wellPanel(h4("Compagnies"), h2(nb_comp))),
        column(3, wellPanel(h4("Avions"), h2(nb_tailnum))),
        column(3, wellPanel(h4("Vols annulés"), h2(nb_cancelled)))
      )
    )
  })

  output$plot_top10 <- renderPlot({
    ggplot(top10_dest, aes(reorder(dest,n), n)) +
      geom_col(fill="darkblue") + coord_flip()
  })

  output$plot_dest_compagnie <- renderPlot({
    ggplot(dest_by_carrier, aes(carrier, nb_dest)) +
      geom_col(fill="steelblue")
  })

  output$plot_dest_compagnie_origin <- renderPlot({
    ggplot(dest_by_carrier_origin, aes(carrier, nb_dest, fill=origin)) +
      geom_col(position="dodge")
  })

  output$table_hou_sea <- renderDT({
    datatable(bind_rows(hou_flights, sea_flights))
  })

  output$table_trie <- renderDT({
    datatable(
      flights %>%
        left_join(airports, by=c("origin"="faa")) %>%
        left_join(airports, by=c("dest"="faa"), suffix=c("_origin","_dest")) %>%
        arrange(dest, origin, carrier)
    )
  })

  output$table_exclu <- renderDT({
    datatable(exclusive_dest)
  })

  output$table_uad <- renderDT({
    datatable(UAD_flights)
  })


  # carte
  output$map <- renderLeaflet({
    airports_df <- airports %>% select(faa, name, lat, lon)

    dest_points <- flights_clean %>%
      count(dest, name="vols") %>%
      left_join(airports_df, by=c("dest"="faa")) %>%
      drop_na(lat)

    leaflet(dest_points) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(~lon, ~lat,
                       radius = ~log(vols)*2,
                       label = ~paste(name, "-", vols, "vols"),
                       color="darkblue",
                       fillOpacity = .7)
  })


  # prediction
  observeEvent(input$predict_btn, {
    pred <- predict(model_lm, newdata = data.frame(
      dep_delay = input$dep,
      distance = input$dist,
      air_time = input$air
    ))

    output$pred_result <- renderText(
      paste("Retard estimé :", round(pred,1), "minutes")
    )
  })


  # tableau recap
  output$table <- renderDT({
    datatable(flights_clean)
  })
}



# lancement
shinyApp(ui, server)