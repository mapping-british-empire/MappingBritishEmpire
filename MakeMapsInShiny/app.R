library(shiny)
library(leaflet)

source("MakeMapsFromData.R")
source("CLIWOCLeaflet.R")
source("SVLeaflet.R")
#setwd("/Users/alexandersherman/Documents/StanfordY2Q3/stanford-ner-2018-10-16")

ui <- fluidPage(
  titlePanel("The Real and the Imaginary British Empire in the Eighteenth Century"),
  sidebarLayout(
    position = "left",
    fluid = TRUE,
    sidebarPanel(
      width = 3,
      fluidRow(
        verticalLayout(
          checkboxGroupInput(
            inputId = "fileChoices1",
            label = "Which text(s) or database would you like to map in the top panel? 
            Choose as many as you'd like. 
            Note that you may select either texts or databases, but may not mix them. 
            The mapping does not normalize for text length, so longer texts (like Green or Raynal's collections) may drown out shorter ones (like the novels) if you combine them.",
            choiceNames = c("DATABASE: CLIWOC (1750-1829)",
                        "DATABASE: Slave Voyages (1750-1811)",
                        "Dampier, New Voyage (1697)",
                        "Defoe, Robinson Crusoe (1719)", 
                        "Defoe, Further Adventures of Robinson Crusoe (1719)",
                        "Chetwood, Richard Falconer (1719)",
                        "Defoe, Captain Singleton (1720)",
                        "Defoe, A New Voyage Round the World (1725)",
                        "Swift, Gulliver's Travels (1726)",
                        "Smollett, Roderick Random (1748)",
                        "Johnson (?), General History of the Pyrates (1724)",
                        "Green (ed.), New General Collection of Voyages (1744-7)",
                        "Anson (Walter ed.), A Voyage Round the World (1748)",
                        "Smollett (ed.) (?), Compendium of Voyages (1756-66)",
                        "Hawkesworth (ed.), Voyages...in the Southern Hemisphere (1773)",
                        "Hawkesworth (ed.), Endeavour Voyage from Voyages...in the Southern Hemisphere (1773)",
                        "Parkinson, Journal of a Voyage to the South Seas (1773)",
                        "Raynal (trans. Justamond), History of the...East and West Indies (1776)",
                        "Banks, Journal from the Endeavour Voyage (1768-71)",
                        "Banks, Remarks from the Endeavour Voyage (1768-71)",
                        "Cook, Journal from the Endeavour Voyage (1768-71)",
                        "Cook, Remarks from the Endeavour Voyage (1768-71)"),
            choiceValues = c(
              "CLIWOC",
              "SVPost1750RawNames.csv",
              "Dampier_NewVoyage.txt_SUNER2021.tsv",
              "Defoe_RobinsonCrusoe.txt_SUNER2021.tsv",
              "Defoe_FurtherAdventuresOfRobinsonCrusoe.txt_SUNER2021.tsv",
              "Chetwood_RichardFalconer_OCR.ocrcleaned.txt_SUNER2021.tsv",
              "Defoe_CaptainSingleton.txt_SUNER2021.tsv",
              "Defoe_NewVoyageRoundTheWorld.txt_SUNER2021.tsv",
              "Swift_GulliversTravels.txt_SUNER2021.tsv",
              "Smollett_RoderickRandom.txt_SUNER2021.tsv",
              "Johnson_Pyrates.txt_SUNER2021.tsv",
              "Green_GeneralCollectionComplete.txt_SUNER2021.tsv",
              "Anson_VoyageRoundTheWorld.txt_SUNER2021.tsv",
              "Smollett_CompendiumComplete.txt_SUNER2021.tsv",
              "Hawkesworth_All.txt_SUNER2021.tsv",
              "Hawkesworth_CookFirst.txt_SUNER2021.tsv",
              "Parkinson_FirstVoyage.txt_SUNER2021.tsv",
              "Raynal_JustamondTrans_FullText.ocrcleaned.txt_SUNER2021.tsv",
              "Banks_FirstVoyageJournal.txt_SUNER2021.tsv",
              "Banks_FirstVoyageRemarks.txt_SUNER2021.tsv",
              "Cook_FirstVoyageJournal.txt_SUNER2021.tsv",
              "Cook_FirstVoyageRemarks.txt_SUNER2021.tsv"
              )
          ),
          checkboxGroupInput(
              inputId = "fileChoices2",
              label = "Which text(s) or database would you like to map in the bottom panel?",
              choiceNames = c("DATABASE: CLIWOC (1750-1829)",
                              "DATABASE: Slave Voyages (1750-1811)",
                              "Dampier, New Voyage (1697)",
                              "Defoe, Robinson Crusoe (1719)", 
                              "Defoe, Further Adventures of Robinson Crusoe (1719)",
                              "Chetwood, Richard Falconer (1719)",
                              "Defoe, Captain Singleton (1720)",
                              "Defoe, A New Voyage Round the World (1725)",
                              "Swift, Gulliver's Travels (1726)",
                              "Smollett, Roderick Random (1748)",
                              "Johnson (?), General History of the Pyrates (1724)",
                              "Green (ed.), New General Collection of Voyages (1744-7)",
                              "Anson (Walter ed.), A Voyage Round the World (1748)",
                              "Smollett (ed.) (?), Compendium of Voyages (1756-66)",
                              "Hawkesworth (ed.), Voyages...in the Southern Hemisphere (1773)",
                              "Hawkesworth (ed.), Endeavour Voyage from Voyages...in the Southern Hemisphere (1773)",
                              "Parkinson, Journal of a Voyage to the South Seas (1773)",
                              "Raynal (trans. Justamond), History of the...East and West Indies (1776)",
                              "Banks, Journal from the Endeavour Voyage (1768-71)",
                              "Banks, Remarks from the Endeavour Voyage (1768-71)",
                              "Cook, Journal from the Endeavour Voyage (1768-71)",
                              "Cook, Remarks from the Endeavour Voyage (1768-71)"),
              choiceValues = c(
                "CLIWOC",
                "SVPost1750RawNames.csv",
                "Dampier_NewVoyage.txt_SUNER2021.tsv",
                "Defoe_RobinsonCrusoe.txt_SUNER2021.tsv",
                "Defoe_FurtherAdventuresOfRobinsonCrusoe.txt_SUNER2021.tsv",
                "Chetwood_RichardFalconer_OCR.ocrcleaned.txt_SUNER2021.tsv",
                "Defoe_CaptainSingleton.txt_SUNER2021.tsv",
                "Defoe_NewVoyageRoundTheWorld.txt_SUNER2021.tsv",
                "Swift_GulliversTravels.txt_SUNER2021.tsv",
                "Smollett_RoderickRandom.txt_SUNER2021.tsv",
                "Johnson_Pyrates.txt_SUNER2021.tsv",
                "Green_GeneralCollectionComplete.txt_SUNER2021.tsv",
                "Anson_VoyageRoundTheWorld.txt_SUNER2021.tsv",
                "Smollett_CompendiumComplete.txt_SUNER2021.tsv",
                "Hawkesworth_All.txt_SUNER2021.tsv",
                "Hawkesworth_CookFirst.txt_SUNER2021.tsv",
                "Parkinson_FirstVoyage.txt_SUNER2021.tsv",
                "Raynal_JustamondTrans_FullText.ocrcleaned.txt_SUNER2021.tsv",
                "Banks_FirstVoyageJournal.txt_SUNER2021.tsv",
                "Banks_FirstVoyageRemarks.txt_SUNER2021.tsv",
                "Cook_FirstVoyageJournal.txt_SUNER2021.tsv",
                "Cook_FirstVoyageRemarks.txt_SUNER2021.tsv"
              )
          )
        )
    )
    ) ,
    mainPanel(
      width = 9,
      fluidRow(
        verticalLayout(
        h2("MAP 1"),
        br(),
      leafletOutput("mymap1", height = "600px"),
      h2("MAP 2"),
      br(),
      leafletOutput("mymap2", height = "600px")
        )
    )
  )
  )
)



server <- function(input, output, session) {
  
  textFns1 <- reactive(
    input$fileChoices1
  )
  
  textFns2 <- reactive(
    input$fileChoices2
  )
  
  output$mymap1 <- renderLeaflet(
    if (textFns1() == "CLIWOC") {
      makeCLIWOCmap("CLIWOCforLeaflet.csv", "CLIWOCSeaCoords.csv")
    }
    else if (textFns1() == "SVPost1750RawNames.csv") {
      makeMapSV(textFns1(), "NamesAndCoordsRef(NoBlanks).csv")
    }
    else {
    #print(textFns)
    makeMap("NamesAndCoordsRef(NoBlanks).csv", 
            textFns1())
    }
  )
  
  output$mymap2 <- renderLeaflet(
    if (textFns2() == "CLIWOC") {
      makeCLIWOCmap("CLIWOCforLeaflet.csv", "CLIWOCSeaCoords.csv")
    }
    else if (textFns2() == "SVPost1750RawNames.csv") {
      makeMapSV(textFns2(), "NamesAndCoordsRef(NoBlanks).csv")
    }
    else {
      #print(textFns)
      makeMap("NamesAndCoordsRef(NoBlanks).csv", 
              textFns2())
    }
  )
}
shinyApp(ui = ui, server = server) 
