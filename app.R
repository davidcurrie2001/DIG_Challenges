#### Load necessary packages and data ####
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(networkD3)
library(ggpubr)
library(shinycssloaders)
library(shinyWidgets)


#### UI ####
# Note if you don't use "shinyUI" then the networkD3 graphs won't work properly 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("ICES DIG Challenges and Opportunities"),
  
  # use default Bootstrap styles
  theme = bs_theme(bootswatch = "lumen"),

  sidebarLayout(
    
    # Controls 
    sidebarPanel(
      selectInput("ChallOppSelect",
                  label="Type",
                  choices=c("Challenge","Opportunity","Issue"),
                  selected=c("Challenge","Opportunity"),
                  multiple = TRUE),
      selectInput("severitySelect",
                  label="Severity",
                  choices=c("Major","Medium","Minor"),
                  selected="Major",
                  multiple = TRUE),
      pickerInput("issuePicker",
                  label="Issue Number",
                  choices=c(),
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 0",
                    countSelectedText = "{0}/{1} issues")
                  ),
      selectInput("paletteSelect",
                  label="Plot palette",
                  choices=c("RGB","RYB","Red","Green","Blue","Grey"),
                  selected="RGB",
                  multiple = FALSE),
      "Dark/light mode:", input_dark_mode(id = "mode")
      , width = 2),
    
    # Main content
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction", 
                 br(),
                 "The ICES Data and Information Group (",
                 a(href=paste0("https://www.ices.dk/community/groups/Pages/DIG.aspx"),"DIG",target="_blank"),
                 ") use GitHub issues to manage their actions.",
                 p(),
                 "Part of the remit of DIG is to evaluate and monitor current and future challenges and opportunities in data management.  
          The DIG Challenges & Opportunities tracker is implemented in ",
                 a(href=paste0("https://github.com/orgs/ices-eg/projects/6/views/1"),"GitHub",target="_blank"),
                 "so that it can be viewed externally.  Entries are split between 'challenge' and 'opportunity' and scored according to likelihood and impact. This allows a severity rating to be calculated for each item; minor, medium and major.",
                 p(),
                 "The next steps for each entry depend on the severity:",
                 tags$ul(
                   tags$li("Major: Include summary of challenge/opportunity in DIG reports and briefing to SCICOM. DIG to make recommendations for action if necessary"), 
                   tags$li("Medium: Include summary of challenge/opportunity in DIG reports. DIG to make recommendations for action if necessary"), 
                   tags$li("Minor: monitored by DIG")
                 ),
                 p(),
                 "This tool allows you to explore the challenges and opportunities and see which issues are related to them.",
                 p(),
                 textOutput("QueryDate"),
                 p(),
                 "You can use this button to create a URL with your filter settings saved:",
                 bookmarkButton()
        ),
        tabPanel("Summary", 
                 dataTableOutput("summaryTable"),
                 plotOutput("relatedIssuePlot")
                 ),
        tabPanel("Network Graph", 
                 forceNetworkOutput("force"),
                 sliderInput( 
                   "networkFont", 
                   "Font size", 
                   value = 14, 
                   min = 1, 
                   max = 20 
                 ),
                 sliderInput( 
                   "networkDistance", 
                   "Link Distance", 
                   value = 50, 
                   min = 1, 
                   max = 100 
                 ),
                 sliderInput( 
                   "networkCharge", 
                   "Node repulsion/attraction", 
                   value = -30, 
                   min = -100, 
                   max = 100 
                 )
                 
              )
      )  
    )
  )
))

#### Server ####
server <- function(input, output, session) {
  
  useLocalData <- TRUE
  queryDate <- NA
  
  defaultPalette <- list("Challenge"="#FF0000",
                         "Opportunity"="#008000",
                         "Issue"="#0000FF")

  # Show a spinner whilst app data is being loaded
  showPageSpinner()
  
  # load palettes
  AllPalettes <- read.csv(file="data/palettes.csv")
  
  # load the data
  if (useLocalData){
    load("data/DIG_Issues.RData")
  } else {
    githubDataURL <- "https://raw.githubusercontent.com/davidcurrie2001/DIG_Challenges/refs/heads/master/data/DIG_Issues.RData"
    load(url(githubDataURL))
  }
  
  # Hide the spinner once data is loaded
  hidePageSpinner()
  
  # When was the date extracted?
  queryDate <- DIG_Issues[["queryDate"]]
  
  
  AllIssues <- DIG_Issues[["issues"]]
  AllIssues$Type <- ifelse(AllIssues$Challenge == TRUE, 'Challenge', 
                                  ifelse(AllIssues$Opportunity == TRUE, 'Opportunity', 'Issue'))
  IssueLinks <- DIG_Issues[["links"]]
  

  
  # Set our palette based on the input chosen
  appPalette <- reactive({
    
      selectedPalette <- AllPalettes[AllPalettes$paletteName == input$paletteSelect,]
      if (nrow(selectedPalette) == 1){
        paletteToUse <- list("Challenge" = selectedPalette[1, "Challenge"], 
                             "Opportunity" = selectedPalette[1, "Opportunity"], 
                             "Issue" = selectedPalette[1, "Issue"] 
        )
      } else {
        warning("Problem reading palettes data - using default values instead")
        paletteToUse <- defaultPalette
      }
  })  

  
  
  # Filter the input data using the widget values
  FilterIssues_Step1 <- reactive({
    
    myIssuesFiltered <- AllIssues
    
    myIssuesFiltered <- myIssuesFiltered[myIssuesFiltered$state == "OPEN",]
    
    # Filter by the type
    if (length(input$ChallOppSelect) >0) {
      myIssuesFiltered <- myIssuesFiltered[myIssuesFiltered$Type  %in% input$ChallOppSelect,]
    } else {
      myIssuesFiltered <- myIssuesFiltered[0==1,]
    }
    
    # Filter by the severity
    if (length(input$severitySelect) >0){
      # "Issues" don't have severity so we won't apply this filter to them
      myIssuesFiltered <- myIssuesFiltered[myIssuesFiltered$Type == "Issue" | 
                                             myIssuesFiltered$projectField %in% input$severitySelect,]
    } else {
      myIssuesFiltered <- myIssuesFiltered[0==1,]
    }
    
    myIssuesFiltered <- myIssuesFiltered[order(myIssuesFiltered$number),]
    
    # For testing
    #myIssuesFiltered <- myIssuesFiltered[myIssuesFiltered$Challenge == TRUE,]
    #myIssuesFiltered <- myIssuesFiltered[myIssuesFiltered$projectField %in% c("Major"),]
    
    myIssuesFiltered
    
  })
  
  
  # Filter the input data using the widget values
  FilterIssues <- reactive({
    
    # Filter using the type and severity inputs
    myIssuesFiltered <- FilterIssues_Step1()
    
    # If we have soemthing in the issue input then filter using that
    if (length(input$issuePicker) == 0 & is.null(input$issuePicker)){
      myIssuesFiltered <- myIssuesFiltered[0==1,]
    } else {
      # Filter by the issue numbers
      myIssuesFiltered <- myIssuesFiltered[myIssuesFiltered$number  %in% input$issuePicker,]
    }
    
    myIssuesFiltered
    
  })
  
  # Update the issue number select input based on the other inputs
  observe({
    fi <- FilterIssues_Step1()
    updatePickerInput(session, "issuePicker", choices = fi$number, selected = fi$number)
  })
  
  # Join issues to their linked issues
  JoinIssuesWithLinks <- function(){
    
    issues <- FilterIssues()
    #issues <- myIssuesFiltered
    myLinks <- IssueLinks
    
    otherIssues <- AllIssues
    names(otherIssues) <- paste("crossRefBy_",names(otherIssues),sep="")
    # TODO Only get the links to issues that aren't Challenges/Opportunities (includes Open and Closed issues)
    #otherIssues <- otherIssues[otherIssues$Challenge == FALSE & otherIssues$Opportunity == FALSE,]
    otherIssues <- inner_join(myLinks,otherIssues,by=c("crossRefById"="crossRefBy_id"))
    #names(otherIssues)
    #otherIssues <- otherIssues[,c("id","crossRefById")]
    myData <- left_join(issues,otherIssues,by="id")
    myData
  }
  
  ## OUTPUTS
  
  # Show the date the data was extracted on
  output$QueryDate <- renderText({
    
    if(!is.na(queryDate)){
      queryDateDisplay <- format(queryDate, format = "%H:%M, %d %B %Y")
      dateText <- paste0("The data displayed in this app was extracted from GitHub at ",queryDateDisplay,".  ")
    } else {
      dateText <- ""
    }
    
    if(useLocalData == TRUE ){
      dateText <- paste0(dateText,"The data is being loaded from the app's local repository.")
    }
    
    dateText
    
  })
  

  output$force <- renderForceNetwork({
    
    myData <- JoinIssuesWithLinks()
    #names(myData)
    
    # the validate function in Shiny was being masked - so use explicit package name
    shiny::validate(need(nrow(myData) > 0, "No data to display"))
    
    # Need to make a single list of nodes
    # First get the challenge/oppotunities
    Nodes <- myData[,c("id","number","shortName","Type")]
    # then get the cross-references nodes
    crossRef <- myData[!is.na(myData$crossRefById),c("crossRefById","crossRefBy_number","crossRefBy_shortName","crossRefBy_Type")]
    names(crossRef) <- c("id","number","shortName","Type")
    # then combind them
    Nodes <- rbind(Nodes,crossRef)
    # the de-dupe, order them, and generate an ID starting at 0
    Nodes <- Nodes[!duplicated(Nodes),]
    Nodes <- Nodes[order(Nodes$number),]
    Nodes$ID <- seq(0,nrow(Nodes)-1)
    rownames(Nodes) <- NULL
    
    # Now need to create the Links, with references to the numeric ID we just defined for each node
    Links <- myData
    Links <- dplyr::left_join(Links,Nodes,by=c("id"="id"))
    Links$source <- Links$ID
    Links$ID <- NULL
    #Links$Group <- NULL
    Links <- dplyr::left_join(Links,Nodes,by=c("crossRefById"="id"))
    Links$target <- Links$ID
    Links <- Links[,c("source","target")]
    Links[is.na(Links$target),"target"] <- Links[is.na(Links$target),"source"]
    
    
    # Define a custom palette - the string should look something like this
    #ColourScale <- 'd3.scaleOrdinal()
    #        .domain(["Challenge", "Opportunity", "Issue" ])
    #       .range(["#FF0000", "#008000", "#0000FF"]);'
    ColourScale <- paste0("d3.scaleOrdinal().domain([",
           paste("'",names(appPalette()),"'", sep="", collapse = ","),
           "]).range([",
           paste("'",appPalette(),"'", sep="", collapse = ","),
           "]);")

    
    forceNetwork(Links = Links, 
                 Nodes = Nodes, 
                 NodeID = "shortName",
                 Source = "source", 
                 Target = "target", 
                 Group = "Type", 
                 opacity = 0.9,
                 fontFamily = "serif",
                 colourScale = JS(ColourScale),
                 opacityNoHover = 1, # always show names
                 zoom = TRUE,
                 legend = TRUE,
                 fontSize = input$networkFont,  
                 linkDistance = input$networkDistance,
                 charge=input$networkCharge)
    
  })
  
  # Display a plot showing the number of related issues
  output$relatedIssuePlot <- renderPlot({
    
    myData <- JoinIssuesWithLinks()
    
    # Count the links to other issues
    myCount <- myData %>%
      group_by(id,title,url,number,state,fullName,shortName,projectField,Challenge,Opportunity,Type) %>%
      summarise(relatedIssueCount = sum(!is.na(crossRefById)), .groups = "keep")
    myCount <- myCount[order(myCount$number),]
    
    # the validate function in Shiny was being masked - so use explicit package name
    shiny::validate(need(nrow(myCount) > 0, "No data to display"))
    
    # Create a consistent palette
    myPalette <- sort(unique(myCount$Type))
    if (length(myPalette)==0) {
      myPalette <- c("Grey")
    } else {
      for(myType in names(appPalette())){
        myPalette <- replace(myPalette,myPalette == myType,appPalette()[[myType]])
      }
    }
    
    ggbarplot(myCount, x = "shortName", y = "relatedIssueCount",
              color = "black", fill = "Type", palette = myPalette) + 
      labs(x = "", y="Number of related issues") +
      rotate()
    
  })
  
  # Display the Challenges/Opportunities
  output$summaryTable <- renderDataTable({
    
    data <- JoinIssuesWithLinks() %>%
      dplyr::mutate(fullNameAndLink =  paste0("<a href='",url, "'  target='_blank'>",fullName,"</a>")) %>%
      dplyr::mutate(crossRefNumberAndLink =  ifelse(is.na(crossRefBy_number),
                                                    "",
                                                    paste0("<a href='",crossRefBy_url, "'  target='_blank'>",crossRefBy_number,"</a>"))) %>%
      dplyr::arrange(desc(fullNameAndLink),crossRefNumberAndLink) %>%
      dplyr::group_by(fullNameAndLink,Type) %>%
      dplyr::mutate(crossRefURLs = paste0(crossRefNumberAndLink, collapse = ", ")) %>%
      dplyr::select(fullNameAndLink,Type,crossRefURLs) %>%
      unique() %>%
      dplyr::arrange(desc(fullNameAndLink)) %>%
      dplyr::rename(Name = fullNameAndLink, Related = crossRefURLs)
  }, rownames= FALSE, escape = FALSE, options = list(dom = 'tp'))
  
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
