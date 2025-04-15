library(ghql)
library(jsonlite)
library(httr)

# Script to get the DIG issues from the right project and format them for the Shiny app

# Set your GitHub personal access token
#GITHUB_TOKEN <- readLines("data-raw/PAT.txt")
GITHUB_TOKEN <- Sys.getenv("GITHUB_TOKEN")

# Create a client to access the github graphql api
initClient <- function(){
  
  # GraphQL client setup
  graphql_client <- GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = list(Authorization = paste("Bearer ", GITHUB_TOKEN))
  )
  
  # Get schema
  graphql_client$load_schema()
  
  graphql_client
  
}

# Get all the projects from a repo
getProjects <- function(client, owner, repo){
  
  owner <- "ices-eg"
  repo <- "DIG"
  
  # Make a query class object
  qry <- Query$new()
  
  myQueryString <- paste0('{
    repository(owner: "',owner,'", name: "',repo,'") {
      projectsV2(first: 100) {
        nodes {
          id
          title
          url
        }
      }
    }
  }')
  
  qry$query('mydata', myQueryString)

  # Execute the query
  x <- client$exec(qry$queries$mydata)
  
  # Parse the response
  myProjects <- as.data.frame(jsonlite::fromJSON(x))
  
  # Return the data frame
  myProjects
}


# Function to fetch all issues with pagination
fetchProjectIssues <- function(client, projectID, resultsPerPage) {
  
  #projectID <- "PVT_kwDOATlSQs4AHje4" 
  #client <- initClient()
  #resultsPerPage <- 100
  
  issues <- list()
  end_cursor <- NULL
  has_next_page <- TRUE
  requestNumber <- 0
  
  while (has_next_page) {
    requestNumber <- requestNumber + 1
    
    # query written by ChatGPT (after a lot of iterations...)
    myQueryString <- sprintf(
      '{
        node(id: \"%s\") {
          ... on ProjectV2 {
            id
            title
            url
            items(first: %s, after: %s) {
              pageInfo {
                endCursor
                startCursor
                hasNextPage
                hasPreviousPage
              }
              nodes {
                content {
                  ... on Issue {
                    id
                    title
                    url
                    number
                    state
                    createdAt
                    labels(first: 50) {
                      nodes {
                        name
                      }
                    }
                    timelineItems(first: 50, itemTypes: CROSS_REFERENCED_EVENT) {
                      nodes {
                        ... on CrossReferencedEvent {
                          source {
                            ... on Issue {
                              id
                              title
                              url
                              number
                              state
                              createdAt
                            }
                          }
                        }
                      }
                    }
                  }
                }
                fieldValues(first: 50) {
                  nodes {
                    __typename
                    ... on ProjectV2ItemFieldSingleSelectValue {
                      name
                    }
                    ... on ProjectV2ItemFieldTextValue {
                      text
                    }
                    ... on ProjectV2ItemFieldNumberValue {
                      number
                    }
                    ... on ProjectV2ItemFieldIterationValue {
                      title
                    }
                  }
                }
              }
            }
          }
        }
      }', projectID, 
        resultsPerPage, 
        ifelse(is.null(end_cursor), "null", paste0("\"", end_cursor, "\"")))
    
    #response <- client$exec(query)
    
    # Make a query class object
    # (Note that I needed to make the query object null and re-create it otherwise
    # it kept running the the very first query when I was trying to page through multi-page 
    # results)
    qry <- NULL
    qry <- Query$new() 
    qry$query('mydata', myQueryString)
    
    print(paste0("Trying to fetch data from GitHub (request number ",
                 requestNumber,
                 ", ",
                 resultsPerPage,
                 " results per page)"))
    
    # Execute the query
    response <- client$exec(qry$queries$mydata)
    print(response)
    
    #project_issues <- fromJSON(x)$data$node$items$nodes
    
    result <- fromJSON(response)$data$node$items
    li <- list(Result = result$nodes)
    issues <- append(issues, li)
    #issues <- append(issues, result$nodes)
    
    has_next_page <- result$pageInfo$hasNextPage
    end_cursor <- result$pageInfo$endCursor
  }
  
  return(issues)
}


# Format our GitHub issues data into a data frame
createIssuesDataFrame <- function(project_issues){
  
  issues_df <- do.call(rbind, lapply(1:nrow(project_issues), function(i) {
    issue_data <- project_issues$content[i, ]
    if (is.null(issue_data)) return(NULL)
    
    labels <- issue_data$labels$nodes
    label_names <- if (!is.null(labels)) sapply(labels, function(label) label$name) else c()
    has_challenge_label <- "tracker: challenge" %in% label_names
    has_opportunity_label <- "tracker: opportunity" %in% label_names
    
    field_values_list <- project_issues$fieldValues$nodes[[i]]  # Get field values for this issue
    select_value <- NA
    
    if (!is.null(field_values_list) && is.data.frame(field_values_list)) {
      single_select <- field_values_list[field_values_list$`__typename` == "ProjectV2ItemFieldSingleSelectValue", ]
      if (nrow(single_select) > 0) {
        select_value <- single_select$name[1]
      }
    }
    
    cross_references <- issue_data$timelineItems$nodes
    
    if (!is.null(cross_references) && length(cross_references) > 0) {
      cross_ref_rows <- do.call(rbind, lapply(cross_references, function(ref) {
        if (!is.null(ref$source)) {
          data.frame(
            id = issue_data$id,
            number = issue_data$number,
            title = issue_data$title,
            fullName = paste0(issue_data$number,") ",issue_data$title),
            shortName = substr(paste0(issue_data$number,") ",issue_data$title),1,40),
            url = issue_data$url,
            state = issue_data$state,
            createdAt = issue_data$createdAt,
            projectField = select_value,
            Challenge = has_challenge_label,
            Opportunity = has_opportunity_label,
            crossRefById = ref$source$id,
            crossRefByNumber = ref$source$number,
            crossRefByTitle = ref$source$title,
            crossRefByFullName = paste0(ref$source$number,") ",ref$source$title),
            crossRefByShortName = substr(paste0(ref$source$number,") ",ref$source$title),1,40),
            crossRefByUrl = ref$source$url,
            crossRefByState = ref$source$state,
            crossRefByCreatedAt = ref$source$createdAt,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            id = issue_data$id,
            number = issue_data$number,
            title = issue_data$title,
            fullName = paste0(issue_data$number,") ",issue_data$title),
            shortName = substr(paste0(issue_data$number,") ",issue_data$title),1,40),
            url = issue_data$url,
            state = issue_data$state,
            createdAt = issue_data$createdAt,
            projectField = select_value,
            Challenge = has_challenge_label,
            Opportunity = has_opportunity_label,
            crossRefById = NA,
            crossRefByNumber = NA,
            crossRefByTitle = NA,
            crossRefByFullName = NA,
            crossRefByShortName = NA,
            crossRefByUrl = NA,
            crossRefByState = NA,
            crossRefByCreatedAt = NA,
            stringsAsFactors = FALSE
          )
        }
      }))
      return(cross_ref_rows)
    } else {
      return(data.frame(
        id = issue_data$id,
        number = issue_data$number,
        title = issue_data$title,
        fullName = paste0(issue_data$number,") ",issue_data$title),
        shortName = substr(paste0(issue_data$number,") ",issue_data$title),1,40),
        url = issue_data$url,
        state = issue_data$state,
        createdAt = issue_data$createdAt,
        projectField = select_value,
        Challengel = has_challenge_label,
        Opportunity = has_opportunity_label,
        crossRefById = NA,
        crossRefByNumber = NA,
        crossRefByTitle = NA,
        crossRefByFullName = NA,
        crossRefByShortName = NA,
        crossRefByUrl = NA,
        crossRefByState = NA,
        crossRefByCreatedAt = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    
  }))
  

  issues_df
  
}

# Function to the issues from a project, along with cross-references to them
getIssuesAndCrossRefsForProject <- function(client, projectID, resultsPerPage = 100){
  
  #projectID <- "PVT_kwDOATlSQs4AHje4"
  #client <- initClient()
  #resultsPerPage <- 40
  
  if(resultsPerPage > 100){
    warning("Can't request more than 100 issues per page")
    resultsPerPage <- 100
  }
  
  # Fetch all issues for the given project
  project_issues <- fetchProjectIssues(client = client, 
                                       projectID = projectID,
                                       resultsPerPage = resultsPerPage)

  print("Converting GitHub response to a data frame")
  # Transform data into a dataframe
  issues_df <- NULL
  if (length(project_issues)>0){
    for(i in seq_along(project_issues)){
      x <- createIssuesDataFrame(project_issues[[i]])
      if (length(issues_df) == 0 & is.null(issues_df)){
        issues_df <- x
      } else {
        issues_df <- rbind(issues_df, x)
      }
    }
  }
  
  # Split the issues and their relationships up
  issues <- unique(issues_df[,c("id","number","title","fullName","shortName","url","state","createdAt","projectField","Challenge","Opportunity")])
  issues <- issues[!is.na(issues$id),]
  
  links <- unique(issues_df[,c("id","crossRefById")])
  links <- links[!is.na(links$id) & !is.na(links$crossRefById),]
  
  DIG_Issues <- list(queryDate = Sys.time(), "issues" = issues, "links" = links)
  
  DIG_Issues
  
}


# create a client to access the Github graphql api
myClient <- initClient()

# get the DIG projects
#projects <- getProjects(client = myClient, owner = "ices-eg", repo = "DIG")

# Get the DIG issues
DIG_Issues <- getIssuesAndCrossRefsForProject(client = myClient, 
                                            projectID = "PVT_kwDOATlSQs4AHje4")

# Save the data
save(DIG_Issues, file="data/DIG_Issues.RData")



