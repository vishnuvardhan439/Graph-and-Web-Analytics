library(shiny)
library(dplyr)
library(readr)
library(igraph)
library(visNetwork)
library(ggplot2)

# --- 1. DATA PROCESSING & GRAPH MATH ---
events <- read_csv("events.csv", show_col_types = FALSE)

# Build edges
edges <- events %>%
  arrange(session_id, timestamp) %>%
  group_by(session_id) %>%
  mutate(next_event = lead(event_type)) %>% 
  ungroup() %>%
  filter(!is.na(next_event)) %>%
  rename(from = event_type, to = next_event) %>%
  select(from, to)

# Calculate edge weights (traffic volume)
edge_list <- edges %>%
  group_by(from, to) %>%
  summarise(traffic = n(), .groups = "drop") %>%
  rename(value = traffic)

web_graph <- graph_from_data_frame(d = edge_list, directed = TRUE)

# --- 2. ADVANCED GRAPH METRICS ---
pr <- page_rank(web_graph)$vector
btn <- betweenness(web_graph, directed = TRUE)
in_deg <- degree(web_graph, mode = "in")

# Community Detection (Find groups of pages usually visited together)
wc <- cluster_walktrap(web_graph)

# Prepare nodes for visNetwork
nodes <- data.frame(
  id = names(pr),
  label = names(pr),
  value = as.numeric(pr) * 100, 
  group = membership(wc), # This automatically colors nodes by community
  title = paste0("<p><b>PageRank:</b> ", round(pr, 4), 
                 "<br><b>Betweenness:</b> ", round(btn, 2),
                 "<br><b>Incoming Traffic:</b> ", in_deg, "</p>")
)

# Prepare dataframe for ggplot charts
metrics_df <- data.frame(
  Page = names(pr),
  PageRank = as.numeric(pr),
  Betweenness = as.numeric(btn)
)

# --- 3. USER INTERFACE ---
ui <- fluidPage(
  titlePanel("Cloud-Hosted Web & Graph Analytics"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dataset Overview"),
      p("Total Sessions: ", format(length(unique(events$session_id)), big.mark=",")),
      p("Total Transitions: ", format(sum(edge_list$value), big.mark=",")),
      hr(),
      h4("Funnel Summary"),
      p("Views: ", format(sum(events$event_type == "page_view"), big.mark=",")),
      p("Carts: ", format(sum(events$event_type == "add_to_cart"), big.mark=",")),
      p("Purchases: ", format(sum(events$event_type == "purchase"), big.mark=","))
    ),
    
    mainPanel(
      tabsetPanel(
        # Tab 1: The Network Map
        tabPanel("Interactive Network", 
                 br(),
                 p("Nodes are sized by PageRank and colored by Community Clusters. Hover over nodes for raw metrics."),
                 visNetworkOutput("network_plot", height = "600px")),
        
        # Tab 2: Bar Charts
        tabPanel("Graph Metrics (Charts)", 
                 br(),
                 h4("Top Hubs (PageRank)"),
                 p("Identifies the most authoritative pages in the network."),
                 plotOutput("pr_plot", height = "300px"),
                 hr(),
                 h4("Biggest Bottlenecks (Betweenness Centrality)"),
                 p("Identifies pages that act as critical bridges. High betweenness means users MUST pass through here."),
                 plotOutput("btn_plot", height = "300px")),
        
        # Tab 3: Markov Chain / Transition Probabilities
        tabPanel("Transition Probabilities",
                 br(),
                 h4("Where do users go next?"),
                 p("This calculates the probability of a user moving to a specific page, based on their current page."),
                 dataTableOutput("transition_table"))
      )
    )
  )
)

# --- 4. SERVER LOGIC ---
server <- function(input, output) {
  
  output$network_plot <- renderVisNetwork({
    visNetwork(nodes, edge_list, width = "100%") %>%
      visEdges(arrows = "to", smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE)
  })
  
  output$pr_plot <- renderPlot({
    ggplot(metrics_df, aes(x = reorder(Page, PageRank), y = PageRank)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "PageRank Score")
  })
  
  output$btn_plot <- renderPlot({
    ggplot(metrics_df, aes(x = reorder(Page, Betweenness), y = Betweenness)) +
      geom_bar(stat = "identity", fill = "coral") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Betweenness Centrality")
  })
  
  output$transition_table <- renderDataTable({
    edge_list %>%
      group_by(from) %>%
      mutate(Probability = paste0(round((value / sum(value)) * 100, 1), "%")) %>%
      arrange(from, desc(value)) %>%
      rename(`Current Page` = from, `Next Page` = to, `Total Clicks` = value)
  }, options = list(pageLength = 10))
}

shinyApp(ui = ui, server = server)
