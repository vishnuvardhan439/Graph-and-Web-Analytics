# Load the libraries
library(dplyr)
library(readr)
library(igraph)

# 1. Load the dataset from your cloud storage
# Make sure the file name matches exactly what you uploaded
events <- read_csv("events.csv")

# 2. Build the Edge List using session_id
edges <- events %>%
  arrange(session_id, timestamp) %>%
  group_by(session_id) %>%
  mutate(next_event = lead(event_type)) %>% 
  ungroup() %>%
  filter(!is.na(next_event)) %>%
  rename(from_page = event_type, to_page = next_event) %>%
  select(from_page, to_page)

# 3. Aggregate the paths 
edge_list <- edges %>%
  group_by(from_page, to_page) %>%
  summarise(traffic_weight = n(), .groups = "drop") %>%
  arrange(desc(traffic_weight))

# 4. Create the Mathematical Graph Object
web_graph <- graph_from_data_frame(d = edge_list, directed = TRUE)

# 5. Run PageRank
pr <- page_rank(web_graph)$vector
print(head(sort(pr, decreasing = TRUE)))
