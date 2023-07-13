
library(glue)
library(jsonlite)
library(ggplot2)
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(visNetwork)
library(lubridate)

function(input, output, session) {
  
  ## Converting a vector to an html list
  to_html_list <- function(v) {
    HTML(
      str_c("<ul>", 
            str_c("<li>", v, "</li>", collapse = ""), 
            "</ul>"
      )
    )
  }
  
  ## A convenience function for formatting numbers in text
  big_num <- function(n) format(x = n, big.mark = ",")
  
  ###############################
  ## Top 50 posts from r/books ##
  ###############################
  
  ## Dynamically loading data
  top50 <- reactive({
    top_posts <- fromJSON("https://www.reddit.com/r/books/top.json?t=all&limit=50&raw_json=1")$data[["children"]]
    tibble(
      Author = top_posts[["data"]][["author"]],
      Title = top_posts[["data"]][["title"]],
      Content = str_c(
        replace_na(top_posts[["data"]][["selftext"]], ""), 
        sep = " ", 
        replace_na(top_posts[["data"]][["url_overridden_by_dest"]], "")
      ) %>%
        str_trim(),
      `Has URL` = !is.na(top_posts[["data"]][["url_overridden_by_dest"]]),
      Year = year(as_datetime(top_posts[["data"]][["created_utc"]])),
      Comments = top_posts[["data"]][["num_comments"]],
      Upvotes = top_posts[["data"]][["ups"]]
    )
  })
  
  ## Table showing tabular data format
  output$rbooks_table <- renderDataTable({
    top_posts <- top50() %>%
      head(n = 20) %>%
      select(-`Has URL`) %>%
      mutate(
        Comments = big_num(Comments),
        Upvotes = big_num(Upvotes)
      )
    
    datatable(
      top_posts,
      options = list(
        scrollY = "250px",
        paging = FALSE,
        searching = FALSE
      ), 
      caption = "Table 1: top 20 posts in r/books.")
  })
  
  ## Bullet points showing descriptive statistics
  output$rbooks_ds <- renderUI(
    to_html_list( 
      c(
        str_c("Average post year: ", round(mean(top50()$Year))),
        str_c("Earliest post: ", min(top50()$Year)), 
        str_c("Latest post: ", max(top50()$Year)),
        str_c("Most upvotes received: ", big_num(max(top50()$Upvotes))),
        str_c("Least upvotes received: ", big_num(min(top50()$Upvotes))),
        str_c("Percentage posts with a URL: ", sum(top50()$`Has URL`) * 100 / 50, "%")
      )
    )
  )
  
  ## Plot showing up-votes vs comments for top 50 r/books posts
  output$rbooks_plot <- renderPlot({
    dt <- top50() %>%
      select(-Author, -Title, -Content, -`Has URL`)
    
    ggplot(data = dt) +
      geom_point(aes(x = Upvotes, y = Comments)) +
      theme_minimal() +
      ggtitle("Number of Comments vs. Number of Upvotes")
  })
  
  ###############################################################
  ## Top 100 most popular subreddits, shared moderator network ##
  ###############################################################
  
  ## Loading the igraph object
  pop100 <- reactive({
    load("top100_popular_igraph.RData")
    G
  })
  
  output$moderator_table <- renderDataTable({
    mod_dt <- tibble(
      Moderator = E(pop100())$label
    ) %>%
      group_by(Moderator) %>%
      summarise(.groups = "keep", N = n()) %>%
      as_tibble() %>%
      arrange(desc(N))
    
    datatable(
      data = mod_dt, 
      options = list(
        paging = TRUE,
        pageLength = 8,
        searching = FALSE,
        lengthChange = FALSE
      ), 
      caption = "Table 2: number of shared moderator ties by user."
    )
  })
  
  ## Convenience function to set attributes identically for visNetwork and visNetworkProxy
  set_graph_attrs <- function(g, edges_to_remove) {
    E(g)$color.color <- rep("#D3D3D3", ecount(g))
    E(g)$color.opacity <- ifelse(E(g)$id %in% edges_to_remove, 0, 0.9)
    # E(g)$font.size <- ifelse(E(g)$id %in% edges_to_remove, 0, 10)
    E(g)$font.size <- 0
    E(g)$font.color <- "#343434"
    V(g)$shape <- "square"
    g
  }
  
  ## Rendering the initial network visualization
  output$network <- renderVisNetwork({
    G <- pop100()
    
    G <- set_graph_attrs(G, c())
    
    G %>%
      visIgraph(randomSeed = 24234) %>%
      visEdges(arrows = list(to = list(enabled = FALSE), from = list(enabled = FALSE)))
  })
  
  ## Get the moderator names entered by the user
  moderator_names <- reactive({
    str_trim(str_split_1(input$exclude_labels, ","))
  })
  
  ## Updating the network to remove selected moderator edges
  observe({
    G <- pop100()
    
    all_edge_ids <- get.edge.attribute(G, "id")
    
    labels_to_remove <- moderator_names()
    
    edges_to_remove <- E(G)[E(G)$label %in% labels_to_remove]$id
    
    G <- set_graph_attrs(G, edges_to_remove)
    
    visNetworkProxy("network") %>%
      visEdges(arrows = list(to = list(enabled = FALSE), from = list(enabled = FALSE))) %>%
      visUpdateEdges(edges = get.data.frame(G, what = "edges"))
    
  })
  
  plot_graph <- function(h, coords, plottitle) {
    set.seed(12342)
    plot.igraph(
      x = simplify(h, remove.multiple = FALSE, remove.loops = TRUE),
      layout = coords,
      vertex.size = if_else(str_starts(V(h)$name, "r"), 30, 10),
      vertex.color = str_starts(V(h)$name, "r") + 1,
      vertex.frame.color = NA,
      vertex.shape = if_else(str_starts(V(h)$name, "r"), "circle", "square"),
      vertex.label.family = "sans",
      vertex.label.color = "black",
      vertex.label.font = if_else(str_starts(V(h)$name, "r"), 2, 1),
      vertex.label.dist = 1,
      vertex.label.cex = 0.75,
      edge.curved = if_else(E(h)$type == "original", 0, 0.25),
      edge.color = if_else(E(h)$type == "original", "lightgray", "darkgray"),
      edge.width = if_else(E(h)$type == "original", 0.9, 1.5),
      main = plottitle
    )
  }
  
  shared_moderator_example <- reactive({
    edges <- tribble(
      ~from, ~to,
      "r/cat_toys", "u/cat_luvr_99",
      "r/cat_pictures", "u/cat_luvr_99",
      "r/kittens", "u/cat_luvr_99",
      "r/cat_care", "u/cat_luvr_99",
      "r/cats", "u/cat_luvr_99",
      "r/cat_toys", "u/catdad67",
      "r/kittens", "u/catdad67",
      "r/cat_videos", "u/catdad67",
      "r/feline_facts", "u/petparent32"
    )
    
    edt <- edges %>%
      group_by(from) %>%
      summarise(.groups = "keep", `Moderated By` = paste0(sort(to), collapse = ", ")) %>%
      as_tibble() %>%
      rename(Subreddit = from)
    
    H <- graph_from_data_frame(d = edges, directed = FALSE)
    V(H)$type <- V(H)$name %>% str_starts("r")
    E(H)$weight <- 1
    E(H)$type <- "original"
    
    L <- layout.auto(H)
    
    Hp <- bipartite.projection(H, which = "true", multiplicity = TRUE, remove.type = FALSE)
    E(Hp)$type <- "projection"
    
    Hcomb <- H
    Hp_edges <- get.data.frame(Hp, "edges")
    for (i in 1:nrow(Hp_edges)) {
      Hp_e <- Hp_edges[i,]
      Hcomb <- Hcomb %>%
        add_edges(
          edges = c(Hp_e$from, Hp_e$to),
          attr = list(
            type = Hp_e$type,
            weight = Hp_e$weight
          )
        )
    }
    
    return(list(
      tab = edt,
      h_orig = H,
      c_orig = L,
      h_comb = Hcomb,
      c_comb = L,
      h_proj = Hp,
      c_proj = L[V(Hcomb)$name %in% V(Hp)$name,]
    ))
    })
  
  output$ex_tab <- renderDataTable({
    datatable(
      data = shared_moderator_example()$tab, 
      options = list(
        paging = FALSE,
        searching = FALSE,
        lengthChange = FALSE
      ), 
      caption = "Table 3: subreddits and moderators."
    )
  })
  
  output$ex_orig <- renderPlot({
    sme <- shared_moderator_example()
    plot_graph(h = sme$h_orig, coords = sme$c_orig, plottitle = "Figure A")
  })
  
  output$ex_comb <- renderPlot({
    sme <- shared_moderator_example()
    plot_graph(h = sme$h_comb, coords = sme$c_comb, plottitle = "Figure B")
  })

  output$ex_proj <- renderPlot({
    sme <- shared_moderator_example()
    plot_graph(h = sme$h_proj, coords = sme$c_proj, plottitle = "Figure C")
  })
  
  output$panel_style1 <- renderUI({
    tags$style(HTML(glue(.open = "@{", .close = "}@",
                         '#thematic_coding_primer div[value^="Learn More: Thematic Coding Primer"] > .panel-heading {
                               color: rgb(0, 0 ,0);
                               background-color: rgb(254,251,234);
                               text-align: left
                               }@;
                               }'
    )))
  })
  
  output$panel_style2 <- renderUI({
    tags$style(HTML(glue::glue(.open = "@{", .close = "}@",
                               '#understanding_network_proj div[value^="Learn More: Understanding Network Projections"] > .panel-heading {
                               color: rgb(0, 0 ,0);
                               background-color: rgb(254,251,234);
                               text-align: left
                               }@;
                               }'
    )))
  })
}
