# setup
pkgs <- c("dplyr", "igraph", "shiny")
for (pkg in pkgs) library(pkg, character.only = TRUE)

# functions
source("app/functions.r")

# read and aggregate data
datafiles <- c("org-org-connections",
               "person-org-connections",
               "person-person-connections")
ds <- lapply(datafiles, function(f) {
  d <- read.csv(paste0("data/", f, ".csv"), colClasses = "character")
  names(d)[1:2] <- c("Entity A", "Entity B")
  d$Type <- gsub("(.*)\\-[^\\-]+$", "\\1", f)
  d
})
alldat <- dplyr::bind_rows(ds)
names(alldat)[4] <- "Source(s)"

# server
server <- function(input, output) {
  
  # select dataset
  twdat <- reactive({
    
    ## implicit links
    #if (input$implicit) {
    #  affdat <- alldat[alldat$Type == "person-org", ]
    #  impdats <- list()
    #  if (input$nodes != "Persons") {
    #    impdats <- c(impdats, list(data.frame(
    #      implicit_pairs(affdat, "Entity A", "Entity B"),
    #      Type = "org-org"
    #    )))
    #  }
    #  if (input$nodes != "Organizations") {
    #    impdats <- c(impdats, list(data.frame(
    #      implicit_pairs(affdat, "Entity B", "Entity A"),
    #      Type = "person-person"
    #    )))
    #  }
    #  impdat <- dplyr::bind_rows(impdats)
    #  names(impdat)[1:3] <- c("Entity A", "Entity B", "Connection")
    #  # bind explicit and implicit links
    #  nr <- nrow(alldat)
    #  alldat <- dplyr::bind_rows(alldat, impdat)
    #  # remove duplicate links
    #  dupes <- duplicated_links(alldat, 1, 2)
    #  alldat <- alldat[!dupes, ]
    #}
    
    # desired dataset
    if (input$nodes == "All") {
      dat <- alldat
    } else if (input$nodes == "Organizations") {
      dat <- alldat %>% dplyr::filter(Type == "org-org")
      names(dat)[1:2] <- c("Organization A", "Organization B")
    } else if (input$nodes == "Persons") {
      dat <- alldat %>% dplyr::filter(Type == "person-person")
      names(dat)[1:2] <- c("Person A", "Person B")
    }
    
    dat
  })
  
  # output node list for ego selection
  output$egos <- renderUI({
    dat <- twdat()
    entities <- dat[, 1:2] %>% unlist() %>% unname() %>% unique() %>% sort()
    default_ego <- if (input$nodes == "Organizations") {
      "THE TRUMP ORGANIZATION, INC."
    } else {
      "DONALD J. TRUMP"
    }
    selectizeInput(
      inputId = "center", label = "Ego(s)",
      choices = entities,
      selected = default_ego,
      multiple = TRUE
    )
  })
  
  # construct graph
  twgraph <- reactive({
    dat <- twdat()
    g <- igraph::graph_from_edgelist(el = as.matrix(dat[, 1:2]),
                                     directed = TRUE)
    # node types
    orgs <- unique(c(dat[dat$Type == "person-org", 1],
                     unlist(dat[dat$Type == "org-org", 1:2])))
    persons <- unique(c(dat[dat$Type == "person-org", 2],
                        unlist(dat[dat$Type == "person-person", 1:2])))
    V(g)$type <- ifelse(match(V(g)$name, c(orgs, persons)) <= length(orgs),
                        "org",
                        "person")
    # return graph
    g
  })
  
  # calculate degree and betweenness
  twcent <- reactive({
    g <- twgraph()
    
    data.frame(
      degree = igraph::degree(g),
      betweenness = igraph::betweenness(g)
    )
  })
  
  # construct ego subgraph
  twego <- reactive({
    g <- twgraph()
    
    # ego graph
    vs <- igraph::ego(
      graph = g,
      order = input$order,
      nodes = which(V(g)$name %in% input$center),
      mode = "all"
    ) %>% unlist() %>% unique()
    h <- igraph::induced_subgraph(
      graph = g,
      vids = vs
    )
    
    # flag ego nodes
    V(h)$is_ego <- V(h)$name %in% input$center
    
    h
  })
  
  twlayout <- reactive({
    g <- twego()
    
    # ego index
    e <- match(input$center, V(g)$name)
    # layout
    l <- if (input$layout.alg == "Tree") {
      -igraph::layout_as_tree(graph = igraph::as.undirected(g), root = e)[, 2:1]
    } else if (input$layout.alg == "Star") {
      igraph::layout_as_star(graph = igraph::as.undirected(g), center = e)
    } else if (input$layout.alg == "Force-directed") {
      igraph::layout_with_fr(graph = igraph::as.undirected(g))
    }
    
    # row names
    rownames(l) <- V(g)$name
    
    l
  })
  
  # ego data
  twegodat <- reactive({
    dat <- twdat()
    g <- twego()
    
    # restrict to ego network
    dat <- dat[dat[, 1] %in% V(g)$name & dat[, 2] %in% V(g)$name, ]
    # order by ego network vertex sequence
    dat <- dat[order(match(as.character(dat[, 1]), V(g)$name),
                     match(as.character(dat[, 2]), V(g)$name)), ]
  })
  
  # ego table
  output$table <- shiny::renderDataTable({
    dat <- twegodat()
    
  }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))
  
  # categorize links in ego network
  twegoconn <- reactive({
    g <- twego()
    dat <- twegodat()
    
    # identify most frequent types of connection
    conntab <- table(dat$Connection)
    #connincl <- names(conntab[conntab > input$conn.thres])
    connincl <- names(conntab[conntab > 15])
    # replace others with "Other"
    connnew <- as.character(dat$Connection)
    connnew[!(connnew %in% connincl)] <- "Other"
    dat$Connection <- as.factor(connnew)
    # assign connection types to edges
    E(g)$connection <- as.character(dat$Connection)
    
    g
  })
  
  # ego plot
  output$plot <- renderPlot({
    g <- twegoconn()
    l <- twlayout()
    
    # aesthetics
    V(g)$frame.color <- "grey"
    # ego aesthetics
    V(g)$color <- ifelse(V(g)$is_ego, "salmon", "lightgrey")
    V(g)$size <- ifelse(V(g)$is_ego, 2 * input$node.size, input$node.size)
    V(g)$shape <- c("square", "circle")[match(V(g)$type, c("org", "person"))]
    if (input$label.scheme == "None") {
      V(g)$label <- NA
    } else {
      V(g)$label <- V(g)$name
      V(g)$label.family <- "sans"
      V(g)$label.color <- "#000000cc"
      V(g)$label.cex <- .75
    }
    if (input$label.scheme == "High-degree") {
      cent <- twcent()
      vals <- cent[V(g)$name, ]$degree
      mindegree <- find_elbow(vals = vals)
      V(g)$label[vals < mindegree] <- NA
    } else if (input$label.scheme == "High-betweenness") {
      cent <- twcent()
      vals <- cent[V(g)$name, ]$betweenness
      minbetweenness <- find_elbow(vals = vals)
      V(g)$label[vals < minbetweenness] <- NA
    }
    if (input$code.links) {
      len <- length(unique(E(g)$connection))
      ecols <- if (len <= 9) {
        RColorBrewer::brewer.pal(n = len, name = "Set1")
      } else if (len <= 12) {
        RColorBrewer::brewer.pal(n = len, name = "Set3")
      } else {
        rainbow(n = len)
      }

      E(g)$color <- ecols[as.factor(E(g)$connection)]
    }
    
    # plot
    plot(x = g,
         layout = l,
         edge.arrow.size = .25, edge.arrow.width = .5)
    
    # legends
    if (input$code.links) {
      legend("topleft", pch = NA, lty = 1, cex = 1, bty = "n",
             col = ecols, legend = unique(E(g)$connection))
    }
  })
  
  # plot text
  output$info <- renderText({
    l <- twlayout()
    
    # scale
    l <- scale(l,
               center = apply(l, 2, function(x) mean(range(x))),
               scale = apply(l, 2, function(x) {diff(range(x)) / 2}))
    # data frame
    ldf <- data.frame(l, name = rownames(l))
    # identify nodes
    nodes <- if (is.null(input$plot.brush)) {
      nearPoints(df = ldf, input$plot.hover, "X1", "X2")
    } else {
      brushedPoints(df = ldf, brush = input$plot.brush, "X1", "X2")
    }
    
    paste(as.character(nodes$name), collapse = ", ")
  })
}
