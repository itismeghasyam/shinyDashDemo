# app.R
library(shiny)
library(bslib)

##### ---- Mock "database" ---- #####

initial_dashboards <- data.frame(
  id = c("sales_pipeline", "sales_forecast", "ops_inventory",
         "finance_pnl", "hr_headcount", "csat_overview"),
  title = c(
    "Sales Pipeline Overview",
    "Sales Forecast Dashboard",
    "Operations Inventory Monitor",
    "Finance P&L Summary",
    "HR Headcount & Attrition",
    "Customer Satisfaction Overview"
  ),
  team = c("Sales", "Sales", "Operations", "Finance", "HR", "Customer Success"),
  tool = c("tableau", "tableau", "smartsheet", "tableau", "smartsheet", "tableau"),
  url_embed = c(
    "https://tableau.company.com/views/sales_pipeline",
    "https://tableau.company.com/views/sales_forecast",
    "https://smartsheet.com/sheets/ops_inventory",
    "https://tableau.company.com/views/finance_pnl",
    "https://smartsheet.com/sheets/hr_headcount",
    "https://tableau.company.com/views/csat_overview"
  ),
  description = c(
    "Weekly pipeline health across segments.",
    "Forecast by region, segment, and product line.",
    "Inventory levels and stockout risk by warehouse.",
    "Monthly P&L with drilldowns by cost center.",
    "Headcount, hiring, and attrition trends.",
    "CSAT, NPS, and key support KPIs."
  ),
  tags = c(
    "sales, pipeline, weekly",
    "sales, forecast, planning",
    "operations, inventory, supply chain",
    "finance, pnl, accounting",
    "hr, people, talent",
    "support, csat, nps"
  ),
  views = c(42, 27, 15, 30, 10, 22),
  stringsAsFactors = FALSE
)

news_items <- data.frame(
  title = c(
    "New Finance P&L Dashboard v2.0",
    "Sales Pipeline Data Refresh Schedule Updated",
    "Inventory Anomalies Alerting Pilot Launched"
  ),
  description = c(
    "We’ve rolled out a new version of the P&L dashboard with better drilldowns.",
    "Sales data now refreshes 4x per day instead of daily.",
    "Ops team is piloting anomaly detection on inventory movements."
  ),
  link = c("#", "#", "#"),
  stringsAsFactors = FALSE
)

teams <- c("Overview", "Sales", "Operations", "Finance", "HR", "Customer Success")
contact_tab_value <- "Contact Support"

##### ---- Helper functions ---- #####

get_current_user <- function(session) {
  # In real life, you'd use session$user or an env/header set by SSO
  user <- session$user
  if (is.null(user) || !nzchar(user)) {
    user <- "test_user@company.com"
  }
  user
}

dashboard_link <- function(id, title) {
  tags$a(
    href = "#",
    title,
    onclick = sprintf(
      "Shiny.setInputValue('open_dashboard', '%s', {priority: 'event'}); return false;",
      id
    )
  )
}

dashboard_list_ui <- function(dash_df) {
  if (nrow(dash_df) == 0) {
    return(tags$p(em("No dashboards to show.")))
  }
  tags$ul(
    class = "dashboard-list",
    lapply(seq_len(nrow(dash_df)), function(i) {
      tags$li(
        class = "dashboard-item",
        dashboard_link(dash_df$id[i], dash_df$title[i]),
        tags$span(class = "dashboard-meta", paste(" · Team:", dash_df$team[i]))
      )
    })
  )
}

home_page_ui <- function(input, dashboards, favorites, recently_viewed) {
  # Popular dashboards
  popular <- dashboards[order(-dashboards$views), , drop = FALSE]
  popular <- head(popular, 5)
  
  # Safe handling of search text (no %||%)
  search_text <- input$search_text
  if (is.null(search_text)) search_text <- ""
  query <- trimws(search_text)
  
  if (nzchar(query)) {
    q <- tolower(query)
    matches <- grepl(q, tolower(dashboards$title)) |
      grepl(q, tolower(dashboards$description)) |
      grepl(q, tolower(dashboards$tags))
    search_results <- dashboards[matches, , drop = FALSE]
  } else {
    search_results <- dashboards[0, , drop = FALSE]
  }
  
  fav_df <- dashboards[dashboards$id %in% favorites, , drop = FALSE]
  recent_df <- dashboards[dashboards$id %in% recently_viewed, , drop = FALSE]
  
  my_ids <- unique(c(favorites, recently_viewed))
  my_all_df <- dashboards[dashboards$id %in% my_ids, , drop = FALSE]
  
  tagList(
    fluidRow(
      column(
        width = 6,
        div(
          class = "card-block",
          h3("Search dashboards"),
          textInput("search_text", NULL,
                    placeholder = "Search by dashboard name, description, or tag"),
          if (nzchar(query)) {
            tagList(
              tags$h5(
                class = "section-subtitle",
                paste("Search results for:", shQuote(query))
              ),
              dashboard_list_ui(search_results)
            )
          } else {
            tags$p(class = "muted", "Type to search across all dashboards.")
          }
        )
      ),
      column(
        width = 6,
        div(
          class = "card-block",
          h3("Popular dashboards"),
          tags$p(class = "muted", "Based on recent view counts."),
          dashboard_list_ui(popular)
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        div(
          class = "card-block",
          h3("My resources & links"),
          tags$p(
            class = "muted",
            "Personal to you. Backed by SSO in real deployment."
          ),
          tabsetPanel(
            id = "my_resources_tabs",
            type = "pills",
            tabPanel(
              "Recently viewed",
              value = "recent",
              dashboard_list_ui(recent_df)
            ),
            tabPanel(
              "Favorites",
              value = "favorites",
              dashboard_list_ui(fav_df)
            ),
            tabPanel(
              "All my resources",
              value = "all",
              dashboard_list_ui(my_all_df)
            )
          )
        )
      ),
      column(
        width = 5,
        div(
          class = "card-block",
          h3("News & updates"),
          tags$p(class = "muted", "Announcements from the analytics hub."),
          tags$ul(
            class = "news-list",
            lapply(seq_len(nrow(news_items)), function(i) {
              tags$li(
                class = "news-item",
                strong(news_items$title[i]),
                tags$p(class = "news-desc", news_items$description[i])
              )
            })
          )
        )
      )
    )
  )
}

team_page_ui <- function(team, dashboards) {
  team_df <- dashboards[dashboards$team == team, , drop = FALSE]
  
  tagList(
    h2(team),
    tags$p(
      class = "muted",
      paste("Dashboards owned by or primarily serving the", team, "team.")
    ),
    dashboard_list_ui(team_df)
  )
}

contact_page_ui <- function() {
  tagList(
    h2("Contact support"),
    tags$p(
      "Tell us what you need help with. This form is just a placeholder; ",
      "in production, it would create a ticket (e.g., JIRA, ServiceNow, etc.)."
    ),
    textInput("contact_subject", "Subject"),
    selectInput(
      "contact_category",
      "Category",
      c("Access issue", "Data question", "Dashboard bug", "Feature request", "Other"),
      selected = "Access issue"
    ),
    textAreaInput("contact_message", "Details", width = "100%", height = "150px"),
    actionButton("submit_contact", "Submit request", class = "btn-primary"),
    uiOutput("contact_submit_status")
  )
}

dashboard_page_ui <- function(dash, is_favorite) {
  if (is.null(dash)) {
    return(tags$p("No dashboard selected."))
  }
  
  favorite_label <- if (is_favorite) "Remove from favorites" else "Add to favorites"
  
  tagList(
    h2(dash$title),
    tags$p(class = "muted", paste("Team:", dash$team, "· Tool:", dash$tool)),
    tags$p(dash$description),
    actionButton("toggle_favorite", favorite_label, class = "btn-outline-primary"),
    tags$hr(),
    tags$p(
      class = "muted",
      "Embedded dashboard (placeholder). In production, this would use ",
      "a proper, auth-aware embed URL."
    ),
    tags$div(
      class = "dashboard-embed-wrapper",
      tags$iframe(
        src = dash$url_embed,
        width = "100%",
        height = "600px",
        frameborder = "0",
        sandbox = "allow-same-origin allow-scripts allow-popups allow-forms"
      )
    )
  )
}

##### ---- Build team tabs correctly (no list directly inside tabsetPanel) ---- #####

team_tab_panels <- c(
  list(tabPanel("Overview", value = "Overview")),
  lapply(teams[teams != "Overview"], function(t) {
    tabPanel(title = t, value = t)
  }),
  list(tabPanel("Contact Support", value = contact_tab_value))
)

team_tabs_ui <- do.call(
  tabsetPanel,
  c(
    list(id = "team_tabs", type = "tabs"),
    team_tab_panels
  )
)

##### ---- UI ---- #####

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  tags$head(
    tags$title("Analytics Hub"),
    tags$style(HTML("
      body {
        background-color: #f7f7f9;
      }

      .app-container {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
      }

      .top-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 0.75rem 1.75rem;
        border-bottom: 1px solid #e5e5e5;
        background-color: #ffffff;
      }

      .top-header-left {
        font-size: 1.25rem;
        font-weight: 600;
      }

      .top-header-right > *:not(:last-child) {
        margin-right: 0.5rem;
      }

      .team-tabs-row {
        background-color: #ffffff;
        border-bottom: 1px solid #e5e5e5;
        padding: 0 1.75rem 0.5rem 1.75rem;
      }

      .main-content {
        flex: 1;
        padding: 1.5rem 1.75rem 2rem 1.75rem;
      }

      .footer {
        padding: 1rem 1.75rem;
        border-top: 1px solid #e5e5e5;
        background-color: #ffffff;
        font-size: 0.85rem;
        color: #777;
      }

      .footer .footer-section-title {
        font-weight: 600;
        margin-bottom: 0.25rem;
      }

      .footer .footer-links a {
        display: block;
        color: #555;
        text-decoration: none;
        margin-bottom: 0.15rem;
      }

      .footer .footer-links a:hover {
        text-decoration: underline;
      }

      .card-block {
        background-color: #ffffff;
        border-radius: 0.5rem;
        padding: 1rem 1.25rem;
        box-shadow: 0 1px 3px rgba(0,0,0,0.06);
        margin-bottom: 1.25rem;
      }

      .section-subtitle {
        margin-top: 0.75rem;
        font-size: 0.9rem;
      }

      .muted {
        color: #777;
        font-size: 0.9rem;
      }

      .dashboard-list {
        list-style-type: none;
        padding-left: 0;
        margin-top: 0.5rem;
      }

      .dashboard-item {
        margin-bottom: 0.35rem;
      }

      .dashboard-item a {
        font-weight: 500;
      }

      .dashboard-meta {
        font-size: 0.8rem;
        color: #999;
      }

      .news-list {
        list-style-type: none;
        padding-left: 0;
      }

      .news-item {
        margin-bottom: 0.75rem;
      }

      .news-desc {
        margin: 0.15rem 0 0 0;
        font-size: 0.85rem;
        color: #777;
      }

      .dashboard-embed-wrapper {
        border: 1px solid #ddd;
        border-radius: 0.5rem;
        overflow: hidden;
        background-color: #fdfdfd;
      }

      .floating-chat-btn {
        position: fixed;
        bottom: 20px;
        right: 20px;
        border-radius: 999px;
        z-index: 1050;
      }

      .feedback-tab-btn {
        position: fixed;
        top: 50%;
        right: 0;
        transform: translateY(-50%);
        border-radius: 999px 0 0 999px;
        padding: 0.5rem 0.75rem;
        z-index: 1040;
      }
    "))
  ),
  div(
    class = "app-container",
    div(
      class = "top-header",
      div(
        class = "top-header-left",
        "Analytics Hub"
      ),
      div(
        class = "top-header-right",
        actionButton("btn_favorites_global", "Favorite dashboards"),
        actionButton("btn_tasks", "My tasks"),
        actionButton("btn_account", "Account")
      )
    ),
    div(
      class = "team-tabs-row",
      team_tabs_ui
    ),
    div(
      class = "main-content",
      uiOutput("main_content")
    ),
    div(
      class = "footer",
      fluidRow(
        column(
          3,
          strong("Analytics Hub"),
          tags$p(class = "muted", "Central portal for analytics dashboards and tools.")
        ),
        column(
          3,
          div(class = "footer-section-title", "People"),
          div(
            class = "footer-links",
            tags$a(href = "#", "Meet the team"),
            tags$a(href = "#", "Org structure")
          )
        ),
        column(
          3,
          div(class = "footer-section-title", "Support"),
          div(
            class = "footer-links",
            tags$a(href = "#", "JIRA"),
            tags$a(href = "#", "Confluence"),
            tags$a(href = "#", "Submit a request"),
            tags$a(href = "#", "FAQ")
          )
        ),
        column(
          3,
          div(class = "footer-section-title", "Links"),
          div(
            class = "footer-links",
            tags$a(href = "#", "Data catalog"),
            tags$a(href = "#", "Governance"),
            tags$a(href = "#", "Training")
          )
        )
      )
    )
  ),
  actionButton(
    "open_chat",
    label = "AI Smart Chat",
    class = "btn-primary floating-chat-btn"
  ),
  actionButton(
    "open_feedback",
    label = "Give feedback",
    class = "btn-secondary feedback-tab-btn"
  )
)

##### ---- Server ---- #####

server <- function(input, output, session) {
  
  dashboards_rv <- reactiveVal(initial_dashboards)
  favorites_rv <- reactiveVal(character(0))
  recently_viewed_rv <- reactiveVal(character(0))
  
  current_view <- reactiveVal("home")  # "home", "team", "dashboard", "contact"
  selected_team <- reactiveVal(NULL)
  selected_dashboard_id <- reactiveVal(NULL)
  
  current_user <- get_current_user(session)
  
  observeEvent(input$team_tabs, {
    tab_val <- input$team_tabs
    
    if (tab_val == "Overview") {
      current_view("home")
      selected_team(NULL)
    } else if (tab_val == contact_tab_value) {
      current_view("contact")
      selected_team(NULL)
    } else {
      current_view("team")
      selected_team(tab_val)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_favorites_global, {
    showModal(modalDialog(
      title = "Favorite dashboards (global header placeholder)",
      "In a future version, this could jump straight to your favorites view.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_tasks, {
    showModal(modalDialog(
      title = "My tasks (placeholder)",
      "This could integrate with JIRA, ServiceNow, or internal workflow tools.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_account, {
    showModal(modalDialog(
      title = "Account (placeholder)",
      paste("You are signed in as:", current_user),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$open_chat, {
    showModal(modalDialog(
      title = "AI Smart Chat (placeholder)",
      tags$p(
        "This is a placeholder for an AI assistant. ",
        "In production, this could call the ChatGPT API or another LLM."
      ),
      textAreaInput(
        "chat_question",
        "Ask a question about the analytics hub or dashboards:",
        width = "100%",
        height = "100px"
      ),
      actionButton("chat_send", "Send"),
      tags$hr(),
      verbatimTextOutput("chat_response"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  output$chat_response <- renderPrint({
    req(input$chat_send)
    cat("AI Smart Chat is not wired up yet.\n\n",
        "You asked:\n", input$chat_question, "\n\n",
        "In production, this would return a real AI-generated answer.")
  })
  
  observeEvent(input$open_feedback, {
    showModal(modalDialog(
      title = "Give feedback",
      textInput("feedback_subject", "Subject"),
      selectInput(
        "feedback_type", "Type",
        c("Bug", "Feature request", "Usability", "Other"),
        selected = "Feature request"
      ),
      textAreaInput(
        "feedback_details",
        "Details",
        width = "100%",
        height = "150px"
      ),
      textInput("feedback_contact", "How can we reach you? (optional)",
                value = current_user),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_feedback", "Submit", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$submit_feedback, {
    showNotification("Thank you for your feedback!", type = "message")
    removeModal()
  })
  
  output$contact_submit_status <- renderUI({
    req(input$submit_contact)
    tags$p(
      class = "muted",
      "Your request has been submitted (placeholder). ",
      "In production, this would create a ticket."
    )
  })
  
  observeEvent(input$open_dashboard, {
    dash_id <- input$open_dashboard
    selected_dashboard_id(dash_id)
    current_view("dashboard")
    
    recent <- recently_viewed_rv()
    new_recent <- unique(c(dash_id, recent))
    if (length(new_recent) > 10) {
      new_recent <- new_recent[1:10]
    }
    recently_viewed_rv(new_recent)
    
    db <- dashboards_rv()
    db$views[db$id == dash_id] <- db$views[db$id == dash_id] + 1
    dashboards_rv(db)
  })
  
  observeEvent(input$toggle_favorite, {
    dash_id <- selected_dashboard_id()
    if (is.null(dash_id)) return()
    
    favs <- favorites_rv()
    if (dash_id %in% favs) {
      favorites_rv(setdiff(favs, dash_id))
      showNotification("Removed from favorites.", type = "default")
    } else {
      favorites_rv(c(favs, dash_id))
      showNotification("Added to favorites.", type = "message")
    }
  })
  
  output$main_content <- renderUI({
    db <- dashboards_rv()
    favs <- favorites_rv()
    recent <- recently_viewed_rv()
    
    view <- current_view()
    
    if (view == "home") {
      home_page_ui(input, db, favs, recent)
    } else if (view == "team") {
      tm <- selected_team()
      if (is.null(tm)) {
        tags$p("No team selected.")
      } else {
        team_page_ui(tm, db)
      }
    } else if (view == "dashboard") {
      dash_id <- selected_dashboard_id()
      dash <- db[db$id == dash_id, , drop = FALSE]
      if (nrow(dash) == 0) {
        tags$p("The selected dashboard could not be found.")
      } else {
        dashboard_page_ui(dash[1, ], is_favorite = dash_id %in% favs)
      }
    } else if (view == "contact") {
      contact_page_ui()
    } else {
      tags$p("Unknown view.")
    }
  })
}

shinyApp(ui, server)
