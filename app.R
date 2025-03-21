library(shiny)
library(bslib)
library(promises)
library(fastmap)
library(duckdb)
library(DBI)
library(fontawesome)
library(reactable)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(here)
library(ellmer)
library(shinychat)
library(survminer)
library(survival)

# run setup.R to prepare voyager_PFS.csv file for duckdb file

# Open the duckdb database
conn <- dbConnect(duckdb(), dbdir = here("pfs.duckdb"), read_only = TRUE)
# Close the database when the app stops
onStop(\() dbDisconnect(conn))

# gpt-4o does much better than gpt-4o-mini, especially at interpreting plots
gemini_model <- "gemini-2.0-flash"

# Dynamically create the system prompt, based on the real data. For an actually
# large database, you wouldn't want to retrieve all the data like this, but
# instead either hand-write the schema or write your own routine that is more
# efficient than system_prompt().
system_prompt_str <- system_prompt(dbGetQuery(conn, "SELECT * FROM pfs"), "pfs")

# This is the greeting that should initially appear in the sidebar when the app
# loads.
greeting <- paste(readLines(here("greeting.md")), collapse = "\n")

icon_explain <- tags$img(src = "stars.svg")

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "Patient trial outcome",
  includeCSS(here("styles.css")),
  sidebar = sidebar(
    width = 400,
    style = "height: 100%;",
    chat_ui("chat", height = "100%", fill = TRUE)
  ),
  useBusyIndicators(),

  # 🏷️ Header
  textOutput("show_title", container = h3),
  verbatimTextOutput("show_query") |>
    tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),

  # 🎯 Value boxes
  layout_columns(
    fill = FALSE,
    value_box(
      showcase = fa_i("user"),
      "Total patients",
      textOutput("total_patients", inline = TRUE)
    ),
    value_box(
      showcase = fa_i("cake-candles"),
      "Median age",
      textOutput("average_age", inline = TRUE)
    ),
    value_box(
      showcase = fa_i("calendar"),
      "Median PFS",
      textOutput("average_pfs", inline = TRUE)
    ),
  ),
  layout_columns(
    style = "min-height: 800px;",
    col_widths = c(6, 6, 12),

    # 📊 Scatter plot
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "PFS vs Age",
        span(
          actionLink(
            "interpret_scatter",
            icon_explain,
            class = "me-3 text-decoration-none",
            aria_label = "Explain scatter plot"
          ),
          popover(
            title = "Add a color variable", placement = "top",
            fa_i("ellipsis"),
            radioButtons(
              "scatter_color",
              NULL,
              c("none", "SEX", "RACE", "COUNTRY", "BEST_RESPONSE_CHAR"),
              inline = TRUE
            )
          )
        )
      ),
      plotlyOutput("scatterplot")
    ),
  
  # 📊 Survival plot
  card(
    card_header("Survival plot"),
    selectInput("mutation", "Select a mutation",
                choices = c("gene1_ex9" = "Gene1_ex9",
                            "gene1_ex11" = "Gene1_ex11",
                            "gene1_ex17_hs" = "Gene1_ex17_hs",
                            "gene1_ex17_others" = "Gene1_ex17_others",
                            "gene1_ex13_V654A"  = "Gene1_ex13_V654A",
                            "gene1_ex13_K642E" = "Gene1_ex13_K642E",
                            "gene1_ex13_others" = "Gene1_ex13_others",
                            "gene1_others" = "Gene1_others",
                            "gene2_ex17" = "Gene2_ex17",
                            "gene2_others" = "Gene2_others"),
                selected = "Gene1_ex13_V654A"),
    plotOutput("survivalplot")
  ),
  
  # 🔍 Data table
  card(
    style = "height: 400px;",
    card_header("PFS data"),
    reactableOutput("table", height = "100%")
  ),
  )
)

server <- function(input, output, session) {
  # 🔄 Reactive state/computation --------------------------------------------

  current_title <- reactiveVal(NULL)
  current_query <- reactiveVal("")

  # This object must always be passed as the `.ctx` argument to query(), so that
  # tool functions can access the context they need to do their jobs; in this
  # case, the database connection that query() needs.
  ctx <- list(conn = conn)

  # The reactive data frame. Either returns the entire dataset, or filtered by
  # whatever Sidebot decided.
  pfs_data <- reactive({
    sql <- current_query()
    if (is.null(sql) || sql == "") {
      sql <- "SELECT * FROM pfs;"
    }
    dbGetQuery(conn, sql)
  })



  # 🏷️ Header outputs --------------------------------------------------------

  output$show_title <- renderText({
    current_title()
  })

  output$show_query <- renderText({
    current_query()
  })



  # 🎯 Value box outputs -----------------------------------------------------

  output$total_patients <- renderText({
    nrow(pfs_data())
  })
  
  output$average_age <- renderText({
    x <- median(pfs_data()$AGE, na.rm=T)
    paste0(formatC(x, format = "f", digits = 1, big.mark = ","), " years")
  })

  output$average_pfs <- renderText({
    x <- median(pfs_data()$PFS_TIME, na.rm=T)
    paste0(formatC(x, format = "f", digits = 1, big.mark = ","), " months")
  })


  # 📊 Scatter plot ----------------------------------------------------------

  scatterplot <- reactive({
    req(nrow(pfs_data()) > 0)

    color <- input$scatter_color

    data <- pfs_data()
    data <- data[!is.na(data$AGE), ] 
    # avoid missing data leading to 
    # non-rendering plot

    p <- plot_ly(data, x = ~PFS_TIME, y = ~AGE, type = "scatter", mode = "markers")

    if (color != "none") {
      p <- plot_ly(data,
        x = ~PFS_TIME, y = ~AGE, color = as.formula(paste0("~", color)),
        type = "scatter", mode = "markers"
      )
    }

    p <- p |> add_lines(
      x = ~PFS_TIME, y = fitted(loess(AGE ~ PFS_TIME, data = data)),
      line = list(color = "rgba(255, 0, 0, 0.5)"),
      name = "LOESS", inherit = FALSE
    )

    p <- p |> layout(showlegend = FALSE)

    return(p)
  })

  output$scatterplot <- renderPlotly({
    scatterplot()
  })

  observeEvent(input$interpret_scatter, {
    explain_plot(chat, scatterplot(), model = gemini_model, .ctx = ctx)
  })
  
  
  # 📊 Survival KM-curve plot -----------------------------------------------------
  survivalplot <- reactive({
    req(nrow(pfs_data()) > 0)
    req(input$mutation)
    
    predictor <- input$mutation
    
    surv_data <- pfs_data()
    surv_data <- surv_data[!is.na(surv_data$AGE), ]
    # avoid missing data leading to
    # non-rendering plot
    
    response <- "Surv(PFS_TIME, PFS_CENSOR) ~ "
    
    formula <- as.formula(paste0(response, predictor))
    km_fit <- survfit(formula, 
                      data = surv_data)
    
    # Perform log-rank test
    test <- survdiff(formula, data = surv_data)
    # Extract p-value
    p_value <- 1 - pchisq(test$chisq, length(test$n) - 1)
    # Format p-value for display
    if (p_value < 0.001) {
      p_text <- "p < 0.001"
    } else {
      p_text <- paste("p =", round(p_value, 3))
    }
    
    plot(km_fit,
         col = 1:length(km_fit$strata),
         xlab = "Months",
         ylab="PFS probability",
    )
    legend("topright", 
           legend = c(names(km_fit$strata), paste("Log-rank test:", p_text)), 
           col = c(1:length(km_fit$strata), NA),
           lwd = c(rep(2, length(km_fit$strata)), 0),
           bty = "n")
    
    p <- recordPlot()
    return(p)
  })
  
  output$survivalplot <- renderPlot({
    par(mar = c(4, 4, 0, 2))
    survivalplot()
  })

  # 🔍 Data table ------------------------------------------------------------
  
  output$table <- renderReactable({
    surv_data <- pfs_data()
    surv_data$PFS_TIME <- round(surv_data$PFS_TIME, digits=2)
    reactable(surv_data,
              pagination = FALSE, compact = TRUE
    )
  })

  
  # ✨ Sidebot ✨ -------------------------------------------------------------

  append_output <- function(...) {
    txt <- paste0(...)
    shinychat::chat_append_message(
      "chat",
      list(role = "assistant", content = txt),
      chunk = TRUE,
      operation = "append",
      session = session
    )
  }

  #' Modifies the data presented in the data dashboard, based on the given SQL
  #' query, and also updates the title.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @param title A title to display at the top of the data dashboard,
  #'   summarizing the intent of the SQL query.
  update_dashboard <- function(query, title) {
    append_output("\n```sql\n", query, "\n```\n\n")

    tryCatch(
      {
        # Try it to see if it errors; if so, the LLM will see the error
        dbGetQuery(conn, query)
      },
      error = function(err) {
        append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )

    if (!is.null(query)) {
      current_query(query)
    }
    if (!is.null(title)) {
      current_title(title)
    }
  }

  #' Reset data dashboard
  #' @param query Empty string
  #' @param title Empty string
  reset_dashboard <- function(query, title) {
    append_output("\n```sql\n", query, "\n```\n\n")
    
    if (!is.null(query)) {
      current_query(query)
    }
    if (!is.null(title)) {
      current_title(title)
    }
  }
  
  #' Perform a SQL query on the data, and return the results as JSON.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @return The results of the query as a JSON string.
  query <- function(query) {
    # Do this before query, in case it errors
    append_output("\n```sql\n", query, "\n```\n\n")

    tryCatch(
      {
        df <- dbGetQuery(conn, query)
      },
      error = function(e) {
        append_output("> Error: ", conditionMessage(e), "\n\n")
        stop(e)
      }
    )
  

    tbl_html <- df_to_html(df, maxrows = 5)
    append_output(tbl_html, "\n\n")

    df |> jsonlite::toJSON(auto_unbox = TRUE)
  }

  # Preload the conversation with the system prompt. These are instructions for
  # the chat model, and must not be shown to the end user.
  chat <- chat_gemini(model = gemini_model,
                      api_key = Sys.getenv("GEMINI_API_KEY"),
                      system_prompt = system_prompt_str)
  chat$register_tool(tool(
    update_dashboard,
    "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement."),
    title = type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")
  ))
  
  chat$register_tool(tool(
    reset_dashboard,
    "Remove all filters and reset dashboard to original state",
    query = type_string(""),
    title = type_string("")
  ))
  
  chat$register_tool(tool(
    query,
    "Perform a SQL query on the data, and return the results as JSON.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement.")
  ))

  # Prepopulate the chat UI with a welcome message that appears to be from the
  # chat model (but is actually hard-coded). This is just for the user, not for
  # the chat model to see.
  chat_append("chat", greeting)

  # Handle user input
  observeEvent(input$chat_user_input, {
    # Add user message to the chat history
    chat_append("chat", chat$stream_async(input$chat_user_input)) %...>% {
      # print(chat)
    }
  })
}

df_to_html <- function(df, maxrows = 5) {
  df_short <- if (nrow(df) > 10) head(df, maxrows) else df

  tbl_html <- capture.output(
    df_short |>
      xtable::xtable() |>
      print(type = "html", include.rownames = FALSE, html.table.attributes = NULL)
  ) |> paste(collapse = "\n")

  if (nrow(df_short) != nrow(df)) {
    rows_notice <- glue::glue("\n\n(Showing only the first {maxrows} rows out of {nrow(df)}.)\n")
  } else {
    rows_notice <- ""
  }

  paste0(tbl_html, "\n", rows_notice)
}

shinyApp(ui, server)
