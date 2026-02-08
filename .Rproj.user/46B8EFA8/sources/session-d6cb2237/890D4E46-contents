library(shiny)
library(igraph)

source("databse.R") 
source("repo.R")

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Praćenje projekata"),
  div(
    style = "margin-bottom: 15px;",
    actionButton("help_app", "O aplikaciji", icon = icon("info-circle"), class = "btn-secondary")
  ),
  
  tabsetPanel(
    tabPanel(
      "Unos",
      div(
        class = "card",
        div(
          class = "grid-3",
          textInput("project", "Projekt"),
          textInput("task", "Zadatak"),
          selectInput("status", "Status", choices = c("Planirano", "U tijeku", "Gotovo")),
          uiOutput("pred_picker")
        ),
        div(
          class = "grid-2",
          dateInput("start_date", "Početak", value = Sys.Date()),
          dateInput("end_date", "Završetak", value = Sys.Date() + 1)
        ),
        div(
          class = "grid-3",
          numericInput("a", "Najkraće trajanje", value = 1, min = 0.1, step = 0.5),
          numericInput("m", "Realno trajanje", value = 2, min = 0.1, step = 0.5),
          numericInput("b", "Najduže trajanje", value = 3, min = 0.1, step = 0.5)
        ),
        div(
          class = "actions",
          actionButton("add", "Dodaj", class = "btn-primary"),
          verbatimTextOutput("msg")
        )
      ),
      div(
        class = "card",
        div(class = "table-wrap", tableOutput("tbl"))
      )
    ),
    
    tabPanel(
      "PERT analiza",
      div(
        class = "card",
        div(class = "section-title", "PERT analiza"),
        div(
          class = "grid-2",
          uiOutput("project_picker"),
          dateInput("deadline_date", "Rok projekta", value = Sys.Date() + 30)
        ),
        hr(),
        tableOutput("pert_summary"),
        hr(),
        verbatimTextOutput("pert_prob")
      )
    ),
    
    tabPanel(
      "CPM",
      div(
        class = "card",
        div(class = "section-title", "CPM i kritični put"),
        actionButton("help_cpm", "Što znače pojmovi?", icon = icon("info-circle"), class = "btn-secondary"),
        uiOutput("cpm_project_picker"),
        hr(),
        tableOutput("cpm_table"),
        hr(),
        plotOutput("aoa_plot", height = "450px")
      )
    ),
    
    tabPanel(
      "Ganttogram",
      div(
        class = "card",
        div(
          class = "grid-2",
          uiOutput("gantt_project_picker"),
          selectInput(
            "gantt_status_filter",
            "Status",
            choices = c("Svi", "Planirano", "U tijeku", "Gotovo"),
            selected = "Svi"
          )
        ),
        hr(),
        plotOutput("gantt_plot", height = "420px")
      )
    )
  )
)

server <- function(input, output, session) {
  refresh <- reactiveVal(0)
  
  compute_cpm <- function(dfp) {
    if (is.null(dfp) || nrow(dfp) == 0) return(dfp)
    
    dfp$duration <- as.numeric(dfp$te)
    dfp$predecessor_id <- suppressWarnings(as.integer(dfp$predecessor_id))
    
    dfp <- dfp[order(is.na(dfp$predecessor_id), dfp$predecessor_id), ]
    dfp$ES <- 0
    dfp$EF <- dfp$duration
    
    idx <- setNames(seq_len(nrow(dfp)), dfp$id)
    
    for (iter in seq_len(nrow(dfp))) {
      for (i in seq_len(nrow(dfp))) {
        pred <- dfp$predecessor_id[i]
        if (!is.na(pred) && as.character(pred) %in% names(idx)) {
          p <- idx[[as.character(pred)]]
          dfp$ES[i] <- max(dfp$ES[i], dfp$EF[p], na.rm = TRUE)
          dfp$EF[i] <- dfp$ES[i] + dfp$duration[i]
        }
      }
    }
    
    project_duration <- max(dfp$EF, na.rm = TRUE)
    
    has_successor <- dfp$id %in% dfp$predecessor_id
    end_tasks <- !has_successor
    
    dfp$LF <- project_duration
    dfp$LS <- dfp$LF - dfp$duration
    
    dfp$LF[end_tasks] <- project_duration
    dfp$LS[end_tasks] <- dfp$LF[end_tasks] - dfp$duration[end_tasks]
    
    succ <- lapply(dfp$id, function(x) dfp$id[dfp$predecessor_id == x])
    
    for (iter in seq_len(nrow(dfp))) {
      for (i in seq_len(nrow(dfp))) {
        my_succ <- succ[[i]]
        if (length(my_succ) > 0) {
          sidx <- idx[as.character(my_succ)]
          vals <- dfp$LS[unlist(sidx)]
          vals <- vals[!is.na(vals)]
          if (length(vals) > 0) {
            min_ls <- min(vals)
            dfp$LF[i] <- min_ls
            dfp$LS[i] <- dfp$LF[i] - dfp$duration[i]
          }
        }
      }
    }
    
    dfp$Slack <- round(dfp$LS - dfp$ES, 3)
    dfp$Critical <- dfp$Slack == 0
    dfp
  }
  
  observeEvent(input$add, {
    if (!nzchar(trimws(input$project)) || !nzchar(trimws(input$task))) {
      output$msg <- renderText("Upiši Projekt i Zadatak.")
      return()
    }
    if (input$end_date < input$start_date) {
      output$msg <- renderText("Datum završetka ne može biti prije datuma početka.")
      return()
    }
    if (!(input$a <= input$m && input$m <= input$b)) {
      output$msg <- renderText("PERT mora zadovoljiti: najkraće ≤ realno ≤ najduže")
      return()
    }
    
    insert_task(
      trimws(input$project),
      trimws(input$task),
      input$status,
      input$start_date,
      input$end_date,
      input$a, input$m, input$b,
      input$pred_id
    )
    
    output$msg <- renderText("Dodano!")
    refresh(refresh() + 1)
  })
  
  output$tbl <- renderTable({
    refresh()
    df <- load_tasks()
    if (is.null(df) || nrow(df) == 0) return(data.frame(Poruka = "Nema unosa."))
    
    colnames(df) <- c(
      "ID", "Projekt", "Zadatak", "Status", "Početak", "Završetak",
      "Najkraće trajanje", "Najvjerojatnije trajanje", "Najduže trajanje",
      "Očekivano trajanje", "Nestabilnost", "Prethodnik"
    )
    df
  })
  
  output$project_picker <- renderUI({
    refresh()
    df <- load_tasks()
    if (is.null(df) || nrow(df) == 0) return(selectInput("project_sel", "Odaberi projekt", choices = character(0)))
    selectInput("project_sel", "Odaberi projekt", choices = sort(unique(df$project)))
  })
  
  output$cpm_project_picker <- renderUI({
    refresh()
    df <- load_tasks()
    if (is.null(df) || nrow(df) == 0) return(selectInput("cpm_project_sel", "Projekt", choices = character(0)))
    selectInput("cpm_project_sel", "Projekt", choices = sort(unique(df$project)))
  })
  
  output$gantt_project_picker <- renderUI({
    refresh()
    df <- load_tasks()
    if (is.null(df) || nrow(df) == 0) return(selectInput("gantt_project_sel", "Projekt", choices = character(0)))
    selectInput("gantt_project_sel", "Projekt", choices = sort(unique(df$project)))
  })
  
  output$pred_picker <- renderUI({
    refresh()
    df <- load_tasks()
    pname <- trimws(input$project %||% "")
    choices <- c("Nema" = NA)
    
    if (nzchar(pname) && !is.null(df) && nrow(df) > 0) {
      dfp <- df[trimws(df$project) == pname, ]
      if (nrow(dfp) > 0) {
        choices <- c(choices, setNames(dfp$id, paste0(dfp$id, " - ", dfp$task)))
      }
    }
    
    selectInput("pred_id", "Prethodnik", choices = choices, selected = NA)
  })
  
  output$pert_summary <- renderTable({
    req(input$project_sel)
    refresh()
    df <- load_tasks()
    dfp <- df[df$project == input$project_sel & df$status != "Gotovo", ]
    
    if (is.null(dfp) || nrow(dfp) == 0) {
      return(data.frame(Poruka = "Nema aktivnih zadataka za odabrani projekt."))
    }
    
    mu <- sum(dfp$te, na.rm = TRUE)
    var_sum <- sum(dfp$var, na.rm = TRUE)
    sigma <- sqrt(var_sum)
    
    start_proj <- min(as.Date(dfp$start_date))
    T_days <- as.numeric(as.Date(input$deadline_date) - start_proj)
    
    data.frame(
      "Projekt" = input$project_sel,
      "Datum početka" = as.character(start_proj),
      "Datum roka" = as.character(as.Date(input$deadline_date)),
      "Ukupno dana" = T_days,
      "Procijenjeno trajanje" = round(mu, 2),
      "Odstupanje" = round(sigma, 4),
      check.names = FALSE
    )
  })
  
  output$pert_prob <- renderText({
    req(input$project_sel)
    refresh()
    df <- load_tasks()
    dfp <- df[df$project == input$project_sel, ]
    
    if (is.null(dfp) || nrow(dfp) == 0) return("Nema zadataka za odabrani projekt.")
    
    mu <- sum(dfp$te, na.rm = TRUE)
    var_sum <- sum(dfp$var, na.rm = TRUE)
    sigma <- sqrt(var_sum)
    
    start_proj <- min(as.Date(dfp$start_date))
    T <- as.numeric(as.Date(input$deadline_date) - start_proj)
    
    if (is.na(sigma) || sigma == 0) return("Nema dovoljno podataka (var=0) za izračun vjerojatnosti.")
    if (is.na(T) || T < 0) return("Rok je prije početka projekta — provjeri datum roka.")
    
    Z <- (T - mu) / sigma
    p <- pnorm(Z)
    
    paste0(
      "Z = ", round(Z, 4),
      "\nVjerojatnost završetka do roka = ", round(100 * p, 2), "%."
    )
  })
  
  output$cpm_table <- renderTable({
    req(input$cpm_project_sel)
    refresh()
    df <- load_tasks()
    dfp <- df[df$project == input$cpm_project_sel, ]
    if (is.null(dfp) || nrow(dfp) == 0) return(data.frame(Poruka = "Nema zadataka za odabrani projekt."))
    
    res <- compute_cpm(dfp)
    res[, c("id", "task", "predecessor_id", "ES", "EF", "LS", "LF", "Slack", "Critical")]
  })
  
  output$aoa_plot <- renderPlot({
    req(input$cpm_project_sel)
    refresh()
    df <- load_tasks()
    dfp <- df[df$project == input$cpm_project_sel, ]
    if (is.null(dfp) || nrow(dfp) == 0) {
      plot.new()
      text(0.5, 0.5, "Nema zadataka.", cex = 1.2)
      return()
    }
    
    res <- compute_cpm(dfp)
    edges <- do.call(rbind, lapply(seq_len(nrow(res)), function(i) {
      if (is.na(res$predecessor_id[i])) return(NULL)
      data.frame(from = res$predecessor_id[i], to = res$id[i])
    }))
    
    if (is.null(edges) || nrow(edges) == 0) {
      plot.new()
      text(0.5, 0.5, "Nema ovisnosti za mrežu.", cex = 1.2)
      return()
    }
    
    g <- graph_from_data_frame(edges, directed = TRUE)
    V(g)$color <- "lightblue"
    crit_nodes <- res$id[res$Critical]
    V(g)$color[V(g)$name %in% as.character(crit_nodes)] <- "red"
    
    plot(
      g,
      vertex.size = 30,
      vertex.label.cex = 0.8,
      main = paste("CPM mreža:", input$cpm_project_sel)
    )
  })
  
  output$gantt_plot <- renderPlot({
    req(input$gantt_project_sel)
    refresh()
    df <- load_tasks()
    dfp <- df[df$project == input$gantt_project_sel, ]
    
    if (input$gantt_status_filter != "Svi") {
      dfp <- dfp[dfp$status == input$gantt_status_filter, ]
    }
    
    if (is.null(dfp) || nrow(dfp) == 0) {
      plot.new()
      text(0.5, 0.5, "Nema zadataka za odabrani filter.", cex = 1.2)
      return()
    }
    
    dfp$start_date <- as.Date(dfp$start_date)
    dfp$end_date <- as.Date(dfp$end_date)
    dfp <- dfp[order(dfp$start_date), ]
    
    cols <- ifelse(
      dfp$status == "Gotovo", "#2ecc71",
      ifelse(dfp$status == "U tijeku", "#fa2525", "#2577fa")
    )
    
    y <- seq_len(nrow(dfp))
    
    plot(
      x = range(c(dfp$start_date, dfp$end_date)),
      y = c(0.5, nrow(dfp) + 0.5),
      type = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      main = input$gantt_project_sel
    )
    
    legend(
      "topleft",
      legend = c("Planirano", "U tijeku", "Gotovo"),
      col = c("#2577fa", "#fa2525", "#2ecc71"),
      lwd = 6,
      bty = "n"
    )
    
    for (i in seq_len(nrow(dfp))) {
      segments(
        x0 = dfp$start_date[i], y0 = y[i],
        x1 = dfp$end_date[i], y1 = y[i],
        lwd = 10,
        col = cols[i]
      )
      text(
        x = dfp$start_date[i] + (dfp$end_date[i] - dfp$start_date[i]) / 2,
        y = y[i] + 0.25,
        labels = dfp$task[i],
        cex = 0.8
      )
    }
  })
  
  observeEvent(input$help_cpm, {
    showModal(
      modalDialog(
        title = "Objašnjenje CPM pojmova",
        size = "l",
        easyClose = TRUE,
        tags$div(
          tags$p(tags$b("ID:"), " Jedinstveni broj zadatka u projektu."),
          tags$p(tags$b("Zadatak:"), " Naziv aktivnosti koja se mora izvršiti."),
          tags$p(tags$b("Prethodnik:"), " Zadatak koji mora biti završen prije nego ovaj može započeti."),
          tags$hr(),
          tags$p(tags$b("ES:"), " Najraniji početak zadatka."),
          tags$p(tags$b("EF:"), " Najraniji završetak (ES + trajanje)."),
          tags$hr(),
          tags$p(tags$b("LS:"), " Najkasniji početak bez kašnjenja projekta."),
          tags$p(tags$b("LF:"), " Najkasniji završetak bez pomicanja roka."),
          tags$hr(),
          tags$p(tags$b("Slack:"), " Rezerva vremena (koliko može kasniti bez utjecaja na rok)."),
          tags$p(tags$b("Critical:"), " Kritični zadatak (Slack = 0). Kašnjenje kasni cijeli projekt.")
        ),
        footer = modalButton("Zatvori")
      )
    )
  })
  
  observeEvent(input$help_app, {
    showModal(
      modalDialog(
        title = "O aplikaciji – Praćenje projekata",
        size = "l",
        easyClose = TRUE,
        tags$div(
          tags$p("Aplikacija služi za unos zadataka, praćenje statusa i analizu projekata (PERT, CPM, Gantt)."),
          tags$hr(),
          tags$p(tags$b("Unos:"), " dodaj projekt, zadatak, datume, status i prethodnika."),
          tags$p(tags$b("PERT analiza:"), " procjena trajanja i vjerojatnost završetka do roka."),
          tags$p(tags$b("CPM:"), " izračun kritičnog puta i mrežni prikaz ovisnosti."),
          tags$p(tags$b("Ganttogram:"), " vizualni prikaz trajanja i statusa zadataka.")
        ),
        footer = modalButton("Zatvori")
      )
    )
  })
}

shinyApp(ui, server)
