# ============================================================================
# Thinking With Models Through Simulation — Interactive Companion
# ============================================================================
# A methodological instrument, not a demo. Each tab embodies a core argument
# from the tutorial paper.
#
# Requirements: shiny, bslib, ggplot2, viridis
# Launch: shiny::runApp("shiny_app", launch.browser = TRUE)
# ============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(viridis)

source("../R/models.R", local = TRUE)

# --- Palette & Theme ---------------------------------------------------------
pal <- c(blue = "#4477AA", cyan = "#66CCEE", red = "#EE6677",
         purple = "#AA3377", yellow = "#CCBB44", grey = "#BBBBBB")

theme_app <- function(base_size = 13) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#F0F0F0", linewidth = 0.3),
      axis.line = element_line(color = "#404040", linewidth = 0.4),
      axis.ticks = element_line(color = "#404040", linewidth = 0.3),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      plot.title = element_text(size = base_size + 2, face = "bold", color = "#2C3E50"),
      plot.subtitle = element_text(size = base_size, color = "#7F8C8D", margin = margin(b = 10)),
      plot.margin = margin(15, 15, 10, 10)
    )
}

# --- Helper: status badge ----------------------------------------------------
status_badge <- function(text, type = "info") {
  cls <- switch(type,
    success = "bg-success", warning = "bg-warning text-dark",
    danger = "bg-danger", info = "bg-info", secondary = "bg-secondary")
  tags$span(class = paste("badge", cls, "fs-6 px-3 py-2"), text)
}

# ============================================================================
# UI
# ============================================================================
ui <- page_navbar(
  title = tags$span(
    tags$strong("Simulation Thinking"),
    tags$span(class = "text-muted ms-2 small", "Interactive Companion")
  ),
  theme = bs_theme(
    version = 5, bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    primary = "#4477AA", success = "#228833",
    "navbar-bg" = "#2C3E50"
  ),
  navbar_options = navbar_options(bg = "#2C3E50"),

  # =========================================================================
  # TAB 0: Welcome
  # =========================================================================
  nav_panel(icon("home"),
    div(class = "container py-5", style = "max-width: 860px;",

      # Hero
      div(class = "text-center mb-5",
        h1(class = "fw-bold mb-2", style = "font-size: 2.4rem; color: #2C3E50;",
           "Simulation Thinking Companion"),
        p(class = "fs-5 mb-1", style = "color: #4477AA;",
          "Interactive supplement to"),
        p(class = "fst-italic mb-3", style = "color: #555;",
          HTML("<em>Thinking With Models Through Simulation: A Methodological Tutorial</em>")),
        p(class = "small text-muted mb-0",
          HTML("Kenny Yu &middot; KU Leuven, Belgium")),
        p(class = "small",
          tags$a(href = "mailto:kenny.yu@kuleuven.be", "kenny.yu@kuleuven.be"))
      ),


      # Four cards
      layout_column_wrap(
        width = 1/2, heights_equal = "row",
        card(class = "border-start border-4 border-primary",
          card_body(class = "py-3",
            div(class = "d-flex align-items-center",
              icon("brain", class = "fa-2x text-primary me-3"),
              div(h5(class = "mb-1", "1. Anticipate"),
                  p(class = "mb-0 text-muted small", "Predict before you simulate.")))
          )
        ),
        card(class = "border-start border-4 border-info",
          card_body(class = "py-3",
            div(class = "d-flex align-items-center",
              icon("eye", class = "fa-2x text-info me-3"),
              div(h5(class = "mb-1", "2. Observation Function"),
                  p(class = "mb-0 text-muted small", "Same latent states, different conclusions.")))
          )
        ),
        card(class = "border-start border-4 border-danger",
          card_body(class = "py-3",
            div(class = "d-flex align-items-center",
              icon("code-compare", class = "fa-2x text-danger me-3"),
              div(h5(class = "mb-1", "3. Critical Test"),
                  p(class = "mb-0 text-muted small", "Force rival models to disagree.")))
          )
        ),
        card(class = "border-start border-4 border-warning",
          card_body(class = "py-3",
            div(class = "d-flex align-items-center",
              icon("chart-area", class = "fa-2x text-warning me-3"),
              div(h5(class = "mb-1", "4. Parameter Sweep"),
                  p(class = "mb-0 text-muted small", "Points are not predictions.")))
          )
        )
      ),

      # Footer
      div(class = "text-center mt-5 pt-3",
        p(class = "text-muted small mb-1",
          "Each tab corresponds to a core diagnostic practice from the paper."),
        p(class = "text-muted small",
          HTML("Code: <a href='https://doi.org/10.5281/zenodo.19234590' target='_blank'>doi.org/10.5281/zenodo.19234590</a>"))
      )
    )
  ),

  # =========================================================================
  # TAB 1: Anticipate & Simulate
  # =========================================================================
  nav_panel("1. Anticipate",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        h5(icon("flask"), " Scenario"),
        radioButtons("t1_design", NULL, inline = TRUE,
          choices = c("Blocking" = "block", "Overexpectation" = "overexp")),
        p(class = "small text-muted",
          conditionalPanel("input.t1_design == 'block'", inline = TRUE,
            "Phase 1: A+ alone. Phase 2: AB+ compound. Does B learn?"),
          conditionalPanel("input.t1_design == 'overexp'", inline = TRUE,
            "Phase 1: A+ and B+ separately. Phase 2: AB+ compound. What happens to V?")
        ),
        hr(),
        h5(icon("cog"), " Design & Parameters"),
        layout_columns(col_widths = c(6, 6),
          numericInput("t1_p1", "Phase 1 trials", 10, min = 2, max = 40, step = 1),
          numericInput("t1_p2", "Phase 2 trials", 10, min = 2, max = 40, step = 1)
        ),
        layout_columns(col_widths = c(6, 6),
          numericInput("t1_alpha", HTML("&alpha;"), 0.4, min = 0.05, max = 0.95, step = 0.05),
          numericInput("t1_beta", HTML("&beta;"), 0.3, min = 0.05, max = 0.95, step = 0.05)
        ),
        hr(),
        h5(icon("question-circle"), " Your Prediction"),
        conditionalPanel("input.t1_design == 'block'",
          p(class = "small text-muted", "After compound training, what will V_B be?"),
          sliderInput("t1_pred_block", "My prediction for V_B:", 0, 1, 0.5, step = 0.05)
        ),
        conditionalPanel("input.t1_design == 'overexp'",
          p(class = "small text-muted", "During compound AB+, will V_A rise, stay, or decline?"),
          radioButtons("t1_pred_oe", NULL,
            choices = c("V_A will rise further" = "rise",
                        "V_A will stay roughly the same" = "stay",
                        "V_A will decline" = "decline"))
        ),
        textAreaInput("t1_reason", "Why? (optional)", rows = 2,
                      placeholder = "e.g., Both cues predict the outcome, so..."),
        actionButton("t1_go", "Commit Prediction & Simulate",
                     icon = icon("play"),
                     class = "btn-primary btn-lg w-100 mt-2")
      ),

      # Main panel
      conditionalPanel("input.t1_go == 0",
        card(class = "border-0",
          div(class = "text-center py-5",
            icon("lock", class = "fa-3x text-muted mb-3"),
            h3("Results are hidden until you commit."),
            p(class = "text-muted fs-5",
              "This is the Anticipate step: without a prediction,",
              "you cannot discover that your understanding was incomplete."))
        )
      ),
      conditionalPanel("input.t1_go > 0",
        layout_columns(col_widths = c(8, 4),
          card(card_header(class = "bg-light", "Learning Trajectories"),
            plotOutput("t1_plot", height = "400px")
          ),
          card(card_header(class = "bg-light", "Verdict"),
            uiOutput("t1_verdict"),
            hr(),
            h6("Key Values"),
            tableOutput("t1_trace")
          )
        )
      )
    )
  ),

  # =========================================================================
  # TAB 2: Observation Function Lab
  # =========================================================================
  nav_panel("2. Observation Function",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        h5(icon("sliders-h"), " Latent States"),
        p(class = "small text-muted", "Internal model values (from blocking):"),
        layout_columns(col_widths = c(6, 6),
          numericInput("t2_va", HTML("V<sub>A</sub>"), 0.85, min = 0, max = 1, step = 0.01),
          numericInput("t2_vb_rw", HTML("V<sub>B</sub> (RW)"), 0.13, min = 0, max = 1, step = 0.01)
        ),
        numericInput("t2_vb_mack", HTML("V<sub>B</sub> (Mack)"), 0.57, min = 0, max = 1, step = 0.01),
        hr(),
        h5(icon("eye"), " Mapping"),
        radioButtons("t2_map", NULL, inline = TRUE,
          choices = c("Identity" = "id", "Sigmoid" = "sig",
                      "Threshold" = "thr", "Luce" = "luce")),
        conditionalPanel("input.t2_map == 'sig'",
          sliderInput("t2_gamma", HTML("&gamma;"), 1, 15, 5, step = 0.5),
          sliderInput("t2_theta_s", HTML("&theta;"), 0, 1, 0.5, step = 0.05)),
        conditionalPanel("input.t2_map == 'thr'",
          sliderInput("t2_theta_t", HTML("&theta;"), 0, 1, 0.3, step = 0.05)),
        conditionalPanel("input.t2_map == 'luce'",
          sliderInput("t2_phi", HTML("&phi;"), 0.5, 10, 3, step = 0.5)),
        hr(),
        h5(icon("bullseye"), " Hypothetical Data"),
        sliderInput("t2_obs", "Observed P(resp|B)", 0, 1, 0.30, step = 0.05)
      ),

      layout_columns(col_widths = c(5, 7),
        card(card_header(class = "bg-light", "Latent vs. Behavioral"),
          plotOutput("t2_mapping_plot", height = "420px")
        ),
        card(card_header(class = "bg-light", "Which Model Wins?"),
          plotOutput("t2_winner_plot", height = "320px"),
          hr(),
          uiOutput("t2_verdict")
        )
      )
    )
  ),

  # =========================================================================
  # TAB 3: Critical Test Designer
  # =========================================================================
  nav_panel("3. Critical Test",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        h5(icon("flask"), " Experiment Design"),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("t3_p1", "P1", 10, min = 2, max = 40),
          numericInput("t3_p2", "P2", 10, min = 2, max = 40),
          numericInput("t3_p3", "P3", 10, min = 2, max = 40)
        ),
        hr(),
        h5(icon("cog"), " Parameters"),
        layout_columns(col_widths = c(6, 6),
          numericInput("t3_alpha", HTML("&alpha;"), 0.4, min = 0.05, max = 0.95, step = 0.05),
          numericInput("t3_beta", HTML("&beta;/S"), 0.3, min = 0.05, max = 0.95, step = 0.05)
        ),
        hr(),
        h5(icon("wrench"), " Diagnostic Controls"),
        sliderInput("t3_gen", "Generalization (g)", 0, 1, 0, step = 0.05),
        checkboxInput("t3_freeze", HTML("Freeze &alpha; (ablation)"), FALSE),
        hr(),
        h5(icon("eye"), " Observation"),
        selectInput("t3_obs", "Mapping", c("Raw V" = "id", "Sigmoid" = "sig", "Threshold" = "thr")),
        conditionalPanel("input.t3_obs == 'thr'",
          sliderInput("t3_theta", HTML("&theta;"), 0, 1, 0.3, step = 0.05))
      ),

      layout_columns(col_widths = c(7, 5),
        card(card_header(class = "bg-light", "Three-Phase Learning"),
          plotOutput("t3_full_plot", height = "420px")
        ),
        card(card_header(class = "bg-light", "Phase 3 Diagnosis"),
          plotOutput("t3_p3_plot", height = "200px"),
          hr(),
          uiOutput("t3_internal"),
          hr(),
          uiOutput("t3_status")
        )
      )
    )
  ),

  # =========================================================================
  # TAB 4: Structural vs Parametric
  # =========================================================================
  nav_panel("4. Sweep",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        h5(icon("chart-area"), " Sweep Configuration"),
        selectInput("t4_x", "X axis", c("alpha_A", "beta", "Phase 1 trials" = "n_p1", "Phase 2 trials" = "n_p2")),
        selectInput("t4_y", "Y axis", c("beta", "alpha_A", "Phase 1 trials" = "n_p1", "Phase 2 trials" = "n_p2"), selected = "beta"),
        sliderInput("t4_res", "Grid resolution", 9, 21, 15, step = 2),
        hr(),
        h5(icon("cog"), " Fixed Settings"),
        numericInput("t4_p3", "Phase 3 trials", 20, min = 5, max = 40),
        sliderInput("t4_gen", "Generalization (g)", 0, 1, 0, step = 0.1),
        hr(),
        actionButton("t4_run", "Run Sweep",
                     icon = icon("play"), class = "btn-warning btn-lg w-100"),
        p(class = "text-muted small mt-2 text-center", "Higher resolution = slower")
      ),

      conditionalPanel("input.t4_run == 0",
        card(class = "border-0",
          div(class = "text-center py-5",
            icon("chart-area", class = "fa-3x text-muted mb-3"),
            h3("Configure and press Run Sweep"),
            p(class = "text-muted", "This will compute the divergence landscape."))
        )
      ),
      conditionalPanel("input.t4_run > 0",
        card(card_header(class = "bg-light", "Divergence Landscape"),
          plotOutput("t4_heat", height = "450px"),
          hr(),
          uiOutput("t4_verdict")
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  # === TAB 1 ================================================================
  t1_res <- eventReactive(input$t1_go, {
    req(input$t1_alpha, input$t1_beta, input$t1_p1, input$t1_p2)
    a <- input$t1_alpha; b <- input$t1_beta
    is_oe <- isolate(input$t1_design) == "overexp"

    if (is_oe) {
      # Overexpectation: Phase 1 = alternating A+ / B+, Phase 2 = AB+
      design <- c(
        unlist(lapply(seq_len(input$t1_p1), function(i) {
          c(make_trials("A", 1, 1, "Phase 1"), make_trials("B", 1, 1, "Phase 1"))
        }), recursive = FALSE),
        make_trials(c("A", "B"), 1, input$t1_p2, "Phase 2")
      )
    } else {
      # Blocking: Phase 1 = A+, Phase 2 = AB+
      design <- c(make_trials("A", 1, input$t1_p1, "Phase 1"),
                  make_trials(c("A", "B"), 1, input$t1_p2, "Phase 2"))
    }

    sim <- rw_simulate(design, alpha = c(A = a, B = a), beta = b)
    # Key values at Phase 1 end
    p1_end_trial <- max(sim$trial[sim$phase == "Phase 1"])
    va_p1 <- sim$V[sim$cue == "A" & sim$trial == p1_end_trial + 1]
    # Final values
    va_final <- tail(sim$V[sim$cue == "A"], 1)
    vb_final <- tail(sim$V[sim$cue == "B"], 1)

    list(sim = sim, va_p1 = va_p1, va_final = va_final, vb_final = vb_final,
         is_oe = is_oe, p1_end = p1_end_trial,
         pred_block = isolate(input$t1_pred_block),
         pred_oe = isolate(input$t1_pred_oe))
  })

  output$t1_plot <- renderPlot({
    r <- t1_res()
    p1_boundary <- r$p1_end + 0.5
    max_t <- max(r$sim$trial)

    p <- ggplot(r$sim, aes(trial, V, color = cue)) +
      annotate("rect", xmin = p1_boundary, xmax = max_t + 0.5,
               ymin = -0.05, ymax = 1.15, fill = "#F8F9FA") +
      geom_vline(xintercept = p1_boundary, linetype = "dashed", color = "#DEE2E6") +
      geom_line(linewidth = 1) + geom_point(size = 1.8) +
      scale_color_manual(values = c(A = pal["blue"], B = pal["red"]),
                         labels = c("Cue A", "Cue B")) +
      scale_y_continuous(limits = c(-0.05, 1.18), breaks = seq(0, 1, 0.25)) +
      theme_app()

    if (r$is_oe) {
      # Overexpectation: annotate V_A decline
      p <- p +
        annotate("text", x = r$p1_end / 2, y = 1.13,
                 label = "Phase 1: A+ / B+ (alternating)", size = 3, color = "#999") +
        annotate("text", x = r$p1_end + input$t1_p2/2 + 0.5, y = 1.13,
                 label = "Phase 2: AB+", size = 3.5, color = "#666", fontface = "bold") +
        annotate("label", x = max_t, y = r$va_final,
                 label = sprintf("V_A = %.3f", r$va_final),
                 size = 3, fill = pal["blue"], color = "white",
                 label.padding = unit(0.25, "lines"), hjust = 1) +
        annotate("segment", x = p1_boundary + 1, xend = p1_boundary + 1,
                 y = r$va_p1, yend = r$va_final,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                 color = pal["purple"], linewidth = 0.6) +
        labs(x = "Trial", y = expression(italic(V)),
             title = "Overexpectation: Rescorla-Wagner Model",
             subtitle = sprintf("V_A declines from %.3f to %.3f during compound", r$va_p1, r$va_final))
    } else {
      # Blocking: annotate prediction and V_B
      pred_y <- r$pred_block
      p <- p +
        geom_hline(yintercept = pred_y, linetype = "dotted", color = pal["purple"], linewidth = 0.5) +
        annotate("text", x = input$t1_p1/2, y = 1.13,
                 label = "Phase 1: A+", size = 3.5, color = "#999") +
        annotate("text", x = input$t1_p1 + input$t1_p2/2 + 0.5, y = 1.13,
                 label = "Phase 2: AB+", size = 3.5, color = "#666", fontface = "bold") +
        annotate("text", x = max_t, y = pred_y + 0.04,
                 label = sprintf("Your prediction: %.2f", pred_y),
                 size = 3, color = pal["purple"], hjust = 1, fontface = "italic") +
        annotate("label", x = max_t, y = r$vb_final,
                 label = sprintf("V_B = %.3f", r$vb_final),
                 size = 3.5, color = "white", fill = pal["red"],
                 label.padding = unit(0.3, "lines"), hjust = 1) +
        labs(x = "Trial", y = expression(italic(V)),
             title = "Blocking: Rescorla-Wagner Model",
             subtitle = sprintf("Final V_B = %.3f", r$vb_final))
    }
    p
  })

  output$t1_verdict <- renderUI({
    r <- t1_res()

    if (r$is_oe) {
      # Overexpectation verdict
      declined <- r$va_final < r$va_p1 - 0.02
      pred_correct <- (r$pred_oe == "decline" && declined) ||
                      (r$pred_oe == "rise" && r$va_final > r$va_p1 + 0.02) ||
                      (r$pred_oe == "stay" && abs(r$va_final - r$va_p1) <= 0.02)

      if (pred_correct) {
        div(status_badge("Prediction matched", "success"),
            p(class = "mt-2", sprintf("V_A went from %.3f to %.3f. You anticipated this correctly.", r$va_p1, r$va_final)))
      } else {
        direction <- if (declined) "declined" else if (r$va_final > r$va_p1 + 0.02) "rose" else "stayed roughly the same"
        div(status_badge("Surprise!", "warning"),
            p(class = "mt-2", sprintf("V_A %s (%.3f to %.3f) despite continued reinforcement.", direction, r$va_p1, r$va_final)),
            if (declined) p(class = "small text-muted",
              "This is overexpectation: V_A + V_B > lambda, so the combined prediction overshoots the outcome.",
              "The negative error drives both values down. This prediction was not designed into the model; it fell out of the same mechanism that produced blocking."))
      }
    } else {
      # Blocking verdict
      err <- abs(r$pred_block - r$vb_final)
      if (err < 0.1) {
        div(status_badge("Prediction matched", "success"),
            p(class = "mt-2", sprintf("You predicted %.2f; actual V_B = %.3f. Close!", r$pred_block, r$vb_final)))
      } else {
        div(status_badge("Surprise!", "warning"),
            p(class = "mt-2", sprintf("You predicted %.2f; actual V_B = %.3f (error = %.2f).", r$pred_block, r$vb_final, err)),
            p(class = "small text-muted",
              "This discrepancy is the epistemic signal. The verbal understanding",
              "'prediction error drives learning' did not prepare you for this quantitative consequence."))
      }
    }
  })

  output$t1_trace <- renderTable({
    r <- t1_res()
    if (r$is_oe) {
      # Show V_A trajectory during Phase 2
      d <- r$sim[r$sim$cue == "A" & r$sim$trial > r$p1_end, ]
      d <- head(d, 6)
      data.frame(Trial = d$trial, V_A = round(d$V, 4))
    } else {
      # Show V_B trajectory during Phase 2
      d <- r$sim[r$sim$cue == "B" & r$sim$trial > r$p1_end, ]
      d <- head(d, 6)
      data.frame(Trial = d$trial, V_B = round(d$V, 4))
    }
  }, striped = TRUE, width = "100%", digits = 4)

  # === TAB 2 ================================================================
  apply_map <- function(v, va_for_luce = NULL) {
    switch(input$t2_map,
      id = v,
      sig = obs_sigmoid(v, input$t2_gamma, input$t2_theta_s),
      thr = obs_threshold(v, input$t2_theta_t),
      luce = {
        if (is.null(va_for_luce)) va_for_luce <- input$t2_va
        ev <- exp(input$t2_phi * c(va_for_luce, v))
        ev[2] / sum(ev)
      })
  }

  output$t2_mapping_plot <- renderPlot({
    # Build mapping curve
    v_seq <- seq(0, 1, length.out = 200)
    p_seq <- sapply(v_seq, function(v) apply_map(v))

    rw_v <- input$t2_vb_rw
    mack_v <- input$t2_vb_mack
    rw_p <- apply_map(rw_v)
    mack_p <- apply_map(mack_v)

    df_curve <- data.frame(V = v_seq, P = p_seq)
    df_pts <- data.frame(
      Model = c("RW", "Mack"), V = c(rw_v, mack_v), P = c(rw_p, mack_p))

    ggplot() +
      geom_line(data = df_curve, aes(V, P), color = "#AAAAAA", linewidth = 1) +
      geom_point(data = df_pts, aes(V, P, color = Model), size = 5) +
      geom_segment(data = df_pts, aes(x = V, xend = V, y = 0, yend = P, color = Model),
                   linetype = "dotted", linewidth = 0.4) +
      geom_segment(data = df_pts, aes(x = 0, xend = V, y = P, yend = P, color = Model),
                   linetype = "dotted", linewidth = 0.4) +
      geom_text(data = df_pts, aes(V, P, label = sprintf("%.2f", P), color = Model),
                vjust = -1.2, size = 4, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = c(RW = pal["blue"], Mack = pal["red"])) +
      scale_x_continuous(limits = c(-0.02, 1.02), breaks = seq(0, 1, 0.25)) +
      scale_y_continuous(limits = c(-0.05, 1.1), breaks = seq(0, 1, 0.25)) +
      labs(x = expression(paste("Latent ", italic(V))),
           y = "P(response)",
           title = "Observation Function Mapping",
           subtitle = paste("Mapping:", input$t2_map)) +
      theme_app()
  })

  output$t2_winner_plot <- renderPlot({
    rw_p <- apply_map(input$t2_vb_rw)
    mack_p <- apply_map(input$t2_vb_mack)
    obs <- input$t2_obs

    df <- data.frame(
      Source = factor(c("RW", "Data", "Mack"), levels = c("RW", "Data", "Mack")),
      Value = c(rw_p, obs, mack_p),
      Fill = c(pal["blue"], "#888888", pal["red"])
    )

    ggplot(df, aes(Source, Value, fill = Fill)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = obs, linetype = "dashed", color = "#555", linewidth = 0.3) +
      geom_text(aes(label = sprintf("%.2f", Value)), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(limits = c(0, 1.15)) +
      labs(x = NULL, y = "P(response|B)", title = "Model vs. Data") +
      theme_app() + theme(legend.position = "none")
  })

  output$t2_verdict <- renderUI({
    rw_p <- apply_map(input$t2_vb_rw)
    mack_p <- apply_map(input$t2_vb_mack)
    obs <- input$t2_obs
    rw_d <- abs(rw_p - obs)
    mack_d <- abs(mack_p - obs)

    if (abs(rw_d - mack_d) < 0.03) {
      badge <- status_badge("Indeterminate", "secondary")
      msg <- "Both models are similarly close to the data."
    } else if (rw_d < mack_d) {
      badge <- status_badge("RW closer", "info")
      msg <- sprintf("RW (%.2f) is closer to data (%.2f) than Mack (%.2f).", rw_p, obs, mack_p)
    } else {
      badge <- status_badge("Mack closer", "danger")
      msg <- sprintf("Mack (%.2f) is closer to data (%.2f) than RW (%.2f).", mack_p, obs, rw_p)
    }
    div(badge, p(class = "mt-2 mb-1", msg),
        p(class = "small text-muted fst-italic",
          "Now switch the mapping type. The winner often changes.",
          "This is why observation functions are theoretical commitments."))
  })

  # === TAB 3 ================================================================
  t3_res <- reactive({
    req(input$t3_alpha, input$t3_beta, input$t3_p1, input$t3_p2, input$t3_p3)
    a <- input$t3_alpha; b <- input$t3_beta; g <- input$t3_gen

    dp12 <- c(make_trials("A", 1, input$t3_p1, "Phase 1"),
              make_trials(c("A","B"), 1, input$t3_p2, "Phase 2"))
    dp3 <- make_trials("B", 1, input$t3_p3, "Phase 3")

    # Full trajectories for plot
    rw12 <- rw_simulate(dp12, alpha = c(A=a, B=a), beta = b)
    rw_V <- extract_final_V(rw12); rw_vb_p2 <- rw_V[["B"]]
    rw_V["B"] <- g * rw_vb_p2
    rw3 <- rw_simulate(dp3, alpha = c(A=a, B=a), beta = b, V_init = rw_V, cue_names = c("A","B"))
    rw_all <- combine_phases(rw12, rw3); rw_all$model <- "RW"

    ms <- mack_final_state(dp12, S = c(A=b, B=b), alpha_init = c(A=0.5, B=0.5))
    mack_vb_p2 <- ms$V[["B"]]; ms$V["B"] <- g * mack_vb_p2
    alpha_use <- if (input$t3_freeze) c(A=0.5, B=0.5) else ms$alpha
    m3 <- mack_simulate(dp3, S = c(A=b, B=b), alpha_init = alpha_use,
                        V_init = ms$V, cue_names = c("A","B"))
    m3_df <- m3[, c("trial","phase","cue","V")]
    mlabel <- if (input$t3_freeze) "Mack (frozen)" else "Mack"
    # Offset Phase 3 trials
    p12_len <- input$t3_p1 + input$t3_p2
    m3_df$trial <- m3_df$trial + p12_len
    m3_df$model <- mlabel
    # Also build Mack P1+P2 trajectory
    m12 <- mack_simulate(dp12, S = c(A=b, B=b), alpha_init = c(A=0.5, B=0.5))
    m12_df <- m12[, c("trial","phase","cue","V")]
    m12_df$model <- mlabel

    rw_b <- rw_all[rw_all$cue == "B", c("trial","phase","cue","V","model")]
    mack_b <- rbind(m12_df[m12_df$cue == "B",], m3_df[m3_df$cue == "B",])

    list(data = rbind(rw_b, mack_b),
         rw_vb = tail(rw3$V[rw3$cue == "B"], 1),
         mack_vb = tail(m3_df$V[m3_df$cue == "B"], 1),
         alpha_b = ms$alpha[["B"]],
         p1_end = input$t3_p1, p2_end = input$t3_p1 + input$t3_p2,
         mlabel = mlabel)
  })

  output$t3_full_plot <- renderPlot({
    r <- t3_res()
    p1e <- r$p1_end; p2e <- r$p2_end; p3e <- max(r$data$trial)

    models <- unique(r$data$model)
    cols <- setNames(c(pal[["blue"]], pal[["red"]]), models)
    lts <- setNames(c("solid", "dashed"), models)

    ggplot(r$data, aes(trial, V, color = model, linetype = model)) +
      annotate("rect", xmin = p2e + 0.5, xmax = p3e + 0.5,
               ymin = -0.05, ymax = 1.1, fill = "#F0F4F8", alpha = 0.6) +
      geom_vline(xintercept = c(p1e + 0.5, p2e + 0.5),
                 linetype = "dashed", color = "#DEE2E6") +
      geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
      annotate("text", x = p1e/2, y = 1.07, label = "Phase 1", size = 3, color = "#AAA") +
      annotate("text", x = (p1e + p2e)/2, y = 1.07, label = "Phase 2", size = 3, color = "#AAA") +
      annotate("text", x = (p2e + p3e)/2, y = 1.07, label = "Phase 3",
               size = 3.5, color = "#444", fontface = "bold") +
      annotate("label", x = p3e, y = r$rw_vb, label = sprintf("%.2f", r$rw_vb),
               fill = pal["blue"], color = "white", size = 3.5, hjust = 1.2,
               label.padding = unit(0.25, "lines")) +
      annotate("label", x = p3e, y = r$mack_vb, label = sprintf("%.2f", r$mack_vb),
               fill = pal["red"], color = "white", size = 3.5, hjust = 1.2,
               label.padding = unit(0.25, "lines")) +
      scale_color_manual(values = cols) +
      scale_linetype_manual(values = lts) +
      scale_y_continuous(limits = c(-0.05, 1.12), breaks = seq(0, 1, 0.25)) +
      labs(x = "Trial", y = expression(italic(V)[B]),
           title = "RW vs. Mackintosh: Full Design",
           subtitle = sprintf("g = %.1f | %s", input$t3_gen,
                              if (input$t3_freeze) "attention frozen" else "full models")) +
      theme_app() + theme(legend.key.width = unit(2, "lines"))
  })

  output$t3_p3_plot <- renderPlot({
    r <- t3_res()
    p3_data <- r$data[r$data$trial > r$p2_end, ]

    models_p3 <- unique(p3_data$model)
    cols_p3 <- setNames(c(pal[["blue"]], pal[["red"]]), models_p3)

    ggplot(p3_data, aes(trial - r$p2_end, V, color = model)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_color_manual(values = cols_p3) +
      scale_y_continuous(limits = c(-0.05, 1.05)) +
      labs(x = "Phase 3 Trial", y = expression(V[B]),
           title = "Phase 3 Close-Up") +
      theme_app(base_size = 11) + theme(legend.position = "none")
  })

  output$t3_internal <- renderUI({
    r <- t3_res()
    div <- abs(r$rw_vb - r$mack_vb)
    tags$div(
      tags$table(class = "table table-sm table-bordered",
        tags$tr(tags$th(""), tags$th("RW"), tags$th(r$mlabel)),
        tags$tr(tags$td("Final V_B"), tags$td(sprintf("%.3f", r$rw_vb)),
                tags$td(sprintf("%.3f", r$mack_vb))),
        tags$tr(tags$td(HTML("&alpha;<sub>B</sub> (into P3)")),
                tags$td("0.40 (fixed)"),
                tags$td(sprintf("%.3f", r$alpha_b)))
      ),
      p(class = "fw-bold", sprintf("Divergence: %.3f", div))
    )
  })

  output$t3_status <- renderUI({
    r <- t3_res()
    div_val <- abs(r$rw_vb - r$mack_vb)
    badges <- list()
    if (input$t3_gen > 0.3)
      badges <- c(badges, list(status_badge("Generalization load-bearing", "warning")))
    if (input$t3_freeze)
      badges <- c(badges, list(status_badge("Attention ablated", "info")))
    if (div_val > 0.15)
      badges <- c(badges, list(status_badge("Strong divergence", "success")))
    else if (div_val > 0.05)
      badges <- c(badges, list(status_badge("Moderate divergence", "secondary")))
    else
      badges <- c(badges, list(status_badge("Weak divergence", "danger")))

    do.call(tagList, c(badges, list(
      p(class = "small text-muted mt-2",
        "Try adjusting generalization, Phase 2 length, or ablation",
        "to see which auxiliary assumptions carry the conclusion."))))
  })

  # === TAB 4 ================================================================
  t4_data <- eventReactive(input$t4_run, {
    xv <- input$t4_x; yv <- input$t4_y; n <- input$t4_res
    n_p3 <- input$t4_p3; gen <- input$t4_gen

    rng <- function(v) switch(v,
      alpha_A = seq(0.1, 0.9, length.out = n),
      beta = seq(0.1, 0.9, length.out = n),
      n_p1 = seq(5, 40, length.out = n), n_p2 = seq(5, 40, length.out = n))

    grid <- expand.grid(x = rng(xv), y = rng(yv))
    grid$div <- NA_real_

    withProgress(message = "Computing...", value = 0, {
      for (i in seq_len(nrow(grid))) {
        p <- list(alpha_A = 0.4, beta = 0.3, n_p1 = 10, n_p2 = 10)
        p[[xv]] <- grid$x[i]; p[[yv]] <- grid$y[i]
        a <- p$alpha_A; b <- p$beta
        d12 <- c(make_trials("A", 1, round(p$n_p1), "P1"),
                 make_trials(c("A","B"), 1, round(p$n_p2), "P2"))
        d3 <- make_trials("B", 1, n_p3, "P3")

        rv <- rw_final(d12, alpha = c(A=a, B=a), beta = b)
        rv["B"] <- gen * rv[["B"]]
        rv3 <- rw_final(d3, alpha = c(A=a, B=a), beta = b, V_init = rv)[["B"]]

        ms <- mack_final_state(d12, S = c(A=b, B=b), alpha_init = c(A=0.5, B=0.5))
        ms$V["B"] <- gen * ms$V[["B"]]
        mv3 <- mack_final_state(d3, S = c(A=b, B=b),
                                alpha_init = ms$alpha, V_init = ms$V)$V[["B"]]
        grid$div[i] <- abs(rv3 - mv3)
        if (i %% 10 == 0) incProgress(10/nrow(grid))
      }
    })
    names(grid)[1:2] <- c(xv, yv)
    list(grid = grid, xv = xv, yv = yv)
  })

  output$t4_heat <- renderPlot({
    r <- t4_data()
    lab <- function(v) switch(v, alpha_A = expression(alpha[A]), beta = expression(beta),
                              n_p1 = "Phase 1 trials", n_p2 = "Phase 2 trials")
    ggplot(r$grid, aes(.data[[r$xv]], .data[[r$yv]])) +
      geom_raster(aes(fill = div), interpolate = TRUE) +
      scale_fill_viridis(option = "D", name = expression(Delta*italic(V)[B]),
                         guide = guide_colorbar(title.position = "top", barwidth = 12)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = lab(r$xv), y = lab(r$yv),
           title = "Divergence Landscape: Structural or Parametric?",
           subtitle = sprintf("Phase 3 = %d trials | g = %.1f", input$t4_p3, input$t4_gen)) +
      coord_fixed() +
      theme_minimal(base_size = 13) +
      theme(panel.grid = element_blank(), legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 15))
  })

  output$t4_verdict <- renderUI({
    g <- t4_data()$grid
    robust <- mean(g$div > 0.05, na.rm = TRUE)
    pct <- round(100 * robust)
    mx <- round(max(g$div, na.rm = TRUE), 3)

    cls <- if (pct > 80) "success" else if (pct > 40) "warning" else "danger"
    lbl <- if (pct > 80) "Structurally robust" else if (pct > 40) "Partially robust" else "Parametric"

    div(
      status_badge(lbl, cls),
      p(class = "mt-2 mb-1",
        sprintf("Divergence > 0.05 in %d%% of the sampled parameter space. Max = %.3f.", pct, mx)),
      p(class = "small text-muted fst-italic",
        "A model result is not yet a theoretical prediction until it survives",
        "across a defensible range of parameter values.")
    )
  })
}

shinyApp(ui, server)
