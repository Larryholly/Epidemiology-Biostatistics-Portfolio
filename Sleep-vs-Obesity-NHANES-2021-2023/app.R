library(shiny)
library(bslib)
library(ggplot2)
library(DT)

res <- readRDS("results.rds")

var_labels   <- res$var_labels
value_labels <- res$value_labels

# ---- helpers -------------------------------------------------------------

vlab <- function(v) {
  unname(ifelse(v %in% names(var_labels), var_labels[v], gsub("_", " ", v)))
}

p_text <- function(p) {
  ifelse(is.na(p), "p = NA",
         ifelse(p < 0.001, "p < 0.001", paste0("p = ", formatC(p, format = "f", digits = 3))))
}

cat_label_vars <- setdiff(names(var_labels), c("age", "height", "bmi"))

clean_term <- function(terms) {
  vapply(terms, function(x) {
    if (x == "(Intercept)") return("Intercept")
    for (v in cat_label_vars) {
      if (startsWith(x, v)) {
        rest <- substring(x, nchar(v) + 1)
        lab  <- value_labels[[v]]
        if (!is.null(lab) && rest %in% names(lab)) rest <- unname(lab[rest])
        return(paste0(var_labels[[v]], ": ", rest))
      }
    }
    x
  }, character(1), USE.NAMES = FALSE)
}

# ---- choices -------------------------------------------------------------

cat_vars  <- unique(res$cat_summary$variable)
num_vars  <- res$num_summary$variable
var_choices <- c(setNames(cat_vars, vlab(cat_vars)),
                 setNames(num_vars, vlab(num_vars)))

biv_vars    <- unique(res$bivariate_prev$variable)
biv_choices <- setNames(biv_vars, vlab(biv_vars))

model_names   <- unique(res$model_or$model)
default_model <- if ("Fully adjusted" %in% model_names) "Fully adjusted" else tail(model_names, 1)

# ---- UI ------------------------------------------------------------------

ui <- page_navbar(
  title = "Sleep Duration and Obesity: NHANES 2021-2023",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel(
    "Overview",
    layout_columns(
      value_box("Obesity prevalence (adults with exam weight)", textOutput("prev_all")),
      value_box("Adults with an exam weight", textOutput("n_exam")),
      value_box("Complete-case model sample", textOutput("n_model")),
      col_widths = c(4, 4, 4)
    ),
    card(
      card_header("Research question"),
      p("Among U.S. adults aged 20 years and older, is sleep duration associated with obesity ",
        "after adjusting for demographic, socioeconomic, and selected behavioral factors?")
    ),
    layout_columns(
      card(card_header("Sample flow"), tableOutput("flow")),
      card(card_header("Sleep duration and obesity (odds ratios, compared with 7-9 hours of sleep)"),
           tableOutput("key_or")),
      col_widths = c(5, 7)
    )
  ),
  
  nav_panel(
    "Variables",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("var", "Variable", choices = var_choices),
        helpText("Percentages are survey-weighted and based on participants with a valid response.",
                 "Counts are unweighted. Missing values are shown as counts only.")
      ),
      card(plotOutput("var_plot", height = "340px")),
      card(DTOutput("var_tbl"))
    )
  ),
  
  nav_panel(
    "Missingness",
    card(
      card_header("Percent missing, adults with an exam weight"),
      plotOutput("miss_plot")
    ),
    card(DTOutput("miss_tbl"))
  ),
  
  nav_panel(
    "Bivariate",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("biv_var", "Variable", choices = biv_choices),
        uiOutput("biv_test"),
        helpText("Bars show the survey-weighted percent obese (BMI 30 or higher) in each category.",
                 "These are unadjusted associations, not causal effects.")
      ),
      card(plotOutput("biv_plot", height = "340px")),
      card(DTOutput("biv_tbl")),
      card(card_header("Weighted tests for all variables"), DTOutput("biv_all"))
    )
  ),
  
  nav_panel(
    "Models",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("models_sel", "Models in the plot",
                           choices = model_names, selected = model_names),
        selectInput("tbl_model", "Coefficient table for", choices = model_names,
                    selected = default_model),
        checkboxInput("hide_int", "Hide intercept", TRUE)
      ),
      card(card_header("Sleep duration compared with 7-9 hours"),
           plotOutput("forest", height = "340px")),
      card(card_header("Model details"), tableOutput("model_info")),
      card(
        card_header("All coefficients (odds ratios)"),
        p(class = "text-muted",
          "The survey design has few degrees of freedom, so p values and intervals use the design",
          "degrees of freedom. Covariate estimates in the fully adjusted model are less precise",
          "and are there for adjustment, not to be interpreted on their own."),
        DTOutput("coef_tbl")
      )
    )
  ),
  
  nav_panel(
    "About",
    card(
      card_header("Data and methods"),
      p("Data: National Health and Nutrition Examination Survey (NHANES), August 2021 to August 2023."),
      p("Analysis: survey-weighted logistic regression using the MEC examination weight, strata, and",
        "primary sampling units. Obesity is measured BMI of 30 or higher. Sleep duration is usual",
        "weekday or workday sleep, grouped as short (<7 hours), recommended (7-9 hours, reference),",
        "and long (>9 hours)."),
      p("Percentages in the Variables and Bivariate pages use adults aged 20 or older with a positive",
        "exam weight. The model pages use only participants with complete data on every model variable.")
    ),
    card(
      card_header("Limitations"),
      uiOutput("limits")
    )
  )
)

# ---- server --------------------------------------------------------------

server <- function(input, output, session) {
  
  # Overview
  output$prev_all <- renderText(paste0(res$prev_overall$prevalence_pct[1], "%"))
  output$n_exam   <- renderText(format(res$sample_flow$n[2], big.mark = ","))
  output$n_model  <- renderText(format(res$sample_flow$n[3], big.mark = ","))
  
  output$flow <- renderTable({
    data.frame(Step = res$sample_flow$step,
               n = format(res$sample_flow$n, big.mark = ","))
  })
  
  output$key_or <- renderTable({
    d <- res$model_or[grepl("^sleep_group", res$model_or$term), ]
    data.frame(
      Model = d$model,
      `Sleep group` = sub("^sleep_group", "", d$term),
      `OR (95% CI)` = sprintf("%.2f (%.2f to %.2f)", d$OR, d$lower, d$upper),
      `p value` = p_text(d$p_value),
      check.names = FALSE
    )
  })
  
  # Variables
  is_num <- reactive(input$var %in% num_vars)
  
  output$var_plot <- renderPlot({
    req(input$var)
    validate(need(!is_num(), "Numeric variable: see the summary table below."))
    d <- res$cat_summary[res$cat_summary$variable == input$var &
                           res$cat_summary$level != "Missing", ]
    d$level <- factor(d$level, levels = rev(unique(d$level)))
    ggplot(d, aes(pct_weighted, level)) +
      geom_col(fill = "#2c7fb8") +
      geom_text(aes(label = paste0(pct_weighted, "%")), hjust = -0.1) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Weighted %", y = NULL, title = vlab(input$var)) +
      theme_minimal(base_size = 14)
  })
  
  output$var_tbl <- renderDT({
    req(input$var)
    if (is_num()) {
      d <- res$num_summary[res$num_summary$variable == input$var, ]
      out <- data.frame(Variable = vlab(d$variable), n = d$n, Missing = d$n_missing,
                        `Weighted mean` = d$mean_weighted, SE = d$se,
                        Min = d$min, Max = d$max, check.names = FALSE)
    } else {
      d <- res$cat_summary[res$cat_summary$variable == input$var, ]
      out <- data.frame(Level = d$level, n = d$n, `Weighted %` = d$pct_weighted,
                        check.names = FALSE)
    }
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })
  
  # Missingness
  output$miss_plot <- renderPlot({
    d <- res$missingness
    d$label <- factor(vlab(d$variable), levels = rev(vlab(d$variable)))
    ggplot(d, aes(pct_missing, label)) +
      geom_col(fill = "#d95f02") +
      geom_text(aes(label = paste0(pct_missing, "%")), hjust = -0.1) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "% missing", y = NULL) +
      theme_minimal(base_size = 14)
  }, height = function() max(320, 28 * nrow(res$missingness)))
  
  output$miss_tbl <- renderDT({
    d <- res$missingness
    datatable(data.frame(Variable = vlab(d$variable), `n missing` = d$n_missing,
                         `% missing` = d$pct_missing, check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })
  
  # Bivariate
  output$biv_test <- renderUI({
    req(input$biv_var)
    t <- res$bivariate_tests[res$bivariate_tests$variable == input$biv_var, ]
    req(nrow(t) == 1)
    p(strong("Weighted Rao-Scott chi-square test: "),
      sprintf("F = %.2f, %s (n = %s)", t$F_stat, p_text(t$p_value), format(t$n, big.mark = ",")))
  })
  
  output$biv_plot <- renderPlot({
    req(input$biv_var)
    d <- res$bivariate_prev[res$bivariate_prev$variable == input$biv_var, ]
    d$level <- factor(d$level, levels = rev(unique(d$level)))
    overall <- res$prev_overall$prevalence_pct[1]
    ggplot(d, aes(pct_obese_weighted, level)) +
      geom_col(fill = "#1b9e77") +
      geom_vline(xintercept = overall, linetype = "dashed") +
      geom_text(aes(label = paste0(pct_obese_weighted, "% (n = ", n, ")")), hjust = -0.05) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
      labs(x = "Weighted % obese", y = NULL, title = vlab(input$biv_var),
           caption = "Dashed line: overall prevalence, adults with an exam weight") +
      theme_minimal(base_size = 14)
  })
  
  output$biv_tbl <- renderDT({
    req(input$biv_var)
    d <- res$bivariate_prev[res$bivariate_prev$variable == input$biv_var, ]
    datatable(data.frame(Level = d$level, n = d$n, `Weighted % obese` = d$pct_obese_weighted,
                         check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })
  
  output$biv_all <- renderDT({
    d <- res$bivariate_tests
    d <- d[order(d$p_value), ]
    datatable(data.frame(Variable = vlab(d$variable), n = d$n, `F statistic` = d$F_stat,
                         `p value` = signif(d$p_value, 3), check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })
  
  # Models
  output$forest <- renderPlot({
    d <- res$model_or[grepl("^sleep_group", res$model_or$term) &
                        res$model_or$model %in% input$models_sel, ]
    validate(need(nrow(d) > 0, "Select at least one model."))
    d$term <- sub("^sleep_group", "", d$term)
    ggplot(d, aes(OR, term, color = model)) +
      geom_pointrange(aes(xmin = lower, xmax = upper),
                      position = position_dodge(width = 0.6)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      scale_x_log10() +
      labs(x = "Odds ratio (95% CI, log scale)", y = NULL, color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(ncol = 1))
  })
  
  output$model_info <- renderTable({
    d <- res$model_info
    data.frame(Model = d$model, n = format(d$n, big.mark = ","),
               Coefficients = d$n_coefficients, `Design df` = d$design_df,
               `Overall sleep test` = p_text(d$sleep_overall_p), check.names = FALSE)
  })
  
  output$coef_tbl <- renderDT({
    req(input$tbl_model)
    d <- res$model_or[res$model_or$model == input$tbl_model, ]
    if (isTRUE(input$hide_int)) d <- d[d$term != "(Intercept)", ]
    datatable(data.frame(Term = clean_term(d$term), OR = d$OR, Lower = d$lower,
                         Upper = d$upper, `p value` = d$p_value, check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })
  
  # About
  output$limits <- renderUI({
    n_exam  <- format(res$sample_flow$n[2], big.mark = ",")
    n_model <- format(res$sample_flow$n[3], big.mark = ",")
    full <- res$model_info[res$model_info$model == "Fully adjusted", ]
    p(paste0(
      "The data are cross-sectional, so the order of sleep and obesity cannot be established and ",
      "results describe associations, not causal effects. Reverse causation is plausible. ",
      "Sleep duration is self-reported weekday or workday sleep, and obesity is defined by measured BMI, ",
      "which does not distinguish fat from lean mass. The adjusted analysis used complete cases only (",
      n_model, " of ", n_exam, " adults with an exam weight), and those excluded may differ from those included. ",
      "The NHANES response rate was low in this cycle. Residual confounding is likely: physical activity, ",
      "diet, sleep apnea, and shift work were not included, alcohol use measures frequency rather than quantity, ",
      "and depressed mood comes from a single questionnaire item. The survey design has ",
      full$design_df, " degrees of freedom while the fully adjusted model estimates ",
      full$n_coefficients, " coefficients, so covariate-level inference should be interpreted cautiously."
    ))
  })
}

shinyApp(ui, server)