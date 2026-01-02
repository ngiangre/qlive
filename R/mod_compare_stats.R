mod_compare_stats_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header("Statistical comparison"),
    shiny::tableOutput(ns("results"))
  )
}

mod_compare_stats_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      extract_lm_stats <- function(fit, term) {
        s <- summary(fit)$coefficients
        data.frame(
          estimate = s[term, "Estimate"],
          statistic = s[term, "t value"],
          p_value = s[term, "Pr(>|t|)"],
          row.names = NULL
        )
      }

      output$results <- shiny::renderTable({
        df <- data()
        shiny::req(df)
        
        df |> 
          dplyr::reframe(
            {
              g1 <- y[group == levels(group)[1]]
              g2 <- y[group == levels(group)[2]]

              # Classical tests
              ttest <- stats::t.test(g1, g2, var.equal = FALSE)
              wilcox <- stats::wilcox.test(g1, g2, exact = FALSE)

              # Linear model equivalents
              lm_param <- stats::lm(y ~ group, data = df)
              lm_rank  <- stats::lm(base::rank(y) ~ group, data = df)

              lm_param_stats <- extract_lm_stats(lm_param, "groupGroup 2")
              lm_rank_stats  <- extract_lm_stats(lm_rank,  "groupGroup 2")

              rbind(
                data.frame(
                  method = "Welch's (t.test)",
                  estimate = unname(diff(ttest$estimate)),
                  statistic = unname(ttest$statistic),
                  p_value = ttest$p.value
                ),
                data.frame(
                  method = "Mann–Whitney (wilcox.test)",
                  estimate = NA_real_,
                  statistic = unname(wilcox$statistic),
                  p_value = wilcox$p.value
                ),
                data.frame(
                  method = "LM: y ~ group",
                  estimate = lm_param_stats$estimate,
                  statistic = lm_param_stats$statistic,
                  p_value = lm_param_stats$p_value
                ),
                data.frame(
                  method = "LM: rank(y) ~ group",
                  estimate = lm_rank_stats$estimate,
                  statistic = lm_rank_stats$statistic,
                  p_value = lm_rank_stats$p_value
                )
              )
            },
            .by = n
          )
        
      }, digits = 6)
    }
  )
}
