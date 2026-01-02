mod_compare_stats_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header("Statistical comparison"),
    shiny::plotOutput(ns("results"),
      width = "100%"
    )
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

      output$results <- shiny::renderPlot({
        df <- data()
        shiny::req(df)
        
        summarised_df <- 
          df |> 
          dplyr::reframe(
            {
              g1 <- y[group == levels(group)[1]]
              g2 <- y[group == levels(group)[2]]

              # Classical tests
              ttest <- stats::t.test(g1, g2, var.equal = TRUE)
              wilcox <- stats::wilcox.test(g1, g2)

              # Linear model equivalents
              lm_param <- stats::lm(y ~ 1 + group)
              lm_rank  <- stats::lm(signed_rank(y) ~ 1 + group)

              lm_param_stats <- extract_lm_stats(lm_param, "group1")
              lm_rank_stats  <- extract_lm_stats(lm_rank,  "group1")
              cbind(
                rbind(
                  data.frame(
                    comparison = "t.test vs. lm",
                    statistic = c(
                      'estimate',
                      'pvalue'
                    ),
                    builtin_value = c(
                      unname(diff(ttest$estimate)),
                      ttest$p.value
                    )
                  ),
                  data.frame(
                    comparison = "wilcox.test vs. lm",
                    statistic = c(
                      'estimate',
                      'pvalue'
                    ),
                    builtin_value = c(
                      NA_real_,
                      wilcox$p.value
                    )
                  )
                ),
                rbind(
                  data.frame(
                    comparison = "t.test vs. lm",
                    statistic = c(
                      'estimate',
                      'pvalue'
                    ),
                    lm_value = c(
                      lm_param_stats$estimate,
                      lm_param_stats$p_value
                    )
                  ),
                  data.frame(
                    comparison = "wilcox.test vs. lm",
                    statistic = c(
                      'estimate',
                      'pvalue'
                    ),
                    lm_value = c(
                      lm_rank_stats$estimate,
                      lm_rank_stats$p_value
                    )
                  )    
                )
              )
            },
            .by = n
          ) |> 
          dplyr::tibble()
        
        summarised_df |> 
          ggplot2::ggplot(
            ggplot2::aes(builtin_value,lm_value,fill=n)
          ) + 
          ggplot2::geom_abline(
            intercept = 0, slope = 1,
            linetype = "dashed",color = "red",
            linewidth = 2
          ) +
          ggplot2::geom_point(pch=21,size=5) + 
          ggplot2::facet_grid(
            statistic~comparison,
            scales="free"
          ) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::theme_bw()
        
      })
    }
  )
}
