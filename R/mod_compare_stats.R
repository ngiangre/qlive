mod_compare_stats_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    ggiraph::girafeOutput(ns("results"))
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

      output$results <- ggiraph::renderGirafe({
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
                    comparison = "t.test(g1, g2, var.equal = TRUE) vs. lm(y ~ 1 + group)",
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
                    comparison = "wilcox.test(g1, g2) vs. lm(signed_rank(y) ~ 1 + group)",
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
                    comparison = "t.test(g1, g2, var.equal = TRUE) vs. lm(y ~ 1 + group)",
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
                    comparison = "wilcox.test(g1, g2) vs. lm(signed_rank(y) ~ 1 + group)",
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
          dplyr::tibble() |> 
          dplyr::mutate(
            id = 1:dplyr::n(),
            tooltip = glue::glue("
              Comparison: {comparison}
              Statistic: {statistic}
              Sample Size: {n}
              Builtin Value: {builtin_value}
              LM Value: {lm_value}
            ")
          )
        
        g <- summarised_df |> 
          ggplot2::ggplot(
            ggplot2::aes(
              builtin_value,
              lm_value,
              color = n,
              id = id,
              tooltip = tooltip)
          ) + 
          ggplot2::geom_abline(
            intercept = 0, slope = 1,
            linetype = "dashed",color = "black"
          ) +
          ggiraph::geom_point_interactive(
            size = 7
          ) + 
          ggplot2::facet_wrap(
            ~comparison + statistic,
            scales="free"
          ) +
          ggplot2::scale_color_viridis_c(
            name = "Sample Size"
          ) +
          ggplot2::theme_bw(
            base_size = 24
          ) +
          ggplot2::theme(
            text = ggplot2::element_text(
              face = "bold"
            )
          )
        
        ggiraph::girafe(
          ggobj = g,
          width_svg = 20L,
          height_svg = 12L
        )
        
      })
    }
  )
}
