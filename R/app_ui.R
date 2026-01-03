app_ui <- function() {
  
  bslib::page_fillable(
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    shiny::tags$h1("Compare generated statistics between R's builtins vs. linear models"),
    shiny::tags$p("When comparing two groups of data, visualize the generated statistics between R's builtin functions and the linear model implementation (see the plot facet titles for syntax)"),
    shiny::tags$p("Select a data distribution the values from each group are generated from."),
    mod_data_gen_ui("data"),
    mod_compare_stats_ui("compare")
  )

}
