app_ui <- function() {
  
  bslib::page_fillable(
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
      mod_data_gen_ui("data"),
      mod_compare_stats_ui("compare")
  )

}
