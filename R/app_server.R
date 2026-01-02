app_server <- function(input, output, session) {
  data <- mod_data_gen_server("data")

  mod_compare_stats_server(
    id = "compare",
    data = data
  )

}
