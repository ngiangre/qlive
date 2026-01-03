mod_data_gen_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(
          ns("dist1"),
          "Group 1 distribution",
          choices = c("normal", "uniform", "exponential", "lognormal")
        )
      ),
      shiny::column(
        6,
        shiny::selectInput(
          ns("dist2"),
          "Group 2 distribution",
          choices = c("normal", "uniform", "exponential", "lognormal")
        )
      )
      )
  )
}

mod_data_gen_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      draw_data <- function(n, dist) {
        switch(
          dist,
          normal      = stats::rnorm(n),
          uniform     = stats::runif(n),
          exponential = stats::rexp(n),
          lognormal   = stats::rlnorm(n)
        )
      }

      rdata <- reactiveVal()
      shiny::observeEvent(
        c(input$dist1,input$dist2), 
        {
          shiny::req(
            input$dist1,input$dist2
          )
          df <- purrr::map(
            3:100,
            ~{
              y1 <- draw_data(.x, input$dist1)
              y2 <- draw_data(.x, input$dist2)

              df <- data.frame(
                n = .x,
                y = c(y1, y2),
                group = factor(
                  c(rep(0, length(y1)), rep(1, length(y2)))
                )
              )
              df
            }
          ) |> 
            purrr::list_rbind()
          
          rdata(df)
        })
      
      rdata
      
    }
  )
}
