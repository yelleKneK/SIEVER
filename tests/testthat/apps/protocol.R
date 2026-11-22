# Test-only acknowledgment of a completed Shiny flush. No production hook.
siever_browser_protocol <- function(app) {
  original <- app$serverFuncSource
  app$serverFuncSource <- function() {
    function(input, output, session) {
      original()(input, output, session)
      shiny::observeEvent(input$siever_test_flush, {
        token <- input$siever_test_flush
        session$onFlushed(function() session$sendCustomMessage("siever-test-flush", token), once = TRUE)
      })
    }
  }
  app
}
