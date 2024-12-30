library(shiny)

# Define UI for button-based calculator with colored buttons and display box
ui <- fluidPage(
  titlePanel("Calculator"),
  
  # Layout for calculator buttons and display
  fluidRow(
    column(4, offset = 4,
           # Display panel (Styled as a box)
           div(
             textOutput("display"),
             style = "border: 2px solid black; background-color: lightyellow; 
                      padding: 10px; width: 252px; height: 50px; 
                      font-size: 24px; text-align: right; margin-bottom: 10px;"
           ),
           # Numeric buttons with colors
           actionButton("btn7", "7", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px; padding: 10px;margin-bottom: 10px;"), 
           actionButton("btn8", "8", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btn9", "9", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btnDiv", "/", style = "background-color: orange; color: white; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           br(),
           
           actionButton("btn4", "4", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btn5", "5", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btn6", "6", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btnMul", "*", style = "background-color: orange; color: white; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           br(),
           
           actionButton("btn1", "1", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btn2", "2", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btn3", "3", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btnSub", "-", style = "background-color: orange; color: white; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           br(),
           
           actionButton("btn0", "0", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btnDot", ".", style = "background-color: lightgray; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btnEq", "=", style = "background-color: green; color: white; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           actionButton("btnAdd", "+", style = "background-color: orange; color: white; width: 60px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;"), 
           br(),
           
           actionButton("btnClear", "C", style = "background-color: red; color: white; width: 252px; height: 60px; font-size: 20px;padding: 10px;margin-bottom: 10px;")  # Clear button across full width
    )
  )
)

# Define server logic for button-based calculator
server <- function(input, output, session) {
  # Reactive value to store the expression to be evaluated
  calc_expression <- reactiveVal("")
  
  # Update the display based on the button clicked
  observeEvent(input$btn0, { calc_expression(paste0(calc_expression(), "0")) })
  observeEvent(input$btn1, { calc_expression(paste0(calc_expression(), "1")) })
  observeEvent(input$btn2, { calc_expression(paste0(calc_expression(), "2")) })
  observeEvent(input$btn3, { calc_expression(paste0(calc_expression(), "3")) })
  observeEvent(input$btn4, { calc_expression(paste0(calc_expression(), "4")) })
  observeEvent(input$btn5, { calc_expression(paste0(calc_expression(), "5")) })
  observeEvent(input$btn6, { calc_expression(paste0(calc_expression(), "6")) })
  observeEvent(input$btn7, { calc_expression(paste0(calc_expression(), "7")) })
  observeEvent(input$btn8, { calc_expression(paste0(calc_expression(), "8")) })
  observeEvent(input$btn9, { calc_expression(paste0(calc_expression(), "9")) })
  observeEvent(input$btnDot, { calc_expression(paste0(calc_expression(), ".")) })
  
  # Handle operator buttons
  observeEvent(input$btnAdd, { calc_expression(paste0(calc_expression(), "+")) })
  observeEvent(input$btnSub, { calc_expression(paste0(calc_expression(), "-")) })
  observeEvent(input$btnMul, { calc_expression(paste0(calc_expression(), "*")) })
  observeEvent(input$btnDiv, { calc_expression(paste0(calc_expression(), "/")) })
  
  # Handle clear button
  observeEvent(input$btnClear, { calc_expression("") })
  
  # Handle equal button
  observeEvent(input$btnEq, {
    expr <- calc_expression()
    
    # Safely evaluate the expression
    result <- tryCatch(
      eval(parse(text = expr)),
      error = function(e) "Error"
    )
    
    calc_expression(as.character(result))
  })
  
  # Display the current expression or result
  output$display <- renderText({
    calc_expression()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
