library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(glue)
library(scales)
library(shinyWidgets)

ROWS <- str_to_upper(letters[1:9])
COLS <- 1:12
HOTELS <- c('LUXOR', 'TOWER', 'AMERICAN', 'FESTIVAL', 'WORLDWIDE', 'CONTINENTAL', 'IMPERIAL')
HOTELS_COLORS <- hue_pal()(length(HOTELS))
HOTELS_COLORS <- sapply(HOTELS_COLORS, function(color) {
  color_rgb <- col2rgb(color)
  rgb(color_rgb[1], color_rgb[2], color_rgb[3], max = 255, alpha = 0.3 * 255)
})
HOTELS_SHORT <- sapply(HOTELS, function(h) str_sub(h, 1, 1), USE.NAMES = FALSE)
HOTELS_VALUES <- c(200, 200, 300, 300, 300, 400, 400)

calc_price <- function(v, count) {
  v_cum <- v
  # 2 to 5, 100 each step
  v_cum <- v_cum + max(min(count - 2, 5 - 2), 0) * 100
  # 6 to 10, 100 total
  if(count > 5) { v_cum = v_cum + 100 }
  # 11 to 20, 100 total
  if(count > 10) { v_cum = v_cum + 100 }
  # 21 to 30, 100 total
  if(count > 20) { v_cum = v_cum + 100 }
  # 31 to 40, 100 total
  if(count > 30) { v_cum = v_cum + 100 }
  # >40, 100 total
  if(count > 40) { v_cum = v_cum + 100 }
  v_cum
}

COLORS <- list('-' = 'white', '*' = 'black')
for(i in 1:length(HOTELS)) {
  COLORS[[HOTELS_SHORT[[i]]]] <- HOTELS_COLORS[[i]]
}

PLAYER_NAMES <- c('Linan', 'Chintan', 'Jenny', 'Zexuan')





board_inputs <- lapply(COLS, function(col) {
  column(
    width = 1,
    lapply(ROWS, function(row) {
      selectInput(paste0('select_input_', col, row), choices = c('-', '*', HOTELS_SHORT), label = paste0(col, '-', row))
    })
  )
})

card_inputs <- lapply(PLAYER_NAMES, function(p) {
  column(
    width = 3,
    p,
    lapply(HOTELS_SHORT, function(h) {
      numericInput(paste0('numeric_input_', p, '_', h), label = h, min = 0, max = 25, value = 0)
    }),
    autonumericInput(paste0('numeric_input_', p, '_cash'), label = NULL, min = 0, value = 6000, wheelStep = 50, currencySymbol = '$', currencySymbolPlacement = 'p', decimalPlaces = 0, formulaMode = TRUE)
  )
})

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML(paste0('.selectize-input:has(> .item[data-value=\"', names(COLORS), '\"]) { background-color: ', c(COLORS),  ' }'))),
      tags$style(HTML(c(sapply(PLAYER_NAMES, function(p) { paste0('#numeric_input_', p, '_', HOTELS_SHORT, ' { background-color: ', c(HOTELS_COLORS), ' }') }))))
    ),
    fluidRow(
      box(
        title = NULL, width = 8,
        board_inputs
      ),
      column(
        width = 4,
        box(
          title = NULL, width = 12,
          card_inputs
        ),
        box(
          title = NULL, width = 12,
          dataTableOutput('table_hotels_on_grid')
        )
      )
    ),
    fluidRow(
      box(
        title = NULL, width = 8, height = '500px'
      ),
      column(
        width = 4
      )
    )
  ),
  skin = 'black'
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  hotels_on_grid_r <- reactive({
    board <- expand_grid(row = ROWS, col = COLS)
    board$coord <- paste0(board$col, board$row)
    board$val <- sapply(board$coord, function(c) input[[paste0('select_input_', c)]])
    
    hotels_on_grid <- tibble(hotel = HOTELS_SHORT, count = 0, init_value = HOTELS_VALUES) %>% 
      left_join(board %>% group_by(hotel = val) %>% summarize(tile = n()), by = c('hotel')) %>% 
      mutate(count = replace_na(count + tile, 0)) %>% 
      select(-tile)
    
    hotels_on_grid$price <- sapply(1:nrow(hotels_on_grid), function(i) {
      calc_price(hotels_on_grid$init_value[i], hotels_on_grid$count[i])
    })
    hotels_on_grid$major <- hotels_on_grid$price * 10
    hotels_on_grid$minor <- hotels_on_grid$price * 5
    hotels_on_grid$init_value <- NULL
    
    hotels_on_grid
  })
  
  output$table_hotels_on_grid <- renderDataTable({ 
    datatable(hotels_on_grid_r(), rownames = FALSE, options = list(dom = 't')) %>% 
      formatCurrency(c('price', 'major', 'minor'), digits = 0) %>% 
      formatStyle(c('hotel'), fontWeight = 'bold', backgroundColor = styleEqual(HOTELS_SHORT, HOTELS_COLORS))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
