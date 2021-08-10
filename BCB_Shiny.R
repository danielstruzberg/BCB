library('shiny')
library('reactable')
library('rhandontable')
library('rhandsontable')


selectBancos <-     
    selectInput(
        "Bancos", 
        label = "Bancos", 
        choices = Analise_Trimestral$Banco %>% unique(),
        multiple = TRUE 
    )


ui <- fluidPage(
    h1("Dashboard Bancos"),
    hr(),
    h3("Filtros:"),
    selectBancos,
    h3("Gráfico:"),
    plotOutput("grafico"),
    h3("Tabela bonita:"),
    reactableOutput("tabela_reactable"),
    h3("Tabela pros viciados:"),
    rHandsontableOutput("tabela_rhanson")
)


server <- function(input, output, session) {
    
    output$grafico <- renderPlot({
        Analise_Anual %>% 
            filter(Banco %in% input$Bancos) %>% 
            ggplot() +
            geom_line(aes(x = ano, y = quantidade_clientes, color = Banco )) +
            geom_point(aes(x = ano, y = quantidade_clientes, color = Banco )) +
            theme_light()
        
    }) 
    
    
    output$tabela_reactable <- renderReactable({
        
        Analise_Anual %>% 
            filter(Banco %in% input$Bancos) %>%
            select(
                Banco,
                ano,
                quantidade_clientes
            ) %>% 
            pivot_wider(
                names_from = ano,
                values_from = quantidade_clientes
            ) %>% 
            reactable(
                defaultColDef = colDef(
                    format = colFormat(digits = 0, separators = TRUE) 
                )
            )
        
    })
    
    
    output$tabela_rhanson <- renderRHandsontable({
        Analise_Anual %>% 
            filter(Banco %in% input$Bancos) %>%
            select(
                Banco,
                ano,
                quantidade_clientes
            ) %>% 
            pivot_wider(
                names_from = ano,
                values_from = quantidade_clientes
            ) %>% 
 
            rhandsontable(
                readOnly = TRUE
            ) %>% 
            hot_cols(
                format = "0,000",
                language = "pt-BR"
            )
        
    })
    
    
}


shinyApp(ui, server)