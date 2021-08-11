library('shiny')
library('reactable')
library('rhandsontable')
library(tidyverse)
library(scales)


Analise_Trimestral <- read_rds("dados_tratados/analise_trimestral.rds")
Analise_Anual <- read_rds("dados_tratados/analise_anual.rds") %>% 
    mutate(
        ano = as.integer(ano)    
    )


selectBancos <-     
    selectInput(
        "Bancos", 
        label = "Bancos", 
        choices = Analise_Trimestral$Banco %>% unique(),
        multiple = TRUE 
    )


tipo_periodo <- radioButtons(
    inputId = "tipoperiodo",
    label = "Tipo período",
    choices = c("Anual", "Trimestral")
)


ui <- fluidPage(
    h1("Dashboard Bancos"),
    hr(),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            width = 2,
            h3("Filtros:"),
            selectBancos,
            tipo_periodo,
        ),
        mainPanel = mainPanel(
            h3("Gráfico:"),
            plotOutput("grafico"),
            h3("Tabela bonita:"),
            reactableOutput("tabela_reactable"),
            h3("Tabela pros viciados:"),
            rHandsontableOutput("tabela_rhanson")
        )
    )
)


server <- function(input, output, session) {
    
    
    dados_usados <- reactive({
        
        if(input$tipoperiodo == "Anual"){
            saida <- Analise_Anual
        } else {
            saida <- Analise_Trimestral
        }
        
        saida %>% 
            filter(Banco %in% input$Bancos)
            
    })
    
    
    output$grafico <- renderPlot({
        
        dados = dados_usados()
        
        if(input$tipoperiodo == "Anual"){
            campo_x <-  "ano"
            camada_escala_x <-  scale_x_continuous(breaks = min(dados$ano):max(dados$ano) )
        } else{
            campo_x <-  "Trimestre_Ano"
            camada_escala_x <-  NULL
        }
        
        

        dados_usados() %>% 
            ggplot() +
            geom_line(aes(x = .data[[campo_x]], y = quantidade_clientes, color = Banco )) +
            geom_point(aes(x = .data[[campo_x]], y = quantidade_clientes, color = Banco )) +
            scale_y_log10(
                labels = number_format(accuracy = 1, big.mark = ".")
            ) +
            theme_light() + 
            theme(
                legend.position = "top"
            ) +
            camada_escala_x
        
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