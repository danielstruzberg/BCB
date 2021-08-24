library('shiny')
library('reactable')
library('rhandsontable')
library(tidyverse)
library(scales)
library(patchwork)
library(writexl)
library(ggiraph)
library(tsibble)


encaixa_grafico_ggiraph <- function(grafico){
    
    girafe(
        code = {print(grafico)},
        width_svg = 10, 
        height_svg = 5,
        options = list(
            opts_selection(type = "multiple", girafe_css(css = "stroke:orange", line = "stroke:orange")),
            opts_tooltip(use_fill = TRUE),
            opts_hover(css = "")
        )
    )
    
}  


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
        selected = "INTER",
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
            splitLayout(
                downloadLink(label = "Baixar dados", outputId = "download_dado")
            ) %>% 
            h3("Gráfico:"),
            girafeOutput("grafico"),
            h3("Número de clientes:"),
            reactableOutput("tabela_reactable")
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
    
    
    dados_todos <- reactive({

        if(input$tipoperiodo == "Anual"){
            saida <- Analise_Anual
        } else {
            saida <- Analise_Trimestral
        }
        
        saida
        
    })

    

    campo_data <- reactive({
        
        if(input$tipoperiodo == "Anual"){
            saida <-  "ano"
        } else{
            saida <-  "Trimestre_Ano"
        }
        
        saida
        
    })    
    
    
    
    mediana_indice_reclamacoes <- reactive({
     

    

        n_categorias <- dados_usados() %>% 
            summarise(
                n = n_distinct(categoria)
            ) %>% 
            pull(n)
            
        if(n_categorias == 1){
            saida <- dados_todos() %>% 
                group_by(
                    across(
                        .cols = any_of(campo_data())
                    )
                ) %>% 
                summarise(
                    reclamacoes_por_cliente = median(reclamacoes_por_cliente, na.rm = TRUE)
                )
        } else{
            saida <- NULL
        }
        
        
           
    })
        
    
    
    output$grafico <- renderGirafe({
        
        dados = dados_usados()
        
        if(input$tipoperiodo == "Anual"){
            campo_x <-  "ano"
            camada_escala_x <-  scale_x_continuous(breaks = min(dados$ano):max(dados$ano) )
        } else{
            campo_x <-  "Trimestre_Ano"
            camada_escala_x <-  NULL
        }
        
        

        grafico_qtd_clientes <-  dados_usados() %>% 
            ggplot() +
            ggtitle('Quantidade de Clientes')+
            geom_line(aes(x = .data[[campo_x]], y = quantidade_clientes, color = Banco )) +
            geom_point_interactive(
            aes(x = .data[[campo_x]], y = quantidade_clientes, color = Banco , 
                tooltip = paste0(
                "Numero de Clientes : ",
                number(quantidade_clientes, big.mark = ".")

            
            ) ))+
            
    
            scale_y_log10(
                labels = number_format(accuracy = 1, big.mark = ".")
            ) +
            theme_light() + 
            theme(
                legend.position = "top"
            ) +
            camada_escala_x
        


        
        grafico_reclamacoes <-  dados_usados() %>% 
            ggplot() +
            ggtitle('Quantidade de Reclamacoes')+
            geom_line(aes(x = .data[[campo_x]], y = reclamacoes_por_cliente * 1000, color = Banco )) +
            geom_point_interactive(
                aes(
                    x = .data[[campo_x]], 
                    y = reclamacoes_por_cliente * 1000, 
                    color = Banco,
                    tooltip = paste0(
                        "Reclamações:", 
                        quantidade_reclamacoes, 
                        "\nClientes:", 
                        number(quantidade_clientes, big.mark = "." ),
                        "\nReclamações / 1000 Clientes:", 
                        number(reclamacoes_por_cliente * 1000, decimal.mark = ",", accuracy = 0.001 )
                    )
                )
            ) +
            geom_line(
                data = mediana_indice_reclamacoes(),
                aes(
                    y = reclamacoes_por_cliente * 1000,
                    x = .data[[campo_x]]
                ),
                color = "black",
                linetype = "dashed",
                alpha = 0.5
                
            ) +
            geom_point_interactive(
                data = mediana_indice_reclamacoes(),
                aes(
                    y = reclamacoes_por_cliente * 1000,
                    x = .data[[campo_x]],
                    tooltip = paste0("Reclamações / 1000 clientes:", number(reclamacoes_por_cliente * 1000, accuracy = .001, decimal.mark = ",")),
                    
                ),
                color = "black",
                alpha = 0.5
            ) +
            scale_y_continuous(
                labels = number_format(accuracy = 0.01, big.mark = ".")
            ) +
            theme_light() + 
            theme(
                legend.position = "top"
            ) +
            camada_escala_x
        
        
        grafico_crescimento_clientes <- dados_usados() %>% 
            ggplot() +
            ggtitle('Variação Trimestral')+
            
            geom_col(
                aes(x = .data[[campo_x]], 
                    y = delta_qtd_clientes, 
                    fill = Banco 
                ),
                position = "dodge"
            ) +
            scale_y_continuous(
                labels = percent_format(accuracy = 0.1, decimal.mark = ",")
            ) +
            theme_light() + 
            theme(
                legend.position = "top"
            ) +
            
            geom_hline(
                yintercept = 0,
                color = "black",
                size = 1.5,
            ) +
            camada_escala_x


                
        encaixa_grafico_ggiraph(grafico_qtd_clientes + grafico_crescimento_clientes + grafico_reclamacoes)
        
        
    }) 
    
    
    output$tabela_reactable <- renderReactable({
        
        
        if(input$tipoperiodo == "Anual"){
            campos_selecionados <- c(
                "Banco",
                "ano",
                "quantidade_clientes"
            )
            
            campo_data <-  "ano"

        } else{
            campos_selecionados <- c(
                "Banco",
                "Trimestre_Ano",
                "quantidade_clientes"
            )
            
            campo_data <- "Trimestre_Ano"
        }

        

        
        dados_usados() %>%             
            select(
                any_of(campos_selecionados)    
            ) %>% 
            as_tibble() %>% 
            arrange(.data[[campo_data]]) %>% 
            pivot_wider(
                names_from = any_of(campo_data),
                values_from = quantidade_clientes
            ) %>% 
            reactable(
                defaultColDef = colDef(
                    format = colFormat(digits = 0, separators = TRUE) 
                )
            )
        
    })
    
    output$download_dado <- downloadHandler(
        filename = "dados.xlsx",
        content =  function(arq){
            write_xlsx(dados_usados(), arq)
        }
    )

    
}


shinyApp(ui, server)

