# ============================================================================
# main.R
#
# Script para análises de dados do Bandcamp.
#
# 2022-05-27 David Souza Pinto
# ============================================================================


# ================================================================= Bibliotecas
library(data.table)
library(ggplot2)
library(openxlsx)
library(lubridate)


# ===================================================================== Leitura
setwd('~/Desktop/r4adr/data') # Ajustar caso necessário

vendas <- fread('1M-bandcamp-sales.csv')
moedas <- fread('https://raw.githubusercontent.com/datasets/currency-codes/master/data/codes-all.csv')



# ================================================================= Organização
# Estrutura dos dados
str(vendas)
str(moedas)

# contagem de NAs
contar_NAs <- \(x) sum(is.na(x))

# verificação de NAs
vendas[, lapply(.SD, contar_NAs)]
vendas[, lapply(.SD, contar_NAs), .SDcols=c('item_price', 'country', 'type')]


# Ajuste dos dados: criação de coluna com dias da semana, dia, mês
vendas[, `:=`(diaSemana = wday(utc_timestamp, label = TRUE, week_start = 1),
              dia       = day(utc_timestamp),
              mes       = month(utc_timestamp)
             )]

# renomeando colunas
setnames(vendas, 'utc_timestamp', 'data')


# remoção da coluna de índice
vendas[, i := NULL]


# Criando uma tabela única para as moedas
moedas <- unique(moedas[, .(Currency, AlphabeticCode)])


# =============================================================== Transformação

# combinando a tabela de moedas ...............................................

# Unindo as tabelas, forma #1
vendas <- moedas[vendas, on = .(AlphabeticCode = currency)]

# Unindo as tabelas, forma #2
vendas <- merge(x = vendas,
    y = moedas,
    by.x = 'currency',
    by.y = 'AlphabeticCode',
    all.x = TRUE)



# Número de países ............................................................
vendas[, .(unique(country))][, .N]


# Range de datas ..............................................................
vendas[, range(data)]

# Número de compras por país ..................................................
(vendas_paises <- vendas[, .(n = .N), by = country][order(-n)])


# Número de compras por dia, sumário ..........................................
vendas_data <- vendas[, .(n = .N), by = data]
vendas_data[, as.list(summary(n))]


# Vendas por dia da semana ....................................................
vendas_dia <- vendas[, .(vendas = .N), by = diaSemana]

# verificação do dia de semana
vendas[data == '2020-10-02', unique(diaSemana)]


# média dos preços, valores pagos por moeda ...................................
vendas[ mean(item_price), by = currency]
vendas[ mean(amount_paid), by = currency]


# Sumário de colunas numéricas ................................................
vendas[, as.list(summary(item_price)), by = currency]
vendas[, as.list(summary(amount_paid)), by = currency]

# Filtro por dólar, combinando país e tipo de mercadoria
vendas[currency == 'USD', as.list(summary(item_price)), by = .(country, type)]


# Maiores preço e valor pago por moeda ........................................
vendas[, .SD[which.max(item_price)], by = .(currency)]
vendas[, .SD[which.max(amount_paid)], by = .(currency)]



# Verificando compras feitas pelo Brasil ou pela Argentina ....................
vendas_paises[country == 'Brazil' | country == 'Argentina']
vendas_paises[country %in% c('Brazil', 'Argentina')]


# Número de compras por moeda .................................................
vendas_moeda <- vendas[, .(n = .N), by = Currency][order(-n)]
vendas_moeda[, pct := 100 * n / sum(.SD), .SDcols = 'n']



# Totalização: valor pago, por moeda ..........................................
vendas_pago <- vendas[, 
    j = .(valor_pago = sum(amount_paid)),
    by = Currency][order(-valor_pago)]


# Itens com preço zerado ......................................................
vendas_preco_zero <- vendas[item_price == 0, .(n = .N)]
vendas_preco_zero[, pct := 100 * n / nrow(vendas)]



# Diferença no valor pago por moeda, país .....................................
vendas_diferenca_moeda <- vendas[,
    .(diferenca = sum(amount_paid - item_price), n = .N),
    by = currency] 
    
vendas_diferenca_pais  <- vendas[,
    .(diferenca = sum(amount_paid - item_price), n = .N),
    by = .(country, currency)]


# vendas[country == 'Switzerland' & currency == 'AUD']


# Agrupamento: vendas físicas ou não por tipo .................................
cubo_vendas_fisicas_tipo <- cube(vendas,
    j = .(vendas = .N),
    by = c('physical', 'type'))

rollup_vendas_fisicas_tipo <- rollup(vendas,
    j = .(vendas = .N),
    by = c('physical', 'type'))


# Agrupamento: valor vendido por moeda, tipo, se é físico ou não
cubo_vendas_moeda_tipo <- cube(vendas,
    j = .(valor = sum(amount_paid)),
    by = c('currency', 'type', 'physical'))




# Pivoteamento de tabela ......................................................
tbl_pivoteada <- dcast(cubo_vendas_moeda_tipo,
    currency ~ type,
    value.var = 'valor')

# Reversão do pivoteamento
cubo <- melt(tbl_pivoteada, id.vars='currency')




#================================================================= Visualização

# Visualizando vendas por data
p_vendas_data <- ggplot(vendas_data, aes(x=data, y=vendas)) + 
    geom_line() +
    ggtitle('Número de vendas por data',
        subtitle = 'Análise feita no perído de 9 set. 2020 a 2 out. 2020') +
    labs(x = 'Data',
        y = 'Vendas',
        caption = 'Fonte: Observable (2022) Bandcamp Sales Data') +
    theme_bw()


# Visualizando vendas por dia da semana
p_vendas_dia <- ggplot(vendas_dia,
    aes(x=vendas,
        y=reorder(diaSemana, vendas),
        fill=reorder(diaSemana, vendas)
        )
    ) + 
    geom_col() +
    labs(title = 'Número de vendas por dia da semana',
        subtitle = 'Análise feita no perído de 9 set. 2020 a 2 out. 2020',
        caption = 'Fonte: Observable (2022) Bandcamp Sales Data',
        fill = 'Dia') +
    ylab('Vendas') +
    xlab('Dia da semana') +
    scale_fill_viridis_d() +
    theme_bw()



# Visualização de vendas por tipo
# Empilhado
cubo_vendas_fisicas_tipo[!is.na(physical) & !is.na(type)] |>
    ggplot(aes(x = vendas,
        y = reorder(physical, vendas),
        fill = type)) +
    geom_col(position="stack") 
    
# Percentual
cubo_vendas_fisicas_tipo[!is.na(physical) & !is.na(type)] |>
    ggplot(aes(x = vendas,
        y = reorder(physical, vendas),
        fill = type)) +
    geom_col(position="fill")




# ============================================ p/ Comunicação: Exportar Tabelas
# Ajuste do diretório
setwd('~/Desktop/r4adr/out/excel')

# Salvar usando openxlsx: lista nomeada de tabelas
lista_tabelas <- list()
names(lista_tabelas) <- c()
arquivo_saida <- paste0(Sys.Date(), '_tabelas_bandcamp.xlsx')

# Exportando
write.xlsx(lista_tabelas, arquivo_saida)




# ============================================ p/ Comunicação: Exportar Imagens
# salvar imagens geradas
setwd('~/Desktop/r4adr/out/img')
ggsave()


