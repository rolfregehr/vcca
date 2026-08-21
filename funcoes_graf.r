source("config.R")
# hgfh
grafico_barras_horizontal <- function(
    dados,
    categorias,
    valores,
    titulo,
    subtitulo = NULL,
    cor = 3
) {
  dados |>
    transmute(
      categoria = as.character({{ categorias }}),
      valor = as.numeric({{ valores }})
    ) |>
    filter(!is.na(categoria), !is.na(valor)) |>
    summarise(
      valor = sum(valor, na.rm = TRUE),
      .by = categoria
    ) |>
    arrange(desc(valor)) |>
    e_charts(categoria, reorder = FALSE) |>
    e_color(background = cor_fundo) |>
    e_text_style(fontFamily = fonte_senado, color = "#333333") |>
    e_bar(
      valor,
      itemStyle = list(color = unname(paleta[cor]))
    ) |>
    e_flip_coords() |>
    e_y_axis(
      inverse = TRUE,
      axisLabel = list(
        width = 250,
        overflow = "break",
        lineHeight = 18
      )
    ) |>
    e_tooltip(trigger = "item") |>
    e_title(
      text = titulo,
      subtext = subtitulo,
      left = "center"
    ) |>
    e_legend(show = FALSE)
}

grafico_linhas <- function(
    dados,
    eixo_x,
    eixo_y,
    categorias,
    titulo,
    subtitulo = "mensal",
    razao = NULL,
    nome_razao = NULL
) {
  dados <- dados |>
    transmute(
      x = {{ eixo_x }},
      y = as.numeric({{ eixo_y }}),
      categoria = {{ categorias }}
    ) |>
    filter(!is.na(x), !is.na(y), !is.na(categoria))

  if (is.character(dados$x) &&
      all(grepl("^[0-9]{6}$", dados$x))) {
    dados$x <- ym(dados$x)
  }

  dados <- dados |>
    summarise(
      y = sum(y, na.rm = TRUE),
      .by = c(x, categoria)
    ) |>
    arrange(x, categoria)

  if (is.null(razao)) {
    grafico <- dados |>
      group_by(categoria) |>
      e_charts(x) |>
      e_line(y, symbol = "none")
  } else {
    dados <- dados |>
      pivot_wider(names_from = categoria, values_from = y) |>
      mutate(.razao = .data[[razao[1]]] / .data[[razao[2]]])

    grafico <- e_charts(dados, x)

    for (categoria in setdiff(names(dados), c("x", ".razao"))) {
      grafico <- e_line_(
        grafico,
        categoria,
        name = categoria,
        symbol = "none"
      )
    }

    if (is.null(nome_razao)) {
      nome_razao <- paste("Razão", paste(razao, collapse = "/"))
    }

    grafico <- grafico |>
      e_line_(
        ".razao",
        name = nome_razao,
        symbol = "none",
        y_index = 1,
        lineStyle = list(type = "dashed")
      ) |>
      e_y_axis(
        index = 1,
        name = nome_razao,
        position = "right",
        scale = TRUE,
        splitLine = list(show = FALSE)
      )
  }

  grafico |>
    e_color(unname(paleta), background = cor_fundo) |>
    e_text_style(fontFamily = fonte_senado, color = "#333333") |>
    e_tooltip(trigger = "axis") |>
    e_title(
      text = titulo,
      subtext = subtitulo,
      left = "center"
    ) |>
    e_legend(type = "scroll", bottom = 0) |>
    e_x_axis(
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      splitLine = list(show = FALSE)
    ) |>
    e_y_axis(
      index = 0,
      scale = TRUE,
      axisLine = list(show = TRUE),
      axisTick = list(show = FALSE),
      splitLine = list(show = TRUE)
    ) |>
    e_grid(
      left = "5%",
      right = if (is.null(razao)) "3%" else "7%",
      top = "15%",
      bottom = "15%",
      containLabel = TRUE
    )
}

grafico_linhas_facetas <- function(
    dados,
    eixo_x,
    eixo_y,
    grupo,
    facet,
    colunas = 2,
    titulo = NULL,
    nome_eixo_x = NULL,
    nome_eixo_y = NULL,
    escala_y_livre = FALSE,
    facetas_y_sem_zero = NULL
) {
  if (length(colunas) != 1 || !is.numeric(colunas) ||
      is.na(colunas) || colunas < 1) {
    stop("`colunas` deve ser um número inteiro maior ou igual a 1.")
  }
  colunas <- as.integer(colunas)

  dados <- dados |>
    transmute(
      .eixo_x = {{ eixo_x }},
      .eixo_y = as.numeric({{ eixo_y }}),
      .grupo = as.character({{ grupo }}),
      .facet = as.character({{ facet }})
    ) |>
    filter(
      !is.na(.eixo_x),
      !is.na(.eixo_y),
      !is.na(.grupo),
      !is.na(.facet)
    ) |>
    summarise(
      .eixo_y = sum(.eixo_y, na.rm = TRUE),
      .by = c(.eixo_x, .grupo, .facet)
    )

  if (nrow(dados) == 0) {
    stop("Não há dados válidos para gerar os gráficos.")
  }

  grupos <- sort(unique(dados$.grupo))
  facetas <- dados |>
    summarise(
      .total_facet = sum(.eixo_y, na.rm = TRUE),
      .by = .facet
    ) |>
    arrange(desc(.total_facet), .facet) |>
    pull(.facet)

  if (!is.null(facetas_y_sem_zero) && !is.character(facetas_y_sem_zero)) {
    stop("`facetas_y_sem_zero` deve ser um vetor de nomes de facetas.")
  }

  dados <- dados |>
    mutate(.grupo = factor(.grupo, levels = grupos)) |>
    arrange(.facet, .grupo, .eixo_x)

  limite_y <- if (escala_y_livre) NULL else max(dados$.eixo_y, na.rm = TRUE)

  graficos <- lapply(facetas, function(valor_facet) {
    dados_facet <- dados |>
      filter(.facet == valor_facet)

    remover_zero_eixo_y <- valor_facet %in% facetas_y_sem_zero

    grafico <- dados_facet |>
      group_by(.grupo) |>
      e_charts(.eixo_x) |>
      e_line(.eixo_y, symbol = "none") |>
      e_color(unname(paleta), background = cor_fundo) |>
      e_text_style(fontFamily = fonte_senado, color = "#333333") |>
      e_tooltip(trigger = "axis") |>
      e_title(
        text = stringr::str_wrap(valor_facet, width = 42),
        left = "center",
        textStyle = list(fontSize = 13, lineHeight = 17)
      ) |>
      e_legend(type = "scroll", bottom = 0) |>
      e_x_axis(
        name = nome_eixo_x,
        nameLocation = "middle",
        nameGap = 25,
        axisTick = list(show = FALSE),
        splitLine = list(show = FALSE)
      ) |>
      e_grid(
        left = "8%",
        right = "4%",
        top = "23%",
        bottom = "20%",
        containLabel = TRUE
      )

    if (is.null(limite_y)) {
      grafico |>
        e_y_axis(
          name = nome_eixo_y,
          scale = remover_zero_eixo_y,
          min = if (remover_zero_eixo_y) NULL else 0,
          axisTick = list(show = FALSE),
          splitLine = list(show = TRUE)
        )
    } else {
      grafico |>
        e_y_axis(
          name = nome_eixo_y,
          min = 0,
          max = limite_y,
          axisTick = list(show = FALSE),
          splitLine = list(show = TRUE)
        )
    }
  })

  paineis <- lapply(graficos, function(grafico) {
    htmltools::div(
      class = "grafico-linhas-facetas__painel",
      grafico
    )
  })

  grade <- do.call(
    htmltools::div,
    c(
      list(
        class = "grafico-linhas-facetas__grade",
        style = paste0(
          "display:grid;",
          "grid-template-columns:repeat(", colunas, ",minmax(0,1fr));",
          "grid-auto-rows:minmax(0,1fr);",
          "align-content:stretch;",
          "gap:12px;",
          "width:100%;",
          "height:100%;",
          "max-width:100%;",
          "min-width:0;",
          "min-height:0;",
          "overflow:hidden;",
          "box-sizing:border-box;"
        )
      ),
      paineis
    )
  )

  htmltools::browsable(
    htmltools::div(
      class = "grafico-linhas-facetas",
      style = paste0(
        "width:100%;",
        "height:100%;",
        "max-width:100%;",
        "min-width:0;",
        "min-height:0;",
        "overflow:hidden;",
        "box-sizing:border-box;",
        "display:grid;",
        "grid-template-rows:auto minmax(0,1fr);"
      ),
      htmltools::tags$style(htmltools::HTML(
        paste0(
          ".grafico-linhas-facetas__painel{",
          "position:relative;min-width:0;min-height:0;",
          "width:100%;height:100%;max-width:100%;max-height:100%;",
          "overflow:hidden;box-sizing:border-box;}",
          ".grafico-linhas-facetas__painel>.html-widget{",
          "position:absolute!important;inset:0;",
          "width:100%!important;height:100%!important;",
          "max-width:100%!important;max-height:100%!important;}",
          "@media(max-width:768px){",
          ".grafico-linhas-facetas__grade{",
          "grid-template-columns:minmax(0,1fr)!important;}}"
        )
      )),
      if (!is.null(titulo)) htmltools::tags$h3(titulo),
      grade
    )
  )
}
