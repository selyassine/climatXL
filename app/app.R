# climatXL : application de tarification d'une tranche XL catastrophe.
# Le visiteur choisit la tranche et le scénario climatique ; la prime pure,
# la VaR à 99,5 % et les périodes de retour se mettent à jour.
# Les paramètres viennent de `parametres.rds` (voir climatXL::model_parameters).

library(shiny)
library(climatXL)

mp <- readRDS("parametres.rds")
u <- mp$threshold
temp_obs <- mp$temp$observed
temp_at <- function(year) mp$temp$intercept + mp$temp$slope * year
noms <- c("2005-08-28" = "Katrina", "2017-08-25" = "Harvey", "2012-10-26" = "Sandy",
          "2008-09-12" = "Ike", "2017-09-20" = "Maria", "2018-11-08" = "Camp Fire",
          "2005-10-23" = "Wilma", "2004-08-13" = "Charley", "2022-09-27" = "Ian",
          "2020-08-26" = "Laura")

fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ",")
pct <- function(x) paste0(ifelse(x > 0, "+", ""), fmt(100 * x, 0), " %")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: system-ui, sans-serif; color: #1d2733; }
    .kpi { background: #f3f6fa; border-radius: 8px; padding: 10px 14px; margin-bottom: 12px; }
    .kpi .v { font-size: 1.6em; font-weight: 600; color: #1f5fa8; }
    .kpi .l { font-size: .85em; color: #5a6675; }
    .note { font-size: .85em; color: #5a6675; }"))),
  titlePanel("climatXL : le climat rend-il la réassurance catastrophe plus chère ?"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Tranche (perte de marché, Md$ 2024)"),
      numericInput("att", "Priorité", 10, min = u, max = 200, step = 1),
      numericInput("lim", "Portée", 5, min = 0.5, max = 200, step = 0.5),
      h4("Scénario climatique"),
      radioButtons("scen", NULL, c("Stationnaire (passé = avenir)" = "stat",
                                   "Climat 2024" = "y2024",
                                   "Réchauffement 2035" = "y2035",
                                   "Anomalie au choix" = "custom")),
      conditionalPanel("input.scen == 'custom'",
        sliderInput("temp", "Anomalie de température (°C)", 0, 2.5, 1.5, step = 0.05)),
      p(class = "note", paste0("Le scénario 2035 prolonge la tendance de température 1996-2024 (",
        fmt(10 * mp$temp$slope, 2), " °C par décennie)."))
    ),
    mainPanel(width = 9,
      fluidRow(
        column(3, div(class = "kpi", div(class = "v", textOutput("prime")), div(class = "l", "Prime pure annuelle"))),
        column(3, div(class = "kpi", div(class = "v", textOutput("charg")), div(class = "l", textOutput("charg_ci")))),
        column(3, div(class = "kpi", div(class = "v", textOutput("var")), div(class = "l", "VaR 99,5 % de la charge annuelle"))),
        column(3, div(class = "kpi", div(class = "v", textOutput("hit")), div(class = "l", "Probabilité d'être touchée dans l'année")))
      ),
      fluidRow(
        column(7, plotOutput("rl", height = 360)),
        column(5, plotOutput("boot", height = 360))
      ),
      h4("Grands sinistres 1996-2024 : période de retour selon le scénario"),
      tableOutput("events"),
      p(class = "note",
        "Données NOAA Storm Events 1996-2024, regroupées par clause des 72 heures et normalisées par le PIB nominal.",
        "Aucun effet climatique n'est significatif à 5 % : les intervalles bootstrap montrent que l'incertitude",
        "dépasse l'effet estimé. Sans limite annuelle ni reconstitution. Code : ",
        a("github.com/selyassine/climatXL", href = "https://github.com/selyassine/climatXL"))
    )
  )
)

server <- function(input, output, session) {
  temp <- reactive(switch(input$scen, stat = NULL,
                          y2024 = temp_at(2024), y2035 = temp_at(2035), custom = input$temp))
  layer <- reactive({
    validate(need(input$att >= u, paste("La priorité doit être au moins", u, "Md$.")),
             need(input$lim > 0, "La portée doit être positive."))
    c(input$att, input$lim)
  })
  prem <- function(p, L) layer_premium(p[["xi"]], p[["beta"]], p[["lambda"]], u, L[1], L[2])
  point <- reactive(scenario_parameters(mp$point, temp()))
  premium <- reactive(prem(point(), layer()))
  premium_stat <- reactive(prem(scenario_parameters(mp$point), layer()))
  boot_load <- reactive({
    L <- layer(); t <- temp()
    vapply(seq_len(nrow(mp$boot)), function(i) {
      b <- mp$boot[i, ]
      prem(scenario_parameters(b, t), L) / prem(scenario_parameters(b), L) - 1
    }, numeric(1))
  })
  sims <- reactive({
    p <- point(); L <- layer()
    simulate_layer(p[["xi"]], p[["beta"]], p[["lambda"]], u, L[1], L[2], n_sim = 50000, seed = 1)
  })

  output$prime <- renderText(paste(fmt(1000 * premium()), "M$"))
  output$charg <- renderText(if (is.null(temp())) "référence" else pct(premium() / premium_stat() - 1))
  output$charg_ci <- renderText({
    if (is.null(temp())) return("Chargement climatique")
    q <- stats::quantile(boot_load(), c(.025, .975), na.rm = TRUE)
    paste0("Chargement climatique, IC 95 % [", pct(q[1]), " ; ", pct(q[2]), "]")
  })
  output$var <- renderText(paste(fmt(1000 * layer_risk(sims())$var), "M$"))
  output$hit <- renderText(paste(fmt(100 * mean(sims() > 0), 0), "%"))

  output$rl <- renderPlot({
    per <- exp(seq(log(1.05 / mp$point$lambda_s), log(500), length = 100))
    ps <- scenario_parameters(mp$point); pc <- point(); L <- layer()
    zs <- return_level_at(per, ps[["xi"]], ps[["beta"]], ps[["lambda"]], u)
    zc <- return_level_at(per, pc[["xi"]], pc[["beta"]], pc[["lambda"]], u)
    par(mar = c(4.2, 4.5, 2.5, 1))
    plot(per, zs, log = "xy", type = "l", lwd = 2, col = "#1f5fa8", ylim = range(c(zs, zc, L[1] + L[2])),
         xlab = "Période de retour (années)", ylab = "Sinistre (Md$ 2024)", main = "Niveaux de retour", yaxt = "n")
    axis(2, at = c(0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000), labels = c("0,5", 1, 2, 5, 10, 20, 50, 100, 200, 500, "1 000", "2 000"), las = 1)
    rect(1e-3, L[1], 1e4, L[1] + L[2], col = adjustcolor("#f0b429", .25), border = NA)
    if (!is.null(temp())) lines(per, zc, lwd = 2, col = "#c0392b", lty = 2)
    legend("topleft", c("Stationnaire", if (!is.null(temp())) "Scénario choisi", "Tranche"),
           col = c("#1f5fa8", if (!is.null(temp())) "#c0392b", adjustcolor("#f0b429", .5)),
           lty = c(1, if (!is.null(temp())) 2, 1), lwd = c(2, if (!is.null(temp())) 2, 8), bty = "n")
  })
  output$boot <- renderPlot({
    par(mar = c(4.2, 4.5, 2.5, 1))
    if (is.null(temp())) {
      plot.new(); text(.5, .5, "Choisissez un scénario climatique\npour voir l'incertitude du chargement.", col = "#5a6675")
      return(invisible())
    }
    x <- 100 * boot_load(); x <- x[is.finite(x)]
    hist(pmin(x, 400), breaks = 40, col = "#9bb8dc", border = "white", main = "Chargement : 300 bootstraps",
         xlab = "Chargement climatique (%)", ylab = "Fréquence")
    abline(v = 0, lty = 2); abline(v = 100 * (premium() / premium_stat() - 1), col = "#c0392b", lwd = 2)
  })
  output$events <- renderTable({
    e <- mp$largest; ps <- scenario_parameters(mp$point); pc <- point()
    data.frame(
      Date = format(as.Date(e$date), "%d/%m/%Y"),
      Nom = unname(noms[format(as.Date(e$date))]),
      Type = e$main_type,
      `Sinistre (Md$ 2024)` = fmt(e$loss, 1),
      `Retour, stationnaire (ans)` = fmt(return_period(e$loss, ps[["xi"]], ps[["beta"]], ps[["lambda"]], u), 0),
      `Retour, scénario (ans)` = fmt(return_period(e$loss, pc[["xi"]], pc[["beta"]], pc[["lambda"]], u), 0),
      check.names = FALSE)
  }, striped = TRUE, spacing = "s")
}

shinyApp(ui, server)
