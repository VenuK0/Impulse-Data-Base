library(shiny)
library(shinythemes)
library(ggplot2)
library(quantmod)
library(forecast)
library(plotly)
library(visNetwork)
library(deSolve)

# ==============================================================================
# UI: ТЁМНЫЙ КИБЕР-ИНТЕРФЕЙС
# ==============================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  title = "Universal R Suite & Code Inspector",
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500;600&family=Inter:wght@400;600;700;800&display=swap');
      
      body { 
        background-color: #0b0f19; 
        color: #e2e8f0; 
        font-family: 'Inter', sans-serif; 
      }
      
      /* Навбар */
      .navbar-default {
        background-color: #111827 !important;
        border: 1px solid #1f2937 !important;
        box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.5);
      }
      .navbar-default .navbar-brand { color: #6366f1 !important; font-weight: 800; letter-spacing: 1px; }
      .navbar-default .navbar-nav > li > a { color: #94a3b8 !important; font-weight: 600; }
      .navbar-default .navbar-nav > .active > a { 
        color: #ffffff !important; 
        background: linear-gradient(135deg, #4f46e5, #6366f1) !important; 
        border-radius: 8px;
      }

      /* Сайдбар */
      .well {
        background-color: #111827 !important;
        border: 1px solid #1f2937 !important;
        border-radius: 16px !important;
        box-shadow: 0 4px 20px rgba(0,0,0,0.3);
      }
      .form-control, .selectize-input {
        background-color: #1f2937 !important;
        color: #f8fafc !important;
        border: 1px solid #374151 !important;
        border-radius: 8px !important;
      }
      label { color: #cbd5e1 !important; font-weight: 600; font-size: 13px; }
      
      /* Кнопки */
      .btn-primary {
        background: linear-gradient(135deg, #4f46e5, #06b6d4) !important;
        border: none !important;
        border-radius: 10px !important;
        font-weight: 700 !important;
        letter-spacing: 0.5px;
        padding: 12px;
        box-shadow: 0 4px 15px rgba(79, 70, 229, 0.4);
        transition: all 0.2s ease-in-out;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(79, 70, 229, 0.6);
      }

      /* Карточка результатов */
      .res-card {
        background: linear-gradient(145deg, #111827, #1e293b);
        border: 1px solid #334155;
        border-radius: 16px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.4);
        position: relative;
        overflow: hidden;
      }
      .res-card::before {
        content: '';
        position: absolute;
        top: 0; left: 0; width: 5px; height: 100%;
        background: linear-gradient(to bottom, #6366f1, #06b6d4);
      }
      .res-badge {
        display: inline-block;
        padding: 4px 10px;
        background: rgba(99, 102, 241, 0.15);
        color: #818cf8;
        border: 1px solid rgba(99, 102, 241, 0.3);
        border-radius: 6px;
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        margin-bottom: 12px;
      }
      .res-content {
        font-family: 'Fira Code', monospace;
        color: #38bdf8;
        font-size: 15px;
        line-height: 1.6;
        white-space: pre-wrap;
      }

      /* Терминал Code Inspector */
      .terminal-window {
        background: #0d1117;
        border: 1px solid #30363d;
        border-radius: 14px;
        overflow: hidden;
        margin-top: 15px;
        margin-bottom: 20px;
        box-shadow: 0 12px 35px rgba(0,0,0,0.5);
      }
      .terminal-header {
        background: #161b22;
        padding: 10px 16px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        border-bottom: 1px solid #30363d;
      }
      .terminal-dots { display: flex; gap: 7px; }
      .dot { width: 11px; height: 11px; border-radius: 50%; display: inline-block; }
      .dot-red { background: #ff5f56; }
      .dot-yellow { background: #ffbd2e; }
      .dot-green { background: #27c93f; }
      .terminal-title { color: #8b949e; font-size: 12px; font-family: 'Fira Code', monospace; font-weight: 500; }
      .terminal-body { padding: 16px; font-family: 'Fira Code', monospace; font-size: 13px; color: #e6edf3; line-height: 1.5; overflow-x: auto; }
      
      .nav-tabs > li > a { color: #94a3b8 !important; border: none !important; font-weight: 600; }
      .nav-tabs > li.active > a { color: #38bdf8 !important; background: transparent !important; border-bottom: 3px solid #38bdf8 !important; }
    "))
  ),
  
  navbarPage(
    title = strong("⚡ R-CORE LAB"),
    
    # --------------------------------------------------------------------------
    # 1. МАТЕМАТИКА
    # --------------------------------------------------------------------------
    tabPanel("📐 Математика (Иерархия)",
             sidebarLayout(
               sidebarPanel(
                 h4("Параметры вычислений", style = "color: #fff; font-weight: 700;"),
                 selectInput("math_category", "Уровень сложности:",
                             choices = c(
                               "Уровень I: Школьная алгебра (База)" = "lvl1",
                               "Уровень II: Продвинутая алгебра" = "lvl2",
                               "Уровень III: Высшая математика" = "lvl3"
                             )),
                 uiOutput("sub_action_ui"),
                 hr(style = "border-color: #374151;"),
                 
                 conditionalPanel(condition = "input.math_action == 'arith'",
                                  numericInput("ar_x", "Число X:", value = 15),
                                  numericInput("ar_y", "Число Y:", value = 4)
                 ),
                 conditionalPanel(condition = "input.math_action == 'pow_sqrt'",
                                  numericInput("ps_base", "Основание (X):", value = 16),
                                  numericInput("ps_exp", "Степень (Y):", value = 2)
                 ),
                 conditionalPanel(condition = "input.math_action == 'trig'",
                                  numericInput("trig_angle", "Угол (в градусах):", value = 60)
                 ),
                 conditionalPanel(condition = "input.math_action == 'log'",
                                  numericInput("log_val", "Число X (X > 0):", value = 100, min = 0.001)
                 ),
                 conditionalPanel(condition = "input.math_action == 'comb'",
                                  numericInput("comb_n", "Всего элементов (n):", value = 7, min = 1, step = 1),
                                  numericInput("comb_k", "Выборка (k):", value = 3, min = 1, step = 1)
                 ),
                 conditionalPanel(condition = "input.math_action == 'quadratic'",
                                  p(strong("Уравнение: a*x² + b*x + c = 0")),
                                  numericInput("quad_a", "Коэффициент a:", value = 1),
                                  numericInput("quad_b", "Коэффициент b:", value = -7),
                                  numericInput("quad_c", "Коэффициент c:", value = 12)
                 ),
                 conditionalPanel(condition = "input.math_action == 'vectors'",
                                  textInput("vec_u", "Вектор U (через запятую):", value = "1, 3, 5"),
                                  textInput("vec_v", "Вектор V (через запятую):", value = "2, 4, 6")
                 ),
                 conditionalPanel(condition = "input.math_action == 'slau'",
                                  p(strong("Уравнение 1: a1*x + b1*y = c1")),
                                  fluidRow(
                                    column(4, numericInput("m_a1", "a1", value = 2)),
                                    column(4, numericInput("m_b1", "b1", value = 1)),
                                    column(4, numericInput("m_c1", "c1", value = 5))
                                  ),
                                  p(strong("Уравнение 2: a2*x + b2*y = c2")),
                                  fluidRow(
                                    column(4, numericInput("m_a2", "a2", value = 1)),
                                    column(4, numericInput("m_b2", "b2", value = 3)),
                                    column(4, numericInput("m_c2", "c2", value = 10))
                                  )
                 ),
                 conditionalPanel(condition = "input.math_action == 'derivative'",
                                  selectInput("deriv_func", "Функция для дифференцирования:",
                                              choices = c("f(x) = x^3 - 3*x^2 + 2" = "poly",
                                                          "f(x) = sin(x) * x" = "sinx",
                                                          "f(x) = exp(x) / x" = "expx")),
                                  numericInput("deriv_pt", "Точка вычисления (x0):", value = 2)
                 ),
                 conditionalPanel(condition = "input.math_action == 'integral'",
                                  numericInput("int_power", "Степень n для f(x) = x^n:", value = 2, min = 1, max = 5),
                                  numericInput("int_a", "Нижний предел (a):", value = 0),
                                  numericInput("int_b", "Верхний предел (b):", value = 3)
                 ),
                 
                 actionButton("btn_run_math", "Выполнить вычисление 🚀", class = "btn-primary w-100")
               ),
               
               mainPanel(
                 uiOutput("math_result_card"),
                 uiOutput("terminal_code_card"),
                 uiOutput("math_plot_wrapper")
               )
             )
    ),
    
    # --------------------------------------------------------------------------
    # 2. СТАТИСТИКА
    # --------------------------------------------------------------------------
    tabPanel("📊 Статистика и Данные",
             sidebarLayout(
               sidebarPanel(
                 h4("Параметры выборки", style = "color: #fff; font-weight: 700;"),
                 sliderInput("stat_n", "Размер выборки (N):", min = 50, max = 1000, value = 250, step = 50),
                 selectInput("stat_dist", "Распределение:",
                             choices = c("Нормальное (Гаусс)" = "norm", "Равномерное" = "unif", "Экспоненциальное" = "exp")),
                 numericInput("stat_param", "Среднее значение / Масштаб:", value = 50),
                 actionButton("btn_run_stats", "Анализировать выборку 📊", class = "btn-primary w-100")
               ),
               mainPanel(
                 uiOutput("stats_metric_card"),
                 fluidRow(
                   column(6, plotOutput("hist_out")),
                   column(6, plotOutput("box_out"))
                 ),
                 uiOutput("stats_terminal_card")
               )
             )
    ),
    
    # --------------------------------------------------------------------------
    # 3. ФИНАНСЫ
    # --------------------------------------------------------------------------
    tabPanel("📈 Финансы & ARIMA",
             sidebarLayout(
               sidebarPanel(
                 h4("Инструменты анализа", style = "color: #fff; font-weight: 700;"),
                 tabsetPanel(
                   id = "fin_sub",
                   tabPanel("Кредит",
                            br(),
                            numericInput("f_loan", "Сумма кредита (₽):", value = 500000),
                            numericInput("f_rate", "Ставка (% годовых):", value = 15),
                            numericInput("f_term", "Срок (месяцев):", value = 24),
                            actionButton("btn_fin_loan", "Рассчитать аннуитет", class = "btn-primary w-100")
                   ),
                   tabPanel("Инвестиции",
                            br(),
                            numericInput("f_dep", "Депозит (₽):", value = 100000),
                            numericInput("f_irate", "Доходность (%):", value = 12),
                            sliderInput("f_years", "Срок (лет):", min = 1, max = 20, value = 5),
                            actionButton("btn_fin_inv", "Сложный процент", class = "btn-primary w-100")
                   ),
                   tabPanel("Yahoo ARIMA",
                            br(),
                            selectInput("f_ticker", "Тикер актива:", choices = c("BTC-USD", "ETH-USD", "AAPL")),
                            sliderInput("f_horizon", "Прогноз (дней):", min = 7, max = 30, value = 14),
                            actionButton("btn_fin_arima", "Построить прогноз", class = "btn-primary w-100")
                   )
                 )
               ),
               mainPanel(
                 uiOutput("fin_metric_card"),
                 plotOutput("fin_plot", height = "320px"),
                 uiOutput("fin_terminal_card")
               )
             )
    ),
    
    # --------------------------------------------------------------------------
    # 4. МОЩЬ R (SHOWCASE БЕЗ КАРТ И БЕЗ БАГОВ)
    # --------------------------------------------------------------------------
    tabPanel("🚀 Мощь R (Showcase)",
             tabsetPanel(
               id = "showcase_tabs",
               
               # 1. 3D Аттрактор Лоренца
               tabPanel("🌀 3D-Хаос: Аттрактор Лоренца",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Параметры диффуравнений"),
                            sliderInput("lorenz_sigma", "Sigma (Число Прандтля):", min = 5, max = 20, value = 10, step = 0.5),
                            sliderInput("lorenz_rho", "Rho (Число Рэлея):", min = 10, max = 40, value = 28, step = 1),
                            sliderInput("lorenz_beta", "Beta (Геометрия):", min = 1, max = 5, value = 8/3, step = 0.1),
                            actionButton("btn_run_lorenz", "Пересчитать 3D-траекторию", class = "btn-primary w-100")
                          ),
                          mainPanel(
                            p(class = "text-muted", "💡 График интерактивный: вращай зажатой левой кнопкой мыши в 3D, приближай колесиком!"),
                            plotlyOutput("lorenz_plot", height = "450px"),
                            uiOutput("lorenz_terminal_card")
                          )
                        )
               ),
               
               # 2. Сетевой граф
               tabPanel("🕸️ Сетевой анализ связей (Graph)",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Настройки кластеризации"),
                            sliderInput("net_nodes", "Количество узлов сети:", min = 10, max = 60, value = 25),
                            actionButton("btn_gen_network", "Сгенерировать случайную сеть", class = "btn-primary w-100")
                          ),
                          mainPanel(
                            p(class = "text-muted", "💡 Узлы можно перетаскивать мышкой, растягивать и выделять сообщества!"),
                            visNetworkOutput("network_plot", height = "450px"),
                            uiOutput("network_terminal_card")
                          )
                        )
               ),
               
               # 3. НОВЫЙ МОДУЛЬ: ФИЗИЧЕСКИЙ СИМУЛЯТОР ОРБИТ (ВМЕСТО КАРТЫ)
               tabPanel("🪐 Гравитационный симулятор орбит (N-Body Physics)",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Гравитационные константы"),
                            sliderInput("orbit_mass", "Масса центральной звезды (M):", min = 500, max = 3000, value = 1200, step = 100),
                            sliderInput("orbit_v0", "Начальная скорость планеты (v0):", min = 1.0, max = 4.0, value = 2.4, step = 0.1),
                            sliderInput("orbit_ecc", "Радиус запуска планеты (R):", min = 10, max = 30, value = 18, step = 1),
                            actionButton("btn_run_orbit", "Запустить симуляцию 🚀", class = "btn-primary w-100")
                          ),
                          mainPanel(
                            p(class = "text-muted", "💡 Численное решение законов Кеплера и Ньютона: гравитационный танец небесных тел!"),
                            plotlyOutput("orbit_plot", height = "450px"),
                            uiOutput("orbit_terminal_card")
                          )
                        )
               )
             )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # --- МАТЕМАТИКА ---
  output$sub_action_ui <- renderUI({
    req(input$math_category)
    if (input$math_category == "lvl1") {
      choices_list <- c("1. Базовая арифметика (+, -, *, /)" = "arith",
                        "2. Степень и Квадратный корень" = "pow_sqrt",
                        "3. Тригонометрия (sin, cos, tg)" = "trig")
    } else if (input$math_category == "lvl2") {
      choices_list <- c("4. Логарифмы (ln, log10)" = "log",
                        "5. Факториал и Сочетания C(n, k)" = "comb",
                        "6. Квадратное уравнение (ax² + bx + c = 0)" = "quadratic")
    } else {
      choices_list <- c("7. Векторы: скалярное произведение" = "vectors",
                        "8. Решение СЛАУ 2x2 (Матрицы)" = "slau",
                        "9. Аналитическая производная f'(x)" = "derivative",
                        "10. Численный интеграл функции" = "integral")
    }
    selectInput("math_action", "Выберите математическое действие:", choices = choices_list)
  })
  
  math_data <- eventReactive(input$btn_run_math, {
    req(input$math_action)
    action <- input$math_action
    
    if (action == "arith") {
      x <- input$ar_x; y <- input$ar_y
      res <- sprintf("Сложение:   %s + %s = %s\nВычитание:  %s - %s = %s\nУмножение:  %s * %s = %s\nДеление:    %s / %s = %.4f",
                     x, y, x + y, x, y, x - y, x, y, x * y, x, y, x / y)
      code <- sprintf("# 1. Базовая арифметика\nx <- %s; y <- %s\nres_add <- x + y\nres_sub <- x - y\nres_mul <- x * y\nres_div <- x / y\nprint(res_add)", x, y)
      return(list(res = res, code = code, tag = "Арифметика", plot = FALSE))
      
    } else if (action == "pow_sqrt") {
      b <- input$ps_base; e <- input$ps_exp
      res <- sprintf("Возведение в степень: %s^%s = %s\nКвадратный корень:   sqrt(%s) = %.4f", b, e, b^e, b, sqrt(b))
      code <- sprintf("# 2. Степень и корень\nx <- %s; y <- %s\npow_val  <- x^y\nsqrt_val <- sqrt(x)", b, e)
      return(list(res = res, code = code, tag = "Степени и Корни", plot = FALSE))
      
    } else if (action == "trig") {
      deg <- input$trig_angle; rad <- deg * pi / 180
      res <- sprintf("Угол: %s° (%.4f rad)\nsin(%s°) = %.4f\ncos(%s°) = %.4f\ntan(%s°) = %.4f",
                     deg, rad, deg, sin(rad), deg, cos(rad), deg, tan(rad))
      code <- sprintf("# 3. Тригонометрия (перевод в радианы)\ndeg <- %s\nrad <- deg * (pi / 180)\ns_val <- sin(rad)\nc_val <- cos(rad)", deg)
      return(list(res = res, code = code, tag = "Тригонометрия", plot = FALSE))
      
    } else if (action == "log") {
      x <- input$log_val
      res <- sprintf("Натуральный логарифм ln(%s)   = %.4f\nДесятичный логарифм log10(%s) = %.4f", x, log(x), x, log10(x))
      code <- sprintf("# 4. Логарифмы в R\nx <- %s\nln_val    <- log(x)\nlog10_val <- log10(x)", x)
      return(list(res = res, code = code, tag = "Логарифмы", plot = FALSE))
      
    } else if (action == "comb") {
      n <- input$comb_n; k <- input$comb_k
      res <- sprintf("Факториал %s! = %s\nСочетания C(%s, %s) = %s", n, factorial(n), n, k, choose(n, k))
      code <- sprintf("# 5. Комбинаторика\nn <- %s; k <- %s\nfact_res <- factorial(n)\ncomb_res <- choose(n, k)", n, k)
      return(list(res = res, code = code, tag = "Комбинаторика", plot = FALSE))
      
    } else if (action == "quadratic") {
      a <- input$quad_a; b <- input$quad_b; c <- input$quad_c
      D <- b^2 - 4*a*c
      if (D > 0) {
        x1 <- (-b + sqrt(D)) / (2*a); x2 <- (-b - sqrt(D)) / (2*a)
        res <- sprintf("Дискриминант D = %.2f\nКорень x1 = %.4f\nКорень x2 = %.4f", D, x1, x2)
      } else if (D == 0) {
        res <- sprintf("Дискриминант D = 0\nЕдинственный корень x = %.4f", -b / (2*a))
      } else {
        res <- sprintf("Дискриминант D = %.2f < 0. Действительных корней нет.", D)
      }
      code <- sprintf("# 6. Квадратное уравнение ax^2 + bx + c = 0\na <- %s; b <- %s; c <- %s\nD <- b^2 - 4 * a * c\nif (D >= 0) {\n  x1 <- (-b + sqrt(D)) / (2 * a)\n  x2 <- (-b - sqrt(D)) / (2 * a)\n}", a, b, c)
      return(list(res = res, code = code, tag = "Квадратные Уравнения", plot = FALSE))
      
    } else if (action == "vectors") {
      u <- as.numeric(unlist(strsplit(input$vec_u, ",")))
      v <- as.numeric(unlist(strsplit(input$vec_v, ",")))
      if (length(u) != length(v)) {
        res <- "Ошибка: Размерности векторов U и V не совпадают!"
        code <- "# Ошибка размерностей"
      } else {
        dot <- sum(u * v)
        res <- sprintf("Скалярное произведение U · V = %.2f\nДлина |U| = %.3f | Длина |V| = %.3f\nКосинус угла cos(θ) = %.4f",
                       dot, sqrt(sum(u^2)), sqrt(sum(v^2)), dot / (sqrt(sum(u^2)) * sqrt(sum(v^2))))
        code <- sprintf("# 7. Скалярное произведение и нормы\nu <- c(%s)\nv <- c(%s)\ndot_prod <- as.numeric(u %%*%% v)\nlen_u <- sqrt(sum(u^2))", paste(u, collapse=", "), paste(v, collapse=", "))
      }
      return(list(res = res, code = code, tag = "Векторный анализ", plot = FALSE))
      
    } else if (action == "slau") {
      A <- matrix(c(input$m_a1, input$m_b1, input$m_a2, input$m_b2), nrow = 2, byrow = TRUE)
      B <- c(input$m_c1, input$m_c2)
      det_A <- det(A)
      if (abs(det_A) < 1e-9) {
        res <- "Определитель det(A) = 0! Система не имеет единственного решения."
      } else {
        X <- solve(A, B)
        res <- sprintf("Решение СЛАУ 2x2:\n  x = %.4f\n  y = %.4f\n(Определитель det(A) = %.4f)", X[1], X[2], det_A)
      }
      code <- sprintf("# 8. Матричный солвер СЛАУ A * X = B\nA <- matrix(c(%s, %s, %s, %s), nrow = 2, byrow = TRUE)\nB <- c(%s, %s)\nX <- solve(A, B)", input$m_a1, input$m_b1, input$m_a2, input$m_b2, input$m_c1, input$m_c2)
      return(list(res = res, code = code, tag = "Линейная Алгебра", plot = FALSE))
      
    } else if (action == "derivative") {
      x0 <- input$deriv_pt
      fn_str <- switch(input$deriv_func,
                       poly = "expression(x^3 - 3*x^2 + 2)",
                       sinx = "expression(sin(x) * x)",
                       expx = "expression(exp(x) / x)")
      expr <- eval(parse(text = fn_str))
      d_expr <- D(expr, "x")
      x <- x0
      val <- eval(d_expr)
      res <- sprintf("Функция: %s\nПроизводная f'(x) = %s\nЗначение в x0 = %s: f'(%s) = %.4f", deparse(expr), deparse(d_expr), x0, x0, val)
      code <- sprintf("# 9. Аналитическая производная\nfx <- %s\ndf <- D(fx, 'x')\nx <- %s\nres_val <- eval(df)", fn_str, x0)
      return(list(res = res, code = code, tag = "Дифференцирование", plot = FALSE))
      
    } else if (action == "integral") {
      p <- input$int_power; a <- input$int_a; b <- input$int_b
      f <- function(x) x^p
      int_res <- integrate(f, lower = a, upper = b)
      res <- sprintf("Интеграл от f(x) = x^%s на [%s, %s]\nЗначение интеграла: %.4f\nТочность (abs error): %e", p, a, b, int_res$value, int_res$abs.error)
      code <- sprintf("# 10. Квадратурное численное интегрирование\nf <- function(x) x^%s\nres <- integrate(f, lower = %s, upper = %s)\nprint(res$value)", p, a, b)
      return(list(res = res, code = code, tag = "Интегральное исчисление", plot = TRUE, p = p, a = a, b = b))
    }
  }, ignoreNULL = FALSE)
  
  output$math_result_card <- renderUI({
    d <- math_data()
    tags$div(class = "res-card",
             tags$span(class = "res-badge", if (!is.null(d)) d$tag else "Готов к работе"),
             tags$div(class = "res-content", if (!is.null(d)) d$res else "Выберите действие и нажмите кнопку запуска")
    )
  })
  
  output$terminal_code_card <- renderUI({
    d <- math_data()
    code_text <- if (!is.null(d)) d$code else "# Здесь появится сгенерированный R-скрипт"
    tags$div(class = "terminal-window",
             tags$div(class = "terminal-header",
                      tags$div(class = "terminal-dots", tags$span(class = "dot dot-red"), tags$span(class = "dot dot-yellow"), tags$span(class = "dot dot-green")),
                      tags$span(class = "terminal-title", "engine_math.R — Code Inspector"),
                      tags$span(style = "color: #58a6ff; font-size: 11px; font-weight: bold;", "R 4.4")
             ),
             tags$div(class = "terminal-body", tags$pre(style = "background:transparent; color:inherit; border:none; padding:0; margin:0;", code_text))
    )
  })
  
  output$math_plot_wrapper <- renderUI({
    calc <- math_data()
    if (!is.null(calc) && !is.null(calc$plot) && calc$plot) {
      plotOutput("math_plot_area", height = "300px")
    } else {
      NULL
    }
  })
  
  output$math_plot_area <- renderPlot({
    calc <- math_data()
    if (!is.null(calc) && !is.null(calc$plot) && calc$plot) {
      p <- calc$p; a <- calc$a; b <- calc$b
      xs <- seq(min(a, b) - 1, max(a, b) + 1, length.out = 200)
      df <- data.frame(x = xs, y = xs^p)
      df_area <- subset(df, x >= min(a, b) & x <= max(a, b))
      ggplot(df, aes(x = x, y = y)) +
        geom_line(color = "#06b6d4", size = 1.3) +
        geom_area(data = df_area, aes(x = x, y = y), fill = "#6366f1", alpha = 0.45) +
        theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "#111827", color = NA),
          panel.background = element_rect(fill = "#111827", color = NA),
          text = element_text(color = "#cbd5e1"),
          axis.text = element_text(color = "#94a3b8"),
          panel.grid.major = element_line(color = "#1f2937"),
          panel.grid.minor = element_blank()
        ) +
        labs(title = paste0("Площадь под кривой f(x) = x^", p), x = "X", y = "Y")
    }
  })
  
  # --- СТАТИСТИКА ---
  stat_data <- eventReactive(input$btn_run_stats, {
    n <- input$stat_n; d <- input$stat_dist; p <- input$stat_param
    if (d == "norm") vals <- rnorm(n, mean = p, sd = p * 0.25)
    else if (d == "unif") vals <- runif(n, min = 0, max = p * 2)
    else vals <- rexp(n, rate = 1 / p)
    code <- sprintf("# Генерация выборки и расчет описательной статистики\ndata <- %s(%d, ...)\nmean_val <- mean(data)\nmed_val  <- median(data)\nsd_val   <- sd(data)\nhist(data, col = '#06b6d4')\nboxplot(data, col = '#6366f1')",
                    ifelse(d=="norm","rnorm",ifelse(d=="unif","runif","rexp")), n)
    list(vals = vals, code = code)
  }, ignoreNULL = FALSE)
  
  output$stats_metric_card <- renderUI({
    v <- stat_data()$vals
    tags$div(class = "res-card",
             tags$span(class = "res-badge", "Дескриптивная статистика"),
             tags$div(class = "res-content", sprintf("N = %d | Среднее: %.3f | Медиана: %.3f | SD: %.3f | IQR: %.3f", length(v), mean(v), median(v), sd(v), IQR(v)))
    )
  })
  output$hist_out <- renderPlot({
    ggplot(data.frame(x = stat_data()$vals), aes(x = x)) +
      geom_histogram(fill = "#06b6d4", color = "#111827", bins = 20) + theme_minimal() +
      theme(plot.background = element_rect(fill = "#111827", color = NA), text = element_text(color = "#cbd5e1"), axis.text = element_text(color = "#94a3b8"), panel.grid = element_line(color = "#1f2937"))
  })
  output$box_out <- renderPlot({
    ggplot(data.frame(x = stat_data()$vals), aes(y = x)) +
      geom_boxplot(fill = "#6366f1", color = "#e2e8f0", alpha = 0.8) + theme_minimal() +
      theme(plot.background = element_rect(fill = "#111827", color = NA), text = element_text(color = "#cbd5e1"), axis.text = element_text(color = "#94a3b8"), panel.grid = element_line(color = "#1f2937"))
  })
  output$stats_terminal_card <- renderUI({
    tags$div(class = "terminal-window",
             tags$div(class = "terminal-header", tags$div(class = "terminal-dots", tags$span(class = "dot dot-red"), tags$span(class = "dot dot-yellow"), tags$span(class = "dot dot-green")), tags$span(class = "terminal-title", "stats_pipeline.R — Code Inspector")),
             tags$div(class = "terminal-body", tags$pre(style = "background:transparent; color:inherit; border:none;", stat_data()$code))
    )
  })
  
  # --- ФИНАНСЫ ---
  fin_vals <- reactiveValues(mode = "loan")
  observeEvent(input$btn_fin_loan, { fin_vals$mode <- "loan" })
  observeEvent(input$btn_fin_inv, { fin_vals$mode <- "inv" })
  observeEvent(input$btn_fin_arima, { fin_vals$mode <- "arima" })
  
  output$fin_metric_card <- renderUI({
    if (fin_vals$mode == "loan") {
      P <- input$f_loan; r <- (input$f_rate / 100) / 12; m <- input$f_term
      pay <- P * (r * (1 + r)^m) / ((1 + r)^m - 1)
      txt <- sprintf("Аннуитет: %.2f ₽/мес\nИтого выплат: %.2f ₽\nПереплата: %.2f ₽ (%.1f%%)", pay, pay * m, pay * m - P, ((pay * m - P)/P)*100)
      tag <- "Кредитный калькулятор"
    } else if (fin_vals$mode == "inv") {
      P <- input$f_dep; r <- input$f_irate / 100; y <- input$f_years
      tot <- P * (1 + r)^y
      txt <- sprintf("Итоговый депозит (%d лет): %.2f ₽\nЧистая прибыль: %.2f ₽", y, tot, tot - P)
      tag <- "Инвестиционный рост"
    } else {
      txt <- sprintf("ARIMA авто-модель для: %s (Горизонт: %d дней)", input$f_ticker, input$f_horizon)
      tag <- "Временные ряды Yahoo Finance"
    }
    tags$div(class = "res-card", tags$span(class = "res-badge", tag), tags$div(class = "res-content", txt))
  })
  
  output$fin_plot <- renderPlot({
    if (fin_vals$mode == "loan") {
      P <- input$f_loan; r <- (input$f_rate / 100) / 12; m <- input$f_term
      pay <- P * (r * (1 + r)^m) / ((1 + r)^m - 1)
      bal <- numeric(m + 1); bal[1] <- P
      for (i in 1:m) bal[i+1] <- max(0, bal[i] - (pay - bal[i] * r))
      ggplot(data.frame(m = 0:m, b = bal), aes(x = m, y = b)) +
        geom_line(color = "#f43f5e", size = 1.3) + geom_area(fill = "#f43f5e", alpha = 0.3) +
        theme_minimal() + theme(plot.background = element_rect(fill = "#111827", color = NA), text = element_text(color = "#cbd5e1"), axis.text = element_text(color = "#94a3b8"), panel.grid = element_line(color = "#1f2937"))
    } else if (fin_vals$mode == "inv") {
      P <- input$f_dep; r <- input$f_irate / 100; y <- input$f_years
      df <- data.frame(Year = 0:y, Cap = P * (1 + r)^(0:y))
      ggplot(df, aes(x = Year, y = Cap)) + geom_bar(stat = "identity", fill = "#10b981", alpha = 0.85) +
        theme_minimal() + theme(plot.background = element_rect(fill = "#111827", color = NA), text = element_text(color = "#cbd5e1"), axis.text = element_text(color = "#94a3b8"), panel.grid = element_line(color = "#1f2937"))
    } else {
      env <- new.env()
      tryCatch({
        getSymbols(input$f_ticker, src = "yahoo", from = Sys.Date() - 180, to = Sys.Date(), env = env)
        ts_data <- ts(as.numeric(na.omit(Cl(env[[input$f_ticker]]))), frequency = 7)
        plot(forecast(auto.arima(ts_data), h = input$f_horizon), main = paste("ARIMA Прогноз:", input$f_ticker), col = "#06b6d4", fcol = "#6366f1", lwd = 2)
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, "Ошибка соединения с Yahoo Finance", col = "red", cex = 1.3)
      })
    }
  })
  
  output$fin_terminal_card <- renderUI({
    code_txt <- if (fin_vals$mode == "loan") {
      "r <- (rate / 100) / 12\nmonthly_payment <- P * (r * (1 + r)^m) / ((1 + r)^m - 1)"
    } else if (fin_vals$mode == "inv") {
      "total_capital <- initial_deposit * (1 + rate)^years"
    } else {
      "library(forecast); library(quantmod)\nfit <- auto.arima(prices)\nfc <- forecast(fit, h = 14)\nplot(fc)"
    }
    tags$div(class = "terminal-window",
             tags$div(class = "terminal-header", tags$div(class = "terminal-dots", tags$span(class = "dot dot-red"), tags$span(class = "dot dot-yellow"), tags$span(class = "dot dot-green")), tags$span(class = "terminal-title", "finance_quant.R — Code Inspector")),
             tags$div(class = "terminal-body", tags$pre(style = "background:transparent; color:inherit; border:none;", code_txt))
    )
  })
  
  # ============================================================================
  # СУПЕРСИЛА R: 3 РАБОЧИХ МОДУЛЯ (БЕЗ ВНЕШНИХ API И БЕЗ БАГОВ)
  # ============================================================================
  
  # 1. 3D Аттрактор Лоренца
  output$lorenz_plot <- renderPlotly({
    input$btn_run_lorenz
    
    sigma <- isolate(input$lorenz_sigma)
    rho   <- isolate(input$lorenz_rho)
    beta  <- isolate(input$lorenz_beta)
    
    lorenz_eq <- function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
        dx <- sigma * (y - x)
        dy <- x * (rho - z) - y
        dz <- x * y - beta * z
        list(c(dx, dy, dz))
      })
    }
    
    parameters <- c(sigma = sigma, rho = rho, beta = beta)
    state      <- c(x = 1, y = 1, z = 1)
    times      <- seq(0, 35, by = 0.015)
    
    out <- as.data.frame(ode(y = state, times = times, func = lorenz_eq, parms = parameters))
    
    plot_ly(out, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
            line = list(width = 3, color = ~time, colorscale = 'Viridis')) %>%
      layout(
        paper_bgcolor = '#111827',
        plot_bgcolor  = '#111827',
        scene = list(
          xaxis = list(color = '#94a3b8', gridcolor = '#1f2937'),
          yaxis = list(color = '#94a3b8', gridcolor = '#1f2937'),
          zaxis = list(color = '#94a3b8', gridcolor = '#1f2937')
        )
      )
  })
  
  output$lorenz_terminal_card <- renderUI({
    code_text <- "# Решение дифференциальной системы Лоренца\nlibrary(deSolve); library(plotly)\nlorenz <- function(t, state, parms) {\n  with(as.list(c(state, parms)), {\n    dx <- sigma * (y - x)\n    dy <- x * (rho - z) - y\n    dz <- x * y - beta * z\n    list(c(dx, dy, dz))\n  })\n}\ntrajectory <- ode(y = c(x=1, y=1, z=1), times = seq(0, 35, 0.015), func = lorenz, parms = params)\nplot_ly(trajectory, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines')"
    tags$div(class = "terminal-window",
             tags$div(class = "terminal-header",
                      tags$div(class = "terminal-dots", tags$span(class = "dot dot-red"), tags$span(class = "dot dot-yellow"), tags$span(class = "dot dot-green")),
                      tags$span(class = "terminal-title", "lorenz_ode_solver.R — Code Inspector"),
                      tags$span(style = "color: #58a6ff; font-size: 11px; font-weight: bold;", "deSolve + plotly")
             ),
             tags$div(class = "terminal-body", tags$pre(style = "background:transparent; color:inherit; border:none;", code_text))
    )
  })
  
  # 2. Сетевой граф
  output$network_plot <- renderVisNetwork({
    input$btn_gen_network
    
    n <- isolate(input$net_nodes)
    nodes <- data.frame(
      id = 1:n,
      label = paste("Node", 1:n),
      group = sample(c("Cluster A", "Cluster B", "Cluster C"), n, replace = TRUE),
      value = sample(10:35, n, replace = TRUE)
    )
    
    m <- round(n * 1.5)
    edges <- data.frame(
      from = sample(1:n, m, replace = TRUE),
      to   = sample(1:n, m, replace = TRUE)
    )
    edges <- edges[edges$from != edges$to, ]
    
    visNetwork(nodes, edges) %>%
      visGroups(groupname = "Cluster A", color = "#06b6d4") %>%
      visGroups(groupname = "Cluster B", color = "#6366f1") %>%
      visGroups(groupname = "Cluster C", color = "#ec4899") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE)
  })
  
  output$network_terminal_card <- renderUI({
    code_text <- "# Интерактивный сетевой граф связей\nlibrary(visNetwork)\nnodes <- data.frame(id = 1:N, label = paste('Node', 1:N), group = clusters, value = centrality)\nedges <- data.frame(from = source_nodes, to = target_nodes)\nvisNetwork(nodes, edges) %>%\n  visGroups(groupname = 'Cluster A', color = '#06b6d4') %>%\n  visPhysics(stabilization = FALSE) %>%\n  visOptions(highlightNearest = TRUE)"
    tags$div(class = "terminal-window",
             tags$div(class = "terminal-header",
                      tags$div(class = "terminal-dots", tags$span(class = "dot dot-red"), tags$span(class = "dot dot-yellow"), tags$span(class = "dot dot-green")),
                      tags$span(class = "terminal-title", "network_graph_clustering.R — Code Inspector"),
                      tags$span(style = "color: #58a6ff; font-size: 11px; font-weight: bold;", "visNetwork")
             ),
             tags$div(class = "terminal-body", tags$pre(style = "background:transparent; color:inherit; border:none;", code_text))
    )
  })
  
  # 3. НОВЫЙ МОДУЛЬ: ГРАВИТАЦИОННЫЙ СИМУЛЯТОР ОРБИТ (100% ЛОКАЛЬНО В PLOTLY)
  output$orbit_plot <- renderPlotly({
    input$btn_run_orbit
    
    GM <- isolate(input$orbit_mass)
    v0 <- isolate(input$orbit_v0)
    R0 <- isolate(input$orbit_ecc)
    
    # Решаем систему диффуравнений гравитации Ньютона d^2r/dt^2 = -GM * r / |r|^3
    gravity_eq <- function(t, state, parms) {
      x <- state[1]; y <- state[2]
      vx <- state[3]; vy <- state[4]
      r3 <- (x^2 + y^2)^(1.5)
      ax <- -GM * x / r3
      ay <- -GM * y / r3
      list(c(vx, vy, ax, ay))
    }
    
    # 2 планеты: одна с ползунка, вторая с фиксированной орбитой
    init_state1 <- c(x = R0, y = 0, vx = 0, vy = v0)
    times <- seq(0, 100, by = 0.1)
    orbit1 <- as.data.frame(ode(y = init_state1, times = times, func = gravity_eq, parms = NULL))
    
    init_state2 <- c(x = -R0 * 0.65, y = 0, vx = 0, vy = -v0 * 1.25)
    orbit2 <- as.data.frame(ode(y = init_state2, times = times, func = gravity_eq, parms = NULL))
    
    plot_ly() %>%
      # Траектория планеты Альфа
      add_trace(data = orbit1, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
                line = list(color = '#06b6d4', width = 2.5), name = 'Орбита Альфа') %>%
      # Траектория планеты Бета
      add_trace(data = orbit2, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
                line = list(color = '#ec4899', width = 2, dash = 'dash'), name = 'Орбита Бета') %>%
      # Центральная звезда
      add_trace(x = c(0), y = c(0), type = 'scatter', mode = 'markers',
                marker = list(color = '#f59e0b', size = 22, symbol = 'circle',
                              line = list(color = '#fde68a', width = 3)), name = 'Центральная Звезда (M)') %>%
      layout(
        paper_bgcolor = '#111827',
        plot_bgcolor  = '#111827',
        xaxis = list(color = '#94a3b8', gridcolor = '#1f2937', zerolinecolor = '#374151', title = 'Координата X (AU)'),
        yaxis = list(color = '#94a3b8', gridcolor = '#1f2937', zerolinecolor = '#374151', title = 'Координата Y (AU)'),
        legend = list(font = list(color = '#e2e8f0'))
      )
  })
  
  output$orbit_terminal_card <- renderUI({
    code_text <- "# Симуляция гравитационной орбиты Ньютона (N-Body Orbit)\nlibrary(deSolve); library(plotly)\n\ngravity_ode <- function(t, state, parms) {\n  x <- state['x']; y <- state['y']; r3 <- (x^2 + y^2)^(1.5)\n  ax <- -GM * x / r3; ay <- -GM * y / r3  # Ускорение свободного падения\n  list(c(state['vx'], state['vy'], ax, ay))\n}\n\n# Численный расчёт координат траектории через ode()\nstate0 <- c(x = R0, y = 0, vx = 0, vy = v0)\ntrajectory <- ode(y = state0, times = seq(0, 100, 0.1), func = gravity_ode, parms = NULL)\nplot_ly(trajectory, x = ~x, y = ~y, type = 'scatter', mode = 'lines')"
    
    tags$div(class = "terminal-window",
             tags$div(class = "terminal-header",
                      tags$div(class = "terminal-dots", tags$span(class = "dot dot-red"), tags$span(class = "dot dot-yellow"), tags$span(class = "dot dot-green")),
                      tags$span(class = "terminal-title", "nbody_gravity_kepler.R — Code Inspector"),
                      tags$span(style = "color: #58a6ff; font-size: 11px; font-weight: bold;", "Physics Engine (deSolve)")
             ),
             tags$div(class = "terminal-body", tags$pre(style = "background:transparent; color:inherit; border:none;", code_text))
    )
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)