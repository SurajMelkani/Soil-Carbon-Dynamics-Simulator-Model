# install.packages(c("shiny","ggplot2","plotly","dplyr","tidyr","SoilR"))

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(SoilR)

MONTHS <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# ---------- Helper: monthly inputs UI (6 months per row) ----------
month_inputs_numeric <- function(prefix, values, step = 0.1, minv = NULL, maxv = NULL) {
  tagList(
    fluidRow(lapply(1:6, function(i) {
      column(2, numericInput(
        inputId = sprintf("%s%02d", prefix, i),
        label   = MONTHS[i],
        value   = values[i],
        step    = step,
        min     = minv,
        max     = maxv
      ))
    })),
    fluidRow(lapply(7:12, function(i) {
      column(2, numericInput(
        inputId = sprintf("%s%02d", prefix, i),
        label   = MONTHS[i],
        value   = values[i],
        step    = step,
        min     = minv,
        max     = maxv
      ))
    }))
  )
}

month_inputs_cover <- function(prefix, default = rep(1, 12)) {
  tagList(
    fluidRow(lapply(1:6, function(i) {
      column(2, selectInput(
        inputId  = sprintf("%s%02d", prefix, i),
        label    = MONTHS[i],
        choices  = c("Covered" = 1, "Bare" = 0),
        selected = as.character(default[i])
      ))
    })),
    fluidRow(lapply(7:12, function(i) {
      column(2, selectInput(
        inputId  = sprintf("%s%02d", prefix, i),
        label    = MONTHS[i],
        choices  = c("Covered" = 1, "Bare" = 0),
        selected = as.character(default[i])
      ))
    }))
  )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Soil Carbon Dynamics: A Multi-Model Comparison Platform"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Quick Guidance"),
      tags$ul(
        tags$li("Enter monthly climatology (Temperature, Precipitaion, Potential Evapotranpiration (PET)) and monthly cover (Covered/Bare)."),
        tags$li("Set clay (%), annual carbon (C) input (t C/ha/yr), years, and initial total soil organic C (SOC) (t C/ha).")
      ),
      hr(),
      
      h3("Monthly Climatology (Averages)"),
      
      h4("Monthly Temperature (°C)"),
      month_inputs_numeric("t", values = c(20,21,23,25,27,28,29,29,28,26,23,21), step = 0.1, minv = -10, maxv = 50),
      
      hr(),
      h4("Monthly Precipitation (mm)"),
      month_inputs_numeric("p", values = c(50,60,80,90,120,200,220,240,200,120,70,60), step = 1, minv = 0, maxv = 1200),
      
      hr(),
      h4("Monthly PET (mm)"),
      month_inputs_numeric("pet", values = c(60,70,90,110,130,150,160,150,130,110,80,70), step = 1, minv = 0, maxv = 600),
      
      hr(),
      h4("Soil Cover (monthly)"),
      month_inputs_cover("cov", default = rep(1, 12)),
      
      hr(),
      h3("Soil & Carbon Inputs"),
      numericInput("clay",    "Clay Content (%):",                value = 10,  min = 0, max = 100, step = 1),
      numericInput("c_input", "Annual Carbon Input (t C/ha/yr):", value = 4.0, min = 0, max = 30,  step = 0.1),
      numericInput("years",   "Simulation Years:",                value = 50,  min = 1, max = 150, step = 1),
      
      hr(),
      p(strong("Initial SOC (t C/ha):"), "Same total SOC used to initialize every model."),
      numericInput("init_soc", "Initial Total SOC (t C/ha):", value = 40, min = 1, max = 600, step = 1),
      
      hr(),
      actionButton("run_models", "Execute Models", class = "btn-primary", width = "100%"),
      
      hr(),
      tags$small(
        "Important note: This app uses an annual time axis for all models. ",
        "Monthly climate is summarized into a simple scalar modifier for RothC here (temp × moisture proxy). ",
        "CenturyModel in SoilR does not use climate directly in its standard signature (still included for consistent inputs)."
      )
    ),
    
    mainPanel(
      h3("Models Included (9)"),
      
      tags$div(
        style = "padding: 12px; border: 1px solid #ddd; border-radius: 10px; margin-bottom: 12px;",
        tags$p(
          style = "margin-bottom: 6px;",
          strong("Goal: "),
          "Compare how different soil carbon model structures divide soil organic carbon into pools and predict long-term soil organic carbon under the same starting soil organic carbon and annual carbon input."
        ),
        tags$ul(
          tags$li(
            strong("Two-pool models (Series / Parallel / Feedback): "),
            "two compartments representing a faster and a slower soil organic carbon fraction."
          ),
          tags$li(
            strong("Three-pool models (Series / Parallel / Feedback): "),
            "three compartments representing fast, intermediate, and very stable soil organic carbon fractions."
          ),
          tags$li(
            strong("Process-based models: "),
            "Rothamsted Carbon Model (RothC) (five pools), Century Model (seven pools), and Introductory Carbon Balance Model (ICBM) (two pools). These have named pools intended for interpretation."
          )
        ),
        
        tags$details(
          tags$summary(strong("Click for model details, assumptions, and how to interpret pools")),
          
          tags$h4("A) Generic two-pool and three-pool architectures (conceptual)"),
          tags$ul(
            tags$li(
              strong("Series: "),
              "carbon flows step-by-step from the fast pool to the slow pool (or from fast to intermediate to very stable). A transfer fraction controls how much decomposed carbon moves to the next pool versus being lost as carbon dioxide."
            ),
            tags$li(
              strong("Parallel: "),
              "incoming carbon is split among pools (for example, 30 percent to the fast pool and 70 percent to the slow pool) and pools decompose independently. This represents different substrates decomposing side-by-side."
            ),
            tags$li(
              strong("Feedback: "),
              "allows exchange in both directions between pools. This can mimic mixing, redistribution, or recycling that moves carbon between faster and slower fractions."
            ),
            tags$li(
              strong("How to interpret: "),
              "the fast pool responds quickly to changes in carbon input; the slower and very stable pools control long-term soil organic carbon. These pools are conceptual unless you calibrate them to measured fraction data."
            )
          ),
          
          tags$hr(),
          
          tags$h4("B) Rothamsted Carbon Model (five pools)"),
          tags$ul(
            tags$li(
              strong("Pools: "),
              "easily decomposed plant material, resistant plant material, microbial biomass, humified organic matter, and inert organic matter."
            ),
            tags$li(
              strong("Key controls: "),
              "temperature and moisture modifiers and a soil cover modifier (typically applied monthly), plus clay percentage which affects stabilization into humified organic matter and microbial turnover."
            ),
            tags$li(
              strong("Inputs: "),
              "plant carbon inputs are divided between easily decomposed and resistant plant material depending on vegetation or residue type."
            ),
            tags$li(
              strong("Interpretation: "),
              "plant material pools represent residues, microbial biomass is short-lived, humified organic matter is the main stabilized pool, and inert organic matter is assumed not to decompose on management time scales."
            )
          ),
          
          tags$hr(),
          
          tags$h4("C) Century Model (seven pools)"),
          tags$ul(
            tags$li(
              strong("Pools: "),
              "surface structural litter, surface metabolic litter, soil structural litter, soil metabolic litter, and three soil organic carbon pools representing fast, intermediate, and very stable soil organic carbon."
            ),
            tags$li(
              strong("Key controls: "),
              "litter quality (structural versus metabolic) and transfers among fast, intermediate, and very stable soil organic carbon. In full implementations, climate and soil texture strongly affect decomposition; in this simplified implementation, inputs and soil texture drive the dynamics."
            ),
            tags$li(
              strong("Interpretation: "),
              "the fast soil organic carbon pool cycles quickly, the intermediate pool cycles on multi-year to decadal time scales, and the very stable pool turns over very slowly."
            )
          ),
          
          tags$hr(),
          
          tags$h4("D) Introductory Carbon Balance Model (two pools)"),
          tags$ul(
            tags$li(
              strong("Pools: "),
              "young soil organic carbon (fresh and active) and old soil organic carbon (stabilized)."
            ),
            tags$li(
              strong("Key controls: "),
              "carbon input rate, turnover rates for young and old pools, and a humification factor that controls how much decomposed young carbon becomes old carbon."
            ),
            tags$li(
              strong("Interpretation: "),
              "useful for scenario comparisons because it is simple and transparent, and it is often used for long-term management comparisons."
            )
          ),
          
          tags$hr(),
          tags$p(
            style = "margin-bottom: 0px;",
            strong("Important note: "),
            "This application is intended for comparison. For publishable results, calibrate each model to measured soil organic carbon stocks or measured fractions and use consistent climate forcing and carbon input partitioning assumptions."
          )
        )
      ),
      
      fluidRow(
        column(6, downloadButton("download_timeseries", "Download Time Series (CSV)", width = "100%")),
        column(6, downloadButton("download_pools",     "Download Final Pools (CSV)", width = "100%"))
      ),
      hr(),
      
      h3("Total SOC Projections"),
      plotlyOutput("carbonPlot", height = "520px"),
      hr(),
      
      h3("Final Pool Composition"),
      tags$small("Each panel has its own legend because pool names differ by model."),
      hr(),
      
      h4("Two-pool Models"),
      fluidRow(
        column(4, plotlyOutput("pool_2_series",   height = "320px")),
        column(4, plotlyOutput("pool_2_parallel", height = "320px")),
        column(4, plotlyOutput("pool_2_feedback", height = "320px"))
      ),
      hr(),
      
      h4("Three-pool Models"),
      fluidRow(
        column(4, plotlyOutput("pool_3_series",   height = "320px")),
        column(4, plotlyOutput("pool_3_parallel", height = "320px")),
        column(4, plotlyOutput("pool_3_feedback", height = "320px"))
      ),
      hr(),
      
      h4("Process-based Models"),
      fluidRow(
        column(4, plotlyOutput("pool_rothc",   height = "320px")),
        column(4, plotlyOutput("pool_century", height = "320px")),
        column(4, plotlyOutput("pool_icbm",    height = "320px"))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  sim_results  <- reactiveVal(NULL)
  pool_results <- reactiveVal(NULL)
  
  # ---- Read monthly vectors from UI ----
  get_monthly <- function(prefix, as_num = TRUE) {
    v <- sapply(1:12, function(i) input[[sprintf("%s%02d", prefix, i)]])
    if (as_num) as.numeric(v) else v
  }
  
  # ---- Extract outputs from SoilR model ----
  extract_outputs <- function(m, model_name, pool_names, t_vec) {
    mat <- getC(m)
    if (!is.matrix(mat)) mat <- as.matrix(mat)
    
    n <- nrow(mat)
    Year <- if (length(t_vec) >= n) t_vec[seq_len(n)] else seq(0, n - 1)
    
    total <- rowSums(mat)
    final_row <- as.numeric(mat[n, ])
    
    if (length(pool_names) != length(final_row)) {
      pool_names <- paste0("Pool ", seq_along(final_row))
    }
    
    list(
      ts = data.frame(Year = Year, Total_SOC = total, Model = model_name, stringsAsFactors = FALSE),
      pools = data.frame(Model = model_name, Pool = pool_names, Carbon = final_row, stringsAsFactors = FALSE)
    )
  }
  
  # ---- Call with only supported args (for version differences) ----
  safe_do_call <- function(fun, args_list) {
    fn_formals <- names(formals(fun))
    args_list <- args_list[names(args_list) %in% fn_formals]
    do.call(fun, args_list)
  }
  
  # ---- Model-aware pool panel plot (ONLY ONE VERSION) ----
  pool_panel_plot <- function(pool_df, title) {
    if (is.null(pool_df) || nrow(pool_df) == 0) {
      p <- ggplot() + theme_void() + ggtitle(paste0(title, "\n(not available)"))
      return(ggplotly(p))
    }
    
    pool_df <- pool_df %>% mutate(Dummy = title)
    
    # 2-Pool: ONLY red + green
    if (grepl("^2-Pool", title)) {
      pool_df <- pool_df %>%
        mutate(
          PoolStd = case_when(
            Pool %in% c("Fast Pool", "Active", "Pool 1", "Young") ~ "Active/Fast (Red)",
            TRUE                                                 ~ "Slow/Old (Green)"
          ),
          PoolStd = factor(PoolStd, levels = c("Slow/Old (Green)", "Active/Fast (Red)"))
        )
      
      cmap <- c("Slow/Old (Green)" = "#2ca02c", "Active/Fast (Red)" = "#d62728")
      
      p <- ggplot(pool_df, aes(x = Dummy, y = Carbon, fill = PoolStd)) +
        geom_col(width = 0.65, color = NA) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = "t C/ha", title = title, fill = "Pool") +
        scale_fill_manual(values = cmap) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      return(ggplotly(p, tooltip = c("fill", "y")) %>%
               layout(legend = list(x = 1.02, y = 1), margin = list(r = 170)))
    }
    
    # 3-Pool: Red + Blue + Green
    if (grepl("^3-Pool", title)) {
      pool_df <- pool_df %>%
        mutate(
          PoolStd = case_when(
            Pool %in% c("Active", "Pool 1") ~ "Active (Red)",
            Pool %in% c("Slow", "Pool 2")   ~ "Slow (Blue)",
            TRUE                           ~ "Passive (Green)"
          ),
          PoolStd = factor(PoolStd, levels = c("Passive (Green)", "Slow (Blue)", "Active (Red)"))
        )
      
      cmap <- c("Active (Red)"="#d62728","Slow (Blue)"="#1f77b4","Passive (Green)"="#2ca02c")
      
      p <- ggplot(pool_df, aes(x = Dummy, y = Carbon, fill = PoolStd)) +
        geom_col(width = 0.65, color = NA) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = "t C/ha", title = title, fill = "Pool") +
        scale_fill_manual(values = cmap) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      return(ggplotly(p, tooltip = c("fill", "y")) %>%
               layout(legend = list(x = 1.02, y = 1), margin = list(r = 170)))
    }
    
    # ICBM: red + green
    if (grepl("^ICBM", title)) {
      pool_df <- pool_df %>%
        mutate(
          PoolStd = ifelse(Pool == "Young", "Young (Red)", "Old (Green)"),
          PoolStd = factor(PoolStd, levels = c("Old (Green)", "Young (Red)"))
        )
      
      cmap <- c("Old (Green)"="#2ca02c", "Young (Red)"="#d62728")
      
      p <- ggplot(pool_df, aes(x = Dummy, y = Carbon, fill = PoolStd)) +
        geom_col(width = 0.65, color = NA) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = "t C/ha", title = title, fill = "Pool") +
        scale_fill_manual(values = cmap) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      return(ggplotly(p, tooltip = c("fill", "y")) %>%
               layout(legend = list(x = 1.02, y = 1), margin = list(r = 170)))
    }
    
    # RothC: green, teal, blue, orange, red (IOM, HUM, RPM, BIO, DPM)
    if (grepl("^RothC", title)) {
      pool_df <- pool_df %>%
        mutate(PoolStd = factor(Pool, levels = c(
          "IOM (Inert)", "HUM (Humified)", "RPM (Resistant)", "BIO (Biomass)", "DPM (Decomposable)"
        )))
      
      cmap <- c(
        "IOM (Inert)"="#2ca02c",
        "HUM (Humified)"="#17becf",
        "RPM (Resistant)"="#1f77b4",
        "BIO (Biomass)"="#ff7f0e",
        "DPM (Decomposable)"="#d62728"
      )
      
      p <- ggplot(pool_df, aes(x = Dummy, y = Carbon, fill = PoolStd)) +
        geom_col(width = 0.65, color = NA) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = "t C/ha", title = title, fill = "Pool") +
        scale_fill_manual(values = cmap, drop = FALSE) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      return(ggplotly(p, tooltip = c("fill", "y")) %>%
               layout(legend = list(x = 1.02, y = 1), margin = list(r = 200)))
    }
    
    # Century: (uses its own pool names; keep order stable)
    if (grepl("^Century", title)) {
      pool_df <- pool_df %>%
        mutate(PoolStd = factor(Pool, levels = c(
          "Passive","Slow","Soil Structural","Soil Metabolic","Surf Structural","Surf Metabolic","Active"
        )))
      
      cmap <- c(
        "Passive"="#006d2c",
        "Slow"="#2ca02c",
        "Soil Structural"="#66bd63",
        "Soil Metabolic"="#17becf",
        "Surf Structural"="#ff7f0e",
        "Surf Metabolic"="#d62728",
        "Active"="#8b0000"
      )
      
      p <- ggplot(pool_df, aes(x = Dummy, y = Carbon, fill = PoolStd)) +
        geom_col(width = 0.65, color = NA) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = "t C/ha", title = title, fill = "Pool") +
        scale_fill_manual(values = cmap, drop = FALSE) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      return(ggplotly(p, tooltip = c("fill", "y")) %>%
               layout(legend = list(x = 1.02, y = 1), margin = list(r = 210)))
    }
    
    # fallback
    p <- ggplot(pool_df, aes(x = Dummy, y = Carbon, fill = Pool)) +
      geom_col(width = 0.65, color = NA) +
      theme_minimal(base_size = 12) +
      labs(x = NULL, y = "t C/ha", title = title, fill = "Pool")
    
    ggplotly(p, tooltip = c("fill", "y")) %>% layout(legend = list(x = 1.02, y = 1))
  }
  
  observeEvent(input$run_models, {
    
    temp_m   <- get_monthly("t",   TRUE)
    precip_m <- get_monthly("p",   TRUE)
    pet_m    <- get_monthly("pet", TRUE)
    cover_m  <- as.numeric(get_monthly("cov", FALSE))
    
    clay    <- as.numeric(input$clay)
    c_input <- as.numeric(input$c_input)
    years   <- as.integer(input$years)
    soc0    <- as.numeric(input$init_soc)
    
    t <- seq(0, years, by = 1)
    
    sim_results(NULL)
    pool_results(NULL)
    
    ts_list <- list()
    pool_list <- list()
    
    withProgress(message = "Running 9 models...", value = 0, {
      
      bump <- function(txt, denom = 9) incProgress(1/denom, detail = txt)
      
      bump("2-Pool Series")
      tryCatch({
        m <- TwopSeriesModel(t = t, ks = c(k1 = 0.8, k2 = 0.006), a21 = 0.1,
                             C0 = c(soc0 * 0.1, soc0 * 0.9), In = c_input)
        out <- extract_outputs(m, "2-Pool Series", c("Fast Pool", "Slow Pool"), t)
        ts_list[["2-Pool Series"]] <- out$ts
        pool_list[["2-Pool Series"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("2-Pool Parallel")
      tryCatch({
        m <- TwopParallelModel(t = t, ks = c(k1 = 0.8, k2 = 0.006),
                               C0 = c(soc0 * 0.1, soc0 * 0.9),
                               In = c_input, gam = 0.3)
        out <- extract_outputs(m, "2-Pool Parallel", c("Pool 1", "Pool 2"), t)
        ts_list[["2-Pool Parallel"]] <- out$ts
        pool_list[["2-Pool Parallel"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("2-Pool Feedback")
      tryCatch({
        m <- TwopFeedbackModel(t = t, ks = c(k1 = 0.8, k2 = 0.006),
                               a21 = 0.1, a12 = 0.001,
                               C0 = c(soc0 * 0.1, soc0 * 0.9), In = c_input)
        out <- extract_outputs(m, "2-Pool Feedback", c("Active", "Slow"), t)
        ts_list[["2-Pool Feedback"]] <- out$ts
        pool_list[["2-Pool Feedback"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("3-Pool Series")
      tryCatch({
        m <- ThreepSeriesModel(t = t, ks = c(k1 = 0.5, k2 = 0.1, k3 = 0.005),
                               a21 = 0.1, a32 = 0.01,
                               C0 = c(soc0 * 0.05, soc0 * 0.25, soc0 * 0.70), In = c_input)
        out <- extract_outputs(m, "3-Pool Series", c("Active", "Slow", "Passive"), t)
        ts_list[["3-Pool Series"]] <- out$ts
        pool_list[["3-Pool Series"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("3-Pool Parallel")
      tryCatch({
        m <- ThreepParallelModel(t = t, ks = c(k1 = 0.5, k2 = 0.05, k3 = 0.005),
                                 C0 = c(soc0 * 0.05, soc0 * 0.25, soc0 * 0.70),
                                 In = c_input, gam1 = 0.2, gam2 = 0.5)
        out <- extract_outputs(m, "3-Pool Parallel", c("Pool 1", "Pool 2", "Pool 3"), t)
        ts_list[["3-Pool Parallel"]] <- out$ts
        pool_list[["3-Pool Parallel"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("3-Pool Feedback")
      tryCatch({
        m <- ThreepFeedbackModel(t = t, ks = c(k1 = 0.5, k2 = 0.1, k3 = 0.005),
                                 a21 = 0.1, a12 = 0.001, a32 = 0.005, a23 = 0.001,
                                 C0 = c(soc0 * 0.05, soc0 * 0.25, soc0 * 0.70), In = c_input)
        out <- extract_outputs(m, "3-Pool Feedback", c("Active", "Slow", "Passive"), t)
        ts_list[["3-Pool Feedback"]] <- out$ts
        pool_list[["3-Pool Feedback"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("RothC (5-pool)")
      tryCatch({
        xi_temp_m <- 47.9 / (1 + exp(106 / (temp_m + 18.3)))
        xi_temp <- mean(xi_temp_m, na.rm = TRUE)
        
        moist_ratio <- precip_m / pmax(precip_m + pet_m, 1e-6)
        cover_factor <- 0.8 + 0.2 * cover_m
        xi_moist <- mean(pmin(1, pmax(0, moist_ratio * cover_factor)), na.rm = TRUE)
        
        xi <- xi_temp * xi_moist
        
        C0_rothc <- c(DPM = soc0 * 0.02, RPM = soc0 * 0.13, BIO = soc0 * 0.02, HUM = soc0 * 0.76, IOM = soc0 * 0.07)
        
        m <- RothCModel(t = t, C0 = C0_rothc, In = c_input, clay = clay, xi = xi)
        
        out <- extract_outputs(m, "RothC (5-pool)",
                               c("DPM (Decomposable)", "RPM (Resistant)", "BIO (Biomass)", "HUM (Humified)", "IOM (Inert)"), t)
        ts_list[["RothC (5-pool)"]] <- out$ts
        pool_list[["RothC (5-pool)"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("Century (7-pool)")
      tryCatch({
        frac <- c(0.02, 0.02, 0.03, 0.03, 0.10, 0.50, 0.30)
        frac <- frac / sum(frac)
        
        C0_century <- c(
          "Surf Structural" = soc0 * frac[1],
          "Surf Metabolic"  = soc0 * frac[2],
          "Soil Structural" = soc0 * frac[3],
          "Soil Metabolic"  = soc0 * frac[4],
          "Active"          = soc0 * frac[5],
          "Slow"            = soc0 * frac[6],
          "Passive"         = soc0 * frac[7]
        )
        
        m <- CenturyModel(t = t, C0 = C0_century, surfaceIn = c_input / 2, soilIn = c_input / 2,
                          LN = 0.1, Ls = 0.1, clay = clay / 100)
        
        out <- extract_outputs(m, "Century (7-pool)", names(C0_century), t)
        ts_list[["Century (7-pool)"]] <- out$ts
        pool_list[["Century (7-pool)"]] <- out$pools
      }, error = function(e) NULL)
      
      bump("ICBM (2-pool)")
      tryCatch({
        fun <- SoilR::ICBMModel
        fn  <- names(formals(fun))
        
        args <- list(t = t)
        if ("C0" %in% fn) args$C0 <- c(Y = soc0 * 0.1, O = soc0 * 0.9)
        if ("c0" %in% fn) args$c0 <- c(Y = soc0 * 0.1, O = soc0 * 0.9)
        
        if ("In" %in% fn)    args$In <- c_input
        if ("I" %in% fn)     args$I <- c_input
        if ("input" %in% fn) args$input <- c_input
        
        if ("FYM" %in% fn) args$FYM <- 0.0
        if ("fym" %in% fn) args$fym <- 0.0
        
        if ("k1" %in% fn) args$k1 <- 0.8
        if ("k2" %in% fn) args$k2 <- 0.00605
        if ("r"  %in% fn) args$r  <- 1.0
        
        m <- safe_do_call(fun, args)
        
        out <- extract_outputs(m, "ICBM (2-pool)", c("Young", "Old"), t)
        ts_list[["ICBM (2-pool)"]] <- out$ts
        pool_list[["ICBM (2-pool)"]] <- out$pools
      }, error = function(e) NULL)
    })
    
    if (length(ts_list) > 0) sim_results(bind_rows(ts_list))
    if (length(pool_list) > 0) pool_results(bind_rows(pool_list))
  })
  
  # ---- Time series plot (plotly manual, legend shows model names) ----
  output$carbonPlot <- renderPlotly({
    req(sim_results())
    df <- sim_results()
    
    group_color <- function(model) {
      if (grepl("^2-Pool", model)) return("#1f77b4")
      if (grepl("^3-Pool", model)) return("#2ca02c")
      if (grepl("RothC", model))   return("#d62728")
      if (grepl("Century", model)) return("#9467bd")
      if (grepl("ICBM", model))    return("#000000")
      "#7f7f7f"
    }
    
    dash_type <- function(model) {
      if (grepl("Series", model))   return("solid")
      if (grepl("Parallel", model)) return("dash")
      if (grepl("Feedback", model)) return("dot")
      "solid"
    }
    
    p <- plot_ly()
    for (m in unique(df$Model)) {
      dd <- df[df$Model == m, ]
      p <- p %>% add_lines(
        data = dd, x = ~Year, y = ~Total_SOC, name = m,
        line = list(color = group_color(m), dash = dash_type(m), width = 3),
        hovertemplate = paste0("<b>", m, "</b><br>Year: %{x}<br>Total SOC: %{y:.2f} t C/ha<extra></extra>")
      )
    }
    
    p %>% layout(
      xaxis = list(title = "Simulation Year"),
      yaxis = list(title = "Total SOC (t C/ha)"),
      legend = list(orientation = "v", x = 1.02, y = 1),
      margin = list(r = 220)
    )
  })
  
  # ---- Helper to pull pools for one model ----
  get_model_pool_df <- function(model_name) {
    df <- pool_results()
    if (is.null(df)) return(NULL)
    df %>% filter(Model == model_name)
  }
  
  # ---- 9 pool panels ----
  output$pool_2_series   <- renderPlotly({ pool_panel_plot(get_model_pool_df("2-Pool Series"),   "2-Pool Series") })
  output$pool_2_parallel <- renderPlotly({ pool_panel_plot(get_model_pool_df("2-Pool Parallel"), "2-Pool Parallel") })
  output$pool_2_feedback <- renderPlotly({ pool_panel_plot(get_model_pool_df("2-Pool Feedback"), "2-Pool Feedback") })
  
  output$pool_3_series   <- renderPlotly({ pool_panel_plot(get_model_pool_df("3-Pool Series"),   "3-Pool Series") })
  output$pool_3_parallel <- renderPlotly({ pool_panel_plot(get_model_pool_df("3-Pool Parallel"), "3-Pool Parallel") })
  output$pool_3_feedback <- renderPlotly({ pool_panel_plot(get_model_pool_df("3-Pool Feedback"), "3-Pool Feedback") })
  
  output$pool_rothc   <- renderPlotly({ pool_panel_plot(get_model_pool_df("RothC (5-pool)"),   "RothC (5-pool)") })
  output$pool_century <- renderPlotly({ pool_panel_plot(get_model_pool_df("Century (7-pool)"), "Century (7-pool)") })
  output$pool_icbm    <- renderPlotly({ pool_panel_plot(get_model_pool_df("ICBM (2-pool)"),    "ICBM (2-pool)") })
  
  # ---- Downloads ----
  output$download_timeseries <- downloadHandler(
    filename = function() paste0("soc_timeseries_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- sim_results()
      if (is.null(df)) df <- data.frame()
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_pools <- downloadHandler(
    filename = function() paste0("soc_final_pools_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- pool_results()
      if (is.null(df)) df <- data.frame()
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)