library("shiny")
library("bslib")
library("shinyFeedback")
library("tidyverse")
library("magrittr")
library("scales")
library("class")

fontCustom <- "Noto Sans"

themeCustom <- theme_bw() +
  theme(text = element_text(family = fontCustom, size = 15), axis.title = element_text(face = "bold"))

theme_set(themeCustom)

vow_wide <- read_tsv(
  "BeckerVowelCorpus.tsv",
  col_types = str_flatten(c("cccccncccncnn", rep("ccnnnn", 14), "_nnnnnn"))
)

vow_names <- vow_wide %>%
  select(V1OS:V14F4) %>%
  colnames()

vow_values <- rep(c("Vowel", "VowelPS", "F1", "F2", "F3", "F4"), 14)

vow_spec <- tibble(
  .name = vow_names,
  .value = vow_values
)

rm(vow_names, vow_values)

vow_long <- vow_wide %>%
  pivot_longer_spec(vow_spec, values_drop_na = TRUE) %>%
  mutate(Vowel = case_when(is.na(Vowel) ~ VowelPS, TRUE ~ Vowel)) %>%
  drop_na(Vowel)

rm(vow_spec)

vow_long %<>%
  plyr::ddply("Vowel", function(d) {if(nrow(d)>10) d else NULL})

vow_long %<>%
  group_by(Language) %>%
  mutate(
    F1z = (F1 - mean(F1, na.rm = TRUE)) / sd(F1, na.rm = TRUE),
    F2z = (F2 - mean(F2, na.rm = TRUE)) / sd(F2, na.rm = TRUE),
    F3z = (F3 - mean(F3, na.rm = TRUE)) / sd(F3, na.rm = TRUE)
  ) %>%
  ungroup()

vow_data <- vow_long %>%
  mutate(
    F1r = sd(F1, na.rm = TRUE) * F1z + mean(F1, na.rm = TRUE),
    F2r = sd(F2, na.rm = TRUE) * F2z + mean(F2, na.rm = TRUE),
    F3r = sd(F3, na.rm = TRUE) * F3z + mean(F3, na.rm = TRUE)
  ) %>%
  select(Vowel, F1r, F2r, F3r) %>%
  arrange(F1r, F2r, F3r) %>%
  rename(F1 = F1r, F2 = F2r, F3 = F3r) %>%
  mutate(Vowel = as.factor(Vowel))

vow_data %<>%
  group_by(Vowel) %>%
  mutate(
    F1 = case_when(is.na(F1) ~ mean(F1, na.rm = TRUE), TRUE ~ F1),
    F2 = case_when(is.na(F2) ~ mean(F2, na.rm = TRUE), TRUE ~ F2),
    F3 = case_when(is.na(F3) ~ mean(F3, na.rm = TRUE), TRUE ~ F3)
  ) %>%
  ungroup()

ui <- page_fillable(
  
  fillable = TRUE,
  
  title = "A formant-based vowel classifier",
  
  theme = bs_theme(
    bg = "#ffffff",
    fg = "#000000",
    primary = "#aa0000",
    secondary = "#0000bb",
    success = "#00bb00",
    base_font = font_google("Noto Sans"),
    code_font = font_google("Noto Sans Mono")
  ),
  
  useShinyFeedback(),
  
  navset_card_tab(
    
    title = "A formant-based vowel classifier",
    
    sidebar = sidebar(
      
      numericInput(inputId = "input_f1",
                   label = "F1:",
                   min = 0,
                   max = 1000,
                   step = 1,
                   value = 462),
      
      textOutput("f1_msg"),
      
      numericInput(inputId = "input_f2",
                   label = "F2:",
                   min = 0,
                   max = 3000,
                   step = 1,
                   value = 1482),
      
      textOutput("f2_msg"),
      
      numericInput(inputId = "input_f3",
                   label = "F3:",
                   min = 0,
                   max = 4000,
                   step = 1,
                   value = 2529),
      
      textOutput("f3_msg")
      
    ),
    
    nav_panel(
      
      card_header("App"),
      
      card_body(
        
        markdown("This 𝒮𝒽𝒾𝓃𝓎 app takes user-supplied formant values and finds the likeliest [IPA](https://www.internationalphoneticassociation.org/content/full-ipa-chart) symbol based on the transcriptions and measurements found in Roy Becker-Kristal's [cross-linguistic vowel corpus](http://phonetics.linguistics.ucla.edu/research/BeckerVowelCorpus.xls) discussed in his [2010 PhD thesis](http://phonetics.linguistics.ucla.edu/research/RBecker_diss.pdf).
        
        Enter your own F1, F2 and F3 values in the fields in the sidebar, populated by default with corpus-wide means, to produce a plot superimposing the \"best guess\" IPA symbol on the rescaled F1–F2 space of Becker-Kristal's corpus. The sidebar should also warn you if you enter illogical or unusual formant values."),
        
        #textOutput("prediction"),
        
        plotOutput("vow_plot")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("About"),
      
      card_body(
        
        markdown("This app is essentially a reimplementation in R of Dmitry Nikolaev's [vowel classifier](https://eurphon.info/static/vowelclf/index.html), originally written in Python.
                 
                 As mentioned on the \"App\" tab, the underlying transcription and formant data come from a corpus compiled by Becker-Kristal. The full data set contains measurements for 3,755 vowels across 206 dialects of 231 languages gathered from 321 sources providing data from a varying number of speakers (range: 1–91) and using various elicitation methods (from words uttered in isolation to running speech).
                 
                 The following edits are made to the original data set in this app. Measurements having `NA` for `Vowel` use the alternative `VowelPS` instead. Entries missing formant values use means for that IPA symbol. The original formant values are *z*-scored within language and then rescaled back into hertz. Finally, vowels having fewer than 10 entries are omitted. No attempt has been made to correct the underlying data.
                 
                 The user-supplied formant values are fed into the *k*-nearest neighbour classification function `knn()` from the [`class`](https://cran.r-project.org/web/packages/class/) package using Becker-Kristal's corpus as the training data. The number of neighbours, `k`, is set arbitrarily to 7.
                 
                 Currently the app requires values for the first three formants, future implementations will make F3 optional and only F1 and F2 obligatory. A future version will also present the user with the *k* nearest neighbours in Becker-Kristal's corpus.
                 
                 The code and data are available on [GitHub](https://github.com/yonosoyunconejo/vowel-classifier).
                 
                 By [Stephen Nichols](https://www.stephen-nichols.me/).")
        
      )
      
    )
    
  )
  
)

server <- function(input, output) {
  
  #bs_themer()
  
  #iv <- InputValidator$new()
  
  f1_msg <- reactive({
    
    insufficientf1 <- input$input_f1 < 1
    
    smallf1 <- input$input_f1 > 0 & input$input_f1 < 100
    
    largef1 <- input$input_f1 > 1000
    
    if (insufficientf1) {
      
      feedbackDanger("input_f1", insufficientf1, "F1 must be positive")
      
    }
    
    else {
      
      if (smallf1) {
        
        feedbackWarning("input_f1", smallf1, "F1 is unusually small")
        
      }
      
      else {
        
        feedbackWarning("input_f1", largef1, "F1 is unusually large")
        
      }
      
    }
    
  })
  
  output$f1_msg <- renderText(f1_msg())
  
  f2_msg <- reactive({
    
    insufficientf2 <- input$input_f2 <= input$input_f1
    
    smallf2 <- input$input_f2 > input$input_f1 & input$input_f2 < 400
    
    largef2 <- input$input_f2 > 2750
    
    if (insufficientf2) {
      
      feedbackDanger("input_f2", insufficientf2, "F2 must be greater than F1")
      
    }
    
    else {
      
      if (smallf2) {
        
        feedbackWarning("input_f2", smallf2, "F2 is unusually small")
        
      }
      
      else {
        
        feedbackWarning("input_f2", largef2, "F2 is unusually large")
        
      }
      
    }
    
  })
  
  output$f2_msg <- renderText(f2_msg())
  
  f3_msg <- reactive({
    
    insufficientf3 <- input$input_f3 <= input$input_f2
    
    smallf3 <- input$input_f3 > input$input_f2 & input$input_f3 < 1750
    
    largef3 <- input$input_f3 > 3750
    
    if (insufficientf3) {
      
      feedbackDanger("input_f3", insufficientf3, "F3 must be greater than F2")
      
    }
    
    else {
      
      if (smallf3) {
        
        feedbackWarning("input_f3", smallf3, "F3 is unusually small")
        
      }
      
      else {
        
        feedbackWarning("input_f3", largef3, "F3 is unusually high")
        
      }
      
    }
    
  })
  
  output$f3_msg <- renderText(f3_msg())
  
  vow_input <- reactive({
    
    vow_input <- data.frame(input$input_f1, input$input_f2, input$input_f3)
    
    colnames(vow_input) <- c("F1", "F2", "F3")
    
    vow_input
    
  })
  
  prediction <- reactive({
    
    knn(train = vow_data[2:4],
        test = vow_input(),
        cl = vow_data$Vowel[1:nrow(vow_data)],
        k = 7)
    
  })
  
  #output$prediction <- reactive({prediction()})
  
  vow_pred <- reactive({
    
    vow_pred <- data.frame(prediction(), input$input_f1, input$input_f2, input$input_f3)
    
    colnames(vow_pred) <- c("Vowel", "F1", "F2", "F3")
    
    as.data.frame(vow_pred)
    
  })
  
  vow_pred <- reactive({
    
    setNames(data.frame(prediction(), input$input_f1, input$input_f2, input$input_f3), c("Vowel", "F1", "F2", "F3"))
    
  })
  
  output$vow_plot <- renderPlot({
    
    vow_data %>%
      ggplot(aes(F2, F1, label = Vowel)) +
      geom_text(aes(colour = Vowel, family = fontCustom), alpha = 0.3, size = 6) +
      geom_text(data = vow_pred(), aes(family = fontCustom, fontface = "bold"), colour = "black", alpha = 0.9, size = 12) +
      scale_x_reverse(breaks = pretty_breaks()) +
      scale_y_reverse(breaks = pretty_breaks()) +
      coord_fixed(ratio = 1.75) +
      theme(legend.position = "none")
    
  })
  
}

shinyApp(ui = ui, server = server)
