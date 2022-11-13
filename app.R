#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(dplyr)
library(tidyr)
library(viridis)
library(ggplot2)
DATA <- read_excel("TableS3_excelfile.xlsx")
DATA <- DATA %>% separate(`Chr:Start-End`, c("Chr", "Start", "End"))
chrOrder <-c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22", "chrM", "chrY", "chrX")
DATA$Chr = factor(DATA$Chr, levels = chrOrder, ordered = TRUE)
DATA$Mode = factor(DATA$Mode)
DATA$Modern_pop = factor(DATA$Modern_pop)
DATA$Archaic_pop = factor(DATA$Archaic_pop)
DATA$Chr = factor(DATA$Chr)
DATA$Genes = factor(DATA$Genes)
DATA$Lit_overlap = factor(DATA$Lit_overlap)

# Define UI for application that draws a histogram
ui <- navbarPage("Project", inverse = TRUE,
  tabPanel("Barplot",
    titlePanel("PLACEHOLDER"),
    sidebarLayout(
      sidebarPanel(
        #Barplot variables
        checkboxGroupInput("Chr_inp", "Barplot_chromosome_input", choices = unique(DATA$Chr), selected = unique(DATA$Chr)),
        sliderInput("count_barplot", "Maximum count and minimum count", min = 0,max = 250, value = c(0, 250)),
        radioButtons("Bar_color_scheme", "Barplot Color scheme", choices = c("Accent", "Set3", "Spectral", "Set1", "Dark2", "Paired",
                                                                            "mako", "OrRd", "Pastel1"), selected = "Pastel1")
        ),
      mainPanel(
        plotOutput("Barplot", click = "click_action"),tableOutput("click_info")
      )
    )
  ),
  tabPanel("Heatmap",
    # Application title
    titlePanel("PLACEHOLDER"),
    # Show a plot of the generated distribution
    sidebarLayout(
      sidebarPanel(
        #heatmap variables
        radioButtons("heatmapColorscheme", "Color scheme", choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket",
                                                                       "mako", "turbo"), selected = "viridis"),
        checkboxGroupInput("heatmapModeInput", "Continents", choices = unique(DATA$Mode), selected = unique(DATA$Mode)),
        checkboxGroupInput("heatmapModernPopInput", "Modern Populations", choices = unique(DATA$Modern_pop), 
                           selected = unique(DATA$Modern_pop)),
        checkboxGroupInput("heatmapArchaicPopInput", "Archaic Populations", choices = unique(DATA$Archaic_pop), 
                           selected = unique(DATA$Archaic_pop))
      ),
      mainPanel(
        #heatmap
        plotOutput("heatmapPlot"),
        dataTableOutput("heatmapResults")
      )
    )
  ),
  tabPanel("Violinplot",
    # Application title
    titlePanel("PLACEHOLDER"),
    # Show a plot of the generated distribution
    sidebarLayout(
      sidebarPanel(
        #violin variables
        sliderInput("violinUniqSharedInput", "Unique Shared", min = min(DATA$Uniq_Shared), 
                    max = max(DATA$Uniq_Shared), value = c(min(DATA$Uniq_Shared), max(DATA$Uniq_Shared))),
        checkboxGroupInput("violinModernPopInput", "Modern Populations", choices = unique(DATA$Modern_pop), 
                           selected = unique(DATA$Modern_pop)),
        checkboxGroupInput("violinArchaicPopInput", "Archaic Populations", choices = unique(DATA$Archaic_pop), 
                           selected = unique(DATA$Archaic_pop))
      ),
      mainPanel(
        #heatmap
        plotOutput("violinPlot"),
        dataTableOutput("violinResults")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # HEATMAP PLOT
  
  
  #TODO: hacer que al pasar el raton por encima del heatmap salga info
  output$heatmapPlot <- renderPlot({
    heatmapFiltered <- DATA %>% filter(
      Mode %in% input$heatmapModeInput,
      Modern_pop %in% input$heatmapModernPopInput,
      Archaic_pop %in% input$heatmapArchaicPopInput
    )
    
    ggplot(data = heatmapFiltered, aes(x = Archaic_pop, y = Modern_pop)) + geom_bin2d() + 
      facet_grid(cols = vars(Mode), scales = "free") + theme(panel.grid.major.x = element_blank()) + 
      xlab("Archaic population") + ylab("Modern population") + scale_fill_viridis("Amount", option = input$heatmapColorscheme)
  })
  
  output$heatmapResults <- renderDataTable({
    heatmapFiltered <- DATA %>% filter(
      Mode %in% input$heatmapModeInput,
      Modern_pop %in% input$heatmapModernPopInput,
      Archaic_pop %in% input$heatmapArchaicPopInput
    )
    heatmapFiltered
  })
  
  # VIOLIN PLOT
  
  #TODO: haver que al pasar el raton por los puntos salga info
  output$violinPlot <- renderPlot({
    violinFiltered <- DATA %>% filter(
      Uniq_Shared >= input$violinUniqSharedInput[1],
      Uniq_Shared <= input$violinUniqSharedInput[2],
      Modern_pop %in% input$violinModernPopInput,
      Archaic_pop %in% input$violinArchaicPopInput
      
    )
    # FIX: LOS LABELS DE ARCHAIC SIEMPRE SON NEA (NOSE PORQUE)
    ggplot(data = violinFiltered, mapping = aes(x = Modern_pop, y = Uniq_Shared)) + geom_jitter(mapping = aes(colour = Archaic_pop)) + 
      geom_violin(alpha = 0.4) + scale_colour_viridis_d("Archaic Population", labels = violinFiltered$Archaic_pop) + 
      labs(xlab = "Modern population", ylab = "Uniquely shared", title = "Uniquely shared alleles in modern populations") + 
      theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$violinResults <- renderDataTable({
    violinFiltered <- DATA %>% filter(
      Uniq_Shared >= input$violinUniqSharedInput[1],
      Uniq_Shared <= input$violinUniqSharedInput[2],
      Modern_pop %in% input$violinModernPopInput,
      Archaic_pop %in% input$violinArchaicPopInput
    )
    violinFiltered
  })
  
  #BARPLOT
  output$Barplot <- renderPlot({
    B_data <- DATA %>% filter(Chr %in% input$Chr_inp, 
                              Chr %in% names(which(table(Chr) >= input$count_barplot[1])), 
                              Chr %in% names(which(table(Chr) <= input$count_barplot[2])))
    ggplot(data = B_data, mapping = aes(x = Chr, fill = Archaic_pop)) + geom_bar() + 
      geom_text(mapping = aes(label = ..count..),position = position_stack(vjust = 0.5), colour = "black", stat = "count") + coord_flip() + 
      labs(title = "Samples for each chromosome", x = "Chromosome") + scale_fill_brewer(name = "Archaic population", 
                                                                                        labels = c("Both", "Denisova-specific","Neanderthal-specific"), palette = input$Bar_color_scheme) + theme_classic()})
  
  output$click_info <- renderTable({
    if(is.null(input$click_action$x)){
      return()
    }
    else{
      selected_level <- levels(DATA$Chr)[round(input$click_action$y)]
      
      res <- DATA[DATA$Chr %in% selected_level,]
      res
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
