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

ui <- fluidPage(
    titlePanel("PLACEHOLDER"),
    
    sidebarLayout(
        sidebarPanel(
          #heatmap variables
          checkboxGroupInput("heatmapModeInput", "Continents", choices = unique(DATA$Mode), selected = unique(DATA$Mode)),
          checkboxGroupInput("heatmapModernPopInput", "Modern Populations", choices = unique(DATA$Modern_pop), selected = unique(DATA$Modern_pop)),
          checkboxGroupInput("heatmapArchaicPopInput", "Archaic Populations", choices = unique(DATA$Archaic_pop), selected = unique(DATA$Archaic_pop)),
          #violin variables
          br(), br(),
          sliderInput("violinUniqSharedInput", "Unique Shared", min = min(DATA$Uniq_Shared), 
                      max = max(DATA$Uniq_Shared), value = c(min(DATA$Uniq_Shared), max(DATA$Uniq_Shared))),
            checkboxGroupInput("violinModernPopInput", "Modern Populations", choices = unique(DATA$Modern_pop), selected = unique(DATA$Modern_pop)),
            checkboxGroupInput("violinArchaicPopInput", "Archaic Populations", choices = unique(DATA$Archaic_pop), selected = unique(DATA$Archaic_pop))
        ),
        
        mainPanel(
          #histogram
          #plotOutput("histogramPlot"),
          #dataTableOutput("histogramResults")
          br(), br(),
          #heatmap
          plotOutput("heatmapPlot"),
          dataTableOutput("heatmapResults"),
          br(), br(),
          #violin plot
          plotOutput("violinPlot"),
          dataTableOutput("violinResults")
        )
    )
)

server <- function(input, output) {
  
    # HEATMAP PLOT
    
    output$heatmapPlot <- renderPlot({
      heatmapFiltered <- DATA %>% filter(
        Mode %in% input$heatmapModeInput,
        Modern_pop %in% input$heatmapModernPopInput,
        Archaic_pop %in% input$heatmapArchaicPopInput
      )
      
      ggplot(data = heatmapFiltered, aes(x = Archaic_pop, y = Modern_pop)) + geom_bin2d() + 
        facet_grid(cols = vars(Mode), scales = "free") + theme(panel.grid.major.x = element_blank()) + 
        xlab("Archaic population") + ylab("Modern population") + scale_fill_viridis("Amount")
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
  
    output$violinPlot <- renderPlot({
      violinFiltered <- DATA %>% filter(
        Uniq_Shared >= input$violinUniqSharedInput[1],
        Uniq_Shared <= input$violinUniqSharedInput[2],
        Modern_pop %in% input$violinModernPopInput,
        Archaic_pop %in% input$violinArchaicPopInput
        
      )
      # FIX: ARCHAIC LABELS BEING ALWAYS NEA
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
}

shinyApp(ui = ui, server = server)
