#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('Org. Wide Plots.R')

# fluidPage <- Defines UI for application
ui <- fluidPage(

    ##### Application Title #####
    titlePanel("Attrition Summary Dashboard"),

    tabsetPanel(
        
        ###########################
        ##### All Departments #####
        ###########################
        
         tabPanel(title = 'All Departments', 
    
    sidebarLayout( # <- Both the sidebar and main panel are specified within sidebarLayout
        # Specify the items appearing on the sidebar
        sidebarPanel(
            checkboxGroupInput('predictors', 'Variables',  choices = names(full_dat_model$model)[-1], selected = c('OverTime', 'Age')),
            actionButton('plotter', 'Update Plot'),
           
            p(),
            fluidRow(
                tableOutput('AttritionCauses'),
            p(), plotOutput("AttritionHist")
                
            )),

        mainPanel(
            fixedRow(
           plotOutput("logisticPlot"),
            verbatimTextOutput('ModelVariables'),

               tableOutput('AtRiskEmployees')
            ), #Close fixedRow()
   
        ), #Close mainPanel()
        
        ) #Close sidebarLayout()
        ), #Close tabPanel() 
    
    #################
    ##### Sales #####
    #################
    
    tabPanel(title = 'Sales', 
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput('sales_predictors', 'Variables',  choices = names(dept_level_model$model)[-1], selected = c('OverTime', 'Age')),
                     actionButton('sales_plotter', 'Update Plot'),
                     
                     
                     
                     
                     p(),
                     fluidRow(
                         tableOutput('sales_AttritionCauses'),
                         p(), plotOutput("sales_AttritionHist")
                         
                     )
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     fixedRow(
                         plotOutput("sales_logisticPlot"),
                         verbatimTextOutput('sales_ModelVariables'),
                         
                         tableOutput('sales_AtRiskEmployees')
                     ),
                     
                     
                     
                 ),
                 
             )
    ),
    
    ##################################
    ##### Research & Development #####
    ##################################
    
    tabPanel(title = 'Research & Development', 
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput('rnd_predictors', 'Variables',  choices = names(dept_level_model$model)[-1], selected = c('OverTime', 'Age')),
                     actionButton('rnd_plotter', 'Update Plot'),
                     
                     
                     
                     
                     p(),
                     fluidRow(
                         tableOutput('rnd_AttritionCauses'),
                         p(), plotOutput("rnd_AttritionHist")
                         
                     )
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     fixedRow(
                         plotOutput("rnd_logisticPlot"),
                         verbatimTextOutput('rnd_ModelVariables'),
                         
                         tableOutput('rnd_AtRiskEmployees')
                     ),
                     
                     
                     
                 ),
                 
             )
    ),
    
    ###########################
    ##### Human Resources #####
    ###########################
    
    tabPanel(title = 'Human Resources', 
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput('hr_predictors', 'Variables',  choices = names(dept_level_model$model)[-1], selected = c('OverTime', 'Age')),
                     actionButton('hr_plotter', 'Update Plot'),
                     

                     p(),
                     fluidRow(
                         tableOutput('hr_AttritionCauses'),
                         p(),
                         plotOutput("hr_AttritionHist")
                     )
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     fixedRow(
                         plotOutput("hr_logisticPlot"),
                         verbatimTextOutput('hr_ModelVariables'),
                         tableOutput('hr_AtRiskEmployees')
                     ),

                 ),
                 
             )
    )
    
    ###########################
    
    
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
##### All Departments #####
    
preds <- eventReactive(input$plotter, # <- Using plotter button prevents plot from updating until 'Update Plot' is clicked
                       { IVs <- input$predictors
    IVs <- paste0(IVs ,collapse = ' + ') # <- Creates a single string of predictors, separated with a +
    })

#output$ModelVariables <-  renderText({c('Predictors:', paste0(input$predictors, collapse = ', '))})

    rv <- reactiveValues(init = paste0(c('OverTime', 'Age'), collapse = ' + '))
    subtitle <-  reactiveValues(pred_string = paste0(c('OverTime', 'Age'), collapse = ', ')) 
    
    observeEvent(input$plotter,{ rv$init <- preds()})
    
    observeEvent(input$plotter,{ subtitle$pred_string <- preds()})
    
    
    output$logisticPlot <- renderPlot({
      
      subtitle$pred_string <- gsub(x = subtitle$pred_string, pattern = '\\ \\+' , replacement = ',')  
        subtitle_text <- paste0('Predictors: ', subtitle$pred_string, collapse = ', ')
      
        frmla <- as.formula(paste0(c('Attrition', rv$init), collapse = ' ~ ')) # <- Concatenates IVs with DV, separated with a ~
        logistic_model = logistic_maker(formulae = frmla, dat = full_dat)
        
        ggPredict(logistic_model, interactive = F) + theme_bw()  +
           ylab('Probability of Attrition')  + ggtitle("Probability of Attrition Across all Departments", subtitle = subtitle_text) 
            })
    
    output$AttritionHist <- renderPlot({attr_plot_maker(dept = 'Organizational', dat = full_dat, fill_color = 'Red')})
   
    output$AtRiskEmployees <- renderTable({
        at_risk <- Likely_Leavers(dataset = full_dat)
        at_risk[,which(colnames(at_risk) %in% c("EmployeeNumber","Probability of Turnover", input$predictors) ), ]})
   output$AttritionCauses <- renderTable({
       frmla <- full_dat_model$call$formula
       attr_causes <- varImp_Constructor(dat = full_dat)
   })
   
   #################
   ##### Sales #####
   #################
   
   sales_preds <- eventReactive(input$sales_plotter, # <- Using plotter button prevents plot from updating until 'Update Plot' is clicked
                          { IVs <- input$sales_predictors
                          IVs <- paste0(IVs ,collapse = ' + ') # <- Creates a single string of predictors, separated with a +
                          })
   
   #output$sales_ModelVariables <-  renderText({c('Predictors:', paste0(input$sales_predictors, collapse = ', '))})
   
   sales_rv <- reactiveValues(init = paste0(c('OverTime', 'Age'), collapse = ' + '))
   sales_subtitle <-  reactiveValues(pred_string = paste0(c('OverTime', 'Age'), collapse = ', '))  
  
    observeEvent(input$sales_plotter,{ sales_rv$init <- sales_preds() })
   observeEvent(input$sales_plotter,{ sales_subtitle$pred_string <- sales_preds()})
   
   
   output$sales_logisticPlot <- renderPlot({
      sales_subtitle$pred_string <- gsub(x = sales_subtitle$pred_string, pattern = '\\ \\+' , replacement = ',')      
      sales_subtitle_text <- paste0('Predictors: ', sales_subtitle$pred_string, collapse = ', ')
      
       frmla <- as.formula(paste0(c('Attrition', sales_rv$init), collapse = ' ~ ')) # <- Concatenates IVs with DV, separated with a ~
       logistic_model = logistic_maker(formulae = frmla, dat = Sales)
       
       ggPredict(logistic_model, interactive = F) + theme_bw()  +
         ylab('Probability of Attrition') + ggtitle("Probability of Attrition Within the Sales Department", subtitle = sales_subtitle_text) 
   })
   
   output$sales_AttritionHist <- renderPlot({attr_plot_maker(dept = 'Sales', dat = Sales, fill_color = 'Green')})
   
   output$sales_AtRiskEmployees <- renderTable({ 
       at_risk <- Likely_Leavers(dataset = Sales)
       at_risk[,which(colnames(at_risk) %in% c("EmployeeNumber","Probability of Turnover", input$predictors) ), ]
       })
  
    output$sales_AttritionCauses <- renderTable({
       frmla <- dept_level_model$call$formula
       attr_causes <- varImp_Constructor(dat = Sales)
       })
   
   ###############
   ##### R&D #####
   ###############
   
   rnd_preds <- eventReactive(input$rnd_plotter, # <- Using plotter button prevents plot from updating until 'Update Plot' is clicked
                                { IVs <- input$rnd_predictors
                                IVs <- paste0(IVs ,collapse = ' + ') # <- Creates a single string of predictors, separated with a +
                                })
   
   #output$rnd_ModelVariables <-  renderText({c('Predictors:', paste0(input$rnd_predictors, collapse = ', '))})
   
    rnd_rv <- reactiveValues(init = paste0(c('OverTime', 'Age'), collapse = ' + '))
    rnd_subtitle <-  reactiveValues(pred_string = paste0(c('OverTime', 'Age'), collapse = ', '))  
    
    observeEvent(input$rnd_plotter,{ rnd_rv$init <- rnd_preds() })
    observeEvent(input$rnd_plotter,{ rnd_subtitle$pred_string <- rnd_preds()})
    
    
    output$rnd_logisticPlot <- renderPlot({
      rnd_subtitle$pred_string <- gsub(x = rnd_subtitle$pred_string, pattern = '\\ \\+' , replacement = ',')      
      rnd_subtitle_text <- paste0('Predictors: ', rnd_subtitle$pred_string, collapse = ', ')
      
      frmla <- as.formula(paste0(c('Attrition', rnd_rv$init), collapse = ' ~ ')) # <- Concatenates IVs with DV, separated with a ~
      logistic_model = logistic_maker(formulae = frmla, dat = Sales)
      
      ggPredict(logistic_model, interactive = F) + theme_bw()  +
        ylab('Probability of Attrition') + ggtitle("Probability of Attrition Within the R&D Department", subtitle = rnd_subtitle_text) 
    })
   
   output$rnd_AttritionHist <- renderPlot({attr_plot_maker(dept = 'R & D', dat = RD, fill_color = 'Blue')})
   
   output$rnd_AtRiskEmployees <- renderTable({ 
       at_risk <- Likely_Leavers(dataset = RD)
       at_risk[,which(colnames(at_risk) %in% c("EmployeeNumber","Probability of Turnover", input$predictors) ), ]
   })
   output$rnd_AttritionCauses <- renderTable({
       frmla <- full_dat_model$call$formula
       attr_causes <- varImp_Constructor(dat = RD)
   })
   
   ##############
   ##### HR #####
   ##############
   hr_preds <- eventReactive(input$hr_plotter, # <- Using plotter button prevents plot from updating until 'Update Plot' is clicked
                                { IVs <- input$hr_predictors
                                
                                IVs <- paste0(IVs ,collapse = ' + ') # <- Creates a single string of predictors, separated with a +
                                })
   
   #output$hr_ModelVariables <-  renderText({c('Predictors:', paste0(input$hr_predictors, collapse = ', '))})
   
   
   hr_rv <- reactiveValues(init = paste0(c('OverTime', 'Age'), collapse = ' + '))
   hr_subtitle <-  reactiveValues(pred_string = paste0(c('OverTime', 'Age'), collapse = ', '))  
   
   observeEvent(input$hr_plotter,{ hr_rv$init <- hr_preds() })
   observeEvent(input$hr_plotter,{ hr_subtitle$pred_string <- hr_preds()})
   
   
   output$hr_logisticPlot <- renderPlot({
     hr_subtitle$pred_string <- gsub(x = hr_subtitle$pred_string, pattern = '\\ \\+' , replacement = ',')      
     hr_subtitle_text <- paste0('Predictors: ', hr_subtitle$pred_string, collapse = ', ')
     
     frmla <- as.formula(paste0(c('Attrition', hr_rv$init), collapse = ' ~ ')) # <- Concatenates IVs with DV, separated with a ~
     logistic_model = logistic_maker(formulae = frmla, dat = Sales)
     
     ggPredict(logistic_model, interactive = F) + theme_bw()  +
       ylab('Probability of Attrition') + ggtitle("Probability of Attrition Within the HR Department", subtitle = hr_subtitle_text) 
   })
   
   
   output$hr_AttritionHist <- renderPlot({attr_plot_maker(dept = 'Human Resources', dat = HR, fill_color = 'Yellow')})
   
   output$hr_AtRiskEmployees <- renderTable({ 
       at_risk <- Likely_Leavers(dataset = HR)
       at_risk[,which(colnames(at_risk) %in% c("EmployeeNumber","Probability of Turnover", input$predictors) ), ]
   })
   output$hr_AttritionCauses <- renderTable({

       attr_causes <- varImp_Constructor(dat = HR)
   })
   
   #################
   
}

# Run the application 
shinyApp(ui = ui, server = server)
