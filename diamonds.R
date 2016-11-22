library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)

col_names<-subset(colnames(diamonds), colnames(diamonds)!='price')

ui <- dashboardPage(
        dashboardHeader(title = "Diamonds"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Explore data", tabName = "explore", icon = icon("dashboard")),
                        menuItem("Create model", tabName = "model", icon = icon("th")),
                        menuItem("Predict", tabName = "predict", icon = icon("cog", lib="glyphicon"))
                )
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "explore", h2("Examin the relation between diamonds characteristics and price"),
                                fluidRow(
                                        plotOutput("plot1", height = 500)
                                ),
                                fluidRow(
                                        
                                        box(width=9,
                                            title = "Controls",
                                            sliderInput("slider", "Randomly choose X observations from data set:", 1, length(diamonds$carat),1000)
                                        ),
                                        box(width=3,
                                            selectInput('color_1','Choose coloring variable', choices=subset(col_names, !(col_names %in% c('price', 'carat'))), selected='clarity'))
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "model",
                                h2("Create linear model to predict diamond's price"),
                                sidebarLayout(
                                        sidebarPanel(
                                                checkboxGroupInput("checkGroup", label = h3("Choose predictors for linear model:"), 
                                                                   choices = list("Carat" = 'carat', "Cut" = 'cut', "Color" = 'color', "Clarity" = "clarity", "Depth"= 'depth', 'Table'='table', 'X'='x', 'Y'='y', 'Z'='z'),
                                                                   selected = 1),
                                                actionButton("run_model", "Run model")
                                        ),
                                        mainPanel(
                                                verbatimTextOutput('model_output')
                                        )
                                )
                        ),
                        tabItem(tabName = "predict", h2("Examin the relation between diamonds characteristics and price"),
                                fluidPage(
                                        fluidRow(
                                                verbatimTextOutput('result'),
                                                actionButton("make_prediction", "Make prediction")
                                        ),
                                        fluidRow(
                                                conditionalPanel(condition="input.checkGroup.indexOf('carat')>=0",
                                                                 box(width=4, sliderInput('carat','Choose carat value:', min=min(diamonds$carat), max=max(diamonds$carat), step=0.1, value=2.5))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('depth')>=0",
                                                                 box(width=4, sliderInput('depth','Choose depth value:', min=min(diamonds$depth), max=max(diamonds$depth), step=0.1, value=60))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('table')>=0",
                                                                 box(width=4, sliderInput('table','Choose table value:', min=min(diamonds$table), max=max(diamonds$table), step=1, value=60))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('x')>=0",
                                                                 box(width=4, sliderInput('x','Choose x value:', min=min(diamonds$x), max=max(diamonds$x), step=0.01, value=4.0))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('y')>=0",
                                                                 box(width=4, sliderInput('y','Choose y value:', min=min(diamonds$y), max=max(diamonds$y), step=0.01, value=4))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('z')>=0",
                                                                 box(width=4, sliderInput('z','Choose z value:', min=min(diamonds$z), max=max(diamonds$z), step=0.01, value=2.5))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('color')>=0",
                                                                 box(width=4, selectInput('color','Choose color:', choices=unique(diamonds$color), selected=NULL))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('cut')>=0",
                                                                 box(width=4, selectInput('cut','Choose cut:', choices=unique(diamonds$cut), selected=NULL))),
                                                conditionalPanel(condition="input.checkGroup.indexOf('clarity')>=0",
                                                                 box(width=4, selectInput('clarity','Choose clarity:', choices=unique(diamonds$clarity), selected=NULL)))
                                        )
                                )
                        )
                        
                )
        )
)

server <- function(input, output) {
        df<-reactive({
                diamonds[sample(length(diamonds$carat), input$slider, replace=FALSE),]
        })
        
        output$plot1 <- renderPlot({
                ggplot(df(), aes_string(x = 'carat', y = 'price', col=input$color_1))+ geom_smooth()+ geom_point()+ scale_y_continuous("Price (USD)", limits= c(0, 2000))+ scale_x_continuous("Carat", limits= c(0, 1))
        })
        v<-reactiveValues(data=NULL)
        
        observeEvent(input$run_model, {
                v$data<-lm(as.formula(paste('price'," ~ ",paste0(input$checkGroup, collapse='+'))), data=diamonds)
                
        })
        
        
        output$model_output<-renderPrint({
                if(is.null(v$data)) return('Choose variables for linear model from the Checkbox on the left')
                summary(v$data)
        })
        
        observe({
                input$checkGroup
        })
        
        f<-reactiveValues(data=NULL)
        
        observeEvent(input$make_prediction, {
                f$data<-input$checkGroup
                
        })
        prediction<-reactive({
                if(is.null(f$data)) return("Generate model in 'Create model' tab")
                check<-f$data
                df<-matrix(ncol=length(check))
                colnames(df)<-check
                df<-as.data.frame(df)
                for (n in seq(length(colnames(df)))){
                      df[1,n]<-eval(parse(text=paste0('input$',colnames(df)[n])))
                }
                df
                model<-v$data
                p<-predict(model, newdata=df)
                paste0('Price of your diamond according to model is: ', round(p,1), '. Change input to see impact on price.')
                
        })
        output$result<-renderPrint({
                prediction()
        })
}

shinyApp(ui, server)

