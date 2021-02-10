library(shiny)
library(tidyverse)
library(shinythemes)

eeg.data.working <- read_csv("EEG_data.csv")
eeg.data.gathered <- read_csv("EEG_data_gathered.csv")

# eeg.data.working$Delta <- eeg.data.working$Delta/1000000
# eeg.data.working$Theta <- eeg.data.working$Theta/1000000
# eeg.data.working$Alpha1 <- eeg.data.working$Alpha1/1000000
# eeg.data.working$Alpha2 <- eeg.data.working$Alpha2/1000000
# eeg.data.working$Beta1 <- eeg.data.working$Beta1/1000000
# eeg.data.working$Beta2 <- eeg.data.working$Beta2/1000000
# eeg.data.working$Gamma1 <- eeg.data.working$Gamma1/1000000
# eeg.data.working$Gamma2 <- eeg.data.working$Gamma2/1000000


ui <- navbarPage("Visualizing the Confused Brain",
                 theme = shinytheme("darkly"),
                 tabPanel("Introduction",
                          
                          p("This data set¹ contains the electroencephalogram (EEG) readings of 10 college students 
    as each watched 10 videos. An EEG is a test that measures the electrical activity of the brain over time. Five of these videos were supposed to confuse them (5-9),
    discussing topics such as Quantum Mechanics, and five were supposed to be
    easy to understand (0-4), such as basic algebra. Researchers continually rated their attentiveness and calmness
    on a scale of 1 to 100 as they watched the videos."),
                          
                          img(src = "eeg_pic.jpg", height = 300, width = 450),
                          
                          p(),
                          p(strong("Our primary research questions:")),
                          p(" - What does confusion look like in the brain?"),
                          p(" - How can we visualize the data to compare a confused and non-confused brain at the same time?"),
                          p(" - Furthermore, which frequency can we use to best determine whether a person is feeling confused or not?"),
                          p(" - How are attention and mediation related to confusion level?"),
                          
                          p("¹Data set available at https://www.kaggle.com/wanghaohan/confused-eeg#EEG_data.csv"),
                          p("Image: www.verywellhealth.com")
                 ),
                 
                 tabPanel("Videos",
                          fluidRow(
                              column(6,
                                     p(strong("Simple Videos:")),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/C-C94Y8DE6E" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/aFiP5c_HFhY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/VBZZSqCpu-o" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/7MY4Ot4ZqBg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/9GU_aISv8Kw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                              ),
                              
                              column(6,
                                     p(strong("Confusing Videos:")),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/RcZdyoYzmn8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/8Vs1wyyW2N4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/8yAZIOAQwJc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/g1sdMFtZ3wM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     HTML('<iframe width="420" height="236" src="https://www.youtube.com/embed/Rd8LKolO2aA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                              )
                          )
                          ),
                 
                 tabPanel("EEG Scans",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "selected.subject",
                                              label = "Select a Test Subject",
                                              choices = unique(eeg.data.working$SubjectID)),
                                  selectInput(inputId = "selected.understanding.video",
                                              label = "Select a video to see their understanding brain",
                                              choices = unique(eeg.data.working$VideoID)),
                                  selectInput(inputId = "selected.confused.video",
                                              label = "Select a video to see their confused brain",
                                              choices = unique(eeg.data.working$VideoID)),
                                  actionButton(inputId = "update",
                                               label =  "Go")
                              ),
                              
                              mainPanel(
                                  "Begin by choosing a student. Then, choose a video to see how their brain activity changed during a video that
    they found confusing and one that they thought they understood.",
                                  sliderInput(inputId = "time.slider",
                                              label = "Press play to animate the graph",
                                              min = 0,
                                              max = 75,
                                              value = 0,
                                              animate = TRUE),
                                  plotOutput(outputId = "graph1"), #understanding video
                                  plotOutput(outputId = "graph2"), #confused video
                              )
                          )),
                 
                 tabPanel("Predicting the Confused Brain",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "selected.boxplot.metric",
                                              label = "Select an EEG metric to see how that measurement differs between confused 
                                  brains and understanding brains (all subjects, all videos)",
                                              choices = unique(eeg.data.gathered$Metric)),
                                  actionButton(inputId = "select.metric",
                                               label = "View this metric")
                              ),
                              
                              mainPanel(
                                  plotOutput(outputId = "graph3") #boxplot
                                  
                              )
                          )),
                 
                 tabPanel("Results/Conclusion",
                          p("Our goal was to determine if certain frequencies could be used to predict a test subject's confusion. 
                          Comparing the metrics of confused and understanding brains with boxplots suggests that there
                            are not large differences in average readings between the two. Further analysis, however, suggests otherwise.
                            Using a combination of both logistic regression and decision trees, we were able to identify the most predictive
                            frequency to whether a person was confused or not: "),
                          
                          img(src = "reg_tree.PNG", height = 300, width = 450),
                          img(src = "variable_importance.PNG", height = 300, width = 450),
                          
                          p(),
                          p("As demonstrated above, our data suggest that Theta may be the most important metric in predicting whether a person
                          finds a topic confusing. These findings are in line with past research as referenced by Hoahan Wang on Kaggle. For code and methods 
                            for logistic regression and decision trees, as well as further analysis, see the attached report."),
                          
                          )
)

server <- function(input, output, session) {
    observe({
        subject <- input$selected.subject
        updateSelectInput(session, "selected.understanding.video",
                          label = "Select a video to see their understanding brain",
                          choices = eeg.data.working%>%
                              filter(SubjectID==subject,`user-definedlabeln`==0)%>%
                              .$VideoID%>%
                              unique())
        updateSelectInput(session, "selected.confused.video",
                          label = "Select a video to see their confused brain",
                          choices = eeg.data.working%>%
                              filter(SubjectID==subject,`user-definedlabeln`==1)%>%
                              .$VideoID%>%
                              unique())
        #updating the available input based on the subject selected
        })
   
    graph1.data <- eventReactive(input$update, {
        eeg.data.working %>%
            filter(SubjectID == input$selected.subject,
                   VideoID == input$selected.understanding.video) %>%
            mutate(row.number=1:length(.$SubjectID))%>%
            mutate(time=row.number*0.5) %>% 
            gather(key = "Metric",
                   value = "Value",
                   -SubjectID,
                   -VideoID,
                   -Attention,
                   -Mediation,
                   -Raw,
                   -predefinedlabel,
                   -`user-definedlabeln`,
                   -row.number,
                   -time)
    })
    
    output$graph1 <- renderPlot({
        graph1.data() %>%
            filter(time <= input$time.slider) %>% 
            ggplot(aes(x = time,
                       y = Value)) +
            geom_line(aes(color = Metric)) + 
            labs(x="Time (in seconds)",y="Volts") +
            theme_minimal() +
            scale_x_continuous(limits = c(0, 75)) +
            scale_y_continuous(limits=c(0,4000000),
                               breaks = c(0,1000000,2000000,3000000,4000000),
                               labels = c(0,1,2,3,4)) +
            ggtitle(paste0("Subject ",input$selected.subject," watching a video that they understood, Video ",input$selected.understanding.video)) +
            theme(legend.position = "right",
                  plot.title = element_text(hjust = 0.5)) +
            scale_color_manual(values = c("red",
                                       "orange",
                                       "yellow",
                                       "green",
                                       "blue",
                                       "purple",
                                       "pink",
                                       "black"))
        
    })
    graph2.data <- eventReactive(input$update, {
        eeg.data.working %>%
            filter(SubjectID == input$selected.subject,
                   VideoID == input$selected.confused.video) %>%
            mutate(row.number=1:length(.$SubjectID))%>%
            mutate(time=row.number*0.5) %>% 
            gather(key = "Metric",
                   value = "Value",
                   -SubjectID,
                   -VideoID,
                   -Attention,
                   -Mediation,
                   -Raw,
                   -predefinedlabel,
                   -`user-definedlabeln`,
                   -row.number,
                   -time)
    })
    output$graph2 <- renderPlot({
        graph2.data() %>%
            filter(time <= input$time.slider) %>% 
            ggplot(aes(x = time,
                       y = Value)) +
            geom_line(aes(color = Metric)) + 
            labs(x="Time (in seconds)",y="Volts") +
            theme_minimal() +
            scale_x_continuous(limits = c(0, 75)) +
            scale_y_continuous(limits=c(0,4000000),
                               breaks = c(0,1000000,2000000,3000000,4000000),
                               labels = c(0,1,2,3,4)) +
            ggtitle(paste0("Subject ",input$selected.subject," watching a video that confused them, Video ",input$selected.understanding.video)) +
            theme(legend.position = "right",
                  plot.title = element_text(hjust = 0.5)) +
            scale_color_manual(values = c("red",
                                          "orange",
                                          "yellow",
                                          "green",
                                          "blue",
                                          "purple",
                                          "pink",
                                          "black"))
    })

    output$graph3 <- renderPlot({
        eeg.data.gathered %>%
            filter(Metric == input$selected.boxplot.metric) %>% 
            ggplot(aes(x = as.factor(`user-definedlabeln`),
                       y = log(Value))) +
            geom_boxplot() +
            theme_minimal() + 
            ylab("log Power (volts)") +
            ggtitle(paste0("Distribution of ", input$selected.boxplot.metric, " value for Understanding and Confused Brains")) +
            scale_x_discrete(labels = c("Understanding", "Confused")) +
            theme(axis.title.x = element_blank(),
                  plot.title = element_text(hjust = 0.5))
    })
        }

shinyApp(ui, server)
