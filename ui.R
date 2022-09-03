library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(rsconnect)
library(tidyverse)
library(ggpubr)
library(grid)
library(dashboardthemes)
dados  <- read.csv("dados_topicos.csv")
dados1 <- read.csv("dados_modFineGray.csv")
dados2 <- subset(dados, dados$statusc != 0) 
grad <- subset(dados, dados$statusc == 1)
jub <- subset(dados, dados$statusc == 2)
mud <- subset(dados, dados$statusc == 3)
attach(dados)

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "Alunos de Estatística",
                                    titleWidth = 225,
                       dropdownMenu(
                                      type = "notifications", 
                                      headerText = strong("INFO"), 
                                      icon = icon("spinner"), 
                                      badgeStatus = NULL,
                                      notificationItem(
                                        text = "Alunos do Bacharelado em Estatística",
                                        icon = icon("group")
                                      ),
                                      notificationItem(
                                        text = "Dados coletados entre 2004 e 2012",
                                        icon = icon("calendar")
                                      ),
                                      notificationItem(
                                        text = "Universidade Federal da Bahia",
                                        icon = icon("map")
                                      ),
                                      notificationItem(
                                        text = "por Camille Menezes P dos Santos",
                                        icon = icon("address-card")
                                      )
                                    ),
                                    tags$li(
                                      a(
                                        img(src = "ufba.png", height = 20),
                                        height = 20,
                                        href = " ",
                                        title = "",
                                        target = "_blank"
                                      ),
                                      class = "dropdown"
                                    )
                    ),
                    
                    
                    
                    dashboardSidebar(width = 225,
                                     sidebarMenu(
                                       #menuItem(icon=icon("home"),"Informações de interesse", tabName="item0"),
                                       menuItem(icon = icon("filter"),"Tipificação",tabName = "item2",
                                                          menuItem("Sexo e idade",tabName = "Subb2"),
                                                          menuItem("Cotas",tabName = "Sub",
                                                                   menuItem("Cotas",tabName = "Subb3"),
                                                                   menuItem("Segundo grau",tabName = "Subb4"),
                                                                   menuItem("Escolaridade das mães",tabName = "Subb5"),
                                                                   menuItem("Renda",tabName = "Subb6")),
                                                          menuItem("Reprovações",tabName = "Subb7")),
                                                 menuItem(icon = icon("list"),"Geral",tabName = "item1",
                                                          menuItem("Sexo e idade",tabName = "Sub1"),
                                                          menuItem("Cotas",tabName = "Sub2"),
                                                          menuItem("Reprovações",tabName = "Sub3"))
                                     )),
                    
                    
                    
 #############Sub1####################################################################                     
                    dashboardBody(
                    #tags$head(tags$style(HTML('
                     #           /* logo */
                      #          .skin-blue .main-header .logo {
                       #         background-color:#001f3f ;
                        #        }
#
 #                               /* logo when hovered */
  #                              .skin-blue .main-header .logo:hover {
   #                             background-color: #D7D8D7;
    #                            }
#
 #                               /* navbar (rest of the header) */
  #                              .skin-blue .main-header .navbar {
   #                             background-color: #D7D8D7;
    #                            }        
#
 #                               /* main sidebar */
  #                              .skin-blue .main-sidebar {
   #                             background-color: #001f3f;
    #                            }
#
 #                               /* active selected tab in the sidebarmenu */
  #                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
   #                             background-color: #001f3f;
    #                            }
#
 #                               /* other links in the sidebarmenu */
  #                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
   #                             background-color: #001f3f;
    #                            color: #FFFFFF;
     #                           }
#
 #                               /* other links in the sidebarmenu when hovered */
  #                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
   #                             background-color: #D7D8D7;
    #                            }
     #                           /* toggle button when hovered  */                    
      #                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
       #                         background-color: #FFFFFF;
        #                        }
         #                       /* body  */                    
          #                      .content-wrapper, .right-side {
           #                     background-color: #FFFFFF;
            #                    }
             #                   .nav-tabs {
              #                  background-color: #001f3f;
               #                 border-color: #001f3f;
                #                color: #001f3f;
                 #                }'))),
                      fluidRow(box(width=12,
                                               valueBoxOutput(width = 4, outputId = "alunos"),
                                               valueBoxOutput(width = 2, outputId = "graduados"),
                                               valueBoxOutput(width = 2, outputId = "jubilados"),
                                               valueBoxOutput(width = 2, outputId = "desistiram"),
                                               valueBoxOutput(width = 2, outputId = "censurados")
                    )),
                      
                      
                    
        ###############################################################################33            
                    
                    
                      tabItems(tabItem(tabName = "item0",
                        box(title=h3("Eventos de interesse por sexo",
                                     align="center"))
                        ),
                        tabItem(tabName = "Sub1",
                                box(title=h3("Eventos de interesse por sexo",
                                            align="center"),
                                  solidHeader = F,
                                  plotlyOutput("plot2.1")
                                ),
                                box(title=h3("Idade ao ingressar no curso em cada evento de interesse",
                                             align="center"),
                                  solidHeader = F,
                                  plotlyOutput("plot2.2")
                                )
                              ), 
                      
 #############Sub2####################################################################                     
                      
                      tabItem(tabName = "Sub2",
                              fluidRow(box(
                                title=h3("Eventos de interesse por cotas",
                                             align="center"),
                                solidHeader = F,
                                plotlyOutput("plot2")
                              ),
                              box(
                                title=h3("Eventos de interesse por renda",
                                         align="center"),
                                solidHeader = F,
                                plotlyOutput("plot2.3")
                              )
                              )),         
                      
                      
 ###############Sub3##################################################################                       
                      
 tabItem(tabName = "Sub3",
         fluidRow(
           box(
             title=h3("Reprovações em Cálculo A por evento de interesse",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot2.4")
           ),
           box(
             title=h3("Reprovações em Geometria Analítica por evento de interesse",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot2.5")
           ),
           box(
             title=h3("Reprovações em Probabilidade 1 por evento de interesse",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot2.6")
           )
         )),                      
                      
                      
                      
 ###############Subb2##################################################################                       
 
 tabItem(tabName = "Subb2",
        fluidRow(
           box(
             title=h3("Sexo",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot1"),
             radioGroupButtons(
               inputId = "Wid00",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           ),
           box(
             title=h3("Idade ao ingressar no curso",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot1.3"),
             radioGroupButtons(
               inputId = "Wid4",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           )
         )),
 
                      
 ###############Subb3##################################################################                       
 
                      
 tabItem(tabName = "Subb3",
         fluidRow(
           box(
             title=h3("Cotas",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot1.1"),
             radioGroupButtons(
               inputId = "Wid2",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           ))),                      
                      
                      
 ###############Subb4##################################################################                       
 
 tabItem(tabName = "Subb4",
         fluidRow(
           box(
             title=h3("Tipo de instituição em que cursou o segundo grau",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot1.2"),
             radioGroupButtons(
               inputId = "Wid3",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           ),
           box(
             title=h3("Tipo de instituição em que cursou o segundo grau por cotas",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot3.1"),
             radioGroupButtons(
               inputId = "Wid11",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           )
         )),                       
                      
                      
 
 #######Subb5#################################################################                       
                      
 tabItem(tabName = "Subb5",
         fluidRow(
           box(
             title=h3("Grau de instrução das mães dos alunos",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot1.7"),
             radioGroupButtons(
               inputId = "Wid8",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           ),
           box(
             title=h3("Grau de instrução das mães por cotas",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot3"),
             radioGroupButtons(
               inputId = "Wid12",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           )
         )),                      
                      
                      
                      
 #########Subb6#################################################################
                      
                      
 tabItem(tabName = "Subb6",
         fluidRow(
           box(
             title=h3("Renda familiar dos estudantes",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot1.8"),
             radioGroupButtons(
               inputId = "Wid9",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           ),
           box(
             title=h3("Renda familiar por cotas",
                      align="center"),
             solidHeader = F,
             plotlyOutput("plot3.2"),
             radioGroupButtons(
               inputId = "Wid13",
               label = "",
               choices = c("Total",
                           "Graduados", 
                           "Jubilados",
                           "Transf. ou desistentes"),
               justified = TRUE,
             )
           )
         )),                      
                      
                      
 #######Subb7#################################################################
 
 tabItem(tabName = "Subb7",
         fluidRow(
           box(
             title=h3("Reprovações por disciplina",
                      align="center"),
           solidHeader = F, 
           plotlyOutput("plot4"),
           radioGroupButtons(
             inputId = "Wid20",
             label = "",
             choices = c("Total",
                         "Graduados", 
                         "Jubilados",
                         "Transf. ou desistentes"),
             justified = TRUE,
           )
         ),
           box(
             title=h3("Reprovações em Geometria analítica",
                      align="center"),
               solidHeader = F,
               plotlyOutput("plot1.4"),
               radioGroupButtons(
                 inputId = "Wid5",
                 label = "",
                 choices = c("Total",
                             "Graduados", 
                             "Jubilados",
                             "Transf. ou desistentes"),
                 justified = TRUE,
               )
             ),
             box(
               title=h3("Reprovações em Cálculo A",
                        align="center"),
               solidHeader = F,
               plotlyOutput("plot1.5"),
               radioGroupButtons(
                 inputId = "Wid6",
                 label = "",
                 choices = c("Total",
                             "Graduados", 
                             "Jubilados",
                             "Transf. ou desistentes"),
                 justified = TRUE,
               )
             ),
             box(
               title=h3("Reprovações em Probabilidade 1",
                        align="center"),
               solidHeader = F,
               plotlyOutput("plot1.6"),
               radioGroupButtons(
                 inputId = "Wid7",
                 label = "",
                 choices = c("Total",
                             "Graduados", 
                             "Jubilados",
                             "Transf. ou desistentes"),
                 justified = TRUE,
               )
             ))))))
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                