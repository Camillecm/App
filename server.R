library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(rsconnect)
library(ggpubr)
library(grid)
dados  <- read.csv("dados_topicos.csv")
dados1 <- read.csv("dados_modFineGray.csv")
dados2 <- subset(dados, dados$statusc != 0) 
grad <- subset(dados, dados$statusc == 1)
jub <- subset(dados, dados$statusc == 2)
mud <- subset(dados, dados$statusc == 3)
attach(dados)

server <- function(input, output, session) {
  
#######Boxes##################################################################  
  output$alunos <- renderValueBox({
    input$action
    valueBox(
      nrow(dados), "Alunos do bacharelado em Estatística da UFBA", icon = icon("list"),
      color = "navy"
    )
  })
  
  output$graduados <- renderValueBox({
    input$action
    valueBox(
      nrow(dados[dados$status == 1, ]), "Graduados", icon = icon("group"),
      color = "teal"
    )
  })
  
  output$jubilados <- renderValueBox({
    input$action
    valueBox(
      nrow(dados[dados$status == 2, ]), "Jubilados", icon = icon("group"),
      color = "purple"
    )
  })
  
  output$desistiram <- renderValueBox({
    input$action
    valueBox(
      nrow(dados[dados$status == 3, ]), "Transf. ou desistentes", icon = icon("group"),
      color = "light-blue"
    )
  })
  
  output$censurados <- renderValueBox({
    input$action
    valueBox(
      nrow(dados[dados$status == 0, ]), "Censurados", icon = icon("group"),
      color = "aqua"
    )
  })


  
  
  

  ###### Sub1 #################################################################
  
  
  
  
  output$plot2.1 <- renderPlotly({
    input$action
    A<-round(prop.table(table(dados2$statusc,dados2$sexo),1),3)
    Porcentagem<-A[0:6]
    Sexo<-c(rep("feminino",3),rep("masculino",3))
    Evento<-as.factor(c("graduação","jubilamento",
                        "mudança ou desistência","graduação","jubilamento",
                        "mudança ou desistência"))
    x=data.frame(Porcentagem,Sexo,Evento)
    fig7<-ggplotly(ggplot(x)+
                     geom_col(aes(x = Evento,y=Porcentagem,fill=Sexo),
                              position = position_dodge(0.7),width = 0.5)+
                     scale_x_discrete(name= "Status",labels=c("Graduação","Jubilamento",
                                                              "Mudança ou desistência"))+
                     scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.2))+
                     scale_fill_manual(labels = c("feminino","masculino"),
                                       values = c("#001f3f","#50a3fc"))+
                     theme_minimal())%>% layout(title = " ",
                                                yaxis = list(title = "Porcentagem de alunos"),
                                                xaxis = list(title = "Eventos"))
    fig7
  })
  
  
  output$plot2.2 <- renderPlotly({
    input$action
    fig8<-ggplotly(ggplot(dados2)+aes(x=factor(statusc),y=idade)+
                     geom_boxplot(fill=c("#39cccc","#605ca8","#0073b7"))+
                     scale_y_continuous("Idade",breaks = seq(15,50,5))+
                     scale_x_discrete("Status",labels=c("Graduação","Jubilamento",
                                                        "Mudança ou desistência"))+
                     theme_minimal())%>% layout(title = " ",
                                                yaxis = list(title = "Idade"),
                                                xaxis = list(title = "Eventos"))
    fig8
  })
  
  
  
  
  
  ###### Sub2 #################################################################
  
  
  output$plot2 <- renderPlotly({
    input$action
    A<-round(prop.table(table(dados2$statusc,dados2$cotas),1),3)
    Porcentagem<-A[0:6]
    Cotas<-c(rep("cotista",3),rep("não cotista",3))
    Evento<-as.factor(c("graduação","jubilamento",
                        "mudança desistência","graduação","jubilamento",
                        "mudança desistência"))
    x=data.frame(Porcentagem,Cotas,Evento)
    attach(x)
    fig6<-ggplotly(ggplot(x)+
                     geom_col(aes(x = Evento,y=Porcentagem,fill=Cotas),
                              position = position_dodge(0.7),
                              width = 0.5)+
                     scale_x_discrete(labels=c("Graduação","Jubilamento",
                                               "Mudança ou desistência"))+
                     scale_y_continuous(labels=scales::percent, breaks=seq(0,1,0.2))+
                     scale_fill_manual(name="Cotas",labels = c("cotista","não cotista"),
                                       values = c("#001f3f","#50a3fc"))+
                     theme_minimal())%>% layout(title = "",
                                                yaxis = list(title = "Porcentagem de alunos"),
                                                xaxis = list(title = "Eventos"))
    fig6
  })
  
  output$plot2.3 <- renderPlotly({
    input$action
    A<-round(prop.table(table(dados2$statusc,dados2$renda),1),3)
    Porcentagem<-A[0:18]
    Renda<-c(rep("1 - até um SM",3),
             rep("2 - maior do que 1 e até 3 SM",3),
             rep("3 - maior que 3 e até 5 SM",3),
             rep("4 - maior que 5 e até 10 SM",3),
             rep("5 - maior que 10 e até 20 SM",3),
             rep("6 - maior que 20 e até 40 SM",3))
    Evento<-c("graduaçãp","jubilamento","mudança ou desistência",
              "graduaçãp","jubilamento","mudança ou desistência",
              "graduaçãp","jubilamento","mudança ou desistência",
              "graduaçãp","jubilamento","mudança ou desistência",
              "graduaçãp","jubilamento","mudança ou desistência",
              "graduaçãp","jubilamento","mudança ou desistência")
    Evento=as.factor(Evento)
    Renda=as.factor(Renda)
    x=data.frame(Porcentagem,Renda,Evento)
    fig9<-ggplotly(ggplot(x)+
                     geom_col(aes(x = Evento,y=Porcentagem,fill=Renda),
                              position = position_dodge(0.7),width = 0.5)+
                     scale_x_discrete(labels=c("Graduação","Jubilamento",
                                               "Mudança ou desistência"))+
                     scale_y_continuous(labels=scales::percent,breaks=seq(0,0.5,0.1))+
                     scale_fill_manual(values=c("#0073b7","#9f00b7","#b74400","#17b700",
                                                "#b70060","#a000b7"))+
                     theme_minimal()+
                     theme(axis.text.x=element_text(angle=-0.0001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                 yaxis = list(title = "Porcentagem de alunos"),
                                                                                                 xaxis = list(title = "Eventos"))
    fig9
  })
  
  
  
  
  
  
  
  ###### Sub3 ################################################################# 
  
  
  
  
  
  
  output$plot2.4 <- renderPlotly({
    input$action
    A<-table(factor(dados2$rep_calc),factor(dados2$statusc))
    `Número de alunos`=c(A[0:7],0,A[8:16],0,A[17:25],0,A[26:27])
    Evento<-c(rep("graduação",10),rep("jubilamento",10),rep("mudança de curso ou desistência",10))
    Reprovacoes<-c(0:7,"não cursou","dispensou",0:7,"não cursou","dispensou",0:7,"não cursou","dispensou")
    
    x<-data.frame(`Número de alunos`,Evento,Reprovacoes)
    
    fig10<-ggplotly(ggplot(x)+aes(x = Reprovacoes,y=`Número de alunos`,fill=Evento)+
                      geom_col()+
                      scale_y_continuous("Número de alunos",breaks=seq(0,60,10))+
                      scale_fill_manual(name="Evento de interesse",values=c("#39cccc","#605ca8","#0073b7"))+
                      theme_minimal()+
                      theme(axis.text.x=element_text(angle=-15,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                              yaxis = list(title = "Número de alunos"),
                                                                                              xaxis = list(title = "Reprovações"))
    fig10
  })
  
  output$plot2.5 <- renderPlotly({
    input$action
    A<-table(factor(dados2$rep_ga),factor(dados2$statusc))
    `Número de alunos`=c(A[0:7],0,A[8:16],0,A[17:25],0,A[26:27])
    Evento<-c(rep("graduação",10),rep("jubilamento",10),rep("mudança de curso ou desistência",10))
    Reprovacoes<-c(0:7,"não cursou","dispensou",0:7,"não cursou","dispensou",0:7,"não cursou","dispensou")
    
    x<-data.frame(`Número de alunos`,Evento,Reprovacoes)
    
    fig11<-ggplotly(ggplot(x)+geom_col(aes(x = Reprovacoes,y=`Número de alunos`,fill=Evento))+
                      scale_y_continuous("Número de alunos",breaks=seq(0,60,10))+
                      scale_fill_manual(name="Evento de interesse",values=c("#39cccc","#605ca8","#0073b7"))+
                      theme_minimal()+
                      theme(axis.text.x=element_text(angle=-15,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                              yaxis = list(title = "Número de alunos"),
                                                                                              xaxis = list(title = "Reprovações"))
    fig11
  })
  
  output$plot2.6 <- renderPlotly({
    input$action  
    A<-table(factor(dados2$rep_prob),factor(dados2$statusc))
    `Número de alunos`=c(A[0:5],rep(0,3),A[6:12],rep(0,3),A[13:19],rep(0,3),A[20:21])
    Evento<-c(rep("graduação",10),rep("jubilamento",10),rep("mudança de curso ou desistência",10))
    Reprovacoes<-c(0:7,"não cursou","dispensou",0:7,"não cursou","dispensou",0:7,"não cursou","dispensou")
    
    x<-data.frame(`Número de alunos`,Evento,Reprovacoes)
    
    fig12<-ggplotly(ggplot(x)+geom_col(aes(x = Reprovacoes,y=`Número de alunos`,fill=Evento))+
                      scale_y_continuous("Número de alunos",breaks=seq(0,80,10))+
                      scale_fill_manual(name="Evento de interesse",values=c("#39cccc","#605ca8","#0073b7"))+
                      theme_minimal()+
                      theme(axis.text.x=element_text(angle=-15,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                              yaxis = list(title = "Número de alunos"),
                                                                                              xaxis = list(title = "Reprovações"))
    fig12
  })
  
  
  
  
#####subb2#################################################################
  
  

  
  output$plot1 <- renderPlotly({
    input$action
    if (input$Wid00=="Total") {
      df <- data.frame(
        Sexo = c("Feminino","Masculino"),
        value = table(sexo))
      fig<-plot_ly(data = df, 
              labels = ~Sexo, 
              values = ~df$value.Freq,
              marker = list(colors = c("#001f3f","#50a3fc")),
              type = 'pie', hole = 0.6,
              textinfo='label+percent',
              showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
    if (input$Wid00=="Graduados") {
      df <- data.frame(
        Sexo = c("Feminino","Masculino"),
        value = table(grad$sexo))
      fig<-plot_ly(data = df, 
              labels = ~Sexo, 
              values = ~df$value.Freq,
              marker = list(colors = c("#39cccc","#217777")),
              type = 'pie', hole = 0.6,
              textinfo='label+percent',
              showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    if (input$Wid00=="Jubilados") { 
      df <- data.frame(
        Sexo = c("Feminino","Masculino"),
        value = table(jub$sexo))
      fig<-plot_ly(data = df, 
              labels = ~Sexo, 
              values = ~df$value.Freq,
              marker = list(colors = c("#605ca8","#2f2d53")),
              type = 'pie', hole = 0.6,
              textinfo='label+percent',
              showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      }
    if (input$Wid00=="Transf. ou desistentes") { 
      df <- data.frame(
        Sexo = c("Feminino","Masculino"),
        value = table(mud$sexo))
     fig<-plot_ly(data = df, 
              labels = ~Sexo, 
              values = ~df$value.Freq,
              marker = list(colors = c("#0073b7","#003e62")),
              type = 'pie', hole = 0.6,
              textinfo='label+percent',
              showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      }
    fig
    }) 
    
  
  
  
  output$plot1.3 <- renderPlotly({
    input$action
    if (input$Wid4=="Total") {
      fig<-ggplotly(dados%>%
                      group_by(idade)%>%
                      ggplot(aes(x=1,y=idade,fill=idade))+
                      geom_boxplot(width = 0.4,fill=c("#001f3f"))+
                      scale_y_continuous(breaks=seq(18,60,4))+
                      scale_x_discrete(" ",limits=c(" "))+
                      theme_minimal())%>% layout(title = "",
                                                 xaxis = list(title = ""),
                                                 yaxis = list(title = "Idade"))
    }
    
    if (input$Wid4=="Graduados") {
      fig<-ggplotly(grad%>%
                      group_by(idade)%>%
                      ggplot(aes(x=1,y=idade,fill=idade))+
                      geom_boxplot(width = 0.4,fill=c("#39cccc"))+
                      scale_y_continuous(breaks=seq(18,60,4))+
                      scale_x_discrete(" ",limits=c(" "))+
                      theme_minimal())%>% layout(title = "",
                                                 xaxis = list(title = ""),
                                                 yaxis = list(title = "Idade"))
    }
    
    if (input$Wid4=="Jubilados") {
      fig<-ggplotly(jub%>%
                      group_by(idade)%>%
                      ggplot(aes(x=1,y=idade,fill=idade))+
                      geom_boxplot(width = 0.4,fill=c("#605ca8"))+
                      scale_y_continuous(breaks=seq(18,60,4))+
                      scale_x_discrete(" ",limits=c(" "))+
                      theme_minimal())%>% layout(title = "",
                                                 xaxis = list(title = ""),
                                                 yaxis = list(title = "Idade"))
    }
    
    if (input$Wid4=="Transf. ou desistentes") {
      fig<-ggplotly(mud%>%
                      group_by(idade)%>%
                      ggplot(aes(x=1,y=idade,fill=idade))+
                      geom_boxplot(width = 0.4,fill=c("#0073b7"))+
                      scale_y_continuous(breaks=seq(18,60,4))+
                      scale_x_discrete(" ",limits=c(" "))+
                      theme_minimal())%>% layout(title = "",
                                                 xaxis = list(title = ""),
                                                 yaxis = list(title = "Idade"))
    }
    
    fig
  })
  
  
  
  
  #####subb3#################################################################
  
  
  
  output$plot1.1 <- renderPlotly({
    input$action
    if (input$Wid2=="Total") {
      df <- data.frame(
        Cotas = c("Cotistas","Não cotistas"),
        value = table(cotas))
      fig<-plot_ly(data = df, 
                   labels = ~Cotas, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#001f3f","#50a3fc")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    
    if (input$Wid2=="Graduados") {
      df <- data.frame(
        Cotas = c("Cotistas","Não cotistas"),
        value = table(grad$cotas))
       fig<-plot_ly(data = df, 
                   labels = ~Cotas, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#39cccc","#217777")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      }  
    
    if (input$Wid2=="Jubilados") {
      df <- data.frame(
        Cotas = c("Cotistas","Não cotistas"),
        value = table(jub$cotas))
     fig<-plot_ly(data = df, 
                   labels = ~Cotas, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#605ca8","#2f2d53")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
 
    } 
    
    if (input$Wid2=="Transf. ou desistentes") {
      df <- data.frame(
        Cotas = c("Cotistas","Não cotistas"),
        value = table(mud$cotas))
      fig<-plot_ly(data = df, 
                   labels = ~Cotas, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#0073b7","#003e62")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      } 
    fig
  })
  
  
  
  
  
  
  
  
  #####subb4################################################################# 
  
  
  
  output$plot1.2 <- renderPlotly({
    input$action
    
    if (input$Wid3=="Total") {
      df <- data.frame(
        Segundo_grau = c("Pública","Privada"),
        value = table(seg_grau))
     fig<-plot_ly(data = df, 
                   labels = ~Segundo_grau, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#001f3f","#50a3fc")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      
    }
    
    if (input$Wid3=="Graduados") {
      df <- data.frame(
        Segundo_grau = c("Pública","Privada"),
        value = table(grad$seg_grau))
      fig<-plot_ly(data = df, 
                   labels = ~Segundo_grau, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#39cccc","#217777")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
     }  
    
    if (input$Wid3=="Jubilados") {
      df <- data.frame(
        Segundo_grau = c("Pública","Privada"),
        value = table(jub$seg_grau))
      fig<-plot_ly(data = df, 
                   labels = ~Segundo_grau, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#605ca8","#2f2d53")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      } 
    
    if (input$Wid3=="Transf. ou desistentes") {
      df <- data.frame(
        Segundo_grau = c("Pública","Privada"),
        value = round(prop.table(table(mud$seg_grau))*100))
      fig<-plot_ly(data = df, 
                   labels = ~Segundo_grau, 
                   values = ~df$value.Freq,
                   marker = list(colors = c("#0073b7","#003e62")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      } 
    fig
  })
  
  
  
  
  
  output$plot3.1 <- renderPlotly({
    input$action  
    if (input$Wid11=="Total") {
    A<-round(prop.table(table(dados$cotas,dados$seg_grau),2),4)
    Porcentagem<-A[0:4]
    Cotas<-c("cotista","não cotista","cotista","não cotista")
    Instituicao<-c("pública","pública","privada","privada")
    
    x<-data.frame(Instituicao,Porcentagem,Cotas)
    
    fig13 <- ggplotly(x%>%
                        group_by(Instituicao,Porcentagem,Cotas)%>%
                        ggplot(aes(x = Instituicao,y=Porcentagem,fill=Cotas))+
                        geom_col(position = position_dodge(0.7),
                                 width = 0.5)+
                        scale_x_discrete(labels=c("Privada","Pública"))+
                        scale_y_continuous(labels=scales::percent,
                                           breaks=seq(0,1,0.2))+
                        scale_fill_manual(name="Cotas",labels = c("Cotista","Não cotista"),
                                          values = c("#001f3f","#50a3fc"))+
                        theme_minimal())%>% layout(title = "",
                                                   yaxis = list(title = "Porcentagem de alunos"),
                                                   xaxis = list(title = "Tipo de instituição"))
    }
    
    if (input$Wid11=="Graduados") {
      A<-round(prop.table(table(grad$cotas,grad$seg_grau),2),4)
      Porcentagem<-A[0:4]
      Cotas<-c("cotista","não cotista","cotista","não cotista")
      Instituicao<-c("pública","pública","privada","privada")
      
      x<-data.frame(Instituicao,Porcentagem,Cotas)
      
      fig13 <- ggplotly(x%>%
                          group_by(Instituicao,Porcentagem,Cotas)%>%
                          ggplot(aes(x = Instituicao,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),
                                   width = 0.5)+
                          scale_x_discrete(labels=c("Privada","Pública"))+
                          scale_y_continuous(labels=scales::percent,
                                             breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",labels = c("Cotista","Não cotista"),
                                            values = c("#39cccc","#217777"))+
                          theme_minimal())%>% layout(title = "",
                                                     yaxis = list(title = "Porcentagem de alunos"),
                                                     xaxis = list(title = "Tipo de instituição"))
      
    }
    
    if (input$Wid11=="Jubilados") {
      A<-round(prop.table(table(jub$cotas,jub$seg_grau),2),4)
      Porcentagem<-A[0:4]
      Cotas<-c("cotista","não cotista","cotista","não cotista")
      Instituicao<-c("pública","pública","privada","privada")
      
      x<-data.frame(Instituicao,Porcentagem,Cotas)
      
      fig13 <- ggplotly(x%>%
                          group_by(Instituicao,Porcentagem,Cotas)%>%
                          ggplot(aes(x = Instituicao,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),
                                   width = 0.5)+
                          scale_x_discrete(labels=c("Privada","Pública"))+
                          scale_y_continuous(labels=scales::percent,
                                             breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",labels = c("Cotista","Não cotista"),
                                            values = c("#605ca8","#2f2d53"))+
                          theme_minimal())%>% layout(title = "",
                                                     yaxis = list(title = "Porcentagem de alunos"),
                                                     xaxis = list(title = "Tipo de instituição"))
      

    }
    if (input$Wid11=="Transf. ou desistentes") {
      A<-round(prop.table(table(mud$cotas,mud$seg_grau),2),4)
      Porcentagem<-A[0:4]
      Cotas<-c("cotista","não cotista","cotista","não cotista")
      Instituicao<-c("pública","pública","privada","privada")
      
      x<-data.frame(Instituicao,Porcentagem,Cotas)
      
      fig13 <- ggplotly(x%>%
                          group_by(Instituicao,Porcentagem,Cotas)%>%
                          ggplot(aes(x = Instituicao,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),
                                   width = 0.5)+
                          scale_x_discrete(labels=c("Privada","Pública"))+
                          scale_y_continuous(labels=scales::percent,
                                             breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",labels = c("Cotista","Não cotista"),
                                            values = c("#0073b7","#003e62"))+
                          theme_minimal())%>% layout(title = "",
                                                     yaxis = list(title = "Porcentagem de alunos"),
                                                     xaxis = list(title = "Tipo de instituição"))
      
      
      
    }
    fig13
    })
  
  
  
  
  
  #####subb5################################################################# 
  output$plot3 <- renderPlotly({
    input$action    
    if (input$Wid12=="Total") {
    tab3 <- table(dados$cotas,dados$instmae)
    A<-round(prop.table(tab3,2),4)
    Porcentagem<-A[0:8]
    Cotas<-c("cotista","não cotista","cotista","não cotista","cotista","não cotista",
             "cotista","não cotista")
    Grau<-c(rep("0 - Fundamental incompleto ou abaixo",2),rep("1 - Fundamental completo",2),
            rep("2 - Ensino Médio completo",2),rep("3 - Superior completo",2))
    x<-data.frame(Porcentagem,Grau,Cotas)
    
    fig12 <- ggplotly(x%>%
                        group_by(Porcentagem,Grau,Cotas)%>%
                        ggplot(aes(x = Grau,y=Porcentagem,fill=Cotas))+
                        geom_col(position = position_dodge(0.7),width = 0.5)+
                        scale_x_discrete(labels=c("Fundamental incompleto ou abaixo",
                                                  "Fundamental completo",
                                                  "Ensino Médio completo",
                                                  "Superior completo"))+
                        scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.2))+
                        scale_fill_manual(name="Cotas",values = c("#001f3f","#50a3fc"))+
                        theme_minimal()+
                        theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                yaxis = list(title = "Porcentagem de alunos"),
                                                                                                xaxis = list(title = "Grau de instrução"))
    }
    
    if (input$Wid12=="Graduados") {
        tab3 <- table(grad$cotas,grad$instmae)
        A<-round(prop.table(tab3,2),4)
        Porcentagem<-A[0:8]
        Cotas<-c("cotista","não costista","cotista","não costista","cotista","não costista",
                 "cotista","não costista")
        Grau<-c(rep("0 - Fundamental incompleto ou abaixo",2),rep("1 - Fundamental completo",2),
                rep("2 - Ensino Médio completo",2),rep("3 - Superior completo",2))
        x<-data.frame(Porcentagem,Grau,Cotas)
        
        fig12 <- ggplotly(x%>%
                            group_by(Porcentagem,Grau,Cotas)%>%
                            ggplot(aes(x = Grau,y=Porcentagem,fill=Cotas))+
                            geom_col(position = position_dodge(0.7),width = 0.5)+
                            scale_x_discrete(labels=c("Fundamental incompleto ou abaixo",
                                                      "Fundamental completo",
                                                      "Ensino Médio completo",
                                                      "Superior completo"))+
                            scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.2))+
                            scale_fill_manual(name="Cotas",values = c("#39cccc","#217777"))+
                            theme_minimal()+
                            theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                    yaxis = list(title = "Porcentagem de alunos"),
                                                                                                    xaxis = list(title = "Grau de instrução"))
        
    }
    
    if (input$Wid12=="Jubilados") {
      tab3 <- table(jub$cotas,jub$instmae)
      A<-round(prop.table(tab3,2),4)
      Porcentagem<-A[0:8]
      Cotas<-c("cotista","não costista","cotista","não costista","cotista","não costista",
               "cotista","não costista")
      Grau<-c(rep("0 - Fundamental incompleto ou abaixo",2),rep("1 - Fundamental completo",2),
              rep("2 - Ensino Médio completo",2),rep("3 - Superior completo",2))
      x<-data.frame(Porcentagem,Grau,Cotas)
      
      fig12 <- ggplotly(x%>%
                          group_by(Porcentagem,Grau,Cotas)%>%
                          ggplot(aes(x = Grau,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),width = 0.5)+
                          scale_x_discrete(labels=c("Fundamental incompleto ou abaixo",
                                                    "Fundamental completo",
                                                    "Ensino Médio completo",
                                                    "Superior completo"))+
                          scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",values = c("#605ca8","#2f2d53"))+
                          theme_minimal()+
                          theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                  yaxis = list(title = "Porcentagem de alunos"),
                                                                                                  xaxis = list(title = "Grau de instrução"))
      
      
    }
    if (input$Wid12=="Transf. ou desistentes") {
      tab3 <- table(mud$cotas,mud$instmae)
      A<-round(prop.table(tab3,2),4)
      Porcentagem<-A[0:8]
      Cotas<-c("cotista","não costista","cotista","não costista","cotista","não costista",
               "cotista","não costista")
      Grau<-c(rep("0 - Fundamental incompleto ou abaixo",2),rep("1 - Fundamental completo",2),
              rep("2 - Ensino Médio completo",2),rep("3 - Superior completo",2))
      x<-data.frame(Porcentagem,Grau,Cotas)
      
      fig12 <- ggplotly(x%>%
                          group_by(Porcentagem,Grau,Cotas)%>%
                          ggplot(aes(x = Grau,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),width = 0.5)+
                          scale_x_discrete(labels=c("Fundamental incompleto ou abaixo",
                                                    "Fundamental completo",
                                                    "Ensino Médio completo",
                                                    "Superior completo"))+
                          scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",values = c("#0073b7","#003e62"))+
                          theme_minimal()+
                          theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                  yaxis = list(title = "Porcentagem de alunos"),
                                                                                                  xaxis = list(title = "Grau de instrução"))
      
      
    } 
    fig12
  })
  
  
  output$plot1.7 <- renderPlotly({
    input$action
    if (input$Wid8=="Total") {
      Porcentagem<-round(prop.table(table(factor(dados$instmae))),3)
      Grau<-c(1:4)
      x<-data.frame(Porcentagem,Grau)
      
      fig4<-ggplotly(ggplot(x)+aes(x=Grau,y=Porcentagem)+
                       geom_col(fill="#001f3f")+
                       scale_y_continuous(labels=scales::percent) +
                       scale_x_discrete(limits=c("Fundamental Incompleto ou abaixo","Fundamental Completo",
                                                 "Ensino Médio Completo", "Superior Completo"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                               xaxis = list(title = "Grau"),
                                                                                               yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid8=="Graduados") {
      Porcentagem<-round(prop.table(table(factor(grad$instmae))),3)
      Grau<-c(1:4)
      x<-data.frame(Porcentagem,Grau)
      
      fig4<-ggplotly(ggplot(x)+aes(x=Grau,y=Porcentagem)+
                       geom_col(fill="#39cccc")+
                       scale_y_continuous(labels=scales::percent) +
                       scale_x_discrete(limits=c("Fundamental Incompleto ou abaixo","Fundamental Completo",
                                                 "Ensino Médio Completo", "Superior Completo"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                               xaxis = list(title = "Grau"),
                                                                                               yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid8=="Jubilados") {
      Porcentagem<-round(prop.table(table(factor(jub$instmae))),3)
      Grau<-c(1:4)
      x<-data.frame(Porcentagem,Grau)
      
      fig4<-ggplotly(ggplot(x)+aes(x=Grau,y=Porcentagem)+
                       geom_col(fill="#605ca8")+
                       scale_y_continuous(labels=scales::percent) +
                       scale_x_discrete(limits=c("Fundamental Incompleto ou abaixo","Fundamental Completo",
                                                 "Ensino Médio Completo", "Superior Completo"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                               xaxis = list(title = "Grau"),
                                                                                               yaxis = list(title = "Porcentagem de alunos"))
      
      
    }
    
    if (input$Wid8=="Transf. ou desistentes") {
      Porcentagem<-round(prop.table(table(factor(mud$instmae))),3)
      Grau<-c(1:4)
      x<-data.frame(Porcentagem,Grau)
      
      fig4<-ggplotly(ggplot(x)+aes(x=Grau,y=Porcentagem)+
                       geom_col(fill="#0073b7")+
                       scale_y_continuous(labels=scales::percent) +
                       scale_x_discrete(limits=c("Fundamental Incompleto ou abaixo","Fundamental Completo",
                                                 "Ensino Médio Completo", "Superior Completo"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-10,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                               xaxis = list(title = "Grau"),
                                                                                               yaxis = list(title = "Porcentagem de alunos"))
      
      
    }
    fig4
  })
  
  
  

  #####subb6#################################################################  
  
  

  
  
  
  output$plot3.2 <- renderPlotly({
    input$action
    if (input$Wid13=="Total") {
   A<-round(prop.table(table(dados$cotas,dados$renda),2),3)
    Porcentagem<-A[0:12]
    Cotas<-c("cotista","não cotista","cotista","não cotista","cotista","não cotista",
             "cotista","não cotista","cotista","não cotista","cotista","não cotista")
    Renda<-Renda<-c(rep("1 - até um SM",2),
                    rep("2 - maior do que 1 e até 3 SM",2),
                    rep("3 - maior que 3 e até 5 SM",2),
                    rep("4 - maior que 5 e até 10 SM",2),
                    rep("5 - maior que 10 e até 20 SM",2),
                    rep("6 - maior que 20 e até 40 SM",2))
    x<-data.frame(Renda,Porcentagem,Cotas)
    fig14 <- ggplotly(x%>%
                        group_by(Renda,Porcentagem,Cotas)%>%
                        ggplot(aes(x = Renda,y=Porcentagem,fill=Cotas))+
                        geom_col(position = position_dodge(0.7),
                                 width = 0.5)+
                        scale_x_discrete(labels=c("Até um SM",
                                                  "Maior do que 1 e até 3 SM",
                                                  "Maior que 3 e até 5 SM",
                                                  "Maior que 5 e até 10 SM",
                                                  "Maior que 10 e até 20 SM",
                                                  "Maior que 20 e até 40 SM"))+
                        scale_y_continuous(labels=scales::percent,
                                           breaks=seq(0,1,0.2))+
                        scale_fill_manual(name="Cotas",labels = c("cotista","não cotista"),
                                          values = c("#001f3f","#50a3fc"))+
                        theme_minimal()+
                        theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                   yaxis = list(title = "Porcentagem de alunos"),
                                                   xaxis = list(title = "Renda"))
    }
    if (input$Wid13=="Graduados") {
      A<-round(prop.table(table(grad$cotas,grad$renda),2),3)
      Porcentagem<-A[0:12]
      Cotas<-c("cotista","não cotista","cotista","não cotista","cotista","não cotista",
               "cotista","não cotista","cotista","não cotista","cotista","não cotista")
      Renda<-Renda<-c(rep("1 - até um SM",2),
                      rep("2 - maior do que 1 e até 3 SM",2),
                      rep("3 - maior que 3 e até 5 SM",2),
                      rep("4 - maior que 5 e até 10 SM",2),
                      rep("5 - maior que 10 e até 20 SM",2),
                      rep("6 - maior que 20 e até 40 SM",2))
      x<-data.frame(Renda,Porcentagem,Cotas)
      fig14 <- ggplotly(x%>%
                          group_by(Renda,Porcentagem,Cotas)%>%
                          ggplot(aes(x = Renda,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),
                                   width = 0.5)+
                          scale_x_discrete(labels=c("Até um SM",
                                                    "Maior do que 1 e até 3 SM",
                                                    "Maior que 3 e até 5 SM",
                                                    "Maior que 5 e até 10 SM",
                                                    "Maior que 10 e até 20 SM",
                                                    "Maior que 20 e até 40 SM"))+
                          scale_y_continuous(labels=scales::percent,
                                             breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",labels = c("cotista","não cotista"),
                                            values = c("#39cccc","#217777"))+
                          theme_minimal()+
                          theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                  yaxis = list(title = "Porcentagem de alunos"),
                                                                                                  xaxis = list(title = "Renda"))
      
    }
    
    if (input$Wid13=="Jubilados") {
      A<-round(prop.table(table(jub$cotas,jub$renda),2),3)
      Porcentagem<-A[0:12]
      Cotas<-c("cotista","não cotista","cotista","não cotista","cotista","não cotista",
               "cotista","não cotista","cotista","não cotista","cotista","não cotista")
      Renda<-Renda<-c(rep("1 - até um SM",2),
                      rep("2 - maior do que 1 e até 3 SM",2),
                      rep("3 - maior que 3 e até 5 SM",2),
                      rep("4 - maior que 5 e até 10 SM",2),
                      rep("5 - maior que 10 e até 20 SM",2),
                      rep("6 - maior que 20 e até 40 SM",2))
      x<-data.frame(Renda,Porcentagem,Cotas)
      fig14 <- ggplotly(x%>%
                          group_by(Renda,Porcentagem,Cotas)%>%
                          ggplot(aes(x = Renda,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),
                                   width = 0.5)+
                          scale_x_discrete(labels=c("Até um SM",
                                                    "Maior do que 1 e até 3 SM",
                                                    "Maior que 3 e até 5 SM",
                                                    "Maior que 5 e até 10 SM",
                                                    "Maior que 10 e até 20 SM",
                                                    "Maior que 20 e até 40 SM"))+
                          scale_y_continuous(labels=scales::percent,
                                             breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",labels = c("cotista","não cotista"),
                                            values = c("#605ca8","#2f2d53"))+
                          theme_minimal()+
                          theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                  yaxis = list(title = "Porcentagem de alunos"),
                                                                                                  xaxis = list(title = "Renda"))
      
    }
    if (input$Wid13=="Transf. ou desistentes") {
      A<-round(prop.table(table(mud$cotas,mud$renda),2),3)
      Porcentagem<-A[0:12]
      Cotas<-c("cotista","não cotista","cotista","não cotista","cotista","não cotista",
               "cotista","não cotista","cotista","não cotista","cotista","não cotista")
      Renda<-Renda<-c(rep("1 - até um SM",2),
                      rep("2 - maior do que 1 e até 3 SM",2),
                      rep("3 - maior que 3 e até 5 SM",2),
                      rep("4 - maior que 5 e até 10 SM",2),
                      rep("5 - maior que 10 e até 20 SM",2),
                      rep("6 - maior que 20 e até 40 SM",2))
      x<-data.frame(Renda,Porcentagem,Cotas)
      fig14 <- ggplotly(x%>%
                          group_by(Renda,Porcentagem,Cotas)%>%
                          ggplot(aes(x = Renda,y=Porcentagem,fill=Cotas))+
                          geom_col(position = position_dodge(0.7),
                                   width = 0.5)+
                          scale_x_discrete(labels=c("Até um SM",
                                                    "Maior do que 1 e até 3 SM",
                                                    "Maior que 3 e até 5 SM",
                                                    "Maior que 5 e até 10 SM",
                                                    "Maior que 10 e até 20 SM",
                                                    "Maior que 20 e até 40 SM"))+
                          scale_y_continuous(labels=scales::percent,
                                             breaks=seq(0,1,0.2))+
                          scale_fill_manual(name="Cotas",labels = c("cotista","não cotista"),
                                            values = c("#0073b7","#003e62"))+
                          theme_minimal()+
                          theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                  yaxis = list(title = "Porcentagem de alunos"),
                                                                                                  xaxis = list(title = "Renda"))
      
      
    } 
    fig14
  })
  
  
 
  
  output$plot1.8 <- renderPlotly({
    input$action
    if (input$Wid9=="Total") {
      Porcentagem<-round(prop.table(table(factor(dados$renda))),3)
      Renda<-c(1:6)
      x<-data.frame(Porcentagem,Renda)
      
      fig5<-ggplotly(ggplot(x)+
                       aes(x=Renda,y=Porcentagem)+
                       geom_col(fill="#001f3f")+
                       scale_y_continuous(labels=scales::percent,breaks=seq(0,0.3,0.1))+
                       scale_x_discrete(limits=c("Até um SM",
                                                 "Maior do que 1 e até 3 SM",
                                                 "Maior que 3 e até 5 SM",
                                                 "Maior que 5 e até 10 SM",
                                                 "Maior que 10 e até 20 SM",
                                                 "Maior que 20 e até 40 SM"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                               yaxis = list(title = "Porcentagem de alunos"),
                                                                                               xaxis = list(title = "Renda"))
      
      
    }
    
    if (input$Wid9=="Graduados") {
      Porcentagem<-round(prop.table(table(factor(grad$renda))),3)
      Renda<-c(1:6)
      x<-data.frame(Porcentagem,Renda)
      
      fig5<-ggplotly(ggplot(x)+
                       aes(x=Renda,y=Porcentagem)+
                       geom_col(fill="#39cccc")+
                       scale_y_continuous(labels=scales::percent,breaks=seq(0,0.3,0.1))+
                       scale_x_discrete(limits=c("Até um SM",
                                                 "Maior do que 1 e até 3 SM",
                                                 "Maior que 3 e até 5 SM",
                                                 "Maior que 5 e até 10 SM",
                                                 "Maior que 10 e até 20 SM",
                                                 "Maior que 20 e até 40 SM"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = " ",
                                                                                               yaxis = list(title = "Porcentagem de alunos"),
                                                                                               xaxis = list(title = "Renda"))
      
      
    }
    
    if (input$Wid9=="Jubilados") {
      Porcentagem<-round(prop.table(table(factor(jub$renda))),3)
      Renda<-c(1:6)
      x<-data.frame(Porcentagem,Renda)
      
      fig5<-ggplotly(ggplot(x)+
                       aes(x=Renda,y=Porcentagem)+
                       geom_col(fill="#605ca8")+
                       scale_y_continuous(labels=scales::percent,breaks=seq(0,0.3,0.1))+
                       scale_x_discrete(limits=c("Até um SM",
                                                 "Maior do que 1 e até 3 SM",
                                                 "Maior que 3 e até 5 SM",
                                                 "Maior que 5 e até 10 SM",
                                                 "Maior que 10 e até 20 SM",
                                                 "Maior que 20 e até 40 SM"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = " ",
                                                                                               yaxis = list(title = "Porcentagem de alunos"),
                                                                                               xaxis = list(title = "Renda"))
      
      
    }
    
    if (input$Wid9=="Transf. ou desistentes") {
      Porcentagem<-round(prop.table(table(factor(mud$renda))),3)
      Renda<-c(1:6)
      x<-data.frame(Porcentagem,Renda)
      
      fig5<-ggplotly(ggplot(x)+
                       aes(x=Renda,y=Porcentagem)+
                       geom_col(fill="#0073b7")+
                       scale_y_continuous(labels=scales::percent,breaks=seq(0,0.3,0.1))+
                       scale_x_discrete(limits=c("Até um SM",
                                                 "Maior do que 1 e até 3 SM",
                                                 "Maior que 3 e até 5 SM",
                                                 "Maior que 5 e até 10 SM",
                                                 "Maior que 10 e até 20 SM",
                                                 "Maior que 20 e até 40 SM"))+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-20,vjust=0.5,hjust=1)))%>% layout(title = " ",
                                                                                               yaxis = list(title = "Porcentagem de alunos"),
                                                                                               xaxis = list(title = "Renda"))
      
      
    }
    fig5
  })
  
  
  
  
  
  ###### Subb7 #################################################################
  
  output$plot4 <- renderPlotly({
    input$action   
    if (input$Wid20=="Total") {
    Porcentagem<-c(0.3276,0.5632,0.1024,0.057,0.2299,0.5402,0.0920,0.1379,0.2414,0.2701,0.0575,0.4310)
    Disciplina<-as.factor(c(rep("geometria analítica",4),rep("cálculo A",4),
                            rep("probabilidade 1",4)))
    Reprovacoes<-as.factor(c("sem reprovação","com reprovação","dispensou","não cursou",
                             "sem reprovação","com reprovação","dispensou","não cursou",
                             "sem reprovação","com reprovação","dispensou","não cursou"))
    
    x<-data.frame(Porcentagem,Disciplina,Reprovacoes)
    
    ordem_dis<-c("geometria analítica","cálculo A","probabilidade 1")
    ordem_rep<-c("sem reprovação","com reprovação","dispensou","não cursou")
    
    fig16 <- ggplotly(x%>%
                        group_by(Porcentagem,Disciplina,Reprovacoes)%>%
                        mutate(Disciplina=fct_relevel(Disciplina,ordem_dis),
                               Reprovacoes=fct_relevel(Reprovacoes,ordem_rep))%>%
                        ggplot(aes(x=Reprovacoes,y=Porcentagem,fill=Disciplina))+
                        geom_col(position = position_dodge(0.7),width = 0.5)+
                        scale_x_discrete(labels=c("Sem reprovações", "Com reprovações",
                                                            "Dispensou","Não cursou"))+
                        scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.1))+
                        scale_fill_manual(name="Disciplinas",values=c("#0073b7","#b70073","#73b700"))+
                        theme_minimal())%>% layout(title = "",
                                                   yaxis = list(title = "Porcentagem de alunos"),
                                                   xaxis = list(title = "Reprovações"))
  }
    if (input$Wid20=="Graduados") {
      rep_g<-prop.table(c(31,11,5,0))
      rep_c<-prop.table(c(20,22,5,0))
      rep_p<-prop.table(c(30,14,3,0))
      
      Porcentagem<-c(rep_g,rep_c,rep_p)
      Disciplina<-as.factor(c(rep("geometria analítica",4),rep("cálculo A",4),
                              rep("probabilidade 1",4)))
      Reprovacoes<-as.factor(c("sem reprovação","com reprovação","dispensou","não cursou",
                               "sem reprovação","com reprovação","dispensou","não cursou",
                               "sem reprovação","com reprovação","dispensou","não cursou"))
      
      x<-data.frame(Porcentagem,Disciplina,Reprovacoes)
      
      ordem_dis<-c("geometria analítica","cálculo A","probabilidade 1")
      ordem_rep<-c("sem reprovação","com reprovação","dispensou","não cursou")
      
      fig16 <- ggplotly(x%>%
                          group_by(Porcentagem,Disciplina,Reprovacoes)%>%
                          mutate(Disciplina=fct_relevel(Disciplina,ordem_dis),
                                 Reprovacoes=fct_relevel(Reprovacoes,ordem_rep))%>%
                          ggplot(aes(x=Reprovacoes,y=Porcentagem,fill=Disciplina))+
                          geom_col(position = position_dodge(0.7),width = 0.5)+
                          scale_x_discrete(labels=c("Sem reprovações", "Com reprovações",
                                                    "Dispensou","Não cursou"))+
                          scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.1))+
                          scale_fill_manual(name="Disciplinas",values=c("#0073b7","#b70073","#73b700"))+
                          theme_minimal())%>% layout(title = "",
                                                     yaxis = list(title = "Porcentagem de alunos"),
                                                     xaxis = list(title = "Reprovações"))
     }
    
    if (input$Wid20=="Jubilados") {
      rep_g<-prop.table(c(19,69,7,1))
      rep_c<-prop.table(c(14,54,5,22))
      rep_p<-prop.table(c(9,19,4,64))
      
      Porcentagem<-c(rep_g,rep_c,rep_p)
      Disciplina<-as.factor(c(rep("geometria analítica",4),rep("cálculo A",4),
                              rep("probabilidade 1",4)))
      Reprovacoes<-as.factor(c("sem reprovação","com reprovação","dispensou","não cursou",
                               "sem reprovação","com reprovação","dispensou","não cursou",
                               "sem reprovação","com reprovação","dispensou","não cursou"))
      
      x<-data.frame(Porcentagem,Disciplina,Reprovacoes)
      
      ordem_dis<-c("geometria analítica","cálculo A","probabilidade 1")
      ordem_rep<-c("sem reprovação","com reprovação","dispensou","não cursou")
      
      fig16 <- ggplotly(x%>%
                          group_by(Porcentagem,Disciplina,Reprovacoes)%>%
                          mutate(Disciplina=fct_relevel(Disciplina,ordem_dis),
                                 Reprovacoes=fct_relevel(Reprovacoes,ordem_rep))%>%
                          ggplot(aes(x=Reprovacoes,y=Porcentagem,fill=Disciplina))+
                          geom_col(position = position_dodge(0.7),width = 0.5)+
                          scale_x_discrete(labels=c("Sem reprovações", "Com reprovações",
                                                    "Dispensou","Não cursou"))+
                          scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.1))+
                          scale_fill_manual(name="Disciplinas",values=c("#0073b7","#b70073","#73b700"))+
                          theme_minimal())%>% layout(title = "",
                                                     yaxis = list(title = "Porcentagem de alunos"),
                                                     xaxis = list(title = "Reprovações"))
    }
    if (input$Wid20=="Transf. ou desistentes") {
      rep_g<-prop.table(c(6,14,5,0))
      rep_c<-prop.table(c(5,14,4,2))
      rep_p<-prop.table(c(2,9,3,11))
      
      Porcentagem<-c(rep_g,rep_c,rep_p)
      Disciplina<-as.factor(c(rep("geometria analítica",4),rep("cálculo A",4),
                              rep("probabilidade 1",4)))
      Reprovacoes<-as.factor(c("sem reprovação","com reprovação","dispensou","não cursou",
                               "sem reprovação","com reprovação","dispensou","não cursou",
                               "sem reprovação","com reprovação","dispensou","não cursou"))
      
      x<-data.frame(Porcentagem,Disciplina,Reprovacoes)
      
      ordem_dis<-c("geometria analítica","cálculo A","probabilidade 1")
      ordem_rep<-c("sem reprovação","com reprovação","dispensou","não cursou")
      
      fig16 <- ggplotly(x%>%
                          group_by(Porcentagem,Disciplina,Reprovacoes)%>%
                          mutate(Disciplina=fct_relevel(Disciplina,ordem_dis),
                                 Reprovacoes=fct_relevel(Reprovacoes,ordem_rep))%>%
                          ggplot(aes(x=Reprovacoes,y=Porcentagem,fill=Disciplina))+
                          geom_col(position = position_dodge(0.7),width = 0.5)+
                          scale_x_discrete(labels=c("Sem reprovações", "Com reprovações",
                                                    "Dispensou","Não cursou"))+
                          scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.1))+
                          scale_fill_manual(name="Disciplinas",values=c("#0073b7","#b70073","#73b700"))+
                          theme_minimal())%>% layout(title = "",
                                                     yaxis = list(title = "Porcentagem de alunos"),
                                                     xaxis = list(title = "Reprovações"))
      } 
    
    fig16
    })
  
  
  output$plot4.1 <- renderPlotly({
    input$action 
    fig17 <- ggplotly(dados1%>%
                        group_by(rep_ga)%>%
                        ggplot(aes(x=1,y=rep_ga))+
                        geom_boxplot(width = 0.4)+
                        scale_y_continuous(breaks=seq(0,6,1))+
                        scale_x_discrete(limits=c(" "))+
                        theme_minimal())%>% layout(title = "",
                                                   xaxis = list(title = ""),
                                                   yaxis = list(title = "Número de reprovações"))
    fig17
  })
  
  output$plot4.2 <- renderPlotly({
    input$action   
    fig18 <- ggplotly(dados1%>%
                        group_by(rep_calc)%>%
                        ggplot(aes(x=1,y=rep_calc))+
                        geom_boxplot(width = 0.4)+
                        scale_y_continuous(breaks=seq(0,6,1))+
                        scale_x_discrete(" ",limits=c(" "))+
                        theme_minimal())%>% layout(title = "",
                                                   xaxis = list(title = ""),
                                                   yaxis = list(title = "Número de reprovações"))
    fig18
    })

 
  
  
  
  


  
  
  
  output$plot1.4 <- renderPlotly({
    input$action
    if (input$Wid5=="Total") {
      A<-round(prop.table(table(dados$rep_ga)),3)
      Porcentagem<-c(A[0:7],0,A[8:9])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      fig1<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill="#001f3f")+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.0000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                      xaxis = list(title = ""),
                                                                                                      yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid5=="Graduados") {
      A<-round(prop.table(table(grad$rep_ga)),3)
      Porcentagem<-c(A[0:3],rep(0,6),A[4])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      fig1<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#39cccc')+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.0000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                      xaxis = list(title = ""),
                                                                                                      yaxis = list(title = "Porcentagem de alunos"))
    }
    
    if (input$Wid5=="Jubilados") {
      A<-round(prop.table(table(jub$rep_ga)),3)
      Porcentagem<-c(A[0:6],rep(0,2),A[7:8])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      fig1<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#605ca8')+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.0000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                      xaxis = list(title = ""),
                                                                                                      yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid5=="Transf. ou desistentes") {
      A<-round(prop.table(table(mud$rep_ga)),3)
      Porcentagem<-c(A[0:4],rep(0,2),A[5],rep(0,2),A[6])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      fig1<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#0073b7')+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.0000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                      xaxis = list(title = ""),
                                                                                                      yaxis = list(title = "Porcentagem de alunos"))
      
    }
    fig1
  })
  
  output$plot1.5 <- renderPlotly({
    input$action
    if (input$Wid6=="Total") {
      A<-round(prop.table(table(dados$rep_calc)),3)
      Porcentagem<-c(A[0:7],0,A[8:9])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig2<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#001f3f')+
                       scale_y_continuous(labels=scales::percent) +
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
      
    }
    
    if (input$Wid6=="Graduados") {
      A<-round(prop.table(table(grad$rep_calc)),3)
      Porcentagem<-c(A[0:4],rep(0,5),A[5])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig2<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#39cccc')+
                       scale_y_continuous(labels=scales::percent) +
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid6=="Jubilados") {
      A<-round(prop.table(table(jub$rep_calc)),3)
      Porcentagem<-c(A[0:6],rep(0,2),A[7:8])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig2<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#605ca8')+
                       scale_y_continuous(labels=scales::percent) +
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid6=="Transf. ou desistentes") {
      A<-round(prop.table(table(mud$rep_calc)),3)
      Porcentagem<-c(A[0:3],rep(0,3),A[4],0,A[5:6])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig2<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill='#0073b7')+
                       scale_y_continuous(labels=scales::percent) +
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    fig2
  })
  
  
  
  output$plot1.6 <- renderPlotly({
    input$action
    if (input$Wid7=="Total") {
      A<-round(prop.table(table(dados$rep_prob)),3)
      Porcentagem<-c(A[0:5],rep(0,3),A[6:7])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig3<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill="#001f3f")+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
      
    }
    
    if (input$Wid7=="Graduados") {
      A<-round(prop.table(table(grad$rep_prob)),3)
      Porcentagem<-c(A[0:3],rep(0,6),A[5])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig3<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill="#39cccc")+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid7=="Jubilados") {
      A<-round(prop.table(table(jub$rep_prob)),3)
      Porcentagem<-c(A[0:5],rep(0,3),A[6:7])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig3<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill="#605ca8")+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
    }
    
    if (input$Wid7=="Transf. ou desistentes") {
      A<-round(prop.table(table(mud$rep_prob)),3)
      Porcentagem<-c(A[0:3],rep(0,5),A[4:5])
      Reprovacoes<-c("0","1","2","3","4","5","6","7","Não cursou","Dispensou")
      Reprovacoes<-as.factor(Reprovacoes)
      x<-data.frame(Porcentagem,Reprovacoes)
      
      fig3<-ggplotly(ggplot(x)+aes(x=Reprovacoes,y=Porcentagem)+
                       geom_col(fill="#0073b7")+
                       scale_y_continuous(labels=scales::percent)+
                       theme_minimal()+
                       theme(axis.text.x=element_text(angle=-0.000001,vjust=0.5,hjust=1)))%>% layout(title = "",
                                                                                                     xaxis = list(title = ""),
                                                                                                     yaxis = list(title = "Porcentagem de alunos"))
      
    }
    fig3
  })
  
  
}

