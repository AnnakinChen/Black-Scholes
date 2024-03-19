library(shiny)
library(shinyjs)
library(ggplot2)

call = function(S,K,t,sigma,rf){
  d1 = (log(S/K)+(rf+sigma^2*0.5)*t)/(sigma*sqrt(t))
  d2 = d1-sigma*sqrt(t)
  C = S*pnorm(d1)-K*exp(-rf*t)*pnorm(d2)
  return(C)
}

put = function(S,K,t,sigma,rf){
  d1 = (log(S/K)+(rf+sigma^2*0.5)*t)/(sigma*sqrt(t))
  d2 = d1-sigma*sqrt(t)
  P = K*exp(-rf*t)*pnorm(-d2)-S*pnorm(-d1)
  return(P)
}

d1 = function(S,K,t,sigma,rf){
  res = (log(S/K)+(rf+sigma^2*0.5)*t)/(sigma*sqrt(t))
  return(res)
}

delta1 = function(S,K,t,sigma,rf,type){
  if(type=="call"){
    res = pnorm(d1(S,K,t,sigma,rf))
    return(res)
  }
  else if(type=="put"){
    res = -pnorm(-d1(S,K,t,sigma,rf))
  }
}

gamma1 = function(S,K,t,sigma,rf){
  res = dnorm(d1(S,K,t,sigma,rf))/(S*sigma*sqrt(t))
  return(res)
}

vega1 = function(S,K,t,sigma,rf){
  res = S*dnorm(d1(S,K,t,sigma,rf))*sqrt(t)
  return(res)
}

theta1 = function(S,K,t,sigma,rf,type){
  d_1 = d1(S,K,t,sigma,rf)
  d_2 = d_1-sigma*sqrt(t)
  if(type=="call"){
    res = -S*dnorm(d_1)*sigma/(2*sqrt(t))-rf*K*exp(-rf*t)*pnorm(d_2)
    return(res)
  }
  else if(type=="put"){
    res = -S*dnorm(d_1)*sigma/(2*sqrt(t))+rf*K*exp(-rf*t)*pnorm(-d_2)
  }
}

rho1 = function(S,K,t,sigma,rf,type){
  d_1 = d1(S,K,t,sigma,rf)
  d_2 = d_1-sigma*sqrt(t)
  if(type=="call"){
    res = K*t*exp(-rf*t)*pnorm(d_2)
    return(res)
  }
  else if(type=="put"){
    res = -K*t*exp(-rf*t)*pnorm(-d_2)
  }
}

ui <- fluidPage(
  title = "Black-Scholes Model",
  useShinyjs(),  # 初始化shinyjs
  tags$head(
    tags$style(HTML("
      #loginForm {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 30%; /* 控制登录表单的宽度 */
        padding: 20px;
        border: 1px solid #ccc;
        border-radius: 5px;
        background-color: #f9f9f9;
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
      }
      #loginForm .btn, .submit-button { /* 目标是登录表单内的按钮和具有.submit-button类的按钮 */
        width: 25%; /* 减小按钮宽度 */
        margin: auto; /* 保持按钮居中 */
        display: block; /* 确保使用块级元素布局 */
      }
      #logoutButton {
        position: fixed;
        right: 20px;
        top: 20px;
      }
      #dashboardContent {
        position: absolute;
        top: 115%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 50%;
        padding: 20px;
        border: 1px solid #ccc;
        border-radius: 5px;
        background-color: #f9f9f9;
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
      }
      .dashboard-row {
        display: block;
        text-align: center;
      }
      .dashboard-column {
        margin: auto;
        padding: 10px;
        width: 100%;
      }
      .shiny-input-container, .shiny-input-action-button {
        margin: 10px auto;
        text-align: center;
        width: auto;
      }
      input, select, .btn {
        width: 100%;
        display: block;
      }
    "))
  ),
  uiOutput("dynamicUI")# 动态生成UI部分

)


# Server逻辑
server <- function(input, output, session) {
  # 使用reactiveValues跟踪登录状态
  values <- reactiveValues(loggedIn = FALSE)
  
  # 动态生成UI内容
  output$dynamicUI <- renderUI({
    if (!values$loggedIn) {
      div(id = "loginForm",
          textInput("username", "Username"),
          passwordInput("password", "Password"),
          actionButton("login", "Login", class = "btn-primary"),
          style = "padding: 20px; border: 1px solid #ccc; border-radius: 5px; background-color: #f9f9f9;"
      )
    } else {
      div(
        div(id = "logoutButton",
            actionButton("logout", "Logout")
        ),
        div(id = "dashboardContent",
            div(class = "dashboard-row",
                div(class = "dashboard-column",
                    textInput('t1','Spot Price',value = 100),
                    textInput('t2','Strike Price',value = 100),
                    textInput('t3','Time to Maturity',value = 1),
                    textInput('t4','Volatility',value = 0.2),
                    textInput('t5','Risk-free Rate',value = 0.05),
                    selectInput('c1','Type',choices = c('call','put')),
                    selectInput('c2','Variable',choices = c('Spot price','Strike price','Volatility','T','Risk-free rate')),
                    actionButton('a1','Submit', class = "btn-primary")
                ),
                div(class = "dashboard-column",
                    verbatimTextOutput('output1'),
                    plotOutput('plot1')
                )
            )
        )
      )
    }
  })
  
  observeEvent(
    input$a1,
    {
      S = as.numeric(input$t1)
      K = as.numeric(input$t2)
      t = as.numeric(input$t3)
      sigma = as.numeric(input$t4)
      rf = as.numeric(input$t5)
      type = input$c1
      if(input$c1=="call"){
        price = call(S,K,t,sigma,rf)
        Delta = delta1(S,K,t,sigma,rf,type)
        Gamma = gamma1(S,K,t,sigma,rf)
        Vega = vega1(S,K,t,sigma,rf)
        Theta = theta1(S,K,t,sigma,rf,type)
        Rho = rho1(S,K,t,sigma,rf,type)
        res = c(list(price),list(Delta),list(Gamma),list(Vega),list(Theta),list(Rho))
        names(res) = c('Call Price','Delta','Gamma','Vega','Theta','Rho')
        output$output1 = renderPrint({
          res
        })
      }
      else{
        price = put(S,K,t,sigma,rf)
        Delta = delta1(S,K,t,sigma,rf,type)
        Gamma = gamma1(S,K,t,sigma,rf)
        Vega = vega1(S,K,t,sigma,rf)
        Theta = theta1(S,K,t,sigma,rf,type)
        Rho = rho1(S,K,t,sigma,rf,type)
        res = c(list(price),list(Delta),list(Gamma),list(Vega),list(Theta),list(Rho))
        names(res) = c('Put Price','Delta','Gamma','Vega','Theta','Rho')
        output$output1 = renderPrint({
          res
        })
      }
    }
  )
  observeEvent(
    input$a1,
    {
      S = as.numeric(input$t1)
      K = as.numeric(input$t2)
      t = as.numeric(input$t3)
      sigma = as.numeric(input$t4)
      rf = as.numeric(input$t5)
      type = input$c1
      if(input$c1=="call"){
        if(input$c2=='Spot price'){
          x = seq(0.5*S,1.5*S,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,call(x,K,t,sigma,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='Strike price'){
          x = seq(0.5*K,1.5*K,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,call(S,x,t,sigma,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='T'){
          x = seq(0.5*t,1.5*t,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,call(S,K,x,sigma,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='Volatility'){
          x = seq(0.5*sigma,1.5*sigma,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,call(S,K,t,x,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='Risk-free rate'){
          x = seq(0.5*rf,1.5*rf,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,call(S,K,t,sigma,x),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
      }
      else{
        if(input$c2=='Spot price'){
          x = seq(0.5*S,1.5*S,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,put(x,K,t,sigma,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='Strike price'){
          x = seq(0.5*K,1.5*K,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,put(S,x,t,sigma,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='T'){
          x = seq(0.5*t,1.5*t,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,put(S,K,x,sigma,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='Volatility'){
          x = seq(0.5*sigma,1.5*sigma,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,put(S,K,t,x,rf),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
        else if(input$c2=='Risk-free rate'){
          x = seq(0.5*rf,1.5*rf,length.out = 1000)
          output$plot1 = renderPlot({
            plot(x,put(S,K,t,sigma,x),type = 'l',lwd=2,col="navyblue",
                 xlab = input$c2,ylab = 'Option price',main = paste('Option price','VS',input$c2))
          })
        }
      }
    }
  )
  
  observeEvent(input$login, {
    if (input$username == "Anakin" && input$password == "1234") {
      values$loggedIn <- TRUE
      shinyjs::alert("Welcome, Master Skywalker!")  # 登录成功的弹出消息
    } else {
      shinyjs::alert("Wrong Password, please try again!")
    }
  })
  
  observeEvent(input$logout, {
    values$loggedIn <- FALSE  # 更改登录状态以显示登录界面
  })
}

# 运行应用
shinyApp(ui = ui, server = server)



