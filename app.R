#### file ####
setwd("C:/Users/muhammet.aydin/Desktop/STOK_DASHBOARD_TR")

#### R file running ####
source("dependencies.R")



#### UI ####

ui<-shinyUI(bootstrapPage(fluidPage(theme = shinytheme("flatly"),
                                    navbarPage("STOCK",
                                               navbarMenu("Products",
                                                          tabPanel("Product Entry",
                                                                   sidebarLayout(
                                                                     sidebarPanel(
                                                                       selectInput("select", label = "Unit:", 
                                                                                   choices = list("Amount","Kg","Lt","Box"), 
                                                                                   selected = "Amount"),
                                                                       dateInput("date1","Date:", value=Sys.Date()),
                                                                       uiOutput("uiselect1"),
                                                                       numericInput("num1", "Entry Quantity:", 1, min=1),
                                                                       useShinyalert(),
                                                                       actionButton("button1", "Add")
                                                                     ),
                                                                     mainPanel(dataTableOutput("entrytable")
                                                                     ))),
                                                          tabPanel("Product Output",
                                                                   sidebarLayout(
                                                                     sidebarPanel(
                                                                       selectInput("select2", "Unit:", 
                                                                                   choices = list("Amount","Kg","Lt","Box"), 
                                                                                   selected = "Amount"),
                                                                       selectInput("select3", "Delivery Unit:", 
                                                                                   choices = list("Mechanic","Process","Customs","Security","Infrastructure","Restaurant","Hotel","Jetty","Automation","Port")),
                                                                       selectInput("select4", "Delivery Person:", 
                                                                                   choices = list("Ahmet","Mehmet","Mahmut","Muhammet")),
                                                                       dateInput("date1.2","Date:", value=Sys.Date()),
                                                                       uiOutput("uiselect2"),
                                                                       numericInput("num1.2", "Output Quantity:", 1, min=1),
                                                                       useShinyalert(),
                                                                       actionButton("button1.2", "Add")
                                                                     ),
                                                                     mainPanel(dataTableOutput("outputtable")
                                                                     )))
                                               ),
                                               tabPanel("Instant Stock",
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            uiOutput("uiselect3"),
                                                            selectInput("select7", "Status:", multiple = T,
                                                                        choices = c("NOTCONSUMED","SOLDOUT","EXISTING","MISSING MATERIAL", "DECREASED"))
                                                          ),
                                                          mainPanel(DTOutput("outputtable2"))
                                                        )),
                                               tabPanel("Stock Usage Report",
                                                        DTOutput("outputtable3")),
                                               tabPanel("Output Usage Report", 
                                                        DTOutput("outputtable4")),
                                               tabPanel("Tables (Live)",
                                                        tabsetPanel(
                                                          tabPanel("Product Entry", dataTableOutput("outputtable5.1")),
                                                          tabPanel("Product Output", dataTableOutput("outputtable5.2")),
                                                          tabPanel("Instant Stock", dataTableOutput("outputtable5.3"))
                                                        )
                                               ),
                                               tabPanel("Search & Add Product",
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            textInput("text1","Brand:"),
                                                            textInput("text2","What is this?"),
                                                            textInput("text3","How many?"),
                                                            useShinyalert(),
                                                            actionButton("button2", "Add")
                                                          ),
                                                          mainPanel(
                                                            dataTableOutput("outputtable6")
                                                          )
                                                        ))
                                               
                                               
                                    ))))








#### SERVER ####



server<- function(input, output, session){
  

  
  output$entrytable <- renderDataTable({
    data.frame("DATE"=input$date1,"UNIT"=input$select, "PRODUCT"=input$select1,
               "ENTRY_QUANTITY"=input$num1)})
  
  output$outputtable = renderDataTable({
    data.frame("DATE"=input$date1.2,"UNIT"=input$select2, "DELIVERY_UNIT"=input$select3,
               "DELIVERY_PERSON"=input$select4, "PRODUCT"=input$select5,
               "OUTPUT_QUANTITY"=input$num1.2)})  
  
  observeEvent(input$button1, {
    shinyalert("Added!", "Related stock data was added target file succesfully", type = "success")
    write.table(data.frame("DATE"=input$date1,"UNIT"=input$select, "PRODUCT"=input$select1,
                           "ENTRY_QUANTITY"=input$num1),
                file = "products_entries.csv", sep = ",", append = TRUE,
                col.names = FALSE, row.names = FALSE)
  })
  
  observeEvent(input$button1.2, {
    shinyalert("Added!", "Related stock data was added target file succesfully", type = "success")
    write.table(data.frame("DATE"=input$date1.2,"UNIT"=input$select2, "DELIVERY_UNIT"=input$select3,
                           "DELIVERY_PERSON"=input$select4, "PRODUCT"=input$select5,
                           "OUTPUT_QUANTITY"=input$num1.2),
                file = "products_output.csv", sep = ",", append = TRUE,
                col.names = FALSE, row.names = FALSE)
  })
  
  fileData1 <- reactiveFileReader(1000, session, 'products_entries.csv', read.csv)
  fileData2 <- reactiveFileReader(1000, session, 'products_output.csv', read.csv)
  
  ####anlikstok####
  
  anlikstok<-reactive({
    prodout1<-fileData2() %>%
      select(PRODUCT, OUTPUT_QUANTITY) %>%
      group_by(PRODUCT)  %>%
      summarise(sum = sum(OUTPUT_QUANTITY, na.rm=T))
    
    prodent1<-fileData1() %>%
      select(PRODUCT, ENTRY_QUANTITY) %>%
      group_by(PRODUCT)  %>%
      summarise(sum = sum(ENTRY_QUANTITY, na.rm=T))
    
    anlikstok1<-left_join(data.frame("PRODUCT"=as.character(unique(prodent1$PRODUCT))),prodent1,
                          by="PRODUCT")
    anlikstok2<-left_join(data.frame("PRODUCT"=as.character(unique(prodent1$PRODUCT))),prodout1,
                          by="PRODUCT")
    anlikstok2[is.na(anlikstok2)] <- 0
    
    anlikstok<- cbind(anlikstok1,anlikstok2[,2])
    colnames(anlikstok)<-c("PRODUCT","ENTRY_QUANTITY","OUTPUT_QUANTITY")
    
    anlikstok$INSTANT_STOCK <-anlikstok$ENTRY_QUANTITY-anlikstok$OUTPUT_QUANTITY
    
    anlikstok$STATUS <- ifelse(anlikstok$OUTPUT_QUANTITY==0, "NOTCONSUMED", 
                               ifelse(anlikstok$INSTANT_STOCK==0, "SOLDOUT",
                                      ifelse(anlikstok$INSTANT_STOCK>anlikstok$ENTRY_QUANTITY/5, "EXISTING",
                                             ifelse(anlikstok$INSTANT_STOCK<0,"MISSING MATERIAL","DECREASED"))))
    
    anlikstok$STATUS <- as.factor(anlikstok$STATUS)
    anlikstok
    
  })
  
  
  output$outputtable2 <- renderDataTable(extensions = c('Buttons'),
                                         class = 'cell-border stripe hover', rownames=F,
                                         filter="top", 
                                         options= list(dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                         ),
                                         anlikstok() %>%
                                           filter(PRODUCT %in% input$select6, STATUS %in% input$select7)
  )
  
  
  stock_usage<-reactive({
    stock_usage<-anlikstok() %>%
      select(STATUS, INSTANT_STOCK) %>%
      group_by(STATUS) %>%
      summarise(n()) %>%
      adorn_totals("row") %>%
      set_names(c("STATUS","ANLIK_STOK"))
    stock_usage$STATUS<-as.factor(stock_usage$STATUS)
    stock_usage
  })
  
  output$outputtable3 <- renderDataTable(extensions = c('Buttons'),
                                         class = 'cell-border stripe hover', rownames=F,
                                         filter="top", 
                                         options= list(dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                         ),
                                         stock_usage()
  )
  
  
  myfileData2<- reactive({
    myfileData2 <- fileData2()
    myfileData2$DATE<-as.Date(myfileData2$DATE)
    myfileData2$PRODUCT<-as.character(myfileData2$PRODUCT)
    myfileData2
  })
  
  
  output$outputtable4 <- renderDataTable(extensions = c('Buttons'),
                                         class = 'cell-border stripe hover', rownames=F,
                                         filter="top", 
                                         options= list(dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       pageLength=nrow(fileData2())),
                                         myfileData2()
  )
  
  myfileData1<- reactive({
    myfileData1 <- fileData1()
    myfileData1$PRODUCT<-as.character(myfileData1$PRODUCT)
    myfileData1$DATE<-as.Date(myfileData1$DATE)
    myfileData1
  })
  
  output$outputtable5.1 <- renderDataTable(
    myfileData1(),
    extensions = c('Buttons'),
    class = 'cell-border stripe hover', rownames=F,
    filter="top",
    options= list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength=nrow(fileData1()))
  )
  
  
  output$outputtable5.2 <- renderDataTable(
    myfileData2(),
    extensions = c('Buttons'),
    class = 'cell-border stripe hover', rownames=F,
    filter="top",
    options= list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength=nrow(fileData2()))
  )
  
  output$outputtable5.3<- renderDataTable({ 
    dat <- datatable(anlikstok(), extensions = c('Buttons'), filter="top", 
                     class = 'cell-border stripe hover', rownames=F,
                     options= list(dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   pageLength=nrow(anlikstok()))) %>%
      formatStyle('STATUS', backgroundColor = 
                    styleEqual(c("MISSING MATERIAL","SOLDOUT","DECREASED","EXISTING"),
                               c("black","red","yellow","blue")),
                  color= styleEqual(c("MISSING MATERIAL","SOLDOUT","DECREASED","EXISTING"),
                                    c("white","white","green","white")))
    
    return(dat)
  })
  
  
  fileData <- reactiveFileReader(1000, session, 'products.csv', read.csv)
  
  
  myfileData<- reactive({
    myfileData <- fileData()
    myfileData$PRODUCTS<-as.character(myfileData$PRODUCTS)
    myfileData
  })
  
  output$uiselect1<-renderUI({
    selectInput("select1", label = "Product:", choices = myfileData())
  })
  
  output$uiselect2<-renderUI({
    selectInput("select5", label = "Product:", choices = myfileData())
  })
  
  
  output$uiselect3<-renderUI({
    selectInput("select6", "Product:", choices = myfileData(), multiple = T)
  })
  
  
  observeEvent(input$button2, {
    shinyalert("Added!", "Related stock data was added target file succesfully", type = "success")
    write.table(data.frame("PRODUCTS"=paste(input$text1,input$text2, input$text3, sep="_")),
                file = "products.csv", sep = ",", append = TRUE,
                col.names = FALSE, row.names = FALSE)
  })
  
  
  
  
  output$outputtable6 <-renderDataTable(
    myfileData(),
    extensions = c('Buttons'),
    class = 'cell-border stripe hover', rownames=F,
    filter="top",
    options= list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength=nrow(fileData()))
  )
  
  
}



#### RUN ####
shinyApp(ui,server)
