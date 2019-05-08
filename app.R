#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(htmlwidgets)
library(shinyBS)
library(shinycssloaders)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(dplyr)
library(purrr) # just for `%||%`
library(plotly)
library(noteMD)
library(webshot)
# webshot::install_phantomjs()

rm(list = ls())
useShinyalert()

my_username <- "test"
my_password <- "test2"


note=readRDS("note.rds")
Completed_ca <- unique(data.frame(note)$Completed)

Logged = FALSE;



header <- dashboardHeader( title = "Business Intelligence" )
sidebar <- dashboardSidebar(withSpinner(uiOutput("sidebarpanel")) )
body <- dashboardBody( uiOutput("body"))


login <- box(title = "Login",textInput("userName", "Username"),  width =600,
             passwordInput("passwd", "Password"),
             br(),actionButton("Login", "Log in"),
             div(h4("Username: test"),
             h4("Password: test2"),
             img(src='image7.jpg',  width =600), style="text-align: center;" ),br(),br(),
             helpText("[1] Jiena McLellan (2019). noteMD: Print text from shiny ui (support markdown syntax) to pdf or word report. R
  package version 0.1.0."),
             helpText("[2]  Carson Sievert (2018) plotly for R. https://plotly-r.com ")
             )


ui<-dashboardPage(header, sidebar, body, skin = "black")


server <- function(input, output, session) {
  
  USER <<- reactiveValues(Logged = Logged)
  
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <<- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  
  
  
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE ){ 

      sidebarMenu(
        menuItem(tags$em("Home Page",style="font-size:140%"),icon=icon("bar-chart-o"), tabName="home"),
        menuItem(tags$em("Data Vis",style="font-size:140%"),icon=icon("bar-chart-o"),tabName="data_vis"),
        textInput("author", label = h5("Author Name of Report:"), value = "Jiena McLellan"),
        sidebarUserPanel("myuser",subtitle = a(icon("user"), "Logout", href="__logout__"))
      )

    }
    
    
  })
  
  
  output$body <- renderUI({
    if (USER$Logged == TRUE ) {
      
      ##### dashboard body ####     
      tabItems(
        tabItem(
          tabName="home",class = "active",br(),
          downloadButton('cv_download',"Download my CV",class="butt" ),
          includeMarkdown("www/about.md"),br(), 
          div( img(src='Picture1.png',  width =700), style="text-align: center;" ),
          helpText("[1] Jiena McLellan (2019). noteMD: Print text from shiny ui (support markdown syntax) to pdf or word report. R
  package version 0.1.0."),
          helpText("[2]  Carson Sievert (2018) plotly for R. https://plotly-r.com ")
        ),

        tabItem(
          tabName="data_vis",
          fluidPage(
            
            # Application title
            titlePanel("DT Editor Minimal Example"),
            ### This is to adjust the width of pop up "showmodal()" for DT modify table 
            tags$head(tags$style(HTML('
                                      .modal-lg {
                                      width: 1200px;
                                      }
                                      '))),
            helpText("Note: Remember to save any updates!"),
            br(),
            ### tags$head() is to customize the download button
            tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
            downloadButton("Trich_csv", "Download in CSV", class="butt"),
            useShinyalert(), # Set up shinyalert
            uiOutput("MainBody_trich"),actionButton(inputId = "Updated_trich",label = "Save"),br(),br(),br(),
            fluidRow(
              column(6,
                     
                     downloadButton('describe_download',"Download Report",class="butt" ),br(),
                     tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                     radioButtons('format', 'Document format', c('PDF', 'Word'),
                                  inline = TRUE),
            plotlyOutput("pie"), uiOutput("back") ),
            column(6, 
                   tags$head(tags$style(".button{background-color:#032863;} .button{color: #e6ebef;}")),
                   downloadButton('presentation_download2',"Download Interactive Presentation",class="button" ),br(),
                   textInput("slide1_title", "Title of your comment slide (only for presentation download):", "Title 1"),br(),
                   fluidRow(
                     column(12,
                            helpText("Note: Any comments made in the box will be printed if you download the summary report.") ),
                     column(12,
                            tags$textarea(
                              "This is a **very important** note:

* Suggestion 1: use `UTF-8`
* Suggestion 2: use `UTF-8`
* Suggestion 3: use `UTF-8` ",
                              id    = 'markdowninput',
                              rows  = 3,
                              style = 'width:100%;')) ),
                   helpText("Preview:"),
                   htmlOutput('htmlmarkdown'),br()
                   )
            )
            )
        )
      )
    }
    
    else {
      login
    }
  })
  
  
  #### Server part of shiny ###########
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-readRDS("note.rds")
 # Completed <- unique(data.frame(vals_trich$Data)$Completed)
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
      hr(),
      column(6,offset = 6,
             HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             ### Optional: a html button 
             # HTML('<input type="submit" name="Add_row_head" value="Add">'),
             HTML('</div>') ),
      
      column(12,dataTableOutput("Main_table_trich")),
      tags$script("$(document).on('click', '#Main_table_trich button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random()) });")
      
    ) 
})
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,selection = 'single',
              escape=F) })
  
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          dateInput(paste0("Date_add", input$Add_row_head), "Date:", value = Sys.Date()),
                          textInput(paste0("Description_add", input$Add_row_head), "Description"),
                          textInput(paste0("Names_add", input$Add_row_head), "Name"),
                          numericInput(paste0("Request_add", input$Add_row_head), "Request Number:",0),  
                          selectInput(paste0("Completed_add", input$Add_row_head), "Status:",choices=c("Yes", "Progressing")),
                          textInput(paste0("Comments_add", input$Add_row_head), "Comments"), 
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      Date=as.character( input[[paste0("Date_add", input$Add_row_head)]] ),
      Description=input[[paste0("Description_add", input$Add_row_head)]],
      Names=input[[paste0("Names_add", input$Add_row_head)]],
      Request=input[[paste0("Request_add", input$Add_row_head)]],
      Completed=input[[paste0("Completed_add", input$Add_row_head)]],
      Comments=input[[paste0("Comments_add", input$Add_row_head)]]
    )
    vals_trich$Data<-rbind(vals_trich$Data,new_row )
    removeModal()
  })
  
  
  
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete",length(input$Main_table_trich_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals_trich$Data=vals_trich$Data[-input$Main_table_trich_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
  }else{
    modalDialog(
      title = "Warning",
      paste("Please select the row that you want to edit!" ),easyClose = TRUE
    )
  }
  
          )
    })
  
  
  
  
  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=vals_trich$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals_trich$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      else if( is.Date(vals_trich$Data[[i]])){
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      }
      else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=row_change
    DT 
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=colnames(vals_trich$Data)
                 vals_trich$Data[input$Main_table_trich_rows_selected]<-DF
                 
               }
  )
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  ### from plotly example: https://plotly-r.com/linking-views-with-shiny.html#drill-down
  ### check it out 
  current_category <- reactiveVal()

  # report sales by category, unless a category is chosen
  curr_data <- reactive({
    if (!length(current_category())) {
      return(count(data.frame(vals_trich$Data), Completed) )
    }
    data.frame(vals_trich$Data) %>%
      filter(Completed %in% current_category()) %>%
      count(Comments)
  })
  pie_reactive=reactive({
    d <- setNames(curr_data(), c("labels", "values"))
    plot_ly(d) %>%
      add_pie(labels = ~labels, values = ~values, customdata = ~labels) %>%
      layout(title = current_category() %||% "Complete Status")
  })
  output$pie <- renderPlotly({
    pie_reactive()
  })
  
  # update the current category if the clicked value matches a category
  observe({
    cd <- event_data("plotly_click")$customdata[[1]]
    if (isTRUE(cd %in% Completed_ca)) current_category(cd)
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(current_category())) 
      actionButton("clear", "Back to Complete Status", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, current_category(NULL))
  
  output$htmlmarkdown = reactive({
    note_in_html(input$markdowninput)
  })
  
  output$presentation_download2 = downloadHandler(
    filename<- function(){
      paste("Presentation",Sys.Date(),".html",sep = "")
    },
    
    content = function(file) {
      #### Progressing indicator
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     ## End of progression
                     src <- normalizePath('test2.Rmd')
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'test2.Rmd', overwrite = TRUE)
                     
                     library(rmarkdown)
                     out <- render('test2.Rmd',  ioslides_presentation())
                     file.rename(out, file)
                     
                   })
      
      
    })
  
  output$describe_download = downloadHandler(
    filename<- function(){
      paste("Summary",Sys.Date(),switch(
        input$format, PDF = '.pdf', Word = '.docx'
      ),sep = "")
    },
    
    content = function(file) {
      if (input$format=="PDF"){
        #### Progressing indicator
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report.Rmd', pdf_document())
                       file.rename(out, file)
                       
                     })
        ### below is the end of pdf content
      }else{
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report_word.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report_word.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report_word.Rmd', word_document())
                       file.rename(out, file)
                     })
      }
      
    })
  
  
  output$cv_download <- downloadHandler(
    filename = "Jiena_McLellan_CV.pdf",
    content = function(file) {
      file.copy("www/Jiena_McLellan_CV.pdf", file)
    }
  )
  #### End of server part of shiny ###
  
  
  
  }

shinyApp(ui, server)


