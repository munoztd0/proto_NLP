# the necessary packages
options(warn=-1)

suppressPackageStartupMessages(pacman::p_load(DBI, dplyr, RSQLite, DT, shiny,
        shinyjs, shinythemes, shinyWidgets, shinydashboard, lubridate,
        shinyauthr, shinyFeedback, stringr, echarts4r, quanteda, quanteda.textstats))

source("global.R")

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  username = c("user"),
  password = c("user1"), 
  password_hash = sapply(c("user1"), sodium::password_store), 
  permissions = c( "admin")
)
sqlite_kw_lo <- tolower(sqlite_kw)

# connect to the database
db <- dbConnect(SQLite(), 'sharks.db') #to proto
# prodtype <- dbGetQuery(db, 'SELECT distinct sharktype from cases')


###############################################
# define the ui function
###############################################
ui <- dashboardPage(
  title="Database Management Platform",
  
  dashboardHeader(
    title = span("Database Management Platform", style = "font-size: 20px"),
    titleWidth = 300,
    tags$li(class = "dropdown", style = "padding: 8px;", actionButton("refresh", "refresh")),
    tags$li(class = "dropdown", 
            tags$a(icon("github"), 
                   href = "https://github.com/munoztd0/DbVieweR",
                   title = "See the code on github")),
    tags$li(class = "dropdown", style = "padding: 8px;",
            shinyauthr::logoutUI("logout"))
    
  ),

          

  dashboardSidebar(collapsed = TRUE, 
                   div(textOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(
                     #menuItem("View Tables", tabName = "view_table", icon = icon("search",verify_fa = FALSE)),
                     #menuItem("Insert Rows", tabName = "insert_rows", icon = icon("plus-square",verify_fa = FALSE)),
                     menuItem("View Stats", tabName = "view_stats", icon = icon("exchange-alt",verify_fa = FALSE)),
                     #menuItem("Modify Entries", tabName = "modify_value", icon = icon("edit",verify_fa = FALSE)),
                     #menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt", verify_fa = FALSE)),
                     menuItem("About", tabName = "about", icon = icon("info-circle",verify_fa = FALSE))
                   )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("returnClick.js")
    ),
    tags$head(tags$link(rel = "shortcut icon", href = "https://github.com/munoztd0/WEBSITE/blob/test/img/logo/logo.png?raw=true")),
    shinyauthr::loginUI("login"),
    uiOutput("user_table"),
    tabItems(
      # Third Tab
      tabItem(
        tabName = 'view_stats',
        uiOutput("tab3UI")
      ),
      # Sixth Tab
      tabItem(
        tabName = 'about',
        uiOutput("tab6UI")
      )
    )
  )
)

###############################################
# define the server function
###############################################

server <- function(input, output, session) {


    #to do better
   backgroundchange <- reactive({
      invalidateLater(1000, session)

      runif(1)
    })
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = username,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, "logout", 
                            reactive(credentials()$user_auth))
  # refresh_init <- callModule(shinyauthr::logout, "logout", 
  #                           reactive(credentials()$user_auth))
  
  # Add or remove a CSS class from an HTML element
  # Here sidebar-collapse
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  # Show the sample login info
  output$user_table <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    # fluidRow(column(6,
    #                 p("Please use the usernames and passwords provided below to test this database management gravite. We have created two kinds of accounts: admin and manager. The manager account allows one to alter tables, columns, and records, and delete tables. The admin account allows one to do everything above except for deleting tables.", 
    #                   class = "text-center", style = "font-size: 15px;"),
    #                 br(),
    #                 renderTable({user_base[, -3]}), offset = 3
    # )
    # )
  })
  
  # pulls out the user information returned from login module
  user_info <- reactive({credentials()$info})
  
  # menu welcome info
  output$welcome <- renderText({
    req(credentials()$user_auth)
    paste0("Welcome ",{user_info()$permissions},"!")
  })


  ############# Tab 1: View Table
  output$tab1UI <- renderUI({
    req(credentials()$user_auth)
    Listener1 <- input$refresh
    isolate(backgroundchange())
    fluidRow(
        column(3,
          dateRangeInput(
            "dat_range_sel",
            "Select time period",
            start = as.Date("2022-01-01"),
            end = Sys.Date(),
            min = as.Date("2006-01-01"),
            max = Sys.Date()
          )
      ),
      column(12, align="left",
              h4(strong("Table Preview")),
              br(),
              DT::DTOutput(outputId = 'sel_table_view'),

              downloadButton('download',"Download the data")          
             )
    )
  })

 

# reactiveDF <- reactive({
		
#     t1 <- input$dat_range_sel[1]; t2 <- input$dat_range_sel[2] +1 

#         readr::read_csv('DB.csv') |>
#         select(Date, Gravite, Sources, Media_Lieu , Auteur, Category, Contenu, Pseudo,Groupe, Attachement, Commentaires, Lieux) |>
#           filter(!is.na(Date)) |> 
#           filter(Date >= t1 & Date <= t2) 

    
#   })

  
  
  output$sel_table_view <- DT::renderDT({

   reactiveDF() |> DT::datatable(
      filter = "top",
    extensions = 'Scroller', 
#     options = list(
#   autoWidth = TRUE,
#   columnDefs = list(list(width = '200px', targets = "_all"))
# ))
    
    options = list(deferRender = F, dom = 'Bfrtip',
            columnDefs = list(list(className = 'dt-left',
                                    targets = "_all")),
            scrollY = 600, scroller = TRUE, scrollX = T,
            pageLength = 20, searching = TRUE))


  })


    output$download <- 
        downloadHandler(
          filename = paste0("actes_", input$dat_range_sel[1], "_", input$dat_range_sel[2] +1, ".xlsx"),
          content = function(file){
            wb <- createWorkbook()
            style_my_workbook(wb, reactiveDF()[input[["sel_table_view_rows_all"]], ], "Cas", TRUE)
            saveWorkbook(wb, file, overwrite = TRUE)
            #writexl::write_xlsx(reactiveDF()[input[["sel_table_view_rows_all"]], ], file)
            #readr::write_excel_csv(reactiveDF()[input[["sel_table_view_rows_all"]], ], file)
          }
        )




  ############# Tab 4: Create Table 
  # Sources in the table name and the column names


 output$tab4UI <- renderUI({

    req(credentials()$user_auth)

    Listener1 <- input$refresh

    isolate(backgroundchange())

    fluidPage(DT::dataTableOutput("table"),
               textInput(width='100%', 'NewContent', 'Contenu'),
               dateInput(width='100%',"NewDate", "Date", value = Sys.Date(), format = "yyyy/mm/dd"),
               textInput(width='100%', 'NewGravite', 'Gravite'),
               textInput(width='100%', 'NewSources', 'Sources'),
               textInput(width='100%', 'NewMedia_Lieu', 'Media_Lieu'),
               textInput(width='100%', 'NewAuteur', 'Auteur'),
               textInput(width='100%', 'NewCategory', 'Category'),
               textInput(width='100%', 'NewPseudo', 'Pseudo'),
               textInput(width='100%', 'NewGroupe', 'Groupe'),
               textInput(width='100%', 'NewAttachement', 'Attachement'),
               textInput(width='100%', 'NewCommentaires', 'Commentaires'),
               textInput(width='100%', 'NewLieux', 'Lieux'),
               br(),
               br(),
               actionButton("goButton", "Insert Row"),
               br(),
               br())
 })

output$table <- DT::renderDataTable(
    df(), 
    selection = 'none', 
    rownames = TRUE,
    filter = "top",
    extensions = 'Scroller',

    options = list(deferRender = F, dom = 'Bfrtip',
            columnDefs = list(list(className = 'dt-left',
                                    targets = "_all")),
            scrollY = 600, scroller = TRUE, scrollX = T,
            pageLength = 20, searching = TRUE),

    class = "display"
  )
    
  
  # mydata <- readr::read_csv('DB.csv') |>
  #       select(Date, Gravite, Sources, Media_Lieu , Auteur, Category, 
  #       Contenu, Pseudo, Groupe, Attachement, Commentaires, Lieux) 

  df <- eventReactive(input$goButton, {
    if(input$NewContent!="" && !is.null(input$NewDate) && input$goButton>0){
      newrow = data.table::data.table(Contenu = input$NewContent,
                          Date = input$NewDate,
                          Gravite = input$NewGravite,
                          Sources = input$NewSources,
                          Media_Lieu = input$NewMedia_Lieu,
                          Auteur = input$NewAuteur,
                          Category = input$NewCategory,
                          Pseudo = input$NewPseudo,
                          Groupe = input$NewGroupe,
                          Attachement = input$NewAttachement,
                          Commentaires = input$NewCommentaires,
                          Lieux = input$NewLieux
                          )
      mydata <<- rbind(mydata, newrow)
      readr::write_excel_csv(mydata, "DB.csv")
    }
    mydata
  }, ignoreNULL = FALSE)

  # Sources in the column names and the column types
  # output$cols <- renderUI({
  #   req(input$ncols>=1)
  #   cols <- vector("list", input$ncols)
  #   for (i in seq_len(input$ncols)) {
  #     cols[[i]] <- box(
  #       title = paste("Column", i), width = 6, solidHeader = TRUE, status = "primary",
  #       textInput(inputId = paste0("colName", i), label = "Column name"),
  #       selectInput(inputId = paste0("colSources", i), label = "Column type", 
  #                   choices = c("NUMERIC", "TEXT","BOOLEAN", D)
  #       )
  #     )
  #   }
  #   cols
  # })
  
  # Create!
  # observeEvent(input$insert_rows, {
  #   # in case the table name is null or has existed
  #   if (tolower(input$table_name) %in% tolower(dbListTables(db)) |
  #       !isTruthy(input$table_name) |
  #       grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$table_name) == FALSE) {
  #     showModal(modalDialog(
  #       title = "Invalid table name",
  #       "You get this message possibly because:
  #        1) the table already exists;
  #        2) the table name is blank;
  #        or 3) this is an invalid table name.",
  #       footer = modalButton("OK"), easyClose = TRUE ) )
  #     return()
  #   }
    
    # in case the input ncols blank
  #   if (!isTruthy(input$ncols)) {
  #     showModal(modalDialog(
  #       title = "Invalid table name",
  #       "Please type in the right column number.",
  #       footer = modalButton("OK"), easyClose = TRUE ) )
  #   }
    
  #   else if (input$ncols < 1) {
  #     showModal(modalDialog(
  #       title = "No columns",
  #       "Each table must have one or more columns.",
  #       footer = modalButton("OK"), easyClose = TRUE
  #     )) 
  #   }  
    
  #   else {
      
  #     # gather all the colnames into a list
  #     col_names_list = list()
  #     for (i in seq_len(input$ncols)) {
  #       col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
  #     }

  #     # in case there are column with no names/duplicate names/informal signs/in sqlite keywords
  #     if ( any(col_names_list == '') | 
  #          sum(duplicated(col_names_list)) > 0 |
  #          any(grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",col_names_list) == FALSE) |
  #          any(tolower(col_names_list) %in% sqlite_kw_lo) ) {
  #       showModal(modalDialog(
  #         title = "Invalid column name",
  #         "You get this message possibly because: 
  #          1) one or more fields are blank;
  #          2) one or more fields contain invalid SQLite column name(s); 
  #          3) there are duplicate column names;
  #          or 4) one or more fields conflict with a SQLite keyword.",
  #         footer = modalButton("OK"), easyClose = TRUE
  #       ) )
  #       return()
  #     }
      
  #     # compile query
  #     query <- paste0('CREATE TABLE ',input$table_name,' (')
  #     for (i in seq_len(input$ncols)) { 
  #       query <- paste0(query,input[[paste0("colName", i)]],' ',input[[paste0("colSources", i)]],',')
  #     }
  #     query <- paste0(str_sub(query,1,-2),')')
  #     dbGetQuery(
  #       conn = db,
  #       statement = query )

  #     # if succuess, after create table, update the list of tables in tab2,3,5 and clear the input box
  #     # if not, no update date
  #     updateNumericInput(session, "ncols", value = '1')
  #     updateTextInput(session, "table_name", value = '')
  #     for (sel_input in c('sel_table_2','sel_table_3','sel_table_3_i','sel_table_3_ii')) {
  #       updateSelectInput(session, sel_input, 
  #                         choices = dbListTables(db))
  #     }
  #     updateSelectInput(session, 'sel_table_1', choices = dbListTables(db))
  #     updateSelectInput(session, 'sel_table_5', 
  #                       choices = dbListTables(db))
  #     showModal(modalDialog(
  #       title = "Success",
  #       "The table has been successfully created.",
  #       footer = modalButton("OK"), easyClose = TRUE ) )
  #   }
  # }
  # )
  
  ############# Tab 3: View Stats
  output$tab3UI <- renderUI({

    req(credentials()$user_auth)
    fluidPage(
        br(),
        # fluidRow(
        #   column(1),
        #   column(2,
        #     dateRangeInput(
        #     "input_period",
        #     "Select time period",
        #     start = as.Date("2022-01-01"),
        #     end = Sys.Date(),
        #     min = as.Date("2006-01-01"),
        #     max = Sys.Date()
        #     )
        #   ),
        #   column(1),
        #   column(3,
        #     radioButtons(
        #      "input_frequency",
        #       label = "Reporting Frequency",
        #       choices = c("Monthly", "Yearly"),
        #       selected = "Monthly",
        #       inline = TRUE
        #     )
        #   )
        # ),

        #   column(1,
        #     radioButtons(
        #      "input_category",
        #       label = "Split by Category",
        #       choices = c("Yes", "No"),
        #       selected = "No",
        #       inline = TRUE
        #     )
        #   ),
        #   column(1,
        #     radioButtons(
        #      "input_groupe",
        #       label = "Split by Groupe",
        #       choices = c("Yes", "No"),
        #       selected = "No",
        #       inline = TRUE,
        #       width = "100%"
        #     )
        #   ),
        #   column(1,
        #     radioButtons(
        #      "input_source",
        #       label = "Split by Source",
        #       choices = c("Yes", "No"),
        #       selected = "No",
        #       inline = TRUE
        #     )
        #   ),
        #   column(1)
        # ),
        # br(),
        # fluidRow(
        #   column(1),
        #   column(
        #     10,
        #     selectInput(
        #      "input_variable",
        #       label = "Select variable of interest",
        #       choices = c(
        #         "count"),
        #       selected = "count"
        #     )
        #   ),
        #   column(1)
        #),
        # br(),
        # br(),
        # br(),
        fluidRow(
          column(1),
          column(10),
          column(1)
        ),
        br(), br(),
        fluidRow(
          column(1),
          column(10),
          column(1)
        ),
         br(), br(),
        fluidRow(
          column(1),
          column(10,
        echarts4rOutput(
              "plot_wordcloud",
              height = 650
            )
          ),
          column(1)
        )
    ) # end fluidPage
} ) 
              
  



    



 output$plot_wordcloud <- renderEcharts4r({

          #corp_uk <- corpus(data_char_ukimmig2010) # or connect whatever
          mestokens <- tokens(corpus(data_char_ukimmig2010) , remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)

          mestokens <- tokens_split(mestokens,"'")
          mots_a_enlever <- c(stopwords("fr"), stopwords("en"), letters, wordlist)


          text <-
            dfm(mestokens) |> 
            dfm_remove(mots_a_enlever)  #|>
           # dfm_trim(min_termfreq = 100, verbose = FALSE) 

          features_dfm_inaug <- quanteda.textstats::textstat_frequency(text, n = 50)

          # Sort by reverse frequency order
          features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))


          features_dfm_inaug$color = rep(rep(viridis::viridis(n=5),each=10),1)

          features_dfm_inaug$color[1] =  "pink"

          features_dfm_inaug |>
            e_charts() |>
            e_cloud(
              feature , frequency , color , shape = "circle", 
              sizeRange = c(20, 40) # make words larger
            ) |>
            e_toolbox_feature(feature = "saveAsImage")  # hit the download button!



  })
      
  
  # # Add Column! 
  # observeEvent(input$add_col, {
  #   req(isTruthy(input$sel_table_3_i))
  #   d <- dbGetQuery(
  #     conn = db,
  #     statement = paste0('Select * from ',input$sel_table_3_i)
  #   )
  #   # in case the col name already exists
  #   if ( !isTruthy(input$add_col_name) | 
  #        input$add_col_name %in% colnames(d) |
  #        grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$add_col_name) == FALSE |
  #        tolower(input$add_col_name) %in% sqlite_kw_lo ) 
  #   {
  #     showModal(modalDialog(
  #       title = "Invalid column name",
  #       "You get this message possibly because: 
  #                      1) the column name already exists;
  #                      2) the field is blank;
  #                      3) this is an invalid SQLite column name;
  #                      or 4) the field name conflicts with a SQLite keyword.",
  #       footer = modalButton("OK"), easyClose = TRUE
  #     ) )
  #   } else {
  #     dbGetQuery(
  #       conn = db,
  #       statement = paste0('ALTER TABLE ',input$sel_table_3_i,
  #                          ' ADD COLUMN ',input$add_col_name,
  #                          ' ',input$add_col_type)
  #     )
  #     # after add column, clear text input
  #     updateTextInput(session, "add_col_name", value = '')
  #     # after add column, update colunm select range
  #     updateSelectInput(session, "sel_table_3",
  #                       choices = dbListTables(db))
  #     updateSelectInput(session, "sel_col_3", choices = colnames(d))
  #     showModal(modalDialog(
  #       title = "Success",
  #       "The column has been successfully added.",
  #       footer = modalButton("OK"), easyClose = TRUE ) )
  #   }
  # }
  # )
  
  ############# Tab 6: About
  output$tab6UI <- renderUI({
    req(credentials()$user_auth)
    
    box(title = 'About this app',width = NULL, status = "primary",solidHeader = TRUE,
      
      "This Shiny app is a prototype of a database management system, featuring a variety of functions. 
          This app addresses the needs in back-end database management with a clean and easy to use UI.",
      br(),
      "Check the code on Github:",
      tags$head(tags$style(HTML("a {color: blue}"))),
      tags$a(icon("github"), 
                   href = "https://github.com/munoztd0/DbVieweR",
                   title = "See the code on github"),

      br(),
      br(),
      br(),
      br(),
      h5(strong("Developer")),
      img(src = 'https://avatars.githubusercontent.com/u/43644805?s=400&u=41bc00f6ee310ed215298c9af27ed53e9b3e1a60&v=4', height = 150, align = "right"),
      
      "David Munoz Tord, Freelance Data Scientist / R Developer",
      tags$a(icon("linkedin"), 
                href = "https://www.linkedin.com/in/david-munoz-tord-409639150",
                title = "See the code on github"),
      br(),
      p("A data enthusiast with steady ambition and restless curiosity.")

      
    )  
    
  })
  
}

# when exiting app, disconnect from the apple database
onStop(
  function()
  {
    dbDisconnect(db)
  }
)

# execute the Shiny app
shinyApp(ui, server)

