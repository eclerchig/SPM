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
library(DT)
library(dplyr)
library(here)
library(xlsx)
library(reactlog)
library(RSQLite)
library(keyring)
library(bslib)
library(bsplus)


#//////оператор конкатенации строк///////
"%+%" <- function(...){
  paste0(...)
}
reactlog::reactlog_enable()

#---------ПОДКЛЮЧЕНИЕ МОДУЛЕЙ------------
source(here::here("modal_dialog.R"))
source(here::here("algorithm/find_ctree.R"))
#----------------------------------------
find_best_tree()
print(best_tree)
row <- data.frame(
  DK = 1,
  KD_CT = 1,
  MDA = 1,
  COD = 1,
  GSH = 1,
  vitE = 1,
  vitA = 1,
  age_visit1 = 1,
  ethnicity = 1,
  PRL2 = 1
) %>% mutate (
  ethnicity = factor(ethnicity, levels = c(1, 2, 3), labels = c('White', 'Asian', 'Mixed'))
)

predict_fibroids(row[1,])
print(predict_fibroids(row[1,])[[1]])



title_pages = c("main_page", "card_patient")
conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
patients_db <- dbGetQuery(conn, "SELECT * FROM Patients")
print(patients_db)
patients_db$birth_day <- as.Date(patients_db$birth_day)
print("1")
eths <- dbGetQuery(conn, "SELECT name_ethnicity FROM Ethnicities") 
list_eths <- eths[1:nrow(eths),1]

# Define UI for application that draws a histogram

ui <- bootstrapPage(
  tags$head(   
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  theme = bs_theme(version = 5),
  fluidRow(
  class = "background justify-content-center",
  style = "background-color: #786868;padding: 50px 50px 50px 50px; margin: 0; height: 100vh;",
  useShinyjs(),
  hidden(
    #---------------------------ОПИСАНИЕ UI MAIN СТРАНИЦЫ----------------------------
    div(
      class = "page col-8 rounded-3 border border-3 border-secondary",
      id = "main_page",
      style="background-color: #dcdcdc;padding: 25px 50px 25px 50px; height: fit-content;",
      div(
        class = "mt-3",
        h1("Еще не придумала заголовок")
      ),
      div(
        class = "mt-5",
        shiny::actionButton(
          inputId = "add_patient",
          label = "Добавить пациента",
          icon = shiny::icon("plus"),
          class = "btn-success"
        )
      ),
      div(
        class = "container-fluid mt-3",
        style = "padding: 0",
        DT::DTOutput(outputId = "dt_table", width = "100%")
      ),
      
      #JAVA-script inlude
      shiny::includeScript("script.js"),
    ),
    #---------------------------ОПИСАНИЕ UI CARD_PATIENT СТРАНИЦЫ----------------------------
    div(
      class = "page col-8 rounded-3 border border-3 border-secondary",
      id = "card_patient",
      style="background-color: #dcdcdc;padding: 25px 50px 25px 50px; height: fit-content;",
      div(
        class="d-grid gap-2 d-md-flex justify-content-md-begin mb-2",
        shiny::actionButton(
          inputId = "goto_patients",
          label = "Вернуться",
          icon = shiny::icon("arrow-left"),
          class = "btn btn-outline-light",
          style = "background-color: inherit; border-color: #000; color: #000;"
        )
      ),
      div(
      class = "mt-3",
        h1("Карта пациента")
      ),
      div(
        class = "main_info border-bottom border-2",
        style = "border-color: #acb1b7!important",
        fluidRow(
          class = " form-group mt-4",
          shiny::tags$label(
            class = "col-2 col-form-label",
            "ФИО пациента"
          ),
          div(
            class = "col-7",
            textInput("FIO", label = NULL)
          )
        ),
        fluidRow(
          class = " form-group mt-4",
          shiny::tags$label(
            class = "col-2 col-form-label",
            "Дата рождения"
          ),
          div(
            class = "col-2",
            textInput("birth_day", 
                      label = NULL #необходим кастом)
            )
          ),
          shiny::tags$label(
            class = "col-2 col-form-label",
            "Национальность"
          ),
          div(
            class = "col-3",
            textInput("ethnicity", 
                      label = NULL #необходим кастом)
            )
          )
        ),
        div(
          class = "d-grid gap-2 d-md-flex justify-content-md-end mb-2",
          actionButton(
            "edit_patient", "Внести изменения"
          )
        )
      ),
      h3("Все посещения пациента в клинике"),
      shiny::actionButton(
        inputId = "add_visit",
        label = "Добавить посещение",
        icon = shiny::icon("plus"),
        class = "btn-success"
      ),
      div(
        class = "container-fluid mt-3",
        style = "padding: 0",
        DT::DTOutput(outputId = "dt_visits", width = "100%")
      ),
      h3("Клинические анализы по посещению",
         class = "mt-4"),
      shiny::actionButton(
        inputId = "add_lab",
        label = "Добавить запись анализов",
        icon = shiny::icon("plus"),
        class = "btn-success"
      ),
      div(
        class="accordion_area mt-2",
        uiOutput("labs_output")
      ),
      div(
        id = "block_diagnosis",
        h3("Вычисленный диагноз",
           class = "mt-4"),
        shiny::textInput(
          inputId ="diagnosis_input",
          label = "Миома матки:", 
          value = ""),
        div(
          class="mb-3",
          textAreaInput(inputId="text_desciption",
                        label = "Описание хода постановки диагноза",
                        width = '100%',
                        rows = 10)
        )
      )
    )
  )
  ),
  HTML(
      "<script src='/assets/js/jquery-3.5.1.min.js'></script>"
  ),
  # # tags$script(HTML(
  # #   "src='https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js' integrity='sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1' crossorigin='anonymous'>"
  # # )),
  # # tags$script(HTML(
  # #   "src='https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js' integrity='sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM' crossorigin='anonymous'>"
  # # )),
  # tags$script(HTML(
  #   "src='https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/js/bootstrap.bundle.min.js' integrity='sha384-pprn3073KE6tl6bjs2QrFaJGz5/SUsLqktiwsUTF55Jfv3qYSDhgCecCxMW52nD2' crossorigin='anonymous'>"
  # )),
  # tags$head((HTML("<link>href='https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css' rel='stylesheet' integrity='sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor' crossorigin='anonymous'</link>")))
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css", integrity="sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor", crossorigin="anonymous")
  # ),
  #shiny::includeCSS("styles.css")
  includeScript(path = "www/jquary.js")
)

#функция добавления HTML-кода для создания кнопок удаления/редактирования
create_btns <- function(x) { 
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                          <button class="btn btn-default action-button btn-info action_button" id="info_',
                       .x, '" type="button" onclick=get_id(this.id)>подробнее</button>
                          <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>
                       </div>'
                     ))
}

create_btns2 <- function(x) { 
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                          <button class="btn btn-default action-button btn-info action_button" id="editVisit_',
                       .x, '" type="button" onclick=get_id_visit(this.id)>Редактировать</button>
                        <button class="btn btn-default action-button btn-info action_button" id="showVisit_',
                       .x, '" type="button" onclick=get_id_visit(this.id)>Отобразить все анализы</button>
                          <button class="btn btn-default action-button btn-danger action_button" id="deleteVisit_',
                       .x, '" type="button" onclick=get_id_visit(this.id)><i class="fa fa-trash-alt"></i></button>
                       </div>'
                     ))
}

if (nrow(patients_db) != 0) {
  html_btns <- create_btns(1:nrow(patients_db))
  html_table <- patients_db %>%                                                       #добавляем столбец с определением HTML-кода кнопки для строки таблицы
    dplyr::bind_cols(tibble("Buttons" = html_btns))
  eths <- c()
  for (i in 1:nrow(patients_db)){
    eths <- c(eths, dbGetQuery(conn, "SELECT name_ethnicity FROM Ethnicities WHERE id = " %+% patients_db$ethnicity[i])[1,1])
  }
  html_table$ethnicity <- eths
} else {
  html_table <- patients_db %>%                                                       #добавляем столбец с определением HTML-кода кнопки для строки таблицы
    dplyr::bind_cols(tibble("Buttons" = character())) %>%
    mutate(FIO = character()) %>%
    mutate(ethnicity = character())
}
colnames(html_table) <- c("id","ФИО пациента","Дата рождения", "Национальность", "Действия")

dbDisconnect(conn)

# Define server logic required to draw a histogram
server <- function(input, output) {
  rv <- shiny::reactiveValues(
    page = "main_page",
    df = html_table, #исх таблица данных
    df_visits = data.frame(id = c(),
                           date_visit = c(),
                           age_patient = c()),
    df_labs = data.frame(id = c(),
                         date_lab = c(),
                         PRL = c(),
                         vitA = c()),
                         
                         
    dt_row = NULL,
    dt_visit_row = NULL,
    dt_lab_row = NULL,
    
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = max(html_table$id)+ 1, #отслеживает id
    patient_info = list(id = NULL,
                        FIO = NULL,
                        birth_day = NULL,
                        ethnicity = NULL),
    keep_track_id_visits = NULL, #отслеживает id visits
    keep_track_id_labs = NULL,
    show_labs = FALSE
  )
  
  change_page <-function(name) {
    if (name %in% title_pages) rv$page <- name
  }
  
  set_patient_info <- function(id, FIO, birth_day, ethnicity){
    rv$patient_info$id = id
    rv$patient_info$FIO = FIO
    rv$patient_info$birth_day = birth_day
    rv$patient_info$ethnicity = ethnicity
    print("set_patient_info")
  }
  
  
  
  output$dt_table <- DT::renderDT(
    {
      shiny::isolate(rv$df) #выполняется 1 раз при инициации зависимостей
    },
    escape = F,
    rownames = FALSE,
    options = list(processing = FALSE)
  )
  
  output$dt_visits <- DT::renderDT(
    {
      shiny::isolate(rv$df_visits) #выполняется 1 раз при инициации зависимостей
    },
    escape = F,
    rownames = FALSE,
    options = list(processing = FALSE)
  )
  
  proxy_patients <- DT::dataTableProxy("dt_table")
  proxy_visits <- DT::dataTableProxy("dt_visits")
  
  shiny::observe({
    DT::replaceData(proxy_patients, rv$df, resetPaging = FALSE, rownames = FALSE)
  })
  
  shiny::observe({
    DT::replaceData(proxy_visits, rv$df_visits, resetPaging = FALSE, rownames = FALSE)
  })
  
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "info"))
    rv$dt_row <- which(stringr::str_detect(rv$df[,"Действия"], pattern = paste0("\\b", input$current_id, "\\b")))
    select_row <- rv$df[rv$dt_row, ]
    print(select_row$id)
    set_patient_info(select_row[,"id"], select_row[,"ФИО пациента"], select_row[,"Дата рождения"], select_row[,"Национальность"])
    print(rv$patient_info)
    rv$page <- "card_patient"
    print(rv$page)
  })
  
  shiny::observeEvent(rv$patient_info, {
    print("done")
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "info"))
    updateTextInput(inputId = "FIO", value = rv$patient_info$FIO)
    updateTextInput(inputId = "birth_day", value = rv$patient_info$birth_day)
    updateTextInput(inputId = "ethnicity", value = rv$patient_info$ethnicity)

    quary = "SELECT * FROM Visits WHERE id_patient = '" %+% rv$patient_info$id %+% "'"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    res <-  dbGetQuery(conn, quary) %>%
            transmute(id = id,
                      date_visit = as.Date(date_visit),
                      age_patient = age_patient)
    
    if (nrow(res) == 0){
      rv$keep_track_id_visits <- 1
    } else{
      rv$keep_track_id_visits = max(res$id)+ 1
    }
    on.exit(dbDisconnect(conn))
    
    if (nrow(res) != 0) {
      btns <- create_btns2(1:nrow(res))
      table <- res %>%                                                       #добавляем столбец с определением HTML-кода кнопки для строки таблицы
        dplyr::bind_cols(tibble("Действия" = btns))
    } else {
      table <- res %>%                                                       #добавляем столбец с определением HTML-кода кнопки для строки таблицы
        mutate(date_visit = as.Date(res$date_visit)) %>%
        dplyr::bind_cols(tibble("Buttons" = character()))
    }
    
    colnames(table) <- c ("id", "Дата посещения", "Возраст пациента", "Действия")
    rv$df_visits <- table
    
    print("update inputs")
  })
  
  #----------------------ОТОБРАЖЕНИЕ/ПРЯТАНЬЕ СТРАНИЦ---------------------
  shiny::observeEvent(rv$page, {
    print(rv$page)
    shinyjs::hide(selector = ".page")
    shinyjs::show(id = rv$page)
  })
  
  shiny::observeEvent(input$goto_patients, {
    rv$page <- "main_page"
    print(rv$page)
    rv$show_labs <- FALSE
    rv$df_labs = NULL
  })
  
  observeEvent(input$edit_patient, {
    row <- rv$df[rv$dt_row, ]
    rv$edit_button = TRUE
    modal_dialog(
      FIO = row$FIO, birth_day = row$birth_day, eth = row$ethnicity, list_eths = list_eths, edit = rv$edit_button
    )
    }
  )
  
  # ПОДТВЕРЖДЕНИЕ ИЗМЕНЕНИЕ ЗАПИСИ ПАЦИЕНТА
  shiny::observeEvent(input$finalEdit_patient, {
    shiny::req(!is.null(input$current_id) & (rv$edit_button == TRUE))

    edited_row <- dplyr::tibble(
      id = as.numeric(rv$df[rv$dt_row, "id"]),
      FIO = input$FIO_modal,
      birth_day = input$birthday_modal,
      ethnicity = input$eth_modal,
      Buttons = rv$df$Buttons[rv$dt_row]
    )
    
    # rv$df[rv$dt_row, "id"] <- rv$dt_row
    # rv$df[rv$dt_row, "FIO"] <- input$FIO_modal
    # rv$df[rv$dt_row, "birth_day"] <- input$birthday_modal
    # rv$df[rv$dt_row, "ethnicity"] <- input$eth_modal
    # rv$df[rv$dt_row, "Buttons"] <- rv$df$Buttons[rv$dt_row]
    browser()
    rv$df[rv$dt_row, ] <- edited_row
    query <- "UPDATE Patients SET
              FIO ='" %+% edited_row[['FIO']] %+% "',
              birth_day = '" %+% edited_row[['birth_day']] %+% "',
              ethnicity ='" %+% which(stringr::str_detect(list_eths, edited_row[['ethnicity']])) %+% "'
              WHERE id = '" %+% edited_row[['id']] %+% "'"
    browser()
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    #обновление на странице карты пациента
    set_patient_info(rv$dt_row, input$FIO_modal, input$birthday_modal, input$eth_modal)
    rv$edit_button = NULL

    shiny::removeModal()
  })
  
  # when final edit button is clicked, table will be changed
  shiny::observeEvent(input$finalEdit_patient, {
    shiny::req(rv$edit_button == FALSE)
    print("final_edit enter")
    if (nrow(rv$df) == 0){
      rv$keep_track_id <- 1
    }  
    add_row <- data.frame(
      id = c(rv$keep_track_id),
      FIO = c(input$FIO_modal),
      birth_day = c(input$birthday_modal),
      ethnicity = c(input$eth_modal),
      Buttons = c(create_btns(rv$keep_track_id))
    )
    print(add_row)
    
    browser()
    colnames(add_row) <- c("id","ФИО пациента","Дата рождения", "Национальность", "Действия")
    browser()
    rv$df <- rv$df %>%
    dplyr::bind_rows(add_row)
    rv$keep_track_id <- max(rv$df[,"id"]) + 1
    browser()
    
    query <- "INSERT INTO Patients (FIO, birth_day, ethnicity) VALUES (
              '" %+% add_row[,'ФИО пациента'] %+% "',
              '" %+% add_row[,'Дата рождения'] %+% "',
              '" %+% which(stringr::str_detect(list_eths, add_row[,'Национальность'])) %+% "')"
    browser()
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    
    rv$edit_button = NULL
    browser()
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$add_patient, {
    print("add enter")
    rv$edit_button = FALSE
    modal_dialog(
      FIO = "", birth_day = "", eth = "", list_eths = list_eths, edit = rv$edit_button)
  })
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete"))
    confirm_delete("Patient")
  })
  
  shiny::observeEvent(input$finalConfirm_deletePatient, {
    rv$dt_row <- which(stringr::str_detect(rv$df[,"Действия"], pattern = paste0("\\b", input$current_id, "\\b")))
    
    current_id = as.numeric(rv$df[rv$dt_row, "id"])
    query <- "DELETE FROM Patients WHERE id = '" %+% current_id %+%"'"
    browser()
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    
    rv$df <- rv$df[-rv$dt_row, ]
    
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$dismiss_confirm, {
    shiny::removeModal()
  })
  
  #-------------------------ФУНКЦИИ НА ВЫЗОВ ВЫБОРА СТРОКИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$current_id_visit, {
    print("change_row_visit")
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "edit"))
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    select_row <- rv$df_visits[rv$dt_visit_row, ]
    print(select_row$id)
    rv$edit_button = TRUE
    modal_dVisit(
      date_visit = select_row[, "Дата посещения"], edit = rv$edit_button)
  })
  
  shiny::observeEvent(input$current_id_visit, {
    print("show_row_visit")
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "show"))
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    select_row <- rv$df_visits[rv$dt_visit_row, ]
    quary <- "SELECT * FROM Lab_tests WHERE id_visit =
              '" %+% select_row$id %+% "'"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    rv$df_labs <-  dbGetQuery(conn, quary) %>%
      transmute(id = id,
                id_visit = id_visit, 
                date_lab = as.Date(date),
                PRL = PRL,
                vitA = vitA)
    
    if (nrow(rv$df_labs) == 0){
      rv$keep_track_id_labs <- 1
    } else{
      rv$keep_track_id_labs = max(rv$df_labs$id)+ 1
    }
    on.exit(dbDisconnect(conn))
    rv$show_labs <- TRUE
  })
  #-------------------------ФУНКЦИЯ НА ВЫЗОВ УДАЛЕНИЯ СТРОКИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$finalConfirm_deleteVisit, {
    rv$dt_row <- which(stringr::str_detect(rv$df[,"Действия"], pattern = paste0("\\b", input$current_id, "\\b")))
    
    current_id = as.numeric(rv$df[rv$dt_row, "id"])
    query <- "DELETE FROM Visits WHERE id = '" %+% current_id %+%"'"

    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))

    rv$df_visits <- rv$df_visits[-rv$dt_visit_row, ]
    
    shiny::removeModal()
  })
  #-------------------------ФУНКЦИЯ НА ВЫЗОВ ПОДТВЕРЖДЕНИЯ УДАЛЕНИЯ СТРОКИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$current_id_visit, {
    print("delete_row_visit")
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "delete"))
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    confirm_delete("Visit")
  })
  
  #-------------------------ФУНКЦИЯ НА СОБЫТИЕ ДОБАВЛЕНИЯ ЗАПИСИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$add_visit, {
      print("add visit")
      rv$edit_button = FALSE
      modal_dVisit(
        date_visit = "", edit = rv$edit_button)
    })
  
  #-------------------------ФУНКЦИЯ НА ДОБАВЛЕНИЕ ЗАПИСИ В БД И ТАБЛИЦУ------------------------------
  shiny::observeEvent(input$finalEdit_visit, {
    shiny::req(rv$edit_button == FALSE)
    print("final_add enter")
    
    add_row <- data.frame(
      id_patient = rv$patient_info$id,
      date_visit = input$visit_modal
    )

    query <- "INSERT INTO Visits (id_patient, date_visit) VALUES (
              '" %+% add_row[,'id_patient'] %+% "',
              '" %+% add_row[,'date_visit'] %+% "')"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    
    query <- "SELECT * FROM Visits WHERE id =
              '" %+% rv$keep_track_id_visits %+% "'"
    add_row <- dbGetQuery(conn,query)
    
    add_row <- add_row %>%                                                       #добавляем столбец с определением HTML-кода кнопки для строки таблицы
      dplyr::bind_cols(tibble("Buttons" = create_btns2(rv$keep_track_id)))
    
    on.exit(dbDisconnect(conn))
  
    
    colnames(add_row) <- c("id","ФИО пациента","Дата посещения", "Возраст пациента", "Действия")
    add_row[,"Дата посещения"] <- as.Date(add_row[,"Дата посещения"])
    
    rv$df_visits <- rv$df_visits %>%
      dplyr::bind_rows(add_row[,c("id","Дата посещения","Возраст пациента", "Действия")])
    
    rv$keep_track_id_visits <- max(rv$df_visits[,"id"]) + 1
    browser()
    
    rv$edit_button = NULL
    shiny::removeModal()
  })
  
  #-------------------------ФУНКЦИЯ НА ИЗМЕНЕНИЕ ЗАПИСИ В БД И ТАБЛИЦУ------------------------------
  shiny::observeEvent(input$finalEdit_visit, {
    shiny::req(!is.null(input$current_id) & (rv$edit_button == TRUE))
    print("final_edit enter")
    
    browser()
    
    query <- "UPDATE Visits SET date_visit =
              '" %+% as.Date(input$visit_modal) %+% "' WHERE id =
              '" %+% as.numeric(rv$df_visits[rv$dt_visit_row, "id"]) %+% "'"
              
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
  
    rv$df_visits[rv$dt_visit_row,"Дата посещения"] <- as.Date(input$visit_modal)

    browser()
    
    rv$edit_button = NULL
    shiny::removeModal()
  })
  
  # ПОДТВЕРЖДЕНИЕ ИЗМЕНЕНИЕ ЗАПИСИ ПАЦИЕНТА
  shiny::observeEvent(input$finalEdit_patient, {
    shiny::req(!is.null(input$current_id) & (rv$edit_button == TRUE))
    
    edited_row <- dplyr::tibble(
      id = as.numeric(rv$df[rv$dt_row, "id"]),
      FIO = input$FIO_modal,
      birth_day = input$birthday_modal,
      ethnicity = input$eth_modal,
      Buttons = rv$df$Buttons[rv$dt_row]
    )
    
    # rv$df[rv$dt_row, "id"] <- rv$dt_row
    # rv$df[rv$dt_row, "FIO"] <- input$FIO_modal
    # rv$df[rv$dt_row, "birth_day"] <- input$birthday_modal
    # rv$df[rv$dt_row, "ethnicity"] <- input$eth_modal
    # rv$df[rv$dt_row, "Buttons"] <- rv$df$Buttons[rv$dt_row]

    rv$df[rv$dt_row, ] <- edited_row
    query <- "UPDATE Patients SET
              FIO ='" %+% edited_row[['FIO']] %+% "',
              birth_day = '" %+% edited_row[['birth_day']] %+% "',
              ethnicity ='" %+% which(stringr::str_detect(list_eths, edited_row[['ethnicity']])) %+% "'
              WHERE id = '" %+% edited_row[['id']] %+% "'"

    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    #обновление на странице карты пациента
    set_patient_info(rv$dt_row, input$FIO_modal, input$birthday_modal, input$eth_modal)
    rv$edit_button = NULL
    
    shiny::removeModal()
  })
  
  output$labs_output <- renderUI({
    req(nrow(rv$df_labs)!= 0)
    ui_parts <- c()
    for(i in 1:nrow(rv$df_labs)){
      ui_parts[[i]] <- div(
        class = "accordion_box",
        id = i,
        fluidRow(
            class = "acc_trigger",
            h4(class = "col-6",
              "Дата: "  %+% rv$df_labs[i,"date_lab"]),
            shiny::icon(class = "col-6", style = "text-align: right",
                        "plus")
        ),
        div(
          class = "acc_container",
          div(
            class = "mb-3 row form-group",
            style = "margin: 0 0",
            tags$label(
              class = "col-2 col-form-label",
              "Значение PRL:"
            ),
            div(
                class = "col-2",
                textInput("PRL_" %+% i,
                          value = rv$df_labs[i,"PRL"],
                          label = NULL #необходим кастом)
                )
            ),
            tags$label(
              class = "col-2 col-form-label",
              "Значение vitA:"
            ),
            div(
              class = "col-2",
              textInput("vitA_" %+% i, 
                        value = rv$df_labs[i,"vitA"],
                        label = NULL #необходим кастом)
              )
            )
          ),
          div(
            class = "row",
            HTML('<div class = "btn-group">
                          <button class="btn btn-default action-button btn-info action_button" id="calcLab_',i,'" type="button" onclick=get_id_lab(this.id)>Рассчиать диагноз</button>
                        <button class="btn btn-default action-button btn-info action_button" id="editLab_',i,'" type="button" onclick=get_id_lab(this.id)>Редактировать</button>
                          <button class="btn btn-default action-button btn-danger action_button" id="deleteLab_',i,'" type="button" onclick=get_id_lab(this.id)><i class="fa fa-trash-alt"></i></button>
                     </div>')
          )
        )
      )
    }
    ui_parts
  })
  
  #------------------------ ФУНКЦИЯ СОРТИРОВКИ ТАБЛИЦЫ LAB_TESTS-------------------
  sort_labs <- function(df){
    return (df[order(as.Date(df$date_lab, format="%Y-%m-%d")),])
  }
  
  #-------------------------ФУНКЦИИ НА ВЫЗОВ ВЫБОРА СТРОКИ В СПИСКЕ LAB_TESTS------------------------------
  shiny::observeEvent(input$current_id_lab, {
    shiny::req(!is.null(input$current_id_lab) & stringr::str_detect(input$current_id_lab, pattern = "delete"))
    print("change_row_lab")
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    confirm_delete("Lab")
  })
  
  shiny::observeEvent(input$current_id_lab, {
    shiny::req(!is.null(input$current_id_lab) & stringr::str_detect(input$current_id_lab, pattern = "edit"))
    print("change_row_lab")
    browser()
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    select_row <- rv$df_labs[rv$dt_lab_row, ]
    
    browser()
    
    rv$edit_button = TRUE
    modal_dLab(
      select_row$date, select_row$PRL, select_row$vitA, edit = rv$edit_button)
  })
  
  shiny::observeEvent(input$current_id_lab, {
    shiny::req(!is.null(input$current_id_lab) & stringr::str_detect(input$current_id_lab, pattern = "calc"))
    print("change_row_lab")
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    select_row <- rv$df_labs[rv$dt_lab_row, ]
  })
  
  #-------------------------ФУНКЦИИ НА ВЫЗОВ ДОБАВЛЕНИЯ СТРОКИ В СПИСОК LAB_TESTS------------------------------
  
  shiny::observeEvent(input$add_lab, {
    print("add lab")
    rv$edit_button = FALSE
    modal_dLab(
      date = "", PRL = "", vitA = "", edit = rv$edit_button)
  })
  
  #-------------------------ФУНКЦИЯ ПОДТВЕРЖДЕНИЯ ИЗМЕНЕНИЯ СТРОКИ LAB_TESTS------------------------------
  
  shiny::observeEvent(input$finalEdit_lab, {
    shiny::req(!is.null(input$current_id) & (rv$edit_button == TRUE))
    
    
    rv$df_labs[rv$dt_lab_row, "date_lab"] <- as.Date(input$date_modal)
    rv$df_labs[rv$dt_lab_row, "vitA"] <- input$vitA_modal
    rv$df_labs[rv$dt_lab_row, "PRL"] <- input$PRL_modal
    
    rv$df_labs <- sort_labs(rv$df_labs)
    
    browser()
    query <- "UPDATE Lab_tests SET
              date ='" %+% as.Date(input$date_modal) %+% "',
              vitA = '" %+% input$vitA_modal %+% "',
              PRL ='" %+% input$PRL_modal %+% "'
              WHERE id = '" %+% rv$df_labs[rv$dt_lab_row, "id"] %+% "'"
    browser()
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    browser()
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    #обновление на странице карты пациента
    browser()
    rv$edit_button = NULL
    
    shiny::removeModal()
  })
  
  #-------------------------ФУНКЦИЯ ПОДТВЕРЖДЕНИЯ ДОБАВЛЕНИЯ СТРОКИ В LABTESTS------------------------------
  
  shiny::observeEvent(input$finalEdit_lab, {
    shiny::req(!is.null(input$current_id) & (rv$edit_button == FALSE))
    print("final_add enter")
    
    add_row <- data.frame(
      id = rv$keep_track_id_labs,
      id_visit = rv$dt_visit_row,
      date_lab = as.Date(input$date_modal),
      PRL = input$PRL_modal,
      vitA = input$vitA_modal
    )
    query <- "INSERT INTO Lab_tests (id_visit, date, PRL, vitA) VALUES (
              '" %+% add_row[,'id_visit'] %+% "',
              '" %+% add_row[,'date_lab'] %+% "',
              '" %+% add_row[,'PRL'] %+% "',
              '" %+% add_row[,'vitA'] %+% "')"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    
    browser()
    rv$df_labs <- rv$df_labs %>%
      dplyr::bind_rows(add_row) %>% sort_labs()
    browser()
    rv$keep_track_id_labs <- max(rv$df_labs[,"id"]) + 1
    browser()
    
    rv$edit_button = NULL
    shiny::removeModal()
  })
  
  #-------------------------ФУНКЦИЯ ПОДТВЕРЖДЕНИЯ УДАЛЕНИЯ СТРОКИ В LABTESTS------------------------------
  shiny::observeEvent(input$finalConfirm_deleteLab, {
    
    current_id = as.numeric(rv$df_labs[rv$dt_lab_row, "id"])
    query <- "DELETE FROM Lab_tests WHERE id = '" %+% current_id %+%"'"
    
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    
    rv$df_labs <- sort_labs(rv$df_labs[-rv$dt_lab_row, ])

    
    shiny::removeModal()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
