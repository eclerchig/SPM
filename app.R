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
library(shinyFeedback)
library(DT)
library(dplyr)
library(here)
library(xlsx)
library(reactlog)
library(RSQLite)
library(keyring)
library(bslib)
library(bsplus)
library(waiter)
library(gsubfn)

#//////оператор конкатенации строк///////
"%+%" <- function(...){
  paste0(...)
}

#---------ПОДКЛЮЧЕНИЕ МОДУЛЕЙ------------
source(here::here("modal_dialog.R"))
source(here::here("algorithm/find_ctree.R"))
#----------------------------------------

#-----------MATH.FINCTIONS---------------
out_rules <- function (tree, test){
  predict_nodes <- predict(tree, test, type = "node")
  term_ns <- partykit::nodeids(tree, terminal = TRUE)
  rule <- partykit:::.list.rules.party(tree)[which(predict_nodes[1] == term_ns)]
  step1 <- rule %>% str_split(' & ') %>% unlist() %>% str_split(' ')
  for (i in 1:length(step1)){
    if (step1[[i]][1] == 'ethnicity'){
      str <- step1[[i]][3] %+% step1[[i]][4] %+% step1[[i]][5]
      step1[[i]][3] <- str_remove_all(str,"NA")
    }
  }
  
  step2 <- step1 %>% lapply(function(x) data.frame(var = x[1], cond = x[2], value = x[3]))
  step3 <- step2 %>% bind_rows #объединение в один дата.фрейм
  step4 <- step3 %>% group_by(var, cond) 
  step5 <- step4 %>% filter(
    if (str_detect(unique(cond), '<')) 1:n() == which.min(as.numeric(value))
    else if (str_detect(unique(cond), '>')) 1:n() == which.max(as.numeric(value))
    else if (str_detect(unique(cond), 'in')) 1:n() == which.min(str_count(value)) 
    else 1:n() == which.min(str_count(value, ',')) %>%
      apply(1, paste, collapse = ' ') %>% paste(collapse = ' & ')
  )
  step5$cond[str_detect(step5$cond, "in")] <- "принадлежит категории(-ям): "
  step5$var[str_detect(step5$var, "ethnicity")]  <- "Национальность"
  step5$var[str_detect(step5$var, "age")]  <- "Возраст пациента"
  step5$value[str_detect(step5$value, "Asian")]  <- sub('Asian','Монголоидная', step5$value[str_detect(step5$value, "Asian")]) 
  step5$value[str_detect(step5$value, "White")]  <- sub('White','Европеоидная', step5$value[str_detect(step5$value, "White")])
  step5$value[str_detect(step5$var, "Национальность")] <- unlist(strsplit(step5$value[str_detect(step5$var, "Национальность")],split='"', fixed=TRUE))[2]
  return (step5)
}

out_diagnosis <- function (tree, test) {
  res <- predict_fibroids(test[1,])
  num_no <- table(best_tree[[1]]$plotTree[res[['node']]]$data[1])[1]
  num_yes <- table(best_tree[[1]]$plotTree[res[['node']]]$data[1])[2]
  
  prob_no <-res[['prob']][1]
  prob_yes <- res[['prob']][2]
  
  #text <- "Количество наблюдений, попавших в категорию \"Отсутствие диагноза\":" %+% num_no %+% ", вероятность: " %+% prob_no %+% "\n Количество наблюдений, попавших в категорию \"Наличие диагноза\":" %+% num_yes %+% ", вероятность: " %+% prob_yes
  text <- paste0("Количество наблюдений, попавших в категорию \"Отсутствие диагноза\": ", num_no,", вероятность: ", format(round(prob_no,2)), "\nКоличество наблюдений, попавших в категорию \"Наличие диагноза\": ", num_yes,", вероятность: ", format(round(prob_yes,2)))
  return(text)
}

#----------------------------------------
result_list <- find_best_tree()
best_tree <<- result_list[1]
best_trees <<- result_list[2]

print("go")

title_pages = c("main_page", "card_patient")
conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
patients_db <- dbGetQuery(conn, "SELECT * FROM Patients")
print(patients_db)
patients_db$birth_day <- as.Date(patients_db$birth_day)
eths <- dbGetQuery(conn, "SELECT name_ethnicity FROM Ethnicities") 
list_eths <- eths[1:nrow(eths),1]

# Define UI for application that draws a histogram

ui <- bootstrapPage(
  useWaiter(),
  useShinyFeedback(), # include shinyFeedback
  tags$head(   
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  theme = bs_theme(version = 5),
  fluidRow(
  class = "background justify-content-center",
  style = "min-height: 749px; background-color: #777777;padding: 50px 50px 50px 50px; margin: 0;",
  useShinyjs(),
  hidden(
    #---------------------------ОПИСАНИЕ UI MAIN СТРАНИЦЫ----------------------------
    div(
      class = "page col-8 rounded-3 border border-3 border-secondary",
      id = "main_page",
      style="background-color: #dcdcdc;padding: 25px 50px 25px 50px; height: fit-content;",
      div(
        class = "mt-2",
        h1("Список пациентов")
      ),
      div(
        class = "mt-3",
        shiny::actionButton(
          inputId = "add_patient",
          label = "Добавить пациента",
          icon = shiny::icon("plus"),
          class = "btn-add"
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
        class="d-grid gap-2 d-md-flex justify-content-md-begin",
        shiny::actionButton(
          inputId = "goto_patients",
          label = "<-     Вернуться",
          class = "btn ololo",
          style = "background-color: inherit; border-color:  inherit"
        )
      ),
      div(
      class = "mt-3",
        h1("Карта пациента")
      ),
      div(
        class = "main_info border-bottom border-2 block-content",
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
          class = " form-group mt-1",
          shiny::tags$label(
            class = "col-2 col-form-label",
            "Дата рождения"
          ),
          div(
            class = "col-2",
            textInput("birth_day", 
                      label = NULL) #необходим кастом)
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
            "edit_patient", label = "Внести изменения", class = "btn-edit"
          )
        )
      ),
      h3("Все посещения пациента в клинике"),
      div(
        class = "border-bottom border-2 block-content",
        shiny::actionButton(
          inputId = "add_visit",
          label = "Добавить посещение",
          icon = shiny::icon("plus"),
          class = "btn-add"
        ),
        div(
          class = "container-fluid mt-3",
          style = "padding: 0",
          DT::DTOutput(outputId = "dt_visits", width = "100%")
        )
      ),
      div(
        class = "border-bottom border-2 block-content",
        h3("Клинические анализы по посещению",
           class = "mt-4"
        ),
        hidden(
          div(
            id = "alert-labs",
            class = "alert alert-warning mt-1",
            "Для отображения анализов необходимо выбрать дату посещения пациента в таблице выше и нажать на кнопку 'Отобразить все анализы' "
          )
        ),
        shiny::actionButton(
          inputId = "add_lab",
          label = "Добавить запись анализов",
          icon = shiny::icon("plus"),
          class = "btn-add"
        ),
        hidden(
          div(
            id = "alert-labsnull",
            class = "alert alert-danger mt-2 row align-items-center",
            shiny::icon("exclamation-triangle",
                        class = "col-1",
                        style = "color: #dc3545;"),
            div(
              class = "col-10 alert-content",
              "Для выбранного посещения отсутствуют сохраненные клинические анализы"
            )
          )
        ),
        div(
          class="accordion_area mt-2",
          uiOutput("labs_output")
        )
      ),
      div(
        id = "block_diagnosis",
        h3("Вычисленный диагноз",
           class = "mt-4"),
        div(
          class="mb-3",
          textAreaInput(inputId="text_description",
                        label = "Описание постановки диагноза",
                        width = '100%',
                        rows = 10)
        ),
        div(
          class = "d-grid gap-2 col-5 mx-auto",
          hidden(
            actionButton(
              inputId = "edit_diagnosis",
              label = "Добавить результат диагноза в базу",
              icon = shiny::icon("edit"),
              class = "btn-edit"
            )
          )
        ),
        hidden(
          div(
            id = "pic",
            div(
              id = "pic_tree",
              h4("Модель дерева",
                class = "text-center"),
              plotOutput(
                outputId = "treePlot")
            ),
            div(
              id = "pic_ROC",
              class="row justify-content-md-center mt-2",
              h4("График ROC-кривой",
                 class = "text-center"),
              p("AUC = " %+% as.character(round(best_tree[[1]]$scoreAUC, digits = 2)),
                 class = "text-center"),
              plotOutput(
                outputId = "ROCplot")
            )
          )
        ),
        div(
          class = "mt-2 d-grid gap-2 d-md-flex justify-content-md-end mb-2",
          downloadButton(
            "report", label = "Вывести документ", class = "btn btn-more me-md-2"
          ),
          downloadButton(
            "dwnd_plot", label = "Сохранить изображение дерева", class = "btn btn-more"
          ),
          downloadButton(
            "dwnd_ROCplot", label = "Сохранить график ROC-кривой", class = "btn btn-more"
          )
        )
      )
    )
  )
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
                          <button class="btn btn-more action-button action_button" id="info_',
                       .x, '" type="button" onclick=get_id(this.id)>Открыть карту пациента</button>
                          <button class="btn action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>
                       </div>'
                     ))
}

create_btns2 <- function(x) { 
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                          <button class="btn btn-edit action-button action_button" id="editVisit_',
                       .x, '" type="button" onclick=get_id_visit(this.id)>Редактировать</button>
                        <button class="btn action-button btn-more action_button" id="showVisit_',
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
                         DK = c(),
                         KD_CT = c(),
                         MDA = c(),
                         COD = c(),
                         GSH = c(),
                         vitE = c(),
                         vitA = c(),
                         PRL = c(),
                         fibroids = c()),
                         
                         
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
    shinyjs::disable("FIO")
    shinyjs::disable("birth_day")
    shinyjs::disable("ethnicity")
    shinyjs::hide("edit_diagnosis")
    rv$dt_row <- which(stringr::str_detect(rv$df[,"Действия"], pattern = paste0("\\b", input$current_id, "\\b")))
    select_row <- rv$df[rv$dt_row, ]
    print(select_row$id)
    set_patient_info(select_row[,"id"], select_row[,"ФИО пациента"], select_row[,"Дата рождения"], select_row[,"Национальность"])
    print(rv$patient_info)
    change_page("card_patient")
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
      table <- res %>%  
        mutate(date_visit = as.Date(res$date_visit)) %>%
        dplyr::bind_cols(tibble("Buttons" = character()))
    } else{
      rv$keep_track_id_visits = max(res$id)+ 1
      btns <- create_btns2(1:nrow(res))
      table <- res %>% dplyr::bind_cols(tibble("Действия" = btns))
    }
    on.exit(dbDisconnect(conn))
    colnames(table) <- c ("id", "Дата посещения", "Возраст пациента", "Действия")
    rv$df_visits <- table
    print("update inputs")
  })
  
  #----------------------ОТОБРАЖЕНИЕ/ПРЯТАНЬЕ СТРАНИЦ---------------------
  shiny::observeEvent(rv$page, {
    print(rv$page)
    shinyjs::hide(id = "pic")
    shinyjs::hide(selector = ".page")
    shinyjs::show(id = rv$page)
    shinyjs::disable("diagnosis_input")
    shinyjs::disable("text_description")
    if (rv$page == "card_patient") {
      shinyjs::show(id = "alert-labs")
    } else {
      shinyjs::hide(id = "alert-labs")
    }
    shinyjs::hide(id = "alert-labsnull")
  })
  
  shiny::observeEvent(input$goto_patients, {
    change_page("main_page")
    rv$show_labs <- FALSE
    rv$df_labs <- NULL
    shinyjs::hide(id = "alert-page")
  })
  
  observeEvent(input$edit_patient, {
    row <- rv$df[rv$dt_row, ]
    rv$edit_button = TRUE
    modal_dialog(
      FIO = row[,"ФИО пациента"], birth_day = row[,"Дата рождения"], eth = row[," Национальность"], list_eths = list_eths, edit = rv$edit_button
    )
    }
  )
  
  # ПОДТВЕРЖДЕНИЕ ИЗМЕНЕНИЕ ЗАПИСИ ПАЦИЕНТА
  shiny::observeEvent(input$finalEdit_patient, {
    
    shiny::req(!is.null(input$current_id) & (rv$edit_button == TRUE))
    errorFIO <- nchar(input$FIO_modal) == 0
    errorBirth <- length(input$birthday_modal) == 0
    
    shinyFeedback::feedbackWarning("birthday_modal", errorBirth, "Введите дату!", icon = NULL)
    shinyFeedback::feedbackWarning("FIO_modal", errorFIO, "Введите текст!", icon = NULL)
    shiny::req(input$FIO_modal, input$birthday_modal)
    
    rv$edit_button = NULL
    edited_row <- dplyr::tibble(
      id = as.numeric(rv$df[rv$dt_row, "id"]),
      FIO = input$FIO_modal,
      birth_day = input$birthday_modal,
      ethnicity = input$eth_modal,
      Buttons = rv$df[rv$dt_row, "Действия"]
    )
    
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
    set_patient_info(edited_row[['id']], input$FIO_modal, input$birthday_modal, input$eth_modal)

    shiny::removeModal()
  })
  
  # when final edit button is clicked, table will be changed
  shiny::observeEvent(input$finalEdit_patient, {
    shiny::req(rv$edit_button == FALSE)
    errorFIO <- nchar(input$FIO_modal) == 0
    errorBirth <- length(input$birthday_modal) == 0
    shinyFeedback::feedbackWarning("birthday_modal", errorBirth, "Введите дату!", icon = NULL)
    shinyFeedback::feedbackWarning("FIO_modal", errorFIO, "Введите текст!", icon = NULL)
    shiny::req(input$FIO_modal, input$birthday_modal)
    
    rv$edit_button = NULL
    
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
    
    colnames(add_row) <- c("id","ФИО пациента","Дата рождения", "Национальность", "Действия")
    rv$df <- rv$df %>%
    dplyr::bind_rows(add_row)
    rv$keep_track_id <- max(rv$df[,"id"]) + 1
    
    query <- "INSERT INTO Patients (FIO, birth_day, ethnicity) VALUES (
              '" %+% add_row[,'ФИО пациента'] %+% "',
              '" %+% add_row[,'Дата рождения'] %+% "',
              '" %+% which(stringr::str_detect(list_eths, add_row[,'Национальность'])) %+% "')"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    
    rv$edit_button = NULL
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$add_patient, {
    print("add enter")
    rv$edit_button = FALSE
    modal_dialog(
      FIO = "", birth_day = Sys.Date(), eth = "", list_eths = list_eths, edit = rv$edit_button)
  })
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete"))
    confirm_delete("Patient")
  })
  
  shiny::observeEvent(input$finalConfirm_deletePatient, {
    rv$dt_row <- which(stringr::str_detect(rv$df[,"Действия"], pattern = paste0("\\b", input$current_id, "\\b")))
    
    current_id = as.numeric(rv$df[rv$dt_row, "id"])
    query <- "DELETE FROM Patients WHERE id = '" %+% current_id %+%"'"
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
    shinyjs::hide(id = "alert labsnull")
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "edit"))
    print("change_row_visit")
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    select_row <- rv$df_visits[rv$dt_visit_row, ]
    print(select_row$id)
    rv$edit_button = TRUE
    modal_dVisit(
      date_visit = select_row[, "Дата посещения"], edit = rv$edit_button)
  })
  
  shiny::observeEvent(input$current_id_visit, {
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "show"))
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    select_row <- rv$df_visits[rv$dt_visit_row, ]
    quary <- "SELECT * FROM Lab_tests WHERE id_visit =
              '" %+% select_row$id %+% "'"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    rv$df_labs <-  dbGetQuery(conn, quary) %>%
      mutate(date_lab = as.Date(date),)
    if (nrow(rv$df_labs) == 0){
      rv$keep_track_id_labs <- 1
      shinyjs::show(id = "alert-labsnull")
    } else{
      rv$keep_track_id_labs = max(rv$df_labs$id)+ 1
      shinyjs::hide(id = "alert-labsnull")
    }
    shinyjs::hide(id = "alert-labs")
    on.exit(dbDisconnect(conn))
    rv$show_labs <- TRUE
  })
  #-------------------------ФУНКЦИЯ НА ВЫЗОВ УДАЛЕНИЯ СТРОКИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$finalConfirm_deleteVisit, {
    
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "delete"))
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    current_id = as.numeric(rv$df_visits[rv$dt_visit_row, "id"])
    query <- "DELETE FROM Visits WHERE id = '" %+% current_id %+%"'"
    
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    rv$df_visits <- rv$df_visits[-rv$dt_visit_row, ]
    shiny::removeModal()
  })
  #-------------------------ФУНКЦИЯ НА ВЫЗОВ ПОДТВЕРЖДЕНИЯ УДАЛЕНИЯ СТРОКИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$current_id_visit, {
    shiny::req(!is.null(input$current_id_visit) & stringr::str_detect(input$current_id_visit, pattern = "delete"))
    print("delete_row_visit")
    rv$dt_visit_row <- which(stringr::str_detect(rv$df_visits[,"Действия"], pattern = paste0("\\b", input$current_id_visit, "\\b")))
    confirm_delete("Visit")
  })
  
  #-------------------------ФУНКЦИЯ НА СОБЫТИЕ ДОБАВЛЕНИЯ ЗАПИСИ В ТАБЛИЦЕ VISIT------------------------------
  shiny::observeEvent(input$add_visit, {
      print("add visit")
      rv$edit_button = FALSE
      modal_dVisit(
        date_visit = Sys.Date(), edit = rv$edit_button)
    })
  
  #-------------------------ФУНКЦИЯ НА ДОБАВЛЕНИЕ ЗАПИСИ В БД И ТАБЛИЦУ------------------------------
  shiny::observeEvent(input$finalEdit_visit, {
    shiny::req(rv$edit_button == FALSE)
    print("final_add enter")
    errorVisit <- length(input$visit_modal) == 0
    shinyFeedback::feedbackWarning("visit_modal", errorVisit, "Введите дату!", icon = NULL)
    shiny::req(input$visit_modal)
    
    add_row <- data.frame(
      id_patient = rv$patient_info$id,
      date_visit = input$visit_modal
    )

    query <- "INSERT INTO Visits (id_patient, date_visit) VALUES (
              '" %+% add_row[,'id_patient'] %+% "',
              '" %+% add_row[,'date_visit'] %+% "')"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    
    query <- "SELECT * FROM Visits WHERE id_patient =
              '" %+% rv$patient_info$id %+% "'"
    add_row <- dbGetQuery(conn,query)
    
    add_row <- add_row %>%                                                       #добавляем столбец с определением HTML-кода кнопки для строки таблицы
      dplyr::bind_cols(tibble("Buttons" = create_btns2(1:nrow(add_row))))

    on.exit(dbDisconnect(conn))
    
    
    colnames(add_row) <- c("id","ФИО пациента","Дата посещения", "Возраст пациента", "Действия")
    add_row[,"Дата посещения"] <- as.Date(add_row[,"Дата посещения"])
    
    rv$df_visits <- add_row[,c("id","Дата посещения","Возраст пациента", "Действия")]
    
    if (nrow(add_row) != 0){
      rv$keep_track_id_visits <- max(rv$df_visits[,"id"]) + 1
    } else rv$keep_track_id_visits = 1
    rv$edit_button = NULL
    shiny::removeModal()
  })
  
  #-------------------------ФУНКЦИЯ НА ИЗМЕНЕНИЕ ЗАПИСИ В БД И ТАБЛИЦУ------------------------------
  shiny::observeEvent(input$finalEdit_visit, {
    shiny::req(!is.null(input$current_id) & (rv$edit_button == TRUE))
    print("final_edit enter")
    
    errorVisit <- length(input$visit_modal) == 0
    shinyFeedback::feedbackWarning("visit_modal", errorVisit, "Введите дату!", icon = NULL)
    shiny::req(input$visit_modal)
    
    query <- "UPDATE Visits SET date_visit =
              '" %+% as.Date(input$visit_modal) %+% "' WHERE id =
              '" %+% as.numeric(rv$df_visits[rv$dt_visit_row, "id"]) %+% "'"
              
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
  
    rv$df_visits[rv$dt_visit_row,"Дата посещения"] <- as.Date(input$visit_modal)
    
    rv$edit_button = NULL
    shiny::removeModal()
  })
  
  output$treePlot <- renderPlot(
    plot(best_tree[[1]]$plotTree)
  )
  
  output$ROCplot <- renderPlot(
    plot(best_tree[[1]]$plotROC)
  )
  
  output$labs_output <- renderUI({
    req(nrow(rv$df_labs)!= 0)
    ui_parts <- c()
    for(i in 1:nrow(rv$df_labs)){
      ui_parts[[i]] <- div(
        class = "accordion_box",
        id = i,
        fluidRow(
            class = "acc_trigger border-bottom border-2",
            h4(class = "col-12",
              "ДАТА: "  %+% rv$df_labs[i,"date_lab"])
        ),
        div(
          class = "acc_container",
          div(
            class = "mt-3 row form-group",
            style = "margin: 0 0",
            tags$label(
              class = "col-1 col-form-label",
              "DK:"
            ),
            div(
              class = "col-2",
              textInput("DK_" %+% i,
                        value = rv$df_labs[i,"DK"],
                        label = NULL
              )
            ),
            tags$label(
              class = "col-1 col-form-label",
              "KD-CT:"
            ),
            div(
              class = "col-2",
              textInput("KDCT" %+% i,
                        value = rv$df_labs[i,"KD_CT"],
                        label = NULL
              )
            ),
            tags$label(
              class = "col-1 col-form-label",
              "MDA:"
            ),
            div(
              class = "col-2",
              textInput("MDA_" %+% i,
                        value = rv$df_labs[i,"MDA"],
                        label = NULL
              )
            ),
            tags$label(
              class = " col-1 col-form-label",
              "COD:"
            ),
            div(
              class = "col-2",
              textInput("COD_" %+% i,
                        value = rv$df_labs[i,"COD"],
                        label = NULL
              )
            ),
            tags$label(
              class = "col-1 col-form-label",
              "GSH:"
            ),
            div(
              class = "col-2",
              textInput("GSH_" %+% i,
                        value = rv$df_labs[i,"GSH"],
                        label = NULL
              )
            ),
            tags$label(
              class = "col-1 col-form-label",
              "vitE:"
            ),
            div(
              class = "col-2",
              textInput("vitE_" %+% i,
                        value = rv$df_labs[i,"vitE"],
                        label = NULL
              )
            ),
            tags$label(
              class = "col-1 col-form-label",
              "vitA:"
            ),
            div(
              class = "col-2",
              textInput("vitA_" %+% i, 
                        value = rv$df_labs[i,"vitA"],
                        label = NULL
              )
            ),
            tags$label(
              class = "col-1 col-form-label",
              "PRL:"
            ),
            div(
                class = "col-2",
                textInput("PRL_" %+% i,
                          value = rv$df_labs[i,"PRL"],
                          label = NULL
                )
            )
          ),
          div(
            class = "row",
            HTML('<div class = "btn-group">
                          <button class="btn btn-default action-button btn-edit action_button" id="editLab_',i,'" type="button" onclick=get_id_lab(this.id)>Редактировать</button>
                          <button class="btn btn-default action-button btn-more action_button" id="calcLab_',i,'" type="button" onclick=get_id_lab(this.id)>Провести диагностику</button>
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
    shinyjs::hide(id = "pic")
    shinyjs::hide("edit_diagnosis")
    shiny::req(!is.null(input$current_id_lab) & stringr::str_detect(input$current_id_lab, pattern = "delete"))
    print("change_row_lab")
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    confirm_delete("Lab")
  })
  
  shiny::observeEvent(input$current_id_lab, {
    shiny::req(!is.null(input$current_id_lab) & stringr::str_detect(input$current_id_lab, pattern = "edit"))
    print("change_row_lab")
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    select_row <- rv$df_labs[rv$dt_lab_row, ]
    
    rv$edit_button = TRUE
    modal_dLab(
      select_row$date, select_row$DK, select_row$KD_CT, select_row$MDA, select_row$COD, select_row$GSH, select_row$vitE, select_row$vitA, select_row$PRL, edit = rv$edit_button)
  })
  
  shiny::observeEvent(input$current_id_lab, {
    shiny::req(!is.null(input$current_id_lab) & stringr::str_detect(input$current_id_lab, pattern = "calc"))
    shinyjs::show(id = "pic")
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    select_row <- rv$df_labs[rv$dt_lab_row, ]
    modify_row <- data.frame(
      DK = select_row[,"DK"],
      KD_CT = select_row[,"KD_CT"],
      MDA = select_row[,"MDA"],
      COD = select_row[,"COD"],
      GSH = select_row[,"GSH"],
      vitE = select_row[,"vitE"],
      vitA = select_row[,"vitA"],
      age_visit1 = as.numeric(rv$df_visits[rv$dt_visit_row, "Возраст пациента"]),
      ethnicity = which(stringr::str_detect(list_eths, rv$patient_info$ethnicity)),
      PRL2 = select_row[,"PRL"]
    ) %>% mutate (
      ethnicity = factor(ethnicity, levels = c(1, 2, 3), labels = c('White', 'Asian', 'Mixed'))
    )
    fibroids <- predict_fibroids(modify_row[1,])
    if (fibroids[["prediction"]] == "No") {
      rv$df_labs[rv$dt_lab_row, "fibroids"] = 0
    } else {
      rv$df_labs[rv$dt_lab_row, "fibroids"] = 1
    }
    description <- out_rules(best_tree[[1]]$plotTree, modify_row[1,])
    text = "Условия постановки конечного диагноза:\n"
    for (i in 1:nrow(description)){
      add <- i %+% ")  " %+% description[i,"var"] %+% " " %+% description[i,"cond"]  %+% " " %+% description[i,"value"]  %+% "\n"
      text <- text %+% add
    }
    updateTextAreaInput(inputId = "text_description", value = out_diagnosis(best_tree[[1]]$plotTree, modify_row) %+% "\n" %+% text)
    shinyjs::show("edit_diagnosis")
  })
  
  shiny::observeEvent(input$edit_diagnosis, {
    shiny::req(!is.null(input$current_id_lab))
    
    rv$dt_lab_row <- as.numeric(strsplit(input$current_id_lab, "_")[[1]][2])
    select_row <- rv$df_labs[rv$dt_lab_row, ]
    query <- "UPDATE Lab_tests SET
              fibroids ='" %+% select_row[['fibroids']] %+% "'
              WHERE id = '" %+% select_row[['id']] %+% "'"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
  })
  
  #-------------------------ФУНКЦИИ НА ВЫЗОВ ДОБАВЛЕНИЯ СТРОКИ В СПИСОК LAB_TESTS------------------------------
  
  shiny::observeEvent(input$add_lab, {
    print("add lab")
    rv$edit_button = FALSE
    modal_dLab(
      date = "", DK = "", KDCT = "", MDA = "", COD = "", GSH = "", vitE = "", vitA = "", PRL = "", edit = rv$edit_button)
  })
  
  #-------------------------ФУНКЦИЯ ПОДТВЕРЖДЕНИЯ ИЗМЕНЕНИЯ СТРОКИ LAB_TESTS------------------------------
  
  shiny::observeEvent(input$finalEdit_lab, {
    shiny::req(!is.null(input$current_id_labs) & (rv$edit_button == TRUE))
    
    errorData <- length(input$date_modal) == 0
    shinyFeedback::feedbackWarning("date_modal", errorData, "Введите дату!", icon = NULL)
    shiny::req(input$date_modal)
    
    rv$df_labs[rv$dt_lab_row, "date_lab"] <- as.Date(input$date_modal)
    rv$df_labs[rv$dt_lab_row, "DK"] <- input$DK_modal
    rv$df_labs[rv$dt_lab_row, "KD_CT"] <- input$KDCT_modal
    rv$df_labs[rv$dt_lab_row, "MDA"] <- input$MDA_modal
    rv$df_labs[rv$dt_lab_row, "COD"] <- input$COD_modal
    rv$df_labs[rv$dt_lab_row, "GSH"] <- input$GSH_modal
    rv$df_labs[rv$dt_lab_row, "vitE"] <- input$vitE_modal
    rv$df_labs[rv$dt_lab_row, "vitA"] <- input$vitA_modal
    rv$df_labs[rv$dt_lab_row, "PRL"] <- input$PRL_modal
    
    rv$df_labs <- sort_labs(rv$df_labs)
    
    query <- "UPDATE Lab_tests SET
              date ='" %+% as.Date(input$date_modal) %+% "',
              DK = '" %+% input$DK_modal %+% "',
              KD_CT = '" %+% input$KDCT_modal %+% "',
              MDA = '" %+% input$MDA_modal %+% "',
              COD = '" %+% input$COD_modal %+% "',
              GSH = '" %+% input$GSH_modal %+% "',
              vitE = '" %+% input$vitE_modal %+% "',
              vitA = '" %+% input$vitA_modal %+% "',
              PRL ='" %+% input$PRL_modal %+% "'
              WHERE id = '" %+% rv$df_labs[rv$dt_lab_row, "id"] %+% "'"

    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    #обновление на странице карты пациента
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
      DK = input$DK_modal,
      KD_CT = input$KDCT_modal,
      MDA = input$MDA_modal,
      COD = input$COD_modal,
      GSH = input$GSH_modal,
      vitE = input$vitE_modal,
      PRL = input$PRL_modal,
      vitA = input$vitA_modal,
      fibroids = NULL
    )
    
    query <- "INSERT INTO Lab_tests (id_visit, date, DK, KD_CT, MDA, COD, GSH, vitE, vitA, PRL) VALUES (
              '" %+% add_row[,'id_visit'] %+% "',
              '" %+% add_row[,'date_lab'] %+% "',
              '" %+% add_row[,'DK'] %+% "',
              '" %+% add_row[,'KD_CT'] %+% "',
              '" %+% add_row[,'MDA'] %+% "',
              '" %+% add_row[,'COD'] %+% "',
              '" %+% add_row[,'GSH'] %+% "',
              '" %+% add_row[,'vitE'] %+% "',
              '" %+% add_row[,'vitA'] %+% "',
              '" %+% add_row[,'PRL'] %+% "')"
    conn <- dbConnect(RSQLite::SQLite(), "PatientsDB.db")
    dbExecute(conn, query)
    on.exit(dbDisconnect(conn))
    
    rv$df_labs <- rv$df_labs %>%
      dplyr::bind_rows(add_row) %>% sort_labs()
    rv$keep_track_id_labs <- max(rv$df_labs[,"id"]) + 1
    
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
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      waiter_show( # show the waiter
        html = spin_fading_circles() # use a spinner
      )
      
      path <- file.path(getwd(), "report.Rmd")
      rmarkdown::render(path, output_file = file,
                        params = list(best_trees = best_trees[[1]][]),
                        envir = new.env(parent = globalenv())
      )
      waiter_hide()
    }
  )
  
  output$dwnd_plot <- downloadHandler(
    filename = "tree.png", 
    content = function(file) {
      png(file)
      plot(best_tree[[1]]$plotTree)
      dev.off()
    }
  )
  
  output$dwnd_ROCplot <- downloadHandler(
    filename = "ROC.png", 
    content = function(file) {
      png(file)
      plot(best_tree[[1]]$plotROC)
      dev.off()
    }
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)
