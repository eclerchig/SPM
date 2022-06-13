modal_dialog <- function(FIO, birth_day, eth, list_eths, edit) {
  if (edit) {
    x <- "Подтвердить изменения"
  } else {
    x <- "Добавить запись"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "Данные пациента",
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "FIO_modal",
          label = "ФИО пациента",
          value = FIO)
      ),
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId ="birthday_modal",
          label = "Дата рождения", 
          value = birth_day,
          format = "yyyy-mm-dd",
          language = "ru")
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = "eth_modal", 
          label = "Национальность", 
          choices = list_eths)
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "finalEdit_patient",
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Закрыть",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}

confirm_delete <- function(mode){
  shiny::modalDialog(
    id = "confirm_modal",
    title = "Подверждение удаления",
    p("Вы действительно хотите удалить запись?"),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "finalConfirm_delete" %+% mode,
        label = "Да, удалить",
        icon = shiny::icon("check"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_confirm",
        label = "Отменить",
        icon = shiny::icon("fa-xmark"),
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}

modal_dVisit <- function(date_visit, edit) {
  if (edit) {
    x <- "Подтвердить изменения"
  } else {
    x <- "Добавить запись"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "Данные посещения",
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId ="visit_modal",
          label = "Дата посещения", 
          value = date_visit,
          format = "yyyy-mm-dd",
          language = "ru")
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "finalEdit_visit",
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Закрыть",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}

modal_dLab <- function(date, PRL, vitA, edit) {
  if (edit) {
    x <- "Подтвердить изменения"
  } else {
    x <- "Добавить запись"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "Данные лабораторных тестов",
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId ="date_modal",
          label = "Дата сдачи тестов", 
          value = date,
          format = "yyyy-mm-dd",
          language = "ru")
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="PRL_modal",
          label = "PRL значение", 
          value = PRL)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="vitA_modal",
          label = "Значение vitA", 
          value = vitA)
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "finalEdit_lab",
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Закрыть",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}
