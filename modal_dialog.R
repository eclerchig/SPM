modal_dialog <- function(FIO, birth_day, eth, list_eths, edit) {
  if (edit) {
    x <- "Подтвердить изменения"
  } else {
    x <- "Добавить запись"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "Форма заполнения информации о пациенте",
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
        class = "col-6",
        shiny::dateInput(
          inputId ="birthday_modal",
          label = "Дата рождения", 
          value = birth_day,
          format = "yyyy-mm-dd",
          language = "ru")
      ),
      div(
        style = "display: inline-block;",
        class = "col-6",
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
        label = "Отменить",
        class = "btn-danger",
        icon = shiny::icon("ban"),
      )
    )
  ) %>% shiny::showModal()
}

confirm_delete <- function(mode){
  shiny::modalDialog(
    id = "confirm_modal",
    title = "Подтверждение удаления записи",
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
        label = "Отмена",
        icon = shiny::icon("ban"),
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
    title = "Форма заполнения информации о посещении",
    div(
      class = "row justify-content-center",
      div(
        style = "display: inline-block;",
        class = "col-6",
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
        label = "Отмена",
        class = "btn-danger",
        icon = shiny::icon("ban")
      )
    )
  ) %>% shiny::showModal()
}

modal_dLab <- function(date, DK, KDCT, MDA, COD, GSH, vitE, vitA, PRL, edit) {
  if (edit) {
    x <- "Подтвердить изменения"
  } else {
    x <- "Добавить запись"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "Форма заполнения информации о клинических исследованиях",
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId ="date_modal",
          label = "Дата сдачи анализов", 
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
          inputId ="DK_modal",
          label = "Величина DK", 
          value = DK)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="KDCT_modal",
          label = "Величина KD-CT", 
          value = KDCT)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="MDA_modal",
          label = "Величина MDA", 
          value = MDA)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="COD_modal",
          label = "Величина COD", 
          value = COD)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="GSH_modal",
          label = "Величина GSH", 
          value = GSH)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="vitE_modal",
          label = "Количество витамина E", 
          value = vitE)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="vitA_modal",
          label = "Количество витамина А", 
          value = vitA)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="PRL_modal",
          label = "Величина PRL", 
          value = PRL)
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
        label = "Отмена",
        class = "btn-danger",
        icon = shiny::icon("ban")
      )
    )
  ) %>% shiny::showModal()
}
