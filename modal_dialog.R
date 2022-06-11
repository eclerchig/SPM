modal_dialog <- function(FIO, birth_day, eth, list_eths, edit) {
  if (edit) {
    x <- "����������� ���������"
  } else {
    x <- "�������� ������"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "������ ��������",
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "FIO_modal",
          label = "��� ��������",
          value = FIO)
      ),
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId ="birthday_modal",
          label = "���� ��������", 
          value = birth_day,
          format = "yyyy-mm-dd",
          language = "ru")
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = "eth_modal", 
          label = "��������������", 
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
        label = "�������",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}

confirm_delete <- function(){
  shiny::modalDialog(
    id = "confirm_modal",
    title = "������������ ��������",
    p("�� ������������� ������ ������� ������?"),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "finalConfirm_delete",
        label = "��, �������",
        icon = shiny::icon("check"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_confirm",
        label = "��������",
        icon = shiny::icon("fa-xmark"),
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}