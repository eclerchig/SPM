modal_dialog <- function(FIO, birth_day, eth, list_eths, edit) {
  if (edit) {
    x <- "����������� ���������"
  } else {
    x <- "�������� ������"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "����� ���������� ���������� � ��������",
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
        class = "col-6",
        shiny::dateInput(
          inputId ="birthday_modal",
          label = "���� ��������", 
          value = birth_day,
          format = "yyyy-mm-dd",
          language = "ru")
      ),
      div(
        style = "display: inline-block;",
        class = "col-6",
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
        label = "��������",
        class = "btn-danger",
        icon = shiny::icon("ban"),
      )
    )
  ) %>% shiny::showModal()
}

confirm_delete <- function(mode){
  shiny::modalDialog(
    id = "confirm_modal",
    title = "������������� �������� ������",
    p("�� ������������� ������ ������� ������?"),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "finalConfirm_delete" %+% mode,
        label = "��, �������",
        icon = shiny::icon("check"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_confirm",
        label = "������",
        icon = shiny::icon("ban"),
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}

modal_dVisit <- function(date_visit, edit) {
  if (edit) {
    x <- "����������� ���������"
  } else {
    x <- "�������� ������"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "����� ���������� ���������� � ���������",
    div(
      class = "row justify-content-center",
      div(
        style = "display: inline-block;",
        class = "col-6",
        shiny::dateInput(
          inputId ="visit_modal",
          label = "���� ���������", 
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
        label = "������",
        class = "btn-danger",
        icon = shiny::icon("ban")
      )
    )
  ) %>% shiny::showModal()
}

modal_dLab <- function(date, DK, KDCT, MDA, COD, GSH, vitE, vitA, PRL, edit) {
  if (edit) {
    x <- "����������� ���������"
  } else {
    x <- "�������� ������"
  }
  shiny::modalDialog(
    id = "edit_modal",
    title = "����� ���������� ���������� � ����������� �������������",
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId ="date_modal",
          label = "���� ����� ��������", 
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
          label = "�������� DK", 
          value = DK)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="KDCT_modal",
          label = "�������� KD-CT", 
          value = KDCT)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="MDA_modal",
          label = "�������� MDA", 
          value = MDA)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="COD_modal",
          label = "�������� COD", 
          value = COD)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="GSH_modal",
          label = "�������� GSH", 
          value = GSH)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="vitE_modal",
          label = "���������� �������� E", 
          value = vitE)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="vitA_modal",
          label = "���������� �������� �", 
          value = vitA)
      )
    ),
    div(
      class = "row",
      div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId ="PRL_modal",
          label = "�������� PRL", 
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
        label = "������",
        class = "btn-danger",
        icon = shiny::icon("ban")
      )
    )
  ) %>% shiny::showModal()
}
