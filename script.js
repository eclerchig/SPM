function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}

function get_id_visit(clicked_id) {
     Shiny.setInputValue("current_id_visit", clicked_id, {priority: "event"});
}

function get_id_lab(clicked_id) {
     Shiny.setInputValue("current_id_lab", clicked_id, {priority: "event"});
}