sidebar <- function () {
  dashboardSidebar(
    useShinyjs(),
    AddHelpText("ADD_HELP_TEXT_HERE"),
    sidebarMenu(
      id = "tabs",
      menuItem("INTRODUCTION", tabName = "intro"),
      conditionalPanel("input.accessToken === 'partial'",
                       sidebarMenu(menuItem("SIMPLE HISTOGRAM", tabName = "simpleHist"))),
      conditionalPanel("input.accessToken === 'full'",
                      sidebarMenu(
                        menuItem("SIMPLE HISTOGRAM", tabName = "simpleHist"),
                        menuItem("MULTI HISTOGRAM", tabName = "multiHist"),
                        menuItem("SIMPLE BAR", tabName = "simpleBar"),
                        menuItem("MULTI BAR", tabName = "multiBar"),
                        menuItem("SIMPLE RADAR", tabName = "simpleRadar"),
                        menuItem("SIMPLE RADAR W/ COMPARISON", tabName = "simpleRadarComp"),
                        menuItem("FANCY RADAR W/ COMPARISON", tabName = "fancyRadarComp"),
                        menuItem("SIMPLE LINE", tabName = "simpleLine"),
                        menuItem("FANCY LINE", tabName = "fancyLine")
                      )),
      textInput(inputId = "userToken", label = "User Code", value = "Enter your token here."),
      textInput(inputId = "accessToken", label = "Access Code", value = "Show/Hide Version.")
    )
  )
}
