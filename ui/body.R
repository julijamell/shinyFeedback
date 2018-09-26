body <- function () {
  dashboardBody(
    tabItems(
      AddIntroductionTab(tabId = "intro",
                         mainTitle = "Introduction to the App",
                         introText = "Introduction.intro"
                        ),
      AddSimpleTab(tabId = "simpleHist",
                   mainTitle = "Simple Histogram Main Title",
                   introText = "simpleHist.intro",
                   descList = c("simpleHist.desc"),
                   plotList = c("simpleHist.plot")
                  ),
      AddMultiTab(tabId = "multiHist",
                  mainTitle = "Multi Histrogram Main Title",
                  introText = "multiHist.intro",
                  headList = c("Heading 1", "Heading 2", "Heading 3"),
                  descList = c("multiHist.desc1", "multiHist.desc2", "multHist.desc3"),
                  plotList = c("multiHist.plot1", "multiHist.plot2", "multiHist.plot3")
                ),
      AddSimpleTab(tabId = "simpleBar",
                   mainTitle = "Simple Bar Main Title",
                   introText = "simpleBar.intro",
                   descList = c("simpleBar.desc"),
                   plotList = c("simpleBar.plot")
                  ),
      AddMultiTab(tabId = "multiBar",
                  mainTitle = "Multi Bar Main Title",
                  introText = "multiBar.intro",
                  headList = c("Heading 1", "Heading 2", "Heading 3"),
                  descList = c("multiBar.desc1", "multiBar.desc2", "multiBar.desc3"),
                  plotList = c("multiBar.plot1", "multiBar.plot2", "multiBar.plot3")
                 ),
      AddSimpleTab(tabId = "simpleRadar",
                   mainTitle = "Simple Radar Main Title",
                   introText = "simpleRadar.intro",
                   descList = c("simpleRadar.desc"),
                   plotList = c("simpleRadar.plot")
                  ),
      AddSimpleTab(tabId = "simpleRadarComp",
                   mainTitle = "Simple Radar With Comparison Main Title",
                   introText = "simpleRadarComp.intro",
                   descList = c("simpleRadarComp.desc"),
                   plotList = c("simpleRadarComp.plot")
                   ),
      AddMultiTab(tabId = "fancyRadarComp",
                  mainTitle = "Fancy Radar With Comparison Main Title",
                  introText = "fancyRadarComp.intro",
                  headList = c("Heading 1", "Heading 2", "Heading 3", "Heading 4"),
                  descList = c("fancyRadarComp.desc1", "fancyRadarComp.desc2", "fancyRadarComp.desc3", "fancyRadarComp.desc4"),
                  plotList = c("fancyRadarComp.plot1", "fancyRadarComp.plot2", "fancyRadarComp.plot3", "fancyRadarComp.plot4")
                  ),
      AddSimpleTab(tabId = "simpleLine",
                   mainTitle = "Simple Line Main Title",
                   introText = "simpleLine.intro",
                   descList = c("simpleLine.desc"),
                   plotList = c("simpleLine.plot")
                   ),
      AddMultiTab(tabId = "fancyLine",
                  mainTitle = "Fancy Line Main Title",
                  introText = "fancyLine.intro",
                  headList = c("Heading 1", "Heading 2", "Heading 3", "Heading 4"),
                  descList = c("fancyLine.desc1","fancyLine.desc2","fancyLine.desc3","fancyLine.desc4"),
                  plotList = c("fancyLine.plot1","fancyLine.plot2","fancyLine.plot3","fancyLine.plot4")
                  )
      )
  )
}