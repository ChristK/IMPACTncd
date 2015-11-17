## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos
 
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.


# shinyUI(fluidPage(
#     titlePanel(h1("IMPACTncd")),
#     
#     sidebarLayout(position = "left",
#         sidebarPanel("sidebar panel"),

#         mainPanel("main panel")
#     )
# ))
# 
shinyUI(fluidPage(
    
    titlePanel(h1("IMPACTncd")),
    
sidebarLayout(
    
    sidebarPanel(
        p("This is where you enter parameters for the simulation")
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Input Parameters",
                     
                     sliderInput("yearstoproject", label = h3("Forecast horizon (in years)"),
                                 min = 1, max = 50, value = 20), 
                     
                     sliderInput("ageLH", label = h3("Age limit for the diseases-model simulation(in years)"),
                                 min = 30, max = 85, value = c(30, 85)),
                     
                     checkboxGroupInput("diseasestoexclude", label = h3("Choose diseases to be included in the simulation"), 
                                        choices = list("CHD" = 1, 
                                                       "Stroke" = 2, "Lung Cancer" = 3),
                                        selected = c(1,2)),
                     numericInput("n", 
                                  label = h3("Define the sample size (set 1 for the whole synthetic population)"), 
                                  value = 100000),
                     sliderInput("cvd.lag", label = h3("Define time lag for CHD and stroke (in years)"),
                                 min = 1, max = 10, value = 5), 
                     
                     sliderInput("cancer.lag", label = h3("Define time lag for cancers (in years)"),
                                 min = 1, max = 10, value = 10)),
            
                     tabPanel("Advanced Parameters", verbatimTextOutput("summary")), 
                     tabPanel("Instructions", p("To be completed..."))
            )
        )
    )
))