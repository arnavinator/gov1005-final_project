#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(readr)
library(readxl)
library(rvest)
library(janitor)
library(fuzzyjoin)
library(shinythemes)
library(shiny)

income <- read_excel("raw-data/Income class of countries.xls", skip = 4) %>% 
    clean_names() %>% 
    select(economy, income_group)
income <- income[-c(1),]
income <- income[-c(219:274),]

income <- income %>% 
    mutate(economy = str_replace(economy, "Antigua and Barbuda", "Antigua And Barbuda"),
           economy = str_replace(economy, "Bahamas, The", "Bahamas"),
           economy = str_replace(economy, "Congo, Dem. Rep.", "Democratic Republic of the Congo"),
           economy = str_replace(economy, "Egypt, Arab Rep.", "Egypt"),
           economy = str_replace(economy, "Gambia, The", "Gambia"),
           economy = str_replace(economy, "Iran, Islamic Rep.", "Iran, Islamic Republic of"),
           economy = str_replace(economy, "Korea, Dem. People's Rep.", "Democratic Peoples Republic of Korea"),
           economy = str_replace(economy, "Korea, Rep.", "Korea, Republic of"),
           economy = str_replace(economy, "Kyrgyz Republic", "Kyrgyzstan"),
           economy = str_replace(economy, "Moldova", "Moldova, Republic of"),
           economy = str_replace(economy, "St. Kitts and Nevis", "Saint Kitts"),
           economy = str_replace(economy, "St. Lucia", "Saint Lucia"),
           economy = str_replace(economy, "St. Vincent and the Grenadines", "Saint Vincent and the Grenadines"),
           economy = str_replace(economy, "São Tomé and Principe", "Sao Tome And Principe"),
           economy = str_replace(economy, "Tanzania", "Tanzania, United Republic of"),
           economy = str_replace(economy, "Slovak Republic", "Slovakia"),
           economy = str_replace(economy, "United States", "United States of America"),
           economy = str_replace(economy, "Venezuela, RB", "Venezuela"),
           economy = str_replace(economy, "Vietnam", "Viet Nam"),
           economy = str_replace(economy, "Yemen, Rep.", "Yemen"),
           economy = str_replace(economy, "Micronesia, Fed. Sts.", "Micronesia, Federated States of"),
           economy = str_replace(economy, "Congo, Rep.", "Congo")
    )


# pulls table from internet which classifies countries by their reigion...
# needed to skip the first line, and filter out all rows where countries were
# not listed (the table had rows to index all countries starting by the same
# letter). Selected for only country name and reigion, all else was irrelevant

page <- read_html("https://www.who.int/choice/demography/by_country/en/")
country_reigion <- html_nodes(page, "table") %>% .[[1]] %>%
    html_table(fill = TRUE) %>% 
    clean_names() %>% 
    mutate(country = x1,
           reigion_num = x2,
           reigion = x3) %>% 
    select(country, reigion) %>% 
    filter(reigion != "")
country_reigion <- country_reigion[-c(1),]
country_reigion$reigion <- factor(country_reigion$reigion, 
                                  levels = c("AFRO", "EMRO", "EURO", "PAHO", "SEARO", "WPRO"),
                                  labels = c("African", "Eastern Mediterranean", "European",
                                             "Americas", "South-East Asia", "Western Pacific")
)


# merges dataset identifying a country's WHO reigion and their WB Income
# Bracket... classification now has info from both!

classification <- country_reigion %>% 
    inner_join(income, by = c("country" = "economy"))



# import new dataset on health spending, and select for relavent columns. Then
# manually match country names for proper joining

health_spend <- read_csv("raw-data/IHME_HEALTH_SPENDING.CSV") %>% 
    select(location_name, year, the_total_mean, dah_total_mean, the_per_cap_mean, the_per_gdp_mean, ghes_per_cap_mean, ppp_per_cap_mean, oop_per_cap_mean, dah_per_cap_mean, dah_per_gdp_mean) %>% 
    mutate(location_name = str_replace(location_name, "Antigua and Barbuda", "Antigua And Barbuda"),
           location_name = str_replace(location_name, "Cote d'Ivoire", "Côte d'Ivoire"),
           location_name = str_replace(location_name, "Iran", "Iran, Islamic Republic of"),
           location_name = str_replace(location_name, "South Korea", "Korea, Republic of"),
           location_name = str_replace(location_name, "Federated States of Micronesia", "Micronesia, Federated States of"),
           location_name = str_replace(location_name, "Syria", "Syrian Arab Republic"),
           location_name = str_replace(location_name, "The Bahamas", "Bahamas"),
           location_name = str_replace(location_name, "The Gambia", "Gambia"),
           location_name = str_replace(location_name, "Sao Tome and Principe", "Sao Tome And Principe"),
           location_name = str_replace(location_name, "United States", "United States of America"),
           location_name = str_replace(location_name, "Vietnam", "Viet Nam"),
           location_name = str_replace(location_name, "Moldova", "Moldova, Republic of"),
           location_name = str_replace(location_name, "North Korea", "Democratic Peoples Republic of Korea")
    )

classified_spend <- classification %>% 
    inner_join(health_spend, by = c("country" = "location_name"))

# import new dataset on deaths by communicable/noncommunicable diseases by age/gender. removed irrelevant columns and changed names of countries manually for proper joining

deaths <- read_csv("raw-data/IHME-GBD_deaths.csv") %>% 
    clean_names() %>% 
    select(-measure, -metric, -upper, -lower) %>% 
    mutate(location = str_replace(location, "Antigua and Barbuda", "Antigua And Barbuda"),
           location = str_replace(location, "Cote d'Ivoire", "Côte d'Ivoire"),
           location = str_replace(location, "Iran", "Iran, Islamic Republic of"),
           location = str_replace(location, "South Korea", "Korea, Republic of"),
           location = str_replace(location, "Federated States of Micronesia", "Micronesia, Federated States of"),
           location = str_replace(location, "Syria", "Syrian Arab Republic"),
           location = str_replace(location, "The Bahamas", "Bahamas"),
           location = str_replace(location, "Sao Tome and Principe", "Sao Tome And Principe"),
           location = str_replace(location, "United States", "United States of America"),
           location = str_replace(location, "Vietnam", "Viet Nam"),
           location = str_replace(location, "Moldova", "Moldova, Republic of"),
           location = str_replace(location, "North Korea", "Democratic Peoples Republic of Korea"), 
           location = str_replace(location, "South Korea", "Korea, Republic of"),
           location = str_replace(location, "Brunei", "Brunei Darussalam"),
           location = str_replace(location, "Federated States of Micronesia", "Macedonia"),
           location = str_replace(location, "The Gambia", "Gambia"),
           location = str_replace(location, "Tanzania", "Tanzania, United Republic of"),
    )

classified_death <- classification %>% 
    inner_join(deaths, by = c("country" = "location"))



# merge classified death and classified spend to analyze relationships between
# deaths and global health spending

spend_death <- classified_death %>% 
    inner_join(classified_spend, by = c("country" = "country", 
                                        "reigion" = "reigion", 
                                        "income_group" = "income_group", 
                                        "year" = "year"))



# import new dataset on education, remove irrelevant columns and extract only
# relavent information of how many students are enrolled in each level of
# education. Then, for easier analysis, I pivotted_wider the table

education <- read_csv("raw-data/education.csv", skip = 1) %>% 
    clean_names() %>% 
    mutate(country = x2) %>% 
    mutate(ed_type = series) %>% 
    select(country, year, ed_type, value) %>% 
    filter(ed_type %in% c("Students enrolled in primary education (thousands)",
                          "Students enrolled in secondary education (thousands)",
                          "Students enrolled in tertiary education (thousands)")
    ) %>% 
    pivot_wider(names_from = ed_type, values_from = value) %>% 
    clean_names()


# import new dataset on immigrants/refugees; 

immig <- read_csv("raw-data/immigrants.csv", skip = 1) %>% 
    clean_names() %>% 
    mutate(country = x2) %>% 
    mutate(imm_type = series) %>% 
    select(country, year, imm_type, value) %>% 
    filter(imm_type %in% c("International migrant stock: Both sexes (% total population)",
                           "Total population of concern to UNHCR (number)")
    ) %>% 
    pivot_wider(names_from = imm_type, values_from = value) %>% 
    clean_names()

# create merged dataset of education and immigrants/refugees; need to change
# names to merge with pop dataset that will be added next

educ_immig <- education %>% 
    inner_join(immig, by = c("country" = "country", "year" = "year")) %>% 
    filter(year %in% c(2005, 2010, 2015, 2017)) %>% 
    mutate(country = str_replace(country, "Bahamas", "Bahamas, The"),
           country = str_replace(country, "Bolivia (Plurin. State of)", "Bolivia"),
           country = str_replace(country, "Congo", "Congo, Rep."),
           country = str_replace(country, "Dem. Rep. of the Congo", "Congo, Dem. Rep."),
           country = str_replace(country, "Egypt", "Egypt, Arab Rep."),
           country = str_replace(country, "Gambia", "Gambia, The"),
           country = str_replace(country, "Iran (Islamic Republic of)", "Iran, Islamic Rep."),
           country = str_replace(country, "Dem. People's Rep. Korea", "Korea, Dem. People’s Rep."),
           country = str_replace(country, "Republic of Korea", "Korea, Rep."),
           country = str_replace(country, "Republic of Moldova", "Moldova"),
           country = str_replace(country, "Kyrgyzstan", "Kyrgyz Republic"),
           country = str_replace(country, "Micronesia (Fed. States of)", "Micronesia, Fed. Sts."),
           country = str_replace(country, "Slovakia", "Slovak Republic"),
           country = str_replace(country, "Saint Kitts and Nevis", "St. Kitts and Nevis"),
           country = str_replace(country, "Saint Lucia", "St. Lucia"),
           country = str_replace(country, "Saint Vincent & Grenadines", "St. Vincent and the Grenadines"),
           country = str_replace(country, "United Rep. of Tanzania", "Tanzania"),
           country = str_replace(country, "United States of America", "United States"),
           country = str_replace(country, "Venezuela (Boliv. Rep. of)", "Venezuela, RB"),
           country = str_replace(country, "Viet Nam", "Vietnam"),
           country = str_replace(country, "Yemen", "Yemen, Rep.")
    )

# import datasets on total population of countries, and the proportion of male
# and female in each country over time... pivot_wider for easier analysis

pop <- read_csv("raw-data/pop_total.csv", skip = 3) %>% 
    clean_names() %>% 
    pivot_longer(c("x2000", "x2001", "x2002", "x2003", "x2004", "x2005", "x2006", "x2007", "x2008", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017"), 
                 names_to = "year", 
                 values_to = "pop"
    ) %>% 
    select(country_name, year, pop) %>% 
    mutate(year = str_replace(year, "x", "")) %>% 
    mutate(year = as.numeric(year))

male <- read_csv("raw-data/prop_male.csv", skip = 3) %>% 
    clean_names() %>% 
    pivot_longer(c("x2000", "x2001", "x2002", "x2003", "x2004", "x2005", "x2006", "x2007", "x2008", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017"), 
                 names_to = "year", 
                 values_to = "prop_male"
    ) %>% 
    select(country_name, year, prop_male) %>% 
    mutate(year = str_replace(year, "x", "")) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(prop_male = prop_male/100)

female <- read_csv("raw-data/prop_female.csv", skip = 3) %>% 
    clean_names() %>% 
    pivot_longer(c("x2000", "x2001", "x2002", "x2003", "x2004", "x2005", "x2006", "x2007", "x2008", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017"), 
                 names_to = "year", 
                 values_to = "prop_female"
    ) %>% 
    select(country_name, year, prop_female) %>% 
    mutate(year = str_replace(year, "x", "")) %>% 
    mutate(year = as.numeric(year)) %>%
    mutate(prop_female = prop_female/100)


# now I join all of these datasets to create a comprehensive dataset of
# population and proportion of male/female for all countries from 2000-2017.
# Then, I mutate the country names manually for better joining of the data with
# educ_immig

pop <- pop %>% 
    inner_join(male, by = c("country_name" = "country_name", "year" = "year"))

pop <- pop %>% 
    inner_join(female, by = c("country_name" = "country_name", "year" = "year"))


# after importing new dataset on education, immigrants/refugees, and
# population in each country, we can join these datasets. I then mutate some
# cols such that instead of comparing metrics by total popluation, we compare by
# proportion of population (ie prop of students in primary school is a metric I
# can compare with any country since standardized measure)

pop_educ_immig <- pop %>%
    inner_join(educ_immig, by = c("country_name" = "country", "year" = "year")) %>% 
    mutate(prop_primary = students_enrolled_in_primary_education_thousands*1000/pop) %>% 
    mutate(prop_secondary = students_enrolled_in_secondary_education_thousands*1000/pop) %>% 
    mutate(prop_tertiary = students_enrolled_in_tertiary_education_thousands*1000/pop) %>% 
    mutate(prop_immigrants = international_migrant_stock_both_sexes_percent_total_population/100) %>% 
    mutate(num_refugee = total_population_of_concern_to_unhcr_number) %>% 
    select(-students_enrolled_in_primary_education_thousands, 
           -students_enrolled_in_secondary_education_thousands,
           -students_enrolled_in_tertiary_education_thousands,
           -international_migrant_stock_both_sexes_percent_total_population,
           -total_population_of_concern_to_unhcr_number)




ui <- navbarPage("Global Health Spending",
                 theme = shinytheme("flatly"),
                 
                 ##########
                 ##ABOUT##
                 #########
                 
                 tabPanel("About",
                          
                          #load first image 
                          
                          imageOutput("earth_pic", width = "100%", height = "100%"),
                          br(),
                          
                          #title and subtitle
                          
                          h2("Global Healthcare Disparity", align = "center"),
                          h4(em("An analysis of trends observed with increased health spending per capita"), align = "center"),
                          br(),
                          div(),
                          
                          
                          
                          br(),
                          
                          fluidRow(column(2), column(8,
                                                     
                                                     h4(strong("About this Project")),          
                                                     
                                                     #text to introduce project
                                                     
                                                     p("For this project, I strive to analyze how global health expenditure varies based on
                                                     economic and geographic factors. From the World Bank (WB), the Global Data Health Exchange,
                                                     and the World Health Organization (WHO), I have gathered data regarding population sex/age 
                                                     demographics; the estimated the burden of diseases; and historic health
                                                     care spending data and distribution of spending (government, foreign assistance, out-of-pocket, etc.);
                                                     for 195 countries from 2000-2018."),
                                                     
                                                     span(),
                                                     
                                                     p("With this data, I hope to explore association between health spending and factors such as income,
                                                     geography, deaths, education, and refugee/immigrant data."),
                                                     
                                                     
                                                     br(),
                                                     
                                                     
                                                     h4("About Me"),
                                                     
                                                     p("My name is Arnav Srivastava, and I am currently a first-year at Harvard College. I am 
                                                         fascinated by global health and improving accessibility to the ever-growing frontier of
                                                        science; I hope to concentrate in biomedical engineering, electrical engineering, or 
                                                         statistics, and help advance the health of our global community. The work for my project 
                                                        can be found on my GitHub repo at ", 
                                                       a(href = "https://github.com/arnavinator/gov1005-final_project", "here."))
                          ))
                 ),
                 
                 
                 ###########
                 ###DATA###
                 ##########
                 
                 tabPanel("Exploring Income and Regional Trends",
                          
                          fixedRow(column(1), column(9,
                          h2(em("Exploring healthcare growth rate by income")),
                          
                          br(),
                          
                          fluidRow(column(7, 
                                          h4("First, I wanted to explore how total health spending per person changes 
                                                based on income level of countries. The following graph shows the change
                                                over time of health spending per person. In observing this plot, it appears 
                                                that there was relatively no growth
                                                in healthspending for low and lower middle income groups.")
                                          ),
                                   
                                   column(4, plotOutput("spending_by_income", width = "700px"))
                                   ),
                          br(),      
                          
                          fluidRow(column(7,
                                          h4("However, after comparing income reigions with their successors (viewing two income
                                          groups at a time), we instead observe that all income groups in most reigions
                                          have witnessed historical health spending increase. However, the historical health spending
                                          rate of increase of one income group is always slower than its next-higher income group."),
                                          h4("In our linear regression model, we observe that wealthier income groups are 
                                             associated with a more accelerated increase in health spending over time."),
                                             
                                          helpText("Choose a set of income groups to get a closer look at 
                                                       health spending growth relative to income."),
                                          selectInput("income_group", "Income Groups",
                                                          choices = list("Low income & Lower middle income" = "low+lower",
                                                                         "Lower middle income & Upper middle income" = "lower+upper",
                                                                         "Upper middle income & High Income" = "upper+high"),
                                                          selected = "low+lower"),
                                          sidebarPanel(
                                              p("Click the graph to explore points!"),
                                              verbatimTextOutput("info")
                                          ),
                                              
                                              
                                          ),
                          
                                   column(4,
                                          plotOutput("spending_by_reigion", width = "700px", click = "plot_click")
                                          )
                          ),
                          br(),
                          fluidRow(column(7,
                                          h4("Next, I wanted to see what type of healthcare spending is responsible
                                             for these relative increases in healthcare spending per income group. Here, we explore the
                                             components of total health care spending: government, out-of-pocket, pre-paid private, and 
                                             foreign development assistance for health (only applicable for lower income recipients)."),
                                          helpText("Choose a income group to view components of healthcare spending"),
                                          selectInput("income_group_2", "Income Group",
                                                      choices = list("Low income" = "low",
                                                                     "Lower middle income" = "lower",
                                                                     "Upper middle income" = "upper",
                                                                     "High income" = "high"),
                                                      selected = "low"),
                                          sidebarPanel(
                                              p("Click the graph to explore points!"),
                                              verbatimTextOutput("info")
                                          )
                                          ),
                                   column(4,
                                          plotOutput("spending_single_reigion", width = "700px", click = "plot_click")
                                   )
                                   )
                          ))
                 )
)
                         
                          
                          


server <- function(input, output, session) {
    
    ##########
    ##ABOUT##
    ########
    
    #output ncaa logo on first page
    
    output$earth_pic <- renderImage({
        
        list(src = 'raw-data/earth.jpg',
             height = 300,
             width = 540,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
    
    ########
    ##DATA##
    ########
    
    
    
    output$spending_by_income <- renderPlot({
        
        classified_spend %>% 
            group_by(income_group, year) %>% 
            summarize(mean = mean(the_per_cap_mean)) %>% 
            ggplot(aes(x = year, y = mean, color = income_group)) +
            geom_line() +
            geom_point() +
            theme_classic() +
            labs(title = "Average Total Health Spending per Person Worldwide",
                 subtitle = "Average Spending Stratified by Income Group of Each Country",
                 x = "Year",
                 y = "Spending (2018 USD)",
                 color = "Income Group of Country")
        
    })
    
    
    
    output$spending_by_reigion <- renderPlot({
        
        # looks at user selection to decide which plot to show
        
        if(input$income_group == "low+lower") {
            classified_spend %>% 
                filter(income_group %in% c("Low income", "Lower middle income")) %>% 
                ggplot(aes(x = year, y = the_per_cap_mean, color = income_group)) +
                geom_point(alpha = 0.3) +
                scale_color_manual(values = c("maroon", "turquoise1")) +
                geom_smooth(method = "lm", se = FALSE) +
                facet_wrap(~reigion) +
                theme_classic() +
                labs(title = "Average Total Spending Per Person in Low and Lower Middle Income Groups",
                     x = "Year",
                     y = "Spending (2018 USD)",
                     color = "Income Group of Each Country")
        } else if (input$income_group == "lower+upper") {
            classified_spend %>% 
                filter(income_group %in% c("Lower middle income", "Upper middle income")) %>% 
                ggplot(aes(x = year, y = the_per_cap_mean, color = income_group)) +
                geom_point(alpha = 0.3) +
                scale_color_manual(values = c("blue", "orange")) +
                geom_smooth(method = "lm", se = FALSE) +
                facet_wrap(~reigion) +
                theme_classic() +
                labs(title = "Average Total Spending Per Person in Lower Middle and Upper Middle Income Groups",
                     x = "Year",
                     y = "Spending (2018 USD)",
                     color = "Income Group of Each Country")
        } else if (input$income_group == "upper+high") {
            classified_spend %>% 
                filter(income_group %in% c("Upper middle income", "High income")) %>% 
                ggplot(aes(x = year, y = the_per_cap_mean, color = income_group)) +
                geom_point(alpha = 0.3) +
                scale_color_manual(values = c("steelblue1", "pink3")) +
                geom_smooth(method = "lm", se = FALSE) +
                facet_wrap(~reigion) +
                theme_classic() +
                labs(title = "Average Total Spending Per Person in Upper Middle and High Income Groups",
                     x = "Year",
                     y = "Spending (2018 USD)",
                     color = "Income Group of Each Country")
        }
        
    })
    
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })

    output$spending_single_reigion <- renderPlot({
        
        # looks at user selection to decide which plot to show
        
        if(input$income_group_2 == "low") {
            classified_spend %>% 
                filter(income_group == "Low income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean),
                          dah = mean(dah_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total")) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"))  +
                geom_line(aes(x = year, y = gov, color = "Government")) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private")) +
                geom_line(aes(x = year, y = dah, color = "Foreign Development Assistance")) +
                labs(title = "Distribution of Total Health Spending Per Person in Low Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") +
                theme_classic() 
            
        } else if (input$income_group_2 == "lower") {
            classified_spend %>% 
                filter(income_group == "Lower middle income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean),
                          dah = mean(dah_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total")) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"))  +
                geom_line(aes(x = year, y = gov, color = "Government")) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private")) +
                geom_line(aes(x = year, y = dah, color = "Foreign Development Assistance")) +
                labs(title = "Distribution of Total Health Spending Per Person in Lower Middle Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") +
                theme_classic()
            
        } else if (input$income_group_2 == "upper") {
            classified_spend %>% 
                filter(income_group == "Upper middle income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total")) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"))  +
                geom_line(aes(x = year, y = gov, color = "Government")) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private")) +
                labs(title = "Distribution of Total Health Spending Per Person in Upper Middle Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") + 
                theme_classic()
            
        } else if (input$income_group_2 == "high") {
            classified_spend %>% 
                filter(income_group == "High income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total")) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"))  +
                geom_line(aes(x = year, y = gov, color = "Government")) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private")) +
                labs(title = "Distribution of Total Health Spending Per Person in High Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") +
                theme_classic()
        }
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

