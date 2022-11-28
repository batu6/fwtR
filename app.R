## after the update many columns are gone, with the stuff I did...

library(shiny)
library(shinydashboard)
library(shinyjs)
library(rtweet)
library(tidyverse) ; theme_set(theme_bw())
library(purrr)
library(tidytext)
#library(wordcloud)
library(lubridate)
#library(rvest)
#library(ggtext)
#library(emoGG)
library(stringi)
library(scales)
library(plotly)
library(igraph)
library(ggraph)
library(ggimage)
library(shinyalert)
library(googlesheets4)
library(ggrepel)
library(gridExtra)
library(DT)

#token <- readRDS(file = "auth_token.rds") #create_token
#auth_as("auth_token")
auth_as(readRDS("auth_token.rds"))

# emojis that can be used to get an image. Later filtered
emo_use <- readRDS(file = "emojis_use.rds")



k = 0
slim <- 3
today <- date("2022-08-12")
day <- 586

# Words to be removed from text analysis.
remove_word <- c("https", "t.co", "ama", "fakat", "lâkin","lakin", "ancak",  "yalnız", "oysa", "oysaki", "hâlbuki", "halbuki",
                 "ve", "ile","ki","de","çünkü", "zira","madem", "mademki","veyahut", "yahut", "veya", "ya" ,"da", "ya da",
                 "şayet", "eğer", "ise","öyleyse", "o halde", "kısacası", "demek ki",  "nitekim","yoksa", "anlaşılan","ne" ,  
                 "hatta", "üstelik", "ayrıca", "hem","yine", "gene","meğer", "bi", "bu", "şu", "mi", "mı", "o", "ben", "sen",
                 "var", "yok", "bir", "iki", "daha", "şey", "kadar", "diye", "en", "benim", "senin", "onun", "değil", "tabii", "öyle", "aynen",
                 "galiba", "için", "cok", "falan", "filan", "yani", "artık", "olsun", "bunu", "şunu","böyle", "şöyle", "çok", "icin",
                 "her","hiç", "gibi", "bence", "oyle", "şimdi", "sonra", "mu", "mü", "zaten", "olabilir", "olur", "oldu", "hemen", "biraz",
                 "belki", "bana", "sana", "ona", "bize", "biz", "bizi","bizden", "seni", "beni", "sizin","siz","size", "sizi","sizden",
                 "bile", "tüm", "onu","onlar","onların")

remove_reg <- "&amp;|&lt;|&gt;"


sent <- read_delim("sent.csv", delim = ";")
sent <- sent %>% rename(word = WORD, Derece = TONE, Duygu = POLARITY) %>%
  mutate(Duygu = case_when(
    Duygu == -1 ~ "Olumsuz",
    Duygu == 1 ~ "Olumlu"
  ))


# Getting texts
trends_sum_input <- function(x, n_tweet, ttype = "recent"){
  x %>% 
    set_names() %>%
    map_dfr(search_tweets, n = n_tweet, type = ttype,include_rts = FALSE, .id = "filename") %>%
    select(created_at,
           id_str,
           text,
           source,
           #is_quote,
           favorite_count,
           retweet_count,
           #hashtags,
           #place_name,
           #country,
           #location,
           #description,
           #account_created_at,
           #followers_count,
           #friends_count,
           filename
    ) %>%  mutate(trend = filename) %>%
    select(-filename) %>% 
    mutate(account_created_at = ymd(str_extract(account_created_at,pattern = "(\\d{4}-\\d{2}-\\d{2})" ) ), 
           year = year(account_created_at),
           created_at_date = ymd(str_extract(created_at,pattern = "(\\d{4}-\\d{2}-\\d{2})" )),
           created_at_time = hms(str_remove(created_at,pattern = "(\\d{4}-\\d{2}-\\d{2} )" ))) %>%
    select(-created_at)
}

trends_sum_input_single <- function(x, n_tweet, ttype = "recent"){
  search_tweets(q = x, n = n_tweet, include_rts = FALSE, retryonratelimit = F, type = ttype) %>%
    mutate(trend = x) %>%
    select(created_at,
           id_str,
           text,
           source,
           #is_quote,
           favorite_count,
           retweet_count,
           #hashtags,
           #place_name,
           #country,
           #location,
           #description,
           #account_created_at,
           #followers_count,
           #friends_count,
           trend
    ) %>%
    mutate(account_created_at = ymd(str_extract(account_created_at,pattern = "(\\d{4}-\\d{2}-\\d{2})" ) ), 
           year = year(account_created_at),
           created_at_date = ymd(str_extract(created_at,pattern = "(\\d{4}-\\d{2}-\\d{2})" )),
           created_at_time = hms(str_remove(created_at,pattern = "(\\d{4}-\\d{2}-\\d{2} )" ))) %>%
    select(-created_at)
}


# url for twitter
url <- "https://twitter.com/intent/tweet?text=bu%20uygulama%20bir%20harika%20dostum&url=https://batu6.shinyapps.io/app_fwtr/"


plotlyUI <- function(id){
  fluidPage(
    column(width = 6,
      plotlyOutput(NS(id,"plot3"),height = 550)
    ),
    column(width = 6,
           plotlyOutput(NS(id,"plot3b"),height = 550)
    ),
  
  
  
           fluidRow(
             tabBox(
               title = "Tweetler",
               side = "left", height = "550px",width = 12,
               selected = "Trend 1",
               tabPanel("Trend 1", DT::dataTableOutput(NS(id,"mytable1") )),
               tabPanel("Trend 2", DT::dataTableOutput(NS(id,"mytable2") ))
               
             )
           )
           
  )

  
  } 

plotlyServer <- function(id, df, sfactor){
  moduleServer(id, function(input, output, session){
    
    maxvals <- reactiveValues()
      
      pp <- reactive({
        
        
        pp2 <- df %>% 
          group_by(trend, id_str) %>%
          mutate(tweet_sayısı = n(),
                 ortalama_beğenme = round(mean(favorite_count)),
                 ortalama_retweet = round(mean(retweet_count))) %>%
          rename(Hesap_tarihi = year)  %>% ungroup() %>%
          mutate(text = map_chr(strwrap(text, 40, simplify=F), paste, collapse="\n" ),
                 text = str_replace(text, "^", replacement = "<b>"),
                 text = str_replace(text, "$", replacement = "</b>")) %>%
          rename(beğenme = favorite_count,
                 retweet = retweet_count,
                 tweet = text)
        
        
        maxvals$maxy <- nchar(max(pp2$followers_count))
        maxvals$maxx <- nchar(max(pp2$friends_count))
        
        splited <- pp2 %>% group_split(trend)
        
        return(splited)
      })
      
      
      
    
      
          
          p3 <- reactive({
            
            
            slim <- ifelse(sfactor == "tweet_sayısı", 3, 40)
            
            pp3 <- ggplot(mapping = aes(tweet = tweet, like = beğenme, retweet = retweet, group=trend, 
                                        Favorited = ortalama_beğenme, Retweeted = ortalama_retweet, Hesap_tarihi = Hesap_tarihi,
                                        tweet_sayısı = tweet_sayısı , Hesap = id_str ))+
              geom_abline()+
              geom_smooth(data = pp()[[1]], mapping = aes(friends_count, followers_count), method = "lm")+
              geom_point(data = pp()[[1]] , mapping = aes(friends_count, followers_count, color = beğenme),alpha=0.5, position = position_jitter(width = 0.01, height = 0.01), size = 1.2)+
              scale_color_gradient(low = "#f7c740", high = "#8f1a1a")+
              geom_point(data = pp()[[1]] %>% filter(.data[[sfactor[[1]]]] > slim) %>% distinct(id_str, .keep_all = T), mapping = aes(friends_count,followers_count, size = .data[[sfactor[[1]]]]),alpha = 0.3, color = "#2681b5")+
              #geom_point(data = pp() %>% filter(.data[[sfactor[[1]]]] <= 1), mapping = aes(followers_count, friends_count, size = .data[[sfactor[[1]]]]), alpha = 0.1)+
              #geom_point(data = pp() %>% filter(.data[[sfactor[[1]]]] > 1), mapping = aes(followers_count, friends_count, size = .data[[sfactor[[1]]]], color= trend), alpha = 0.3)+
              scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1,10^maxvals$maxx)),labels = label_number(drop0trailing = TRUE)) +
              scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1,10^maxvals$maxy)),labels = label_number(drop0trailing = TRUE))+
              facet_wrap(~trend)+
              theme(legend.position = "none",
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=14,face="bold"),
                    strip.text.x = element_text(size = 14, face = "bold"),
                    plot.title = element_text(size = 14,face = "bold"))+ 
              ggtitle("Hesapların Takipçi - Takip Edilen Grafiği")+
              xlab("Takip Edilen Kişi Sayısı")+
              ylab("Takipçi Sayısı\n")+
              coord_cartesian(ylim = c(1,10^maxvals$maxy),
                              xlim = c(1,10^maxvals$maxx))
            
            return(pp3)
            
            
            
            
          })
          
          p3b <- reactive({
           
            
            
            slim <- ifelse(sfactor == "tweet_sayısı", 3, 40)
            
            pp2 <- ggplot(mapping = aes(tweet = tweet, like = beğenme, retweet = retweet, group=trend, 
                                        Favorited = ortalama_beğenme, Retweeted = ortalama_retweet, Hesap_tarihi = Hesap_tarihi,
                                        tweet_sayısı = tweet_sayısı, Hesap = id_str ))+
              geom_abline()+
              geom_smooth(data = pp()[[2]], mapping = aes(friends_count, followers_count), method = "lm")+
              geom_point(data = pp()[[2]] , mapping = aes(friends_count, followers_count, color = beğenme),alpha=0.5, position = position_jitter(width = 0.01, height = 0.01), size = 1.2)+
              scale_color_gradient(low = "#f7c740", high = "#8f1a1a")+
              geom_point(data = pp()[[2]] %>% filter(.data[[sfactor[[1]]]] > slim) %>% distinct(id_str, .keep_all = T), mapping = aes(friends_count,followers_count, size = .data[[sfactor[[1]]]]),alpha = 0.3, color = "#2681b5")+
              #geom_point(data = pp() %>% filter(.data[[sfactor[[1]]]] <= 1), mapping = aes(followers_count, friends_count, size = .data[[sfactor[[1]]]]), alpha = 0.1)+
              #geom_point(data = pp() %>% filter(.data[[sfactor[[1]]]] > 1), mapping = aes(followers_count, friends_count, size = .data[[sfactor[[1]]]], color= trend), alpha = 0.3)+
              scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1,10^maxvals$maxx)),labels = label_number(drop0trailing = TRUE)) +
              scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1,10^maxvals$maxy)),labels = label_number(drop0trailing = TRUE))+
              facet_wrap(~trend)+
              theme(legend.position = "none",
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=14,face="bold"),
                    strip.text.x = element_text(size = 14, face = "bold"),
                    plot.title = element_text(size = 14,face = "bold"),
                    axis.title.y = element_blank())+ 
              ggtitle("Hesapların Takipçi - Takip Edilen Grafiği")+
              xlab("Takip Edilen Kişi Sayısı")+
              ylab("Takipçi Sayısı\n")+
              coord_cartesian(ylim = c(1,10^maxvals$maxy),
                              xlim = c(1,10^maxvals$maxx))
            
            return(pp2)
            
            
          })
          
          
          output$plot3 <- renderPlotly({
            
            ggplotly(p3(), tooltip = c("tweet", "like", "retweet", "Favorited", "Retweeted", "Hesap_tarihi", "tweet_sayısı", "Hesap") )
            
          })
          
          
          output$plot3b <- renderPlotly({
            
            ggplotly(p3b(), tooltip = c("tweet", "like", "retweet", "Favorited", "Retweeted", "Hesap_tarihi", "tweet_sayısı", "Hesap") )
            
          })
          
          output$mytable1 <- renderDataTable({
            datatable(pp()[[1]] %>%
                        rename(Hesap = id_str,
                               Takipçiler = followers_count,
                               Takip_Edilenler = friends_count) %>%
                        select(trend, Hesap, tweet, beğenme, retweet, Takipçiler, Takip_Edilenler)
                      )
          })

     
          output$mytable2 <- renderDataTable({
            datatable(pp()[[2]] %>%
                        rename(Hesap = id_str,
                               Takipçiler = followers_count,
                               Takip_Edilenler = friends_count) %>%
                        select(trend, Hesap, tweet, beğenme, retweet, Takipçiler, Takip_Edilenler)
                      )
          })
          
    })  
    
}

plotlyUI2 <- function(id){
  fluidPage(align = "center",
    column(width = 12,
           plotlyOutput(NS(id,"plot3s"),height = 750, width = 1000)
    ),
    
    
    fluidRow(
      tabBox(
        title = "Tweetler",
        side = "left", height = "550px",width = 12,
        selected = "Trend 1",
        tabPanel("Trend 1", DT::dataTableOutput(NS(id,"mytable3") ))
        
      )
    )
  )
  
} 

plotlyServer2 <- function(id, df, sfactor){
  moduleServer(id, function(input, output, session){
  
      
      
      pp <- reactive({
        
        
        pp2 <- df %>% 
          group_by(trend, id_str) %>%
          mutate(tweet_sayısı = n(),
                 ortalama_beğenme = round(mean(favorite_count)),
                 ortalama_retweet = round(mean(retweet_count))) %>%
          rename(Hesap_tarihi = year)  %>% ungroup() %>%
          mutate(text = map_chr(strwrap(text, 40, simplify=F), paste, collapse="\n" ),
                 text = str_replace(text, "^", replacement = "<b>"),
                 text = str_replace(text, "$", replacement = "</b>")) %>%
          rename(beğenme = favorite_count,
                 retweet = retweet_count,
                 tweet = text)
        
        splited <- pp2 %>% group_split(trend)
        
        return(splited)
      })
      
      
      
      
      p3s <- reactive({
        
        
        slim <- ifelse(sfactor == "tweet_sayısı", 3, 40)
        
        pp3 <- ggplot(mapping = aes(tweet = tweet, like = beğenme, retweet = retweet, group=trend, 
                                    Favorited = ortalama_beğenme, Retweeted = ortalama_retweet, Hesap_tarihi = Hesap_tarihi,
                                    tweet_sayısı = tweet_sayısı, Hesap = id_str ))+
          geom_abline()+
          geom_smooth(data = pp()[[1]], mapping = aes(friends_count, followers_count), method = "lm")+
          geom_point(data = pp()[[1]] , mapping = aes(friends_count, followers_count, color = beğenme),alpha=0.5, position = position_jitter(width = 0.01, height = 0.01), size = 1.2)+
          scale_color_gradient(low = "#f7c740", high = "#8f1a1a")+
          geom_point(data = pp()[[1]] %>% filter(.data[[sfactor[[1]]]] > slim) %>% distinct(id_str, .keep_all = T), mapping = aes(friends_count,followers_count, size = .data[[sfactor[[1]]]]),alpha = 0.3, color = "#2681b5")+
          #geom_point(data = pp() %>% filter(.data[[sfactor[[1]]]] <= 1), mapping = aes(followers_count, friends_count, size = .data[[sfactor[[1]]]]), alpha = 0.1)+
          #geom_point(data = pp() %>% filter(.data[[sfactor[[1]]]] > 1), mapping = aes(followers_count, friends_count, size = .data[[sfactor[[1]]]], color= trend), alpha = 0.3)+
          scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = label_number(drop0trailing = TRUE)) +
          scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = label_number(drop0trailing = TRUE))+
          facet_wrap(~trend)+
          theme(legend.position = "none",
                axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 14, face = "bold"),
                plot.title = element_text(size = 14,face = "bold"))+ 
          ggtitle("Hesapların Takipçi - Takip Edilen Grafiği")+
          xlab("Takip Edilen Kişi Sayısı")+
          ylab("Takipçi Sayısı")
        
        return(pp3)
        
        
        
        
      })
      
      
      output$plot3s <- renderPlotly({
        
        ggplotly(p3s(), tooltip = c("tweet", "like", "retweet", "Favorited", "Retweeted", "Hesap_tarihi", "tweet_sayısı","Hesap") )
        
      })
      
      
      output$mytable3 <- renderDataTable({
        datatable(pp()[[1]] %>%
                    rename(Hesap = id_str,
                           Takipçiler = followers_count,
                           Takip_Edilenler = friends_count) %>%
                    select(trend, Hesap, tweet, beğenme, retweet, Takipçiler, Takip_Edilenler)
                  )
      })
      
    
  })  
  
}

ui <- dashboardPage(
  
  
  # App title -----

  dashboardHeader(title =  "fwtR"),
  
  
  # Sidebar layout with input and output definitions ----
  
  
  # Sidebar to demonstrate various slider options ----
  dashboardSidebar(
    
   
    #bg color of side panel
    #tags$style(".well {background-color:#039f98;}"),
    
    
    
    sidebarMenu(
      id = "menus",
      
      menuItem(tags$b("Giriş", style = "color: #c73838;font-size: 18px;"), tabName = "intro", icon = icon("palette")),
      sidebarMenuOutput("dynamic_content"),
      menuItem(tags$b("Diğer", style = "color: #c73838;font-size: 18px;"), tabName = "others", icon = icon("cat"))
      
      
      
    
      
    ),
    
    
    fluidRow(
          column(width = 12,offset =2,
                 column(width = 12,
                        actionButton("twitter_share",
                                     label = "Tweet",
                                     icon = icon("twitter"),
                                     onclick = sprintf("window.open('%s')", url),
                                     style="color: #fff; background-color: #1DA1F2; border-color: #2e6da4")
           )
                  )
      )
    
    
    #https://rstudio.github.io/shinydashboard/structure.html
    
    
  ),
  
  
  
  dashboardBody(
    
    
    tags$head(tags$style(HTML('* {font-family: Another Typewriter;}'))),
    
    
    useShinyjs(), # it is recommended to add it in the begginnig of the body for dashboards
    #useShinyalert(),
    
    fluidRow( box(div(htmlOutput("samples"),style="font-size: 18px; text-align: center;"), width = 12)),
    
    fluidRow( align="center",
      conditionalPanel(
        condition = "input.menus == 'textmine1'| input.menus =='textmine' | input.menus == 'textmine2'| input.menus =='sentiment'",
          box(radioButtons("tvsd", label = "Kelime Analizinin Yapılacağı Kaynak", 
                       choices = c("Tweet", "Profil Bio"), selected = "Tweet"),width = 12, height = 90)
      )),
    
    
    #fluidRow( align="center",
    #          conditionalPanel(
    #            condition = "input.menus == 'textmine2'| input.menus =='sentiment'",
    #            box(radioButtons("tvsd2", label = "Kelime Analizinin Yapılacağı Kaynak", 
    #                             choices = c("Tweet", "Profil Bio"), selected = "Tweet"),width = 12, height = 90)
    #          )),
    
    

    
    
    tabItems(
      tabItem(tabName = "intro",
            
              fluidRow(
                column(width= 6,
                       
                 box(background = "light-blue", width = NULL,
                   div(radioButtons("type", label = h4(tags$b("Tweet Türü:")), 
                               choices = c("recent", "mixed", "popular"), selected = "recent",inline = T), style="color:black; text-align: center; font-size: 18px;")
                   ),
                
                 box(width = NULL, background = "light-blue",
                   h4(tags$b("Gündemdekilerden trend seçin:"), style="color:black"),
                   
                   
                     div(selectInput('inp', 'ilk trend', c(Seç= "") ,selectize = F, multiple=F), style="color:black; text-align: center; font-size: 18px;") ,
                     div(selectInput('inp2', 'ikinci trend', c(Seç= "") ,selectize = F, multiple=F), style="color:black; text-align: center; font-size: 18px;") ,
                     
                     div(style="font-size: 30px; text-align: center;color: #eb4f34;" , icon("chevron-down")),
                     

                    column(width = 12, align="center",             
                    disabled(actionButton("search", tags$b("Ara"),style="color: #000; background-color: #eb4f34; border-color: #2e6da4; padding:4px; font-size:200%;",
                                          width = 100)),
                    ),
                     
                     div(style="font-size: 30px; text-align: center;color: #eb4f34;" ,icon("chevron-up")),
                     
                     h4(tags$b("İki trend yazın:"), style="color:black;"),
                     div(textInput("hash1", "ilk trend"), style="color:black; text-align: center; font-size: 18px;"),
                     div(textInput("hash2", "ikinci trend"), style="color:black; text-align: center; font-size: 18px;"),
                     
                   ),
                 
                 box(background = "light-blue", width = NULL,
                     div(
                       shinyjs::disabled(
                       sliderInput("ntw", label = h4(tags$b("Trend başına tweet sayısı")),min = 1500, max = 4500, value = 1500, step = 500)),style="color:black; text-align: center; font-size: 18px;")
                 )
                 
                 
                 ),
                column(width= 6,
                     
                       
                box(width = NULL, background = "red",     
                       
                  valueBoxOutput("boun",width = 12), 
                  
                      
                  h2(tags$b("Hoş Geldiniz", style = "color: #000;")),
             
                  p("Karşılaştırmak istediğiniz trendleri soldaki gündemdeki trendleri gösteren listeden seçip ya da kendiniz yazıp
                         ara butonuna basarak aratabilirsiniz.",
                         style = "color: #000;font-size: 18px;"),
              
                  br(),
                  p("Eğer sadece bir trende bakmak istiyorsanız, iki boşluğa da aynı trendi yazıp arayabilirsiniz.",
                         style = "color: #000;font-size: 18px;"),
                  br(),
            
                  p("'Recent', 'mixed' ve 'popular' seçenekleri, çekilecek tweetlerin özelliklerini belirtiyor. Ben 'recent' ya da 'mixed' 
                         seçeneklerinden birini kullanmanızı öneririm. 'Popular' seçeneği çok fazla tweet alamayabiliyor.",
                         style = "color: #000;font-size: 18px;"),
                  br(),
                  br(),
                  tags$b('recent',style = "font-size: 18px; color: #0d5a66;"), tags$em("- Son atılan tweetler", style = "color: #000;font-size: 16px;"),
                  
                  br(),
                  tags$b('mixed',style = "font-size: 18px; color: #0d5a66;"), tags$em("- Son atılan ve popüler tweetler karışık", style = "color: #000;font-size: 16px;"),
                  
                  br(),
                  tags$b('popular',style = "font-size: 18px; color: #0d5a66;"), tags$em("- Popüler tweetler", style = "color: #000;font-size: 16px;"),
                  
                  br(),
                  br(),
                  tags$b('Trend başına tweet sayısı',style = "font-size: 18px; color: #0d5a66;"),
                  tags$em("- Eğer uygulama yakın zamanda kullanılmadıysa incelenecek tweet sayısını buradan arttırabilirsiniz.", style = "color: #000;font-size: 16px;"),
                  
                  hr(),
                  
                  h3(tags$b("Motivasyon:", style = "color: #0d5a66;")),
                  
                  p("Gözlemlerim sonucu, bazen 'değişik' trendlerin bir anda gündeme oturabildiğini fark ettim. 
                       Biraz dikkatli bakıldığında bu trendlerde bir yapaylık olduğu da göze çarpabiliyordu (Yeni açılmış hesaplarca atılmış tweetlerin çokluğu,
                       bir kişinin çok kere aynı şeyi paylaşması vs.). Bu şekilde gündeme oturan trendleri hızlıca inceleyebilmek ve karşılaştırabilmek amacıyla
                    bu uygulamayı yapmaya başladım. Distopik gündeme katlananlara, direnenlere, hakkını arayanlara, düşünenlere ve sorgulayanlara selamlar.",
                    style = "color: #000;font-size: 16px;"),
                  
                  tags$em("Kararmasın yeter ki sol memenin altındaki cevahir...", style = "color: #000;font-size: 16px;"),
                       
                  h3(tags$b("Teknik Bilgi:", style = "color: #0d5a66;")),
                  
                  p("İki trend seçildiğinde trend başına yaklaşık 1500 tweet, tek trend seçildiğinde ise 3000 tweet çekiliyor.
                       İlk sayfalar daha genel verilere, sonraki sayfalar ise kelime analizlerine odaklanıyor. Oturum başına sadece bir
                    arama yapabiliyorsunuz.",
                    style = "color: #000;font-size: 16px;")
                       
                       ))
                 )
                
              
              
              
      ),
    
      
      tabItem(tabName = "general",
              
              
              
              fluidRow(width = 12,
                       column(width = 12,
                              box(plotOutput("plot1"),width = 6),
                              box(plotOutput("plot2"),width = 6)
                       )
              ),
              
              fluidRow(
                column(width = 12,
                       box(plotOutput("plotboxp", height = 700),width = 6, height = 700),
                       box(plotOutput("plot2month"),width = 6)
                ))

              
      ),
      
      tabItem(tabName = "general2",
              
              fluidRow(
                tabBox(
                  title = "Takip Edilen vs Takipçi Grafikleri",
                  side = "left", height = "550px",width = 12,
                  selected = "Tweet Sayısı",
                  tabPanel("Tweet Sayısı", plotlyUI("tsayısı")),
                  tabPanel("Ortalama Beğenme", plotlyUI("mlike")),
                  tabPanel("Ortalama Retweet", plotlyUI("mrt")),
                  tabPanel("Bilgi", htmlOutput("t1"))
                           
                )
              )
              
              
             
              
              
      ), 
              
      
      tabItem(tabName = "general2a",
              fluidRow(
                tabBox(
                       title = "Takip Edilen vs Takipçi Grafikleri",
                       side = "left", height = "750px",width = 12,
                       selected = "Tweet Sayısı",
                       tabPanel("Tweet Sayısı", plotlyUI2("tsayısı")),
                       tabPanel("Ortalama Beğenme", plotlyUI2("mlike")),
                       tabPanel("Ortalama Retweet", plotlyUI2("mrt")),
                       tabPanel("Bilgi", htmlOutput("t2")
                       )
                       
                )
                
              )
              
              
             ),
      
      tabItem(tabName = "textmine1",
              
              fluidRow(align="center",
                       
                       
                column(width= 12,
                       
                       box(width = NULL,
                           
                           plotOutput("fr2", height = 550))
                       
                )
                
              ),
              
              fluidRow(align="center",
                       
                       
                       column(width= 10,offset = 1,
                              
                              
                              box(width = NULL,
                                  
                                  plotOutput("em2", height = 650))
                              
                       )
                       
              )
              
              
              
              
      ),
      
      
      tabItem(tabName = "textmine",
              
              
              #fluidRow(align="center",
              #         box(radioButtons("tvsd", label = "Kelime Analizinin Yapılacağı Kaynak", 
              #                          choices = c("Tweet", "Profil Bio"), selected = "Tweet"),width = 12)
              #),
              
              column(width = 6,
                     box(plotOutput("plot", height = 600), width = 12)
              ),
              
              column(width = 6,
                     box(plotOutput("fr", height = 550), width = 12)
                     
              ),
              
              fluidRow(
              
                column(width = 10,offset = 1,
                       box(plotOutput("plotemo", height = 600), width = 12)
                )  
              )
              
              
              
      ),
      
      
      tabItem(tabName = "textmine2",
              
              fluidRow(align="center",
                       column(width = 12,offset = 3,
                              box(sliderInput(inputId = "dens",
                                              label = div(style='width:500px;', 
                                                          div(style='float:left;', 'Karmaşık'), 
                                                          div(style='float:right;', 'Şık')), min = 5, max = 75, value = 20, step = 5, width = '500px'),
                                  height = 110, width = 6)
                       )
              ),
              
              fluidRow(
                column(width = 12,
                       box(plotOutput("bigram3", height = 900),width = 12, height = 900)
                ))
              
            
      ),
      
      tabItem(tabName = "sentiment",
              
              
              fluidRow(
                column(width = 12,
                       box(plotOutput("sent1", height = 600),width = 6),
                       box(plotOutput("sent2", height = 600),width = 6)
                ))
              
              
      ),
      
      
      tabItem(tabName = "others",
              
            fluidRow(
              column(width = 4,  
                     box(width = NULL,background = "navy",height = 250,
                         div(uiOutput(outputId = "yeter"), style = "text-align: center") )
                     
              ),
              
              
              column(width = 4,  
                     box(width = NULL,background = "maroon",height = 250,
                         
                         
                         textAreaInput(inputId = "comment", label = tags$em("Yorumlar, Öneriler, Sorular:", style = "font-size:18px; color: #000;"), rows=6, resize = "none"),
                         actionButton("commentSubmit", "Gönder",class="btn btn-info"),
                         
                         
                     ),
                     
                     
                     
              ),
              
              column(width = 4,  
                     box(width = NULL,background = "navy",height = 250)
                     
              )
              
            ),
            
            fluidRow(
              
              column(width = 4,  
                     box(width = NULL,background = "navy",height = 250)
                     
              ),
              
              column(width = 4,  
                     box(width = NULL,background = "navy",height = 250)
                     
              ),
              
              column(width = 4, 
                     
                     box(width = NULL,background = "teal",height = 250,
                         
                         tags$b("R ile kodlanmıştır.", style = "font-size: 20px; color: #000;"),
                         
                         br(),
                         
                         tags$b("Kullanılan Paketler:", style = "font-size: 18px; color: #000;"),
                         
                         br(),
                         
                         p(paste("shiny", "shinydashboard", "shinyjs", "rtweet", "tidyverse", "purrr", "tidytext", "lubridate", "stringi", "scales", "plotly", 
                           "igraph", "ggraph", "ggimage", "shinyalert", "googlesheets4", "ggrepel", "gridExtra", sep = ", "), style = "font-size: 18px; color: #000;"),
                         
                         hr(),
                         
                         tags$b("Geliştirici", style = "font-size: 20px; color: #000;"),
                         
                         br(),
                         
                         p("Batuhan Akçabozan", style = "font-size: 18px; color: #000;")
                     )
                     
                     
              )
              
            ),
            
            fluidRow(
              column(width = 4, 
                     
                     box(width = NULL,background = "yellow",height = 250,
                         
                         tags$b("Diğer Uygulamalar:", style = "font-size: 20px; color: #000;"),
                         
                         br(),
                         br(),
                         
                         tags$li(tags$u(tags$a(href = "https://batu6.shinyapps.io/app_music/", "Your Daily Music" , target="_blank", style = "font-size: 18px; color: #000; text-align: center;")),style = "text-align: center;"),
                   
                         br(),
                         br(),
                         
                        tags$li(tags$u(tags$a(href = "https://batu6.shinyapps.io/app_bgremover/", "BgRemoveR", target="_blank" , style = "font-size: 18px; color: #000; ")),style = "text-align: center;")

                    
                     )
                     
                     
              ),
              
              column(width = 4,  
                     box(width = NULL,background = "navy",height = 250)
                     
              ),
              
              column(width = 4,  
                     box(width = NULL,background = "navy",height = 250)
                     
              )
              
              
              
            )
                     
                    
              
              
      )
      
    )
    
    
    
    
  )
)




server <- function(input, output, session){
  
  
  output$t1 <- renderUI({
    HTML("Her nokta bir tweeti gösteriyor. Kırmızıya çalan noktalar yüksek beğeni alan tweetleri gösteriyor.
          Büyük mavi çemberler seçilen sekmenin adına göre bazı kişileri özel olarak belirtiyor: <br>
          <b>Tweet Sayısı:</b> 3'ten fazla tweet atan hesapların tweetleri,  <br>
         <b>Ortalama Beğenme:</b> Tweetleri ortalama 40'tan fazla beğeni alan hesapların tweetleri, <br>
          <b>Ortalama Retweet:</b> Tweetleri ortalama 40'tan fazla retweet alan hesapların tweetleri, <br>
         mavi çember içine alınıyor. <br> <br>
         Ortadaki siyah çizgi y = x doğrusunu, mavi çizgi ise noktalara fit edilen doğruyu gösteriyor.
         Bir noktanın üzerine gelindiğinde o tweet ve tweeti atan hesap ile ilgili bilgi gösteriliyor.<br>
         Eksenler logaritmik olarak artıyor.")
  })
  
  output$t2 <- renderUI({
    HTML("Her nokta bir tweeti gösteriyor. Kırmızıya çalan noktalar yüksek beğeni alan tweetleri gösteriyor.
          Büyük mavi çemberler seçilen sekmenin adına göre bazı kişileri özel olarak belirtiyor: <br>
          <b>Tweet Sayısı:</b> 3'ten fazla tweet atan hesapların tweetleri,  <br>
         <b>Ortalama Beğenme:</b> Tweetleri ortalama 40'tan fazla beğeni alan hesapların tweetleri, <br>
          <b>Ortalama Retweet:</b> Tweetleri ortalama 40'tan fazla retweet alan hesapların tweetleri, <br>
         mavi çember içine alınıyor. <br> <br>
         Ortadaki siyah çizgi y = x doğrusunu, mavi çizgi ise noktalara fit edilen doğruyu gösteriyor.
         Bir noktanın üzerine gelindiğinde o tweet ve tweeti atan hesap ile ilgili bilgi gösteriliyor.<br>
         Eksenler logaritmik olarak artıyor.")
  })
  
  source(file="comments.R", local=T) 

  counter <- reactiveValues(s = 0)
  menuStuff <- reactiveVal(NULL)
  menuStuff2 <- reactiveVal(NULL)
  menuStuff3 <- reactiveVal(NULL)
  menuStuff4 <- reactiveVal(NULL)
  menuStuff5 <- reactiveVal(NULL)
  
  observe({
    
    req(trend_data())
    
    if(input$search == 1 & counter$s ==0){
      my_list = menuItem(tags$b("Grafikler 2", style = "color: #22a2bf;font-size: 14px;"), tabName = "general2", icon = icon("microscope"))
    
      my_list2 = list(menuItem(tags$b("Kelime-Emoji Tercihleri", style = "color: #7939db;font-size: 14px;"), tabName = "textmine1", icon = icon("chess-knight")),
                      menuItem(tags$b("Kelime-Emoji Kıyaslaması", style = "color: #7939db;font-size: 14px;"), tabName = "textmine", icon = icon("chess")),
                      menuItem(tags$b("2'li Kalıplar", style = "color: #7939db;font-size: 14px;"), tabName = "textmine2", icon = icon("project-diagram")),
                      menuItem(tags$b("Duygu Analizi", style = "color: #7939db;font-size: 14px;"), tabName = "sentiment", icon = icon("theater-masks")))
    
      menuStuff2(my_list2[[1]])
      menuStuff3(my_list2[[2]])
      menuStuff4(my_list2[[3]])
      menuStuff5(my_list2[[4]])
      
      
    } else if(input$search == 1 & counter$s ==1){
      my_list = menuItem(tags$b("Grafikler 2", style = "color: #22a2bf;font-size: 14px;"), tabName = "general2a", icon = icon("microscope"))
      
      my_list2 = list(menuItem(tags$b("Kelime-Emoji Tercihleri", style = "color: #7939db;font-size: 14px;"), tabName = "textmine1", icon = icon("chess-knight")),
                      menuItem(tags$b("2'li Kalıplar", style = "color: #7939db;font-size: 14px;"), tabName = "textmine2", icon = icon("project-diagram")),
                      menuItem(tags$b("Duygu Analizi", style = "color: #7939db;font-size: 14px;"), tabName = "sentiment", icon = icon("theater-masks")))
    
      menuStuff2(my_list2[[1]])
      menuStuff3(my_list2[[2]])
      menuStuff4(my_list2[[3]])
    }
   menuStuff(my_list)
   
    
    
    
    
  })
  
  output$dynamic_content <- renderMenu({
    
    sidebarMenu(.list = list(
      
            
            
            
            menuItem(tags$b("Genel", style = "color: #22a2bf;font-size: 18px;"), tabName = "generalMain", icon = icon("mug-hot"),
                     menuItem(tags$b("Grafikler", style = "color: #22a2bf;font-size: 14px;"), tabName = "general", icon = icon("shapes")),
                     menuStuff()
                     
            ),
            
            menuItem(tags$b("Kelime Analizi", style = "color: #7939db;font-size: 18px;"), tabName = "textM", icon = icon("cookie-bite"),
                     
                     menuStuff2(),
                     menuStuff3(),
                     menuStuff4(),
                     menuStuff5()
                     
            )
                          )
    )
  })
  
  
  
  
  
  tr <- get_trends("turkey")
  
  tr <- tr %>% 
    select(trend, tweet_volume) %>% 
    arrange(desc(tweet_volume))
  
  observe({
    
    updateSelectInput(inputId = "inp",choices =  c(Sec= "",tr$trend) )
    updateSelectInput(inputId = "inp2",choices = c(Sec= "",tr$trend) )
    
  }, priority = 6)
  
  observeEvent(input$search,{
    
    if (input$inp !="" | input$inp2 != ""){
      updateRadioButtons(inputId = "sd",choices =  c("Hepsi",input$inp ,input$inp2), inline = T )
    } else {
      updateRadioButtons(inputId = "sd",choices =  c("Hepsi",input$hash1 ,input$hash2), inline = T )
    }
    
  
    
  })
  
  
  observeEvent(c(input$inp, input$inp2),{
    
    req(input$inp,input$inp2)
    
    if (input$inp == "" | input$inp2 == "" ){  #| input$inp2 == input$inp
      shinyjs::disable("search")
    } else {
      shinyjs::enable("search")
    }
    
  },priority = 0, ignoreNULL = T, ignoreInit = T)
  
  
  observeEvent(c(input$hash1, input$hash2),{
    
    if (input$hash1 != "" & input$hash2 != ""  & k == 0){  # ??
      
      shinyjs::enable("search")
      
      k <<- k + 1
      print(k)
      
    } 
    
    if ( (input$hash1 == "" | input$hash2 == ""  ) & k == 1 ){  # | (input$hash2 == input$hash1)
      shinyjs::disable("search")
      k <<- 0
    }
    
    
  },ignoreInit = T)
  
  
  org <- reactive({
    
    originality <- trend_data() %>% 
      group_by(trend, id_str) %>%
      mutate(appears = n(),
             twice = appears == 2,
             more = appears > 2) 
    
    originality <- originality %>% 
      select(more, twice) %>% 
      group_by(trend) %>% 
      summarize(
        İki_kere = sum(twice) / n(),
        İkiden_fazla = sum(more) / n()) %>%
      pivot_longer(cols = 2:3, names_to = "Kaç_kere") 
    
 
    
    return(originality)
  })
  
  
  observeEvent(input$search,{
    
    output$plot1 <- renderPlot({
      
        ggplot(org())+
        geom_col(aes(trend,value, fill = Kaç_kere))+
        ggtitle("Kullanıcıların Yüzde Kaçı Birden Fazla Tweet Atmış")+
        xlab("Trendler")+
        ylab("Yüzde")+
        scale_fill_viridis_d()+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size = 14,face = "bold"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14,face = "bold"))+ 
        scale_y_continuous(labels = scales::percent)
      
    })
    
    output$plot2 <- renderPlot({
      
        ggplot(trend_data())+
          geom_density(aes(account_created_at, fill= trend), alpha = .5)+
          scale_x_date(date_labels="%y",date_breaks  ="1 year")+
          ggtitle("Tweet Atanların Hesap Açma Tarihileri Dağılımı")+
          xlab("Yıllar")+
          ylab("Yoğunluk")+
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14,face = "bold"),
                plot.title = element_text(size = 14,face = "bold"))
      
      
      
    })
    
    output$plotboxp <- renderPlot({
      
     
      grid.arrange(
        trend_data() %>% 
          group_by(trend, year) %>% distinct(id_str, .keep_all = T) %>%
          group_by(trend) %>%
          mutate(med = median(followers_count)) %>%
          ggplot(aes(year, followers_count,fill = trend, group = interaction(year, trend)))+
          geom_boxplot()+
          scale_y_log10(breaks = trans_breaks("log10", function(x)10^x ),labels = label_number(drop0trailing = TRUE))+
          scale_x_continuous(breaks = round(seq(min(trend_data()$year), max(trend_data()$year), by = 1),1))+
          ylab("Takipçi Sayısı")+
          #xlab("Hesabın Oluşturulma Senesi")+
          ggtitle("Hesap Oluşturma Tarihleri ve Takipçi Sayıları")+
          theme(legend.position = "top",
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                plot.title = element_text(size = 14,face = "bold"))+
          geom_hline(aes(yintercept = med,color = trend), lty = 2)+
          geom_text(aes(x = min(year)-1, y = med, label = med, color = trend)),
        
        trend_data() %>% 
          group_by(trend, year) %>% distinct(id_str, .keep_all = T) %>%
          group_by(trend) %>%
          mutate(med = median(friends_count)) %>%
          ggplot(aes(year, friends_count,fill = trend, group = interaction(year, trend)))+
          geom_boxplot()+
          scale_y_log10(breaks = trans_breaks("log10", function(x)10^x ),labels = label_number(drop0trailing = TRUE))+
          scale_x_continuous(breaks = round(seq(min(trend_data()$year), max(trend_data()$year), by = 1),1))+
          ylab("Takip Edilen Kişi Sayısı")+
          xlab("Hesabın Oluşturulma Senesi")+
          theme(legend.position = "none",
                axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                plot.title = element_text(size = 14,face = "bold"))+
          geom_hline(aes(yintercept = med,color = trend), lty = 2)+
          geom_text(aes(x = min(year)-1, y = med, label = med, color = trend)),
        nrow=2,
        heights = c(0.55,0.45)
      )
   
      
      
      
    })
  
    
  })
  
  p2m <- reactive({
    p2m <- trend_data() %>% 
      group_by(trend) %>%
      summarize(new_account = year == year(Sys.time()) &  
                  month(account_created_at) %in% c(month(Sys.time()), month(Sys.time()) - 1),
                new_acc_month = case_when(
                  new_account == T &  month(account_created_at) %in% month(Sys.time()) ~ month(Sys.time(), label = T),
                  new_account == T &  month(account_created_at) %in% month(Sys.time())-1  ~  month(lubridate::month(Sys.time()-1) - 1, label = T)
                )) %>%
      count(new_acc_month) %>%
      group_by(trend) %>%
      mutate(total = sum(n),
             freq = n /total) %>%
      drop_na() %>%
      rename(Aylar = new_acc_month)
    
    
    return(p2m)
    
  })
  
  observeEvent(input$search,{
    
    output$plot2month <- renderPlot({
      ggplot(p2m()) +
        geom_col(aes(trend, freq, fill = Aylar))+
        ggtitle("Tweetlerin Yüzde Kaçı Son İki Ayda Açılmış Hesaplar Tarafından Atılmış")+
        xlab("Trendler")+
        ylab("Yüzde")+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14,face = "bold"),
              plot.title = element_text(size = 14,face = "bold"))+ 
        scale_y_continuous(labels = scales::percent)
    })
    
  })
  

  
  plotlyServer("tsayısı", df = trend_data(), sfactor ="tweet_sayısı")
  plotlyServer("mlike", df = trend_data(), sfactor ="ortalama_beğenme")
  plotlyServer("mrt", df = trend_data(), sfactor ="ortalama_retweet")
 # 
  plotlyServer2("tsayısı", df = trend_data(), sfactor ="tweet_sayısı")
  plotlyServer2("mlike", df = trend_data(), sfactor ="ortalama_beğenme")
  plotlyServer2("mrt", df = trend_data(), sfactor ="ortalama_retweet")
  
  
  freq2 <- reactive({
    frequency2 <- textmine() %>%
      group_by(trend) %>%
      count(word,sort = T)%>%
      left_join(textmine() %>% 
                  group_by(trend) %>% 
                  summarise(total = n())) %>%
      mutate(freq = n/total,
             trend = paste0("trend_", str_replace_all(string = trend,pattern = " " ,replacement =  "_")),
             trend = str_replace_all(string = trend,pattern = "#" ,replacement =  ""))
    # frequency of words
    
    return(frequency2)
  })%>%
    bindCache(input$tvsd,cache = "session") %>%
    bindEvent(input$tvsd)
  
  
  observeEvent(input$search,{
    
    output$fr2 <- renderPlot({
      

      freq2() %>% slice(2:25) %>% 
        ggplot(aes(reorder_within(word, n, trend), n))+
        geom_col(aes(fill = trend))+
        scale_x_reordered() +
        #geom_text(aes(label = n))+
        facet_wrap(~trend, scales = "free_y")+
        coord_flip()+ 
        #scale_y_continuous(labels = scales::percent)+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              plot.title = element_text(size = 14,face = "bold"),
              legend.position = "none")+
        ggtitle("Trendlerde En Çok Geçen Kelimeler")+
        ylab("Kullanılma Sayısı")+
        xlab("")
      
    })  %>%
      bindCache(freq2(),cache = "session") %>%
      bindEvent(input$tvsd)
    
  
  })

  

observeEvent(input$search,{
  
  output$em2 <- renderPlot({
    
    emo2 <- left_join(logemo1(), emo_use,by = "emoji" )%>% 
      filter(url != "")  %>%
      distinct(emoji, .keep_all = T)
    
    emo2_freq <- emo2 %>%
      group_by(trend) %>%
      mutate(total = length(trend),
             freq = n / total) %>% 
      arrange(desc(n)) %>% slice(1:10)
    
    
    emo2_freq %>%
      ggplot(aes(reorder_within(emoji,n,trend) , n, fill = trend, label = label)) +
      scale_x_reordered() +
      geom_col(show.legend = FALSE) +
      geom_image(aes(image = url), size = 0.06) +
      coord_flip() +
      facet_wrap(~trend, scales = "free_y")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            strip.text.x = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 14,face = "bold"))+
      ggtitle("Trendlerde En Çok Geçen Emojiler")+
      ylab("Kullanılma Sayısı")+
      xlab("")
    

    
  })  %>%
    bindCache(logemo1(),cache = "session") %>%
    bindEvent(input$tvsd)

  
})
  
  
  
  textmine <- reactive({
    
    if(input$tvsd == "Tweet") {
      
      trend_tweet <- trend_data()  %>%
        mutate(text = str_remove_all(text, remove_reg)) %>%
        unnest_tokens(word, text) %>%
        filter(!word %in% remove_word) %>% 
        anti_join(stop_words)
      
    } else if(input$tvsd == "Profil Bio"){
      
      trend_tweet <- trend_data()  %>%
        mutate(description = str_remove_all(description, remove_reg)) %>%
        group_by(trend) %>% 
        distinct(id_str,.keep_all = T) %>%
        ungroup() %>%
        unnest_tokens(word, description) %>%
        filter(!word %in% remove_word) %>% 
        anti_join(stop_words)
    }
  }) %>%
    bindCache(input$tvsd,cache = "session") %>%
    bindEvent(input$tvsd)
  
  freq <- reactive({
    
    frequency <- textmine() %>%
      group_by(trend) %>%
      count(word,sort = T)%>%
      left_join(textmine() %>% 
                  group_by(trend) %>% 
                  summarise(total = n())) %>%
      mutate(freq = n/total,
             trend = paste0("trend_", str_replace_all(string = trend,pattern = " " ,replacement =  "_")),
             trend = str_replace_all(string = trend,pattern = "#" ,replacement =  ""))
    # frequency of words
    
    
    frequency <- frequency %>%
      select(trend, freq, word) %>%
      pivot_wider(names_from = trend, values_from = freq)%>% 
      drop_na() %>% 
      mutate(renk = log(.[[2]] / .[[3]]) )
    
    
    
    return(frequency)
  })%>%
    bindCache(input$tvsd,cache = "session") %>%
    bindEvent(input$tvsd)
  
  
  observeEvent(input$search,{
    
    output$fr <- renderPlot({
      
      
      ggplot(freq(), aes_string(x=names(freq())[2], y=names(freq())[3]), environment = environment(), label = word, hjust = 1, vjust = 1) +
        geom_jitter(alpha = 0.2, size = 2.5, width = 0.3, height = 0.3, aes(color = renk)) +
        geom_text(aes(label = word), check_overlap = T,position=position_jitter(width=0.1,height=0.1))  + 
        scale_color_gradient2(low = "darkorange", high = "blue", mid = "gray90", midpoint = 0) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        geom_abline(color = "red")+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size = 14,face = "bold"),
              legend.position = "none")+
        ggtitle("Kelimelerin Kullanılma Yüzdeleri")
      
    })  %>%
      bindCache(freq(),cache = "session") %>%
      bindEvent(input$tvsd)
    
  })
  
  
  logplot <- reactive({
    
    trend_tweet <- textmine() %>%
      filter(!str_detect(word, "^@")) %>%
      count(word, trend) %>%
      group_by(word) %>%
      filter(sum(n) >= 10) %>%
      ungroup() %>%
      pivot_wider(names_from = trend, values_from = n, values_fill = 0) %>%
      mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1)))%>%
      mutate(logratio = log( .[[2]] / .[[3]] )) %>%
      arrange(desc(logratio))
    

    
    trend_tweet <- trend_tweet %>% 
      group_by(lrt = logratio < 0) %>%
      top_n(25, abs(logratio)) %>%
      ungroup()  %>%
      mutate(word = reorder(word, logratio),
             logratio = abs(logratio),
             lrt = case_when(
               lrt == T ~ names(trend_tweet)[3],
               lrt == F ~ names(trend_tweet)[2]
             ))
    
    
    return(trend_tweet)
    
  }) %>%
    bindCache(input$tvsd,cache = "session") %>%
    bindEvent(input$tvsd)
  
  
  observeEvent(input$search,{
    
    output$plot <- renderPlot({
      
      ggplot(data = logplot(), mapping = aes(reorder(word,logratio), logratio, fill = lrt)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~lrt, scales = "free_y")+
        coord_flip() +
        ylab(paste("log odds ratio (",names(logplot())[2],"/",names(logplot())[3],")")) +
        xlab("Kelimeler")+
        scale_fill_discrete(name = "", labels = c(names(logplot())[2], names(logplot())[3]))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              plot.title = element_text(size = 14,face = "bold"))+
        ggtitle("Hangi Kelimeler Özellikle Hangi Trendde Karşımıza Çıkıyor")+
        scale_fill_brewer(palette = "Dark2")
    })  %>%
      bindCache(logplot(),cache = "session") %>%
      bindEvent(input$tvsd)
    
  })
  
  
  ## Emoji section
  
  
  logemo1 <- reactive({
    
    
    if(input$tvsd == "Tweet") {
      
      trend_emo <- trend_data() %>% 
        mutate(emoji = str_extract_all(string =  text,pattern = '\\p{So}|\\p{Cn}')) %>% 
        select(id_str,trend,text,emoji) %>% 
        unnest(emoji) %>%
        group_by(trend) %>%
        count(emoji) 
      
    } else if(input$tvsd == "Profil Bio"){
      
      trend_emo <- trend_data() %>% group_by(trend) %>% distinct(id_str, .keep_all = T) %>% ungroup() %>%
        mutate(emoji = str_extract_all(string =  description,pattern = '\\p{So}|\\p{Cn}')) %>% 
        select(id_str,trend,description,emoji) %>% 
        unnest(emoji) %>%
        group_by(trend) %>%
        count(emoji) 
      
    }
    
    
    trend_emo$emoji <- stri_escape_unicode(trend_emo$emoji)
    
    trend_emo <- trend_emo %>% mutate(emoji = str_extract(string = emoji, pattern = "(?<=u|U.{3}).*"))
  }) %>%
    bindCache(input$tvsd,cache = "session") %>%
    bindEvent(input$tvsd)
    
    
    
  logemo <- reactive({
    
    trend_tweet_emo <- logemo1() %>%
      group_by(emoji) %>%
      filter(sum(n) >= 10) %>%
      ungroup() %>%
      pivot_wider(names_from = trend, values_from = n, values_fill = 0) %>%
      mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
      mutate(logratio = log(.[[2]]  / .[[3]])) %>%
      arrange(desc(abs(logratio )))
    
    
    trend_tweet_emo <- trend_tweet_emo %>%
      group_by(neg = logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(emoji = reorder(emoji, logratio),
             logratio = abs(logratio),
             neg = case_when(
               neg == T ~ names(trend_tweet_emo)[3],
               neg == F ~ names(trend_tweet_emo)[2]
             ))
    
    trend_tweet_emo <- left_join(trend_tweet_emo, emo_use,by = "emoji" )%>% 
      filter(url != "")  %>%
      distinct(emoji, .keep_all = T)
    
    
    return(trend_tweet_emo)
    
  }) %>%
    bindCache(input$tvsd,cache = "session") %>%
    bindEvent(input$tvsd)
  
  
  observeEvent(input$search,{
    
    output$plotemo <- renderPlot({
      
      ggplot(logemo(),aes(x = reorder(emoji,logratio) , y = logratio, fill = neg, label = label)) +
        geom_col(show.legend = FALSE, alpha = 0.5) +
        geom_image(aes(image = url), size = 0.06) +
        # geom_richtext( fill = NA, label.color = NA, # remove background and outline
       #                label.padding = grid::unit(rep(0, 4), "pt"))+ # remove padding
        coord_flip() +
        facet_wrap(~neg, scales = "free_y")+
        ylab(paste("log odds ratio (",names(logemo())[2],"/",names(logemo())[3],")")) +
        xlab("Emojiler")+
        scale_fill_discrete(name = "", labels = c(names(logemo())[2], names(logemo())[3]))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              plot.title = element_text(size = 14,face = "bold"))+
        ggtitle("Hangi Emojiler Özellikle Hangi Trendde Karşımıza Çıkıyor")+
        scale_fill_brewer(palette = "Dark2")
      
    }) %>%
      bindCache(logemo(),cache = "session") %>%
      bindEvent(input$tvsd)
    
  })
  
  
 rr <- reactive({

   
   
   tidy_counts <- textmine() %>% select(id_str, trend, word) %>%
     count(word, trend, sort = T) %>%
     inner_join(sent) %>%
     group_by(trend) %>%
     mutate(tot = sum(n),
            freq = n/tot,
            fxd = freq*Derece)
   
   # ngrams

  
   tidy_counts2 <- bigr()$ngram %>% select(id_str, trend, bigram) %>%
     count(bigram, trend, sort = T) %>%
     rename(word = bigram) %>%
     inner_join(sent) %>%
     group_by(trend) %>%
     mutate(tot = sum(n),
            freq = n/tot,
            fxd = freq*Derece)
   
   
   
   tidy_counts <- bind_rows(tidy_counts, tidy_counts2)
   
   return(tidy_counts)
   
 })  %>%
   bindCache(input$tvsd,cache = "session") %>%
   bindEvent(input$tvsd)
  

 output$sent1 <- renderPlot({
   rr() %>% 
     group_by(trend, Duygu) %>% 
     arrange(desc(abs(fxd))) %>%
     filter(abs(Derece) > 0.2, n > 5) %>%
     slice(1:20) %>%
     ungroup() %>% 
     ggplot()+
     #geom_point(data = x,aes(Derece, n), alpha = 0.05)+
     geom_point(aes(Derece, n, color = trend),key_glyph = "rect")+
     geom_text_repel(aes(Derece, n,label = word, color = trend), size = 5, max.overlaps = 20,key_glyph = "rect")+
     geom_vline(xintercept = 0)+
     coord_cartesian(xlim= c(-1,1))+
     #facet_wrap(~trend, nrow = 1)+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           strip.text.x = element_text(size = 14, face = "bold"),
           plot.title = element_text(size = 14,face = "bold"),
           legend.position = "top",
           legend.title = element_blank(),
           legend.text = element_text(size = 12))+
     ggtitle("Kelimelerin Duygusal Çağrışımları")+
     ylab("Kelimenin kullanılma sayısı")+
     xlab("Olumsuzluk - Olumluluk Derecesi")
 })  %>%
   bindCache(rr(),cache = "session") %>%
   bindEvent(input$tvsd)

  
 output$sent2 <- renderPlot({
   rr() %>%
     group_by(trend, Duygu) %>%
     summarise(k = sum(n)) %>%
     group_by(trend) %>%
     mutate(total = sum(k),
            freq = k/total) %>%
     ggplot()+
     geom_col(aes(trend, freq, fill = Duygu))+
     scale_fill_viridis_d()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           strip.text.x = element_text(size = 14, face = "bold"),
           plot.title = element_text(size = 14,face = "bold"),
           legend.text = element_text(size = 12),
           legend.position = "top")+
     ggtitle("Kelimelerin Çağrışım Oranlarının Kıyaslaması")+
     ylab("Oran")+
     xlab("Trend")+
     coord_flip()
 })  %>%
   bindCache(rr(),cache = "session") %>%
   bindEvent(input$tvsd)
  
  # negatif ve poszitif kelime kıyaslaması
  

  
  
  ## Bigram
  
  bigr <- reactive({
    
    
    if(input$tvsd == "Tweet") {
      
      # ngrams
      tidy_ngram <- trend_data() %>%  
        mutate(text = str_remove_all(text, remove_reg)) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)
      
    } else if(input$tvsd == "Profil Bio"){
      
      # ngrams
      tidy_ngram <- trend_data() %>%  group_by(trend) %>% distinct(id_str, .keep_all = T) %>% ungroup() %>%
        mutate(description = str_remove_all(description, remove_reg)) %>%
        unnest_tokens(bigram, description, token = "ngrams", n = 2)
      
    }
    
    
    
    
    bigrams_separated <- tidy_ngram %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word1 %in% remove_word) %>%
      filter(!word2 %in% remove_word)
    
    
    # new bigram counts:
    bigram_counts2 <- bigrams_filtered %>% group_by(trend) %>% 
      count(word1, word2, sort = TRUE)   %>% select(2,3,4,1) %>% drop_na()
    
    
    # 
    # 
    # bigrams_united <- bigrams_filtered %>%
    #   unite(bigram, word1, word2, sep = " ")
    # 
    # ## search certain words ? display it on graph
    # 
    # bigram_tf_idf <- bigrams_united %>%
    #   count(trend, bigram) %>%
    #   bind_tf_idf(bigram, trend, n) %>%
    #   arrange(desc(tf_idf))
    
    
    bigram_graph <- bigram_counts2 %>% ungroup() %>%
      filter(n > input$dens) 
    
    if(nrow(bigram_graph) == 0){
      bigram_graph <- bigram_counts2 %>% ungroup() %>%
        filter(n > (max(n) - 1) ) %>%
        graph_from_data_frame()
      
      
      return(list(bigram_graph= bigram_graph, ngram = tidy_ngram) )
      
    } else {
      bigram_graph <- bigram_counts2 %>% ungroup() %>%
        filter(n > input$dens) %>%
        graph_from_data_frame()
      
      
      return(list(bigram_graph= bigram_graph, ngram = tidy_ngram) )
    }
    
    
    
  })
  
  observeEvent(input$search,{
    
    
    output$bigram3 <- renderPlot({
      
      
      set.seed(123)
      
      ggraph(bigr()$bigram_graph, layout = "fr") +
        geom_edge_link(aes(color = factor(trend)), width =1.5) +
        geom_node_point( size = 3, color = "firebrick") +
        geom_node_text(aes(label = name),color= "black", vjust = 1, hjust = 1, size = 5,fontface=2)+
        theme_void()+
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 14, face = "bold"))
      
      
    }) 
    
    
    
    
  })
  

  
  output$table <- renderDataTable({
    logemo()
  })
  
  observeEvent(trend_data(),{
    
    removeUI(
      selector = "div:has(> #text)"
    )
    
  })
  
  
  
  trend_data <- eventReactive(input$search,{
    
    showModal(modalDialog("Arkanıza yaslanın ve rahatlayın..", footer=NULL,size = "l"))
    
    updateTabItems(session, "menus",
                      selected = "general")
    
    shinyalert("Selam!", "Siz bu satırları okurken uzman kadromuz, Twitter'dan gerekli belgeleri almak üzere yola çıktı bile.\n
               İncelemek istediğiniz tweet miktarına göre sizi birazcık bekletebiliriz.", type = "info",
               showConfirmButton = T,confirmButtonText = "Peki..")
    
    withCallingHandlers({
      shinyjs::html(id = "text", html = "")
      if(input$inp != "" & input$inp2 != "" & input$search == 1){
              if(input$inp != input$inp2){
              
              trend_data <- trends_sum_input(c(input$inp  ,input$inp2), n_tweet= isolate({input$ntw}), ttype = input$type)
              
              } else if (input$inp == input$inp2){
                
                counter$s <- 1
                
                trend_data <- trends_sum_input_single(input$inp, n_tweet= isolate({input$ntw * 2}) , ttype = input$type)
                
              }
      } else if(input$hash1 != "" & input$hash2 != "" & input$search == 1){
        
              if(input$hash1 != input$hash2){
                
                trend_data <- trends_sum_input(c(input$hash1  ,input$hash2),n_tweet= isolate({input$ntw}), ttype = input$type)
                
              } else if (input$hash1 == input$hash2){
                
                counter$s <- 1
                
                trend_data <- trends_sum_input_single(input$hash1,n_tweet= isolate({input$ntw * 2}), ttype = input$type)
                
              }
        
      }
    },
    message = function(m) {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
        })

    shinyjs::disable("search")
    shinyjs::disable("inp")
    shinyjs::disable("inp2")
    shinyjs::disable("hash1")
    shinyjs::disable("hash2")
    
    removeModal()
    
    return(trend_data)
    
    
    
  })
  
  

  o1 <-  observeEvent(input$menus, { 
    if (input$menus == "textmine1"& input$search == 1) {
      shinyalert("", "Bu sayfadaki grafikler doğrudan kelimelerin ya da emojilerin kullanılma sıklıklarını
                 trend içinde gösteriyor. \n
                 Bu analizler ile tweeti atan kişilerin profil bilgilerinde yazanları ya da attıkları tweetleri kıyaslayabilirsiniz.", type = "info",
                 showConfirmButton = T,confirmButtonText = "Peki..")
      o1$destroy()
    }
  }, ignoreInit = T)
  
  
  
    
o <-  observeEvent(input$menus, { 
    if (input$menus == "textmine"& input$search == 1) {
      shinyalert("hey :)", "Bu sayfa eğlenceli! \n
      Önceki sayfaya göre burada yapılan kelime analizleri iki trendi karşılaştırıyor. 
      Eğer özellikle bazı kelimeler ya da emojiler bi trendde yoğunlukla geçmişse, onları burada görebiliyorsunuz. \n
      İlk grafik kelimeler için 'Göreceli Olasılıklar Oranı'nı gösteriyor. (Log Odds Ratio.) \n
                 Yani, hangi kelimeler hangi trende daha spesifik. \n
                 İkinci grafik, geçen kelimelerin trendlere göre yüzdelerini gösteriyor. Çizgiye yakın kelimeler iki trendde de benzer
                 oranlarda kullanılmış kelimeleri gösterirken, uzaktaki kelimeler belirtilen bi trendde daha sık kullanılmış oluyor.\n
                 En altta ise kullanılan emojilerin analizleri var (Hangi emojiler hangi trende daha spesifik.) :) \n
                 Bu analizler ile tweeti atan kişilerin profil bilgilerinde yazanları ya da attıkları tweetleri kıyaslayabilirsiniz.", type = "info",
                 showConfirmButton = T,confirmButtonText = "Peki..")
      o$destroy()
    }
  }, ignoreInit = T)
  
  
 o2 <- observeEvent(input$menus, { 
    if (input$menus == "textmine2" & input$search == 1) {
      shinyalert("İkili Kalıp Analizleri", "Burada yan yana bulunan kelimeler incelemeye tutuluyor ve bu kelimelerin bir haritası çıkartılıyor. \n
                 Üstteki zımbırtı ile kelime eşlerinin minimum görülme sayısını belirleyebilirsiniz. 
                 (Özellikle grafik çok karmaşıksa, sayıyı yükseltmenizi öneririm.)", type = "info",
                 showConfirmButton = T,confirmButtonText = "Peki..")
      o2$destroy()
    }
  }, ignoreInit = T)
  
 o3 <- observeEvent(input$menus, { 
   if (input$menus == "sentiment" & input$search == 1) {
     shinyalert("Duygu(Sentiment) Analizi", "Burada tweetlerde olumlu ya da olumsuz çağrışım yapan kelime ve kelime kalıplarının incelemesi var.\n
                Soldaki grafikte, bir trenddeki 'duygu'ya en çok etki yapan kelimeleri göstermeye çalıştım. Y ekseni, bir kelimenin trendde toplamda
                kaç kere geçtiğini gösteriyor. X ekseni ise kelimenin olumluluk veya olumsuzluk derecesini(Sağlam, 2016; Sağlam, 2019). \n
                Sağdaki grafik, olumlu ve olumsuz çağrışımlı kelimelerin oranlarını kıyaslıyor.", type = "info",
                showConfirmButton = T,confirmButtonText = "Peki..")
     o3$destroy()
   }
 }, ignoreInit = T)
 
 
      output$samples <- renderText({
        
        ss <- trend_data() %>% 
                group_by(trend) %>%
                summarize(Tweet = n()) 
        
        if (counter$s == 0)
          paste(ss[1,1],":", tags$b(ss[1,2]), tags$b("Tweet"),  tags$b(tags$em("-  &  -")) ,ss[2,1],":", tags$b(ss[2,2]), tags$b("Tweet"))
        else if (counter$s == 1)
          paste(ss[1,1],":", tags$b(ss[1,2]), tags$b("Tweet"))
        
        
        
      })
 
  #observeEvent(input$search,{
  #
  #  showNotification("Tweets are being retrieved please wait", duration = 10)
  #  
  #})
      
      direniyor <- reactive({
        #invalidateLater(864, session)
        
        thatday <- lubridate::date(Sys.time())
        
        if(thatday == today){
          day <- paste0(day,".")

        } else if (thatday != today) {
          
          interval_period = interval(today, thatday)
          yıl = (interval_period %/% years(1))*365
          hafta = (interval_period %% years(1) %/% weeks(1))*7
          gün = interval_period %% years(1) %% weeks(1) %/% days(1)
          
          time <- yıl + hafta + gün
          
          day <- day + time
          
          day <- paste0(day,".")
          
        }
        
        diren <- paste("Boğaziçi Direnişi",day, "Gününde")
        
        return(diren)
        
      })
      
 
        #output$diren <- renderText({
        #  direniyor()
        #})
    
        output$boun <- renderValueBox({
          valueBox(
            tags$div(direniyor(), style = "font-family: Another Typewriter;"), tags$div(tags$a(href = "https://www.youtube.com/channel/UC9pGbJ_YTz7Js4HdEiaJtfw", "#BoğaziçiDireniyor", target="_blank", style = "color: #0ae3f2; "), style = "font-size: 200%; font-family: Another Typewriter;"), icon = icon("cat"),
            color = "blue"
          )
        })
        
        
       # 
       # limit <- observe({ 
       # 
       #   if (rate_limit("search_tweets")$remaining == 180 | 
       #       (str_extract(rate_limit("search_tweets")$reset, "[0-9].") < 7  &  rate_limit( "search_tweets")$remaining > 140) ){ 
       #     shinyjs::enable("ntw")
       #   } else if (rate_limit("search_tweets")$remaining < 30){
       #     shinyalert("Site şu an yoğun istek altında", 
       #                paste(str_extract(rate_limit("search_tweets")$reset, "[0-9]."), "dakika sonra tekrar deneyin."), type = "warning",
       #                showConfirmButton = T,confirmButtonText = "Şimdi gidiyorum ama dönüşüm muhteşem olacak.")
       #     shinyjs::disable("search")
       #     
       #   } 
       #   
       #   limit$destroy()          
       #   
       # })
  
  
  
}

shinyApp(ui, server)

