


## googlesheets4 related necessary part ----------

m <- read.delim("mail.txt")

options(gargle_oauth_cache = ".secrets",
        gargle_oauth_email = m)

gs4_auth()
# check the value of the option, if you like
#gargle::gargle_oauth_cache()

# trigger auth on purpose to store a token in the specified cache
#googlesheets4::gs4_auth()

# sheets reauth with specified token and email address
# gs4_auth(
#   cache = ".secrets",
#   email = ""
# )

# Find the used google docs file-------------
Data <- gs4_find("fwt")


  
  observeEvent(input$commentSubmit,{ # -------
    
    if(input$comment != "" & input$commentSubmit == 1){
      Data  <- Data  %>%                                                                      
        sheet_append(data = data.frame(input$comment, Sys.time()),  sheet = 2 )
      
      disable("commentSubmit")
      disable("comment")
      
      shinyalert(text = "Mesajınız Alınmıştır. Teşekkürler :)", type = "success", timer = 2000, animation = "slide-from-top")

      
      #removeUI(selector = "div:has(> #commentSubmit)", immediate = T)
      #removeUI(selector = "div:has(> #comment)", immediate = T)
    } else if(input$comment == "" & input$commentSubmit == 3){
      
      removeUI(selector = "div:has(> #commentSubmit)", immediate = T)
      removeUI(selector = "div:has(> #comment)", immediate = T)
      
      output$yeter <- renderUI({
        img(src='yeter.png', height = '240px')
      })
    }
    
  })
  


  observeEvent(input$search,{ # -------
    
    if(input$search == 1){
        
      med <- trend_data() %>% 
        group_by(id_str, trend) %>%
        distinct(id_str, .keep_all = T) %>%
        group_by(trend) %>%
        summarise(medYear = median(year),
                  medTakipci = median(followers_count),
                  medTakipet= median(friends_count))
      
      med <- cbind(med[1,],med[2,])
      
      
        Data  <- Data  %>%                                                                      
          sheet_append(data = data.frame(Sys.time(),
                                         input$ntw,
                                         nrow(trend_data()),
                                         input$type,
                                         org()[1,1],
                                         org()[1,3],
                                         org()[2,3],
                                         org()[3,1],
                                         org()[3,3],
                                         org()[4,3]
                                         ),  sheet = 1 ) %>%
          sheet_append(data = data.frame(med
          ),  sheet = 3 )
      
      }
    
  },once = T)


