getGoogleOAuth2 <- function(service, dotfile="~/.RGoogleDocsToken"){
  library(RCurl) # die if no RCurl 
  library(rjson)
  
  CLIENTSECRET = "0PbpdhGEIgJQaIwb9NegcaJh"
  CLIENTID = "381327617901-vou27i50f1d3i749k4ld35iousr0s09n.apps.googleusercontent.com"
  REDIRECTURI= "urn:ietf:wg:oauth:2.0:oob"

  if(!file.exists(dotfile)) {
    if(!interactive()) stop("Authorization token not found")
      
    #following https://developers.google.com/identity/protocols/OAuth2InstalledApp
    auth.args <- c(
      response_type = "code",
      client_id = CLIENTID,
      redirect_uri = REDIRECTURI,
      scope = "https://spreadsheets.google.com/feeds https://docs.google.com/feeds"
    )
    
    ENDPOINT = "https://accounts.google.com/o/oauth2/auth"
    
    auth.args = paste(names(auth.args), curlEscape(auth.args), sep='=', collapse='&')
    
    message("Please visit: ")
    message(paste0(ENDPOINT, "?", auth.args)) 
    code <- readline("Enter authorization code:")

    auth.args <- list(code=code,
                      client_id=CLIENTID,
                      client_secret=CLIENTSECRET,
                      redirect_uri=REDIRECTURI,
                      grant_type="authorization_code" )
  
    token <- postForm("https://accounts.google.com/o/oauth2/token", .params=auth.args)

    cat(token, file=dotfile)
    
  }
  
  token <- fromJSON(file=dotfile)
  
  # check if expired
  if(difftime(Sys.time() , file.info(dotfile)$mtime, units="secs") > token$expires_in) {
    
    auth.args <- list(refresh_token=token$refresh_token,
                      client_id=CLIENTID,
                      client_secret=CLIENTSECRET,
                      grant_type="refresh_token" )
  
    token2 <- postForm("https://accounts.google.com/o/oauth2/token", .params=auth.args)
    token2 <- fromJSON(token2)
    
    token[names(token2)] <- token2
    cat(toJSON(token), file=dotfile)

    
  }
  
   ans = new("GoogleDocsAuthentication", 
             c(Authorization=paste(token$token_type, token$access_token)))
   attr(ans, "service") = service
   ans
  
#   ## from thingy
#   code = "4/KZ80ATgDjw6Kj09pXWT1TjrDKZvxTwKhBFZZvUqg1mk.IubhIOSRu3QXEnp6UAPFm0H86leLmwI"
#   
# )
#   
# 
#   curl  -s https://accounts.google.com/o/oauth2/token   -H "Content-Type: application/x-www-form-urlencoded"  -d code='4/Di2qdAUWJEYDdmD7xlgEqTb_-0v6VXB7BECtNOUdyRg.4p0dViO6Z6oXEnp6UAPFm0FOfquKmwI'  -d client_id='381327617901-vou27i50f1d3i749k4ld35iousr0s09n.apps.googleusercontent.com'  -d client_secret='0PbpdhGEIgJQaIwb9NegcaJh'  -d redirect_uri='urn:ietf:wg:oauth:2.0:oob'  -d grant_type='authorization_code'

  
}