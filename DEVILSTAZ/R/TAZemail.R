TAZemail<-function(user=user, recipient=recipient, password=password, subject=subject, bodyText=bodyText){
  
  sender<-'devilstazaat@gmail.com'
  
  body<-paste("Hello ",user ,",", "\n", "\n", 'You have reieved the following update from TAZ:', "\n", "\n", bodyText, "\n","\n", 'Cheers,', '\n', "TAZ", '\n', sep='')
  
  #send.mail(from = sender,to = recipient,subject = subject,body = body,smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = sender, passwd = password, ssl = TRUE), authenticate=T)

  cmd<-paste('printf "Subject: ', subject,'\n\n ',body,'\n" | sendmail ',recipient,' -f devilstazaat@gmail.com', sep='')
  system(cmd)
  
  #echo -e "Subject:Hello \n\n I would like to buy a hamburger\n" | sendmail -f devilstazaat@gmail.com luke.j.davies@uwa.edu.au
  
}