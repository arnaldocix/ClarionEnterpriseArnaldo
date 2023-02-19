#!  Debuger - Extension Template
#!
#!
#TEMPLATE(Debuger,'Debuger Template'),FAMILY('ABC')
#!
#EXTENSION(DebugerABC,'Add Debuger Class'),APPLICATION
 #BOXED('Debuger Variables')
  #DISPLAY('')
  #Prompt('Always make Debuger active?',check),%debugtrue,default(0),at(10)
  #Prompt('Duplicates before message',@n6),%numdups,default(50)
 #ENDBOXED

 #AT(%AfterGlobalIncludes)
 Include('Debuger.INC'),once
 #ENDAT
 
 #AT(%ProgramEnd)
   db.kill
  #ENDAT
 
 #AT(%ProgramSetup)
   db.init('%Application',%debugtrue,%numdups)  ! intiialize the debuger class 
 #ENDAT  
 
#AT(%GlobalData)
db Debuger
 #ENDAT  
 
 
