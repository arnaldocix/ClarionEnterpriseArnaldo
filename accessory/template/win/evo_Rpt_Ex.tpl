#!õõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõ
#TEMPLATE (Ec_RptExportar,'Report Export  -  Templates Clarion'),Family('ABC')
#EXTENSION(Ec_Rpt_Ex_Global,'Report Export  -  Global Extension (Templates Clarion) '), APPLICATION
#SHEET
  #Tab('General')
      #INSERT(%EvoVersion)
      #PROMPT('Separator File ASCII:',@S3),%SeparatorASCII,DEFAULT(';'), AT(75,,20,)
      #Display()
      #PROMPT('Language:', DROP('Spanish|English|Portuguese')), %EcLenguaje, REQ, DEFAULT('Spanish'), AT(75,,115,)
      #Display()
      #BOXED('Format to Export')
      #DISPLAY()
      #DISPLAY()
      #IMAGE('ec_explorer.ico'),AT(10,100)
      #IMAGE('ec_Excel.ico'),AT(40,100)
      #IMAGE('ec_word.ico'),AT(70,100)
      #IMAGE('ec_ascii.ico'),AT(100,100)
      #IMAGE('ec_xml.ico'),AT(130,100)
      #ENDBOXED
      #Display()
      #Prompt('Use Professional Version?',CHECK),%EcversionRpt2,DEFAULT(0),AT(10)
      #BOXED(''),WHERE(%EcversionRpt2=1)
      #DISPLAY('The exported Procedure will call "EVO_RPT_Export"')
      #DISPLAY('that will be use to show the exportation')
      #DISPLAY(' screen which is  contained in the "TXD" file. ')
      #DISPLAY('contained in the "TXD" file. ')
       #ENDBOXED
      #Image('cajaCWT.bmp')
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
      #Display()
  #EndTab
 #TAB('Registration')
   #PREPARE
 #IF (%Dataemail='')
 #CALL(%GolRtvPar, 'R_e-mail',%Dataemail)
 #ENDIF
 #IF (%DataName='')
 #CALL(%GolRtvPar, 'R_UserName',%DataName)
 #ENDIF
 #IF (%DatosSerial='')
 #CALL(%GolRtvPar, 'R_SerialNumber',%DatosSerial)
 #ENDIF
  #ENDPREPARE
    #DISPLAY
    #PROMPT('e-mail: ',@s100),%Dataemail
    #PROMPT('User Name: ',@s50),%DataName
    #PROMPT('Serial Number: ',@s20),%DatosSerial
    #DISPLAY
    #DISPLAY
    #DISPLAY('You can put TCW.ini licensed in ')
    #DISPLAY( LONGPATH() )
    #DISPLAY
    #DISPLAY('For example')
    #DISPLAY('[LIC]')
    #DISPLAY('R_e-mail=yourmail')
    #DISPLAY('R_UserName=UserName')
    #DISPLAY('R_SerialNumber=Serial')
    #DISPLAY
    #DISPLAY
  #ENDTAB
#TAB('About')
#INSERT(%EvoVersion)
#INSERT(%EvoImage)
#ENDTAB
#EndSheet
#ATSTART
#DECLARE(%EvoLib)
#DECLARE(%EvoLibl)
#DECLARE(%EvoDll)
#SET(%EvoLib,'Claevo.lib')
#SET(%EvoLibl,'Claevol.Lib')
#SET(%EvoDll,'claevo.Dll')
#ENDAT

#AT(%ShipList)
     Templates Clarion
--------------------------
      #IF(%Target32)
  ___    %EvoDll - Manejo de archivos
      #ELSE
  ___    %EvoDll - Manejo de archivos
      #END
#ENDAT
#AT (%GlobalMap)
#INSERT(%EvoMap)
#ENDAT

#!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#At(%CustomGlobalDeclarations)
#IF(%ApplicationLocalLibrary)
#Project(%EvoLibl)
#Project('ClaOLEl.lib')
#ELSE
#Project(%EvoLib)
#Project('ClaDOS.lib')
#Project('ClaOLE.lib')
#END
#EndAt

#!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#INCLUDE('evo_rpt_ex1.tpw')
#INCLUDE('evo_rpt_ex2.tpw')
#INCLUDE('evo_rpt_ex9.tpw')