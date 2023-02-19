#!õõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõõ
#TEMPLATE (Ec_RptExportarLeg,'(Report) Templates Clarion - Reportes')
#EXTENSION(Ec_Rpt_Ex_GlobalLeg,'Templates Clarion - Exportacion Reportes '), APPLICATION
#SHEET
  #Tab('General')
      #INSERT(%EvoVersion)
      #PROMPT('Separator File ASCII:',@S3),%SeparatorASCII,DEFAULT(';'), AT(75,,20,)
      #Display()
      #PROMPT('Language:', DROP('Spanish|English|Portuguese')), %EcLenguaje, REQ, DEFAULT('Spanish'), AT(75,,115,)
      #Display()
      #BOXED('Formatos a Exportar')
      #DISPLAY('Este Extension nos permite Exportar en Reportes')
      #DISPLAY('Registros a los siquientes Formatos')
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
#TAB('About')
#INSERT(%EvoVersion)
#INSERT(%EvoImage)
#ENDTAB
#EndSheet
#ATSTART
#DECLARE(%EvoDll)
#DECLARE(%EvoLib)
#DECLARE(%EvoLibl)
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
#IF(%ApplicationLocalLibrary)
  MODULE('%EvoLibl')
  EcRptExport(*QUEUE,*QUEUE,*QUEUE,SHORT,STRING,*GROUP)
  ExportarRptII(*QUEUE,*QUEUE,*QUEUE,SHORT,STRING,*GROUP,*QUEUE)
  PDP(*QUEUE,Long)
  END
#ELSE
  MODULE('%EvoDll')
  EcRptExport(*QUEUE,*QUEUE,*QUEUE,SHORT,STRING,*GROUP)
  ExportarRptII(*QUEUE,*QUEUE,*QUEUE,SHORT,STRING,*GROUP,*QUEUE)
  PDP(*QUEUE,Long)
  END
#END
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

#INCLUDE('evo_rpt_exLeg.tpw')