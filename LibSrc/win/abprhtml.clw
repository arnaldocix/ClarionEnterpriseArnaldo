  MEMBER

  INCLUDE('ABPRHTML.INC'),ONCE
  MAP
    INCLUDE('CWUTIL.INC'),ONCE
GetAlignStyle  PROCEDURE(STRING text),STRING
  END
  
TOP_STYLE EQUATE(1)
LEFT_STYLE EQUATE(2)
  
  ITEMIZE,PRE(Type)
Text  EQUATE
Rect  EQUATE
  END
  
  PRAGMA ('link (EXP_HTM.ICO)')

HTMLGenerator.CONSTRUCT      PROCEDURE
  CODE
  SELF.PosQ       &= NEW PosQueue
  SELF.TextStyleQ &= NEW TextStyleQueue
  SELF.RectStyleQ &= NEW RectStyleQueue
  SELF.Output     &= NEW TargetGenerator
  SELF.CSSOutput  &= NEW TargetGenerator
  SELF.OutNameG   &= NEW NameGenerator
  SELF.ImageNameG &= NEW NameGenerator
  SELF.Attribute  &= NEW ReportAttributeManager

HTMLGenerator.DESTRUCT       PROCEDURE
  CODE
   FREE(SELF.PosQ)
   DISPOSE(SELF.PosQ)
   FREE(SELF.TextStyleQ)
   DISPOSE(SELF.TextStyleQ)
   FREE(SELF.RectStyleQ)
   DISPOSE(SELF.RectStyleQ)
   DISPOSE(SELF.Output)
   DISPOSE(SELF.CSSOutput)
   DISPOSE(SELF.OutNameG)
   DISPOSE(SELF.ImageNameG)
   DISPOSE(SELF.Attribute)

HTMLGenerator.Init           PROCEDURE(<STRING FileName>)
  CODE
    FREE(SELF.PosQ)
    FREE(SELF.TextStyleQ)
    FREE(SELF.RectStyleQ)
    SELF.SetDocumentName('Clarion Report')
    SELF.SetNavigationText('First','Last','Next','Prior','Select Page','Page_','Load Page')
    SELF.SetSubDirectory(True,'_Files','_Images')
    SELF.SetUp()

HTMLGenerator.SetUp                   PROCEDURE()
 CODE

HTMLGenerator.SetDocumentName         PROCEDURE(STRING pDocName)
 CODE
 SELF.DocumentName=pDocName

HTMLGenerator.SetNavigationText       PROCEDURE(STRING pNavFirst,STRING pNavLast,STRING pNavNext,STRING pNavPrior,STRING pNavPage0,STRING pNavPageText,STRING pNavGoPage)
 CODE
 SELF.NavFirst=pNavFirst
 SELF.NavLast=pNavLast
 SELF.NavNext=pNavNext
 SELF.NavPrior=pNavPrior
 SELF.NavPage0=pNavPage0
 SELF.NavPageText=pNavPageText
 SELF.NavGoPage=pNavGoPage

HTMLGenerator.SetSubDirectory         PROCEDURE(BYTE pCreateSubDirectory,<STRING pFilesSubDirectory>,<STRING pImagesSubDirectory>)
L      UNSIGNED,AUTO
 CODE
  SELF.CreateSubDirectory = pCreateSubDirectory
  IF OMITTED(3) AND OMITTED(4)
     IF SELF.CreateSubDirectory
        SELF.ImagesSubDirectory = '_Images'
        SELF.FilesSubDirectory  = '_Files'
     END
  ELSE
     IF NOT OMITTED(3)
        SELF.FilesSubDirectory = CLIP (pFilesSubDirectory)
        IF OMITTED(4)
           SELF.ImagesSubDirectory = SELF.FilesSubDirectory
        END
     END
     IF NOT OMITTED(4)
        SELF.ImagesSubDirectory = CLIP (pImagesSubDirectory)
        IF OMITTED(3)
           SELF.FilesSubDirectory = SELF.ImagesSubDirectory
        END
     END
  END

  L = LEN (SELF.ImagesSubDirectory)
  IF L <> 0 AND SELF.ImagesSubDirectory[L] = '\'
    SELF.ImagesSubDirectory[L] = '<0>'
  END

  L = LEN (SELF.FilesSubDirectory)
  IF L <> 0 AND SELF.FilesSubDirectory[L] = '\'
    SELF.FilesSubDirectory[L] = '<0>'
  END

HTMLGenerator.SetSingleFile   PROCEDURE(BYTE pSingleFile = 1)
 CODE
    SELF.SingleFileContent = pSingleFile
    IF SELF.SingleFileContent
       SELF.FilesSubDirectory = '.'
    END
    
HTMLGenerator.FlushNavigation PROCEDURE()
lPos            BYTE
lLastPos        BYTE
lIndex          SHORT
lFirstPageName  STRING(255),AUTO
lBasePageName   STRING(255),AUTO
lJSBasePageName CSTRING(6001),AUTO
lNameExtension  STRING(10),AUTO
 CODE
    IF SELF.SingleFileContent
       SELF.Output.WriteLine('<<head>')
       SELF.Output.WriteLine('<<title>'&CLIP(SELF.DocumentName)&'<</title>')
       SELF.Output.WriteLine('<<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">')
       SELF.Output.WriteLine('<<meta name="generator" content="Clarion Report Output Manager">')       
       SELF.Output.WriteLine('<<script>')
       
       SELF.Output.WriteLine('var pages        = 0;')
       SELF.Output.WriteLine('var currentpageN = 1;')

       SELF.Output.WriteLine('function loadRptPage() {{')
       SELF.Output.WriteLine('   if (document.frmNav.selPage.selectedIndex > 0)')
       SELF.Output.WriteLine('   {{')
       SELF.Output.WriteLine('    hidePage(currentpageN);')
       SELF.Output.WriteLine('		var numericID = document.frmNav.selPage.selectedIndex;')
       SELF.Output.WriteLine('		currentpageN = numericID;')
       SELF.Output.WriteLine('		showPage(currentpageN);')
       SELF.Output.WriteLine('   }')
       SELF.Output.WriteLine('}')
       SELF.Output.WriteLine('function loadOptions() {{')
       SELF.Output.WriteLine('    var pgName = '''';')
       SELF.Output.WriteLine('    var i = 0;')
       SELF.Output.WriteLine('    with (document.frmNav) {{')
       SELF.Output.WriteLine('        selPage.options.length = 0;')
       SELF.Output.WriteLine('        selPage.options[0] = new Option('''&CLIP(SELF.NavPage0)&''','''');')
       SELF.Output.WriteLine('        for (i = 1; i <= pages; i++) {{')
       SELF.Output.WriteLine('            pgName = '''&CLIP(SELF.NavPageText)&''' + i;')
       SELF.Output.WriteLine('            selPage.options[i] = new Option(pgName, i);')
       SELF.Output.WriteLine('            hidePage(i);')
       SELF.Output.WriteLine('        }')
       SELF.Output.WriteLine('		}')
       SELF.Output.WriteLine('}')
       SELF.Output.WriteLine('function showPage(pageN)')
       SELF.Output.WriteLine('{{')
       SELF.Output.WriteLine('  if (pageN > 0 && pageN < pages+1)')
       SELF.Output.WriteLine('  {{')
       SELF.Output.WriteLine('		var pgName = '''&CLIP(SELF.NavPageText)&''' + pageN;')
       SELF.Output.WriteLine('		var obj = document.getElementById(pgName);')
       SELF.Output.WriteLine('		obj.style.display=''block'';')
       SELF.Output.WriteLine('	}')
       SELF.Output.WriteLine('}')
       SELF.Output.WriteLine('function hidePage(pageN)')
       SELF.Output.WriteLine('{{')
       SELF.Output.WriteLine('  if (pageN > 0 && pageN < pages+1)')
       SELF.Output.WriteLine('  {{')
       SELF.Output.WriteLine('		var pgName = '''&CLIP(SELF.NavPageText)&''' + pageN;')
       SELF.Output.WriteLine('		var obj = document.getElementById(pgName);')
       SELF.Output.WriteLine('		obj.style.display=''none'';')
       SELF.Output.WriteLine('	}')
       SELF.Output.WriteLine('}')

       SELF.Output.WriteLine('function quickNav(page) {{')
       SELF.Output.WriteLine('var thisPage = currentpageN;')
       SELF.Output.WriteLine('    switch (page) {{')
       SELF.Output.WriteLine('    case ''First'':')
       SELF.Output.WriteLine('        document.frmNav.selPage.selectedIndex = 1;')
       SELF.Output.WriteLine('        loadRptPage();')
       SELF.Output.WriteLine('        break')
       SELF.Output.WriteLine('    case ''Prior'':')
       SELF.Output.WriteLine('        if (currentpageN > 1) {{')
       SELF.Output.WriteLine('            document.frmNav.selPage.selectedIndex = thisPage-1;')
       SELF.Output.WriteLine('            loadRptPage();')
       SELF.Output.WriteLine('            }')
       SELF.Output.WriteLine('        break')
       SELF.Output.WriteLine('    case ''Next'':')
       SELF.Output.WriteLine('        if (currentpageN < pages) {{')
       SELF.Output.WriteLine('            document.frmNav.selPage.selectedIndex = thisPage+1;')
       SELF.Output.WriteLine('            loadRptPage();')
       SELF.Output.WriteLine('            }')
       SELF.Output.WriteLine('        break')
       SELF.Output.WriteLine('    case ''Last'':')
       SELF.Output.WriteLine('        document.frmNav.selPage.selectedIndex = pages;')
       SELF.Output.WriteLine('        loadRptPage();')
       SELF.Output.WriteLine('        break')
       SELF.Output.WriteLine('    default:')
       SELF.Output.WriteLine('    }')
       SELF.Output.WriteLine('}')
       SELF.Output.WriteLine('<</script>')
       SELF.Output.WriteLine('<</head>')
       SELF.Output.WriteLine('<<body topmargin=0 bottommargin=0 leftmargin=0 rightmargin=0 >')
       SELF.Output.WriteLine('<<table align="justify" bgcolor="#C0C0C0">')
       SELF.Output.WriteLine('<<tr>')
       SELF.Output.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''First'');">['&CLIP(SELF.NavFirst)&']<</a><</td>')
       SELF.Output.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''Prior'');">['&CLIP(SELF.NavPrior)&']<</a><</td>')
       SELF.Output.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''Next'');">['&CLIP(SELF.NavNext)&']<</a><</td>')
       SELF.Output.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''Last'');">['&CLIP(SELF.NavLast)&']<</a><</td>')
       SELF.Output.WriteLine('    <<td>&nbsp;<</td>')
       SELF.Output.WriteLine('    <<form id="frmNav" name="frmNav" method="POST">')
       SELF.Output.WriteLine('    <<td><<select id="selPage" name="selPage" width="10"><</select><</td>')
       SELF.Output.WriteLine('    <</form>')
       SELF.Output.WriteLine('    <<td><<a href="JAVASCRIPT:loadRptPage();">'&CLIP(SELF.NavGoPage)&'<</a><</td>')
       SELF.Output.WriteLine('<</tr>')
       SELF.Output.WriteLine('<</table>')     
    ELSE
       !Writing the Index file
       IF SELF.CreateSubDirectory
          lBasePageName ='.\'&SELF.OutNameG.GetName(Name:Name) &SELF.FilesSubDirectory&'\'&SELF.OutNameG.GetName(Name:Name)&'_'
       ELSE
          lBasePageName ='.\'&SELF.OutNameG.GetName(Name:Name)&'_'
       END
       lNameExtension=SELF.OutNameG.GetExtension()
       IF SELF.CreateSubDirectory
          lFirstPageName='.\'&SELF.OutNameG.GetName(Name:Name) &SELF.FilesSubDirectory&'\'&SELF.OutNameG.GetName(Name:NoPath,1)
       ELSE
          lFirstPageName='.\'&SELF.OutNameG.GetName(Name:NoPath,1)
       END

       lJSBasePageName=SELF.OutNameG.GetName(Name:Name)&'_'

       SELF.CSSOutput.SetFileName(SELF.OutNameG.GetName(Name:Full))
       SELF.CSSOutput.OpenFile()
       SELF.CSSOutput.WriteLine('<<html>')
       SELF.CSSOutput.WriteLine('<<head>')
       SELF.CSSOutput.WriteLine('<<title>'&CLIP(SELF.DocumentName)&'<</title>')
       SELF.CSSOutput.WriteLine('<<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">')
       SELF.CSSOutput.WriteLine('<<meta name="generator" content="Clarion Report Output Manager">')
       SELF.CSSOutput.WriteLine('<</head>')
       SELF.CSSOutput.WriteLine('<<frameset rows="30,*" frameborder="NO" border="0" framespacing="0" cols="*">')
       SELF.CSSOutput.WriteLine('  <<frame name="NavFrame" scrolling="NO" noresize src="'&CLIP(lBasePageName)&'pgNav'&CLIP(lNameExtension)&'" marginheight="0">')
       SELF.CSSOutput.WriteLine('  <<frame name="RptFrame" src="'&CLIP(lFirstPageName)&'" marginheight="0">')
       SELF.CSSOutput.WriteLine('<</frameset>')
       SELF.CSSOutput.WriteLine('<<noframes>')
       SELF.CSSOutput.WriteLine('<<body bgcolor="#FFFFFF"topmargin=0 bottommargin=0 leftmargin=0 rightmargin=0>')
       SELF.CSSOutput.WriteLine('<</body>')
       SELF.CSSOutput.WriteLine('<</noframes>')
       SELF.CSSOutput.WriteLine('<</html>')

       SELF.CSSOutput.CloseFile()


       IF SELF.CreateSubDirectory
          SELF.CSSOutput.SetFileName(SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name) &SELF.FilesSubDirectory&'\'&SELF.OutNameG.GetName(Name:Name)&'_pgNav'&SELF.OutNameG.GetExtension())
       ELSE
          SELF.CSSOutput.SetFileName(SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name)&'_pgNav'&SELF.OutNameG.GetExtension())
       END
       SELF.CSSOutput.OpenFile()
       SELF.CSSOutput.WriteLine('<<html>')
       SELF.CSSOutput.WriteLine('<<head>')
       SELF.CSSOutput.WriteLine('<<title>'&CLIP(SELF.DocumentName)&'<</title>')
       SELF.CSSOutput.WriteLine('<<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">')
       SELF.CSSOutput.WriteLine('<<meta name="generator" content="Clarion Report Output Manager">')
       SELF.CSSOutput.WriteLine('<<script>')
       SELF.CSSOutput.WriteLine('<<!--')
       SELF.CSSOutput.WriteLine('var basename        = "'&CLIP(lJSBasePageName)&'";')
       SELF.CSSOutput.WriteLine('')
       IF SELF.CreateSubDirectory
          SELF.CSSOutput.WriteLine('if ((navigator.appName == "Netscape") && (parseInt(navigator.appVersion) == 4)) {{')
          SELF.CSSOutput.WriteLine('basename = ''./''  + "'&SELF.OutNameG.GetName(Name:Name) & SELF.FilesSubDirectory&'" + ''/'' + basename;')
          SELF.CSSOutput.WriteLine('};')
       END
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('var pages           = '&SELF.TotalPageNo&';')
       SELF.CSSOutput.WriteLine('var pagenumpad      = '&LEN(CLIP(LEFT(SELF.TotalPageNo)))&';')
       SELF.CSSOutput.WriteLine('var extension       = '''&CLIP(lNameExtension)&''';')
       SELF.CSSOutput.WriteLine('var extensionwidth  = '&LEN(CLIP(lNameExtension))&';')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('var firstpage   = basename + '''' + pad(1,pagenumpad) +  extension;')
       SELF.CSSOutput.WriteLine('var lastpage    = basename + '''' + pad(pages,pagenumpad) +  extension;')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('function loadRptPage() {{')
       SELF.CSSOutput.WriteLine('   if (document.frmNav.selPage.options[document.frmNav.selPage.selectedIndex].value != "") {{')
       SELF.CSSOutput.WriteLine('     parent.RptFrame.location.href = document.frmNav.selPage.options[document.frmNav.selPage.selectedIndex].value;')
       SELF.CSSOutput.WriteLine('   }')
       SELF.CSSOutput.WriteLine('}')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('function pad(number,length) {{')
       SELF.CSSOutput.WriteLine('    var str = '''' + number;')
       SELF.CSSOutput.WriteLine('    while (str.length << length)')
       SELF.CSSOutput.WriteLine('    str = ''0'' + str;')
       SELF.CSSOutput.WriteLine('    return str;')
       SELF.CSSOutput.WriteLine('}')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('function loadOptions() {{')
       SELF.CSSOutput.WriteLine('    var pgName = '''';')
       SELF.CSSOutput.WriteLine('    var i = 0;')
       SELF.CSSOutput.WriteLine('    with (document.frmNav) {{')
       SELF.CSSOutput.WriteLine('        selPage.options.length = 0;')
       SELF.CSSOutput.WriteLine('        selPage.options[0] = new Option('''&CLIP(SELF.NavPage0)&''','''');')
       SELF.CSSOutput.WriteLine('        for (i = 1; i <<= pages; i++) {{')
       SELF.CSSOutput.WriteLine('            pgName = '''&CLIP(SELF.NavPageText)&''' + pad(i,pagenumpad) ;')
       SELF.CSSOutput.WriteLine('            selPage.options[i] = new Option(pgName, basename + pad(i,pagenumpad) + extension);')
       SELF.CSSOutput.WriteLine('        }')
       SELF.CSSOutput.WriteLine('    }')
       SELF.CSSOutput.WriteLine('}')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('function quickNav(page) {{')
       SELF.CSSOutput.WriteLine(' var thisPage = currentPage();')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('    switch (page) {{')
       SELF.CSSOutput.WriteLine('    case ''First'':')
       SELF.CSSOutput.WriteLine('        parent.RptFrame.location.href = firstpage;')
       SELF.CSSOutput.WriteLine('        document.frmNav.selPage.selectedIndex = 1;')
       SELF.CSSOutput.WriteLine('        break')
       SELF.CSSOutput.WriteLine('    case ''Prior'':')
       SELF.CSSOutput.WriteLine('        if (thisPage > 1) {{')
       SELF.CSSOutput.WriteLine('            parent.RptFrame.location.href = basename + '''' + pad((thisPage-1),pagenumpad) + extension;')
       SELF.CSSOutput.WriteLine('            document.frmNav.selPage.selectedIndex = thisPage-1;')
       SELF.CSSOutput.WriteLine('            }')
       SELF.CSSOutput.WriteLine('        break')
       SELF.CSSOutput.WriteLine('    case ''Next'':')
       SELF.CSSOutput.WriteLine('        if (thisPage << pages) {{')
       SELF.CSSOutput.WriteLine('            parent.RptFrame.location.href = basename + '''' + pad((thisPage+1),pagenumpad) + extension;')
       SELF.CSSOutput.WriteLine('            document.frmNav.selPage.selectedIndex = thisPage+1;')
       SELF.CSSOutput.WriteLine('            }')
       SELF.CSSOutput.WriteLine('        break')
       SELF.CSSOutput.WriteLine('    case ''Last'':')
       SELF.CSSOutput.WriteLine('        parent.RptFrame.location.href = lastpage;')
       SELF.CSSOutput.WriteLine('        document.frmNav.selPage.selectedIndex = pages;')
       SELF.CSSOutput.WriteLine('        break')
       SELF.CSSOutput.WriteLine('    default:')
       SELF.CSSOutput.WriteLine('    }')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('}')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('function currentPage() {{')
       SELF.CSSOutput.WriteLine(' var strURL     = parent.RptFrame.location.href + '''';')
       SELF.CSSOutput.WriteLine(' var startpos   = pagenumpad + extensionwidth;')
       SELF.CSSOutput.WriteLine(' var endpos     = extensionwidth;')
       SELF.CSSOutput.WriteLine(' return( Math.abs(strURL.slice(strURL.length-startpos,strURL.length-endpos)) );')
       SELF.CSSOutput.WriteLine('}')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('// -->')
       SELF.CSSOutput.WriteLine('<</script>')
       SELF.CSSOutput.WriteLine('<</head>')
       SELF.CSSOutput.WriteLine('<<body bgcolor="#C0C0C0" topmargin=0 bottommargin=0 leftmargin=0 rightmargin=0 onload="JAVASCRIPT:loadOptions();JAVASCRIPT:quickNav(''First'');">')
       SELF.CSSOutput.WriteLine('')
       SELF.CSSOutput.WriteLine('<<table align="center">')
       SELF.CSSOutput.WriteLine('<<tr>')
       SELF.CSSOutput.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''First'');">['&CLIP(SELF.NavFirst)&']<</a><</td>')
       SELF.CSSOutput.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''Prior'');">['&CLIP(SELF.NavPrior)&']<</a><</td>')
       SELF.CSSOutput.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''Next'');">['&CLIP(SELF.NavNext)&']<</a><</td>')
       SELF.CSSOutput.WriteLine('    <<td valign="middle"><<a href="JAVASCRIPT:quickNav(''Last'');">['&CLIP(SELF.NavLast)&']<</a><</td>')
       SELF.CSSOutput.WriteLine('    <<td>&nbsp;<</td>')
       SELF.CSSOutput.WriteLine('    <<form id="frmNav" name="frmNav" method="POST">')
       SELF.CSSOutput.WriteLine('    <<td><<select id="selPage" name="selPage" width="10"><</select><</td>')
       SELF.CSSOutput.WriteLine('    <</form>')
       SELF.CSSOutput.WriteLine('    <<td><<a href="JAVASCRIPT:loadRptPage();">'&CLIP(SELF.NavGoPage)&'<</a><</td>')
       SELF.CSSOutput.WriteLine('<</tr>')
       SELF.CSSOutput.WriteLine('<</table>')
       SELF.CSSOutput.WriteLine('<</body>')
       SELF.CSSOutput.WriteLine('<</html>')

       SELF.CSSOutput.CloseFile()
    END
HTMLGenerator.FlushStyleSheet PROCEDURE()
idx  UNSIGNED,AUTO
HexCol CSTRING(10)
tmpOutput &TargetGenerator
maxheight UNSIGNED,AUTO
currentheight UNSIGNED,AUTO
  CODE
    IF SELF.SingleFileContent
       tmpOutput &= SELF.CSSOutput
       SELF.CSSOutput &= SELF.Output
    ELSE
       IF SELF.CreateSubDirectory
          SELF.CSSOutput.SetFileName(SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name) &SELF.FilesSubDirectory&'\'&SELF.OutNameG.GetName(Name:Name)&'.css')
       ELSE
          SELF.CSSOutput.SetFileName(SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name)&'.css')
       END
       SELF.CSSOutput.OpenFile()   
    END

    SELF.CSSOutput.WriteLine('.LeftStyle')
    SELF.CSSOutput.WriteLine('{{')
    SELF.CSSOutput.WriteLine('  text-align: left;')
    SELF.CSSOutput.WriteLine('}')

    SELF.CSSOutput.WriteLine('.CenterStyle')
    SELF.CSSOutput.WriteLine('{{')
    SELF.CSSOutput.WriteLine('  text-align: center;')
    SELF.CSSOutput.WriteLine('}')

    SELF.CSSOutput.WriteLine('.RightStyle')
    SELF.CSSOutput.WriteLine('{{')
    SELF.CSSOutput.WriteLine('  text-align: right;')
    SELF.CSSOutput.WriteLine('}')
    LOOP idx = 1 TO RECORDS(SELF.TextStyleQ)
      GET(SELF.TextStyleQ, idx)
      SELF.CSSOutput.WriteLine('.' & SELF.TextStyleQ.Name)
      SELF.CSSOutput.WriteLine('{{')
      SELF.CSSOutput.WriteLine('  font-family: ' & CLIP(SELF.TextStyleQ.Face) & ';')
      SELF.CSSOutput.WriteLine('  font-size: ' & SELF.TextStyleQ.Size & 'pt;')
      HexCol = LongToHex(SELF.TextStyleQ.Color)
      SELF.CSSOutput.WriteLine('  color: #' & HexCol[7:8]&HexCol[5:6]&HexCol[3:4] & ';')
      CASE SELF.TextStyleQ.Style
      OF 0 OROF FONT:Regular
         SELF.CSSOutput.WriteLine('  font-weight: normal;')
      OF FONT:Bold
         SELF.CSSOutput.WriteLine('  font-weight: bold;')
      ELSE
         SELF.CSSOutput.WriteLine('  font-weight: ' & SELF.TextStyleQ.Style & ';')
      END
      SELF.CSSOutput.WriteLine('}')
    END 
    LOOP idx = 1 TO RECORDS(SELF.RectStyleQ)
      GET(SELF.RectStyleQ, idx)
      SELF.CSSOutput.WriteLine('.' & SELF.RectStyleQ.Name)
      SELF.CSSOutput.WriteLine('{{')
      IF NOT SELF.RectStyleQ.IsLine
         IF SELF.RectStyleQ.Color = COLOR:None
            SELF.CSSOutput.WriteLine('  background-color: # transparent;')
         ELSE
            HexCol = LongToHex(SELF.RectStyleQ.Color)
            SELF.CSSOutput.WriteLine('  background-color: #' & HexCol[7:8]&HexCol[5:6]&HexCol[3:4] & ';')
         END
      END  
      IF SELF.RectStyleQ.BorderWidth
        HexCol = LongToHex(SELF.RectStyleQ.BorderColor)
        SELF.CSSOutput.WriteLine('  border-color: #' & HexCol[7:8]&HexCol[5:6]&HexCol[3:4] & ';')
        SELF.CSSOutput.WriteLine('  border-width: ' & SELF.RectStyleQ.BorderWidth & 'mm;')
        IF SELF.RectStyleQ.IsLine = TOP_STYLE
          SELF.CSSOutput.WriteLine('  border-top-style: solid;')
        ELSIF SELF.RectStyleQ.IsLine = LEFT_STYLE
          SELF.CSSOutput.WriteLine('  border-left-style: solid;')
        ELSE  
          SELF.CSSOutput.WriteLine('  border-style: solid;')
        END  
      END  
      SELF.CSSOutput.WriteLine('}')
    END 
    maxheight = 0
    LOOP idx = 1 TO RECORDS(SELF.PosQ)
      GET(SELF.PosQ, idx)
      SELF.CSSOutput.WriteLine('#' & SELF.PosQ.Name)
      SELF.CSSOutput.WriteLine('{{')
      SELF.CSSOutput.WriteLine('  position: absolute;')
      SELF.CSSOutput.WriteLine('  left: ' & SELF.PosQ.XPos & 'mm;')
      SELF.CSSOutput.WriteLine('  top: ' & SELF.PosQ.YPos & 'mm;')
      SELF.CSSOutput.WriteLine('  width: ' & SELF.PosQ.Width & 'mm;')
      SELF.CSSOutput.WriteLine('  height: ' & SELF.PosQ.Height & 'mm;')
      SELF.CSSOutput.WriteLine('}')
      currentheight = SELF.PosQ.YPos + SELF.PosQ.Height
      IF currentheight > maxheight
         maxheight = currentheight
      END
    END 
    IF SELF.SingleFileContent
       SELF.Output.WriteLine('.page{{')
       !SELF.Output.WriteLine(' background-color:#E2EBED;')
       SELF.Output.WriteLine(' /* This one should not be changed */')
       SELF.Output.WriteLine(' display:none;')
       SELF.Output.WriteLine(' position: absolute;')
       SELF.Output.WriteLine(' left: 0mm;')
       SELF.Output.WriteLine(' top: 0mm;')       
       SELF.Output.WriteLine(' /* This is done to allow scroll the page after the last line */')
       SELF.Output.WriteLine(' /* and make the line visible*/')
       SELF.Output.WriteLine(' height: '&maxheight+5&'mm;')  
       SELF.Output.WriteLine('}')
    END

    IF SELF.SingleFileContent
       SELF.CSSOutput &= tmpOutput
    ELSE
       SELF.CSSOutput.CloseFile()
    END

HTMLGenerator.ProcessCheck   PROCEDURE(*CheckFormatGrp cf, STRING Text)
chsize SIGNED,AUTO
  CODE
    IF cf.prompt.Size <> 0
       chsize = cf.prompt.Size + 5
    ELSE
       chsize = 14
    END
    SELF.ProcessString(cf.prompt, Text)
    SELF.ProcessLocation(cf.Pos)
    CLEAR(SELF.TextStyleQ)
    SELF.TextStyleQ.Size  = chsize
    SELF.TextStyleQ.Face = 'Wingdings'
    SELF.TextStyleQ.Style = CHOOSE (cf.Style.BorderWidth > 1, FONT:Bold, FONT:Regular)
    SELF.TextStyleQ.Color = cf.Style.BorderColor
    GET(SELF.TextStyleQ, SELF.TextStyleQ.Size,SELF.TextStyleQ.Style,SELF.TextStyleQ.Color,SELF.TextStyleQ.Face)
    IF ERRORCODE() <> 0
      SELF.TextStyleQ.Name = 'TextStyle' & SELF.TextStyleCnt
      SELF.TextStyleCnt += 1    
      ADD(SELF.TextStyleQ)
    END
    SELF.Output.WriteLine('<<SPAN class="' & SELF.TextStyleQ.Name & '" id="' & SELF.PosQ.Name & '">' & CHOOSE (NOT cf.Checked, 'o', 'x') & '<</SPAN>')

HTMLGenerator.ProcessImage      PROCEDURE(*ImageFormatGrp imgf, STRING iName)
  CODE
    IF iName THEN
       SELF.ProcessLocation(imgf.Pos)
       SELF.ImageNameG.Init('','')
       SELF.ImageNameG.ExtractFileName(iName)
       IF SELF.CreateSubDirectory
          COPY(iName,SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name) &SELF.ImagesSubDirectory&'\'&SELF.ImageNameG.GetName(Name:NoPath))
       ELSE
          COPY(iName,SELF.OutNameG.GetName(Name:Path) & SELF.ImageNameG.GetName(Name:NoPath))
       END
       IF SELF.IsHyperLink THEN
          SELF.IsHyperLink = False
          IF SELF.CreateSubDirectory
             SELF.Output.WriteLine('<<SPAN id="' & SELF.PosQ.Name & '">' & '<<a'&CLIP(SELF.HyperLinkTarget)&' href="'&CLIP(SELF.HyperLink)&'"><<IMG src="' & CLIP('..\'&SELF.OutNameG.GetName(Name:Name) &SELF.ImagesSubDirectory&'\'&SELF.ImageNameG.GetName(Name:NoPath)) & '"  WIDTH="100%" HEIGHT="100%" ><</a>' & '<</SPAN>')
          ELSE
             SELF.Output.WriteLine('<<SPAN id="' & SELF.PosQ.Name & '">' & '<<a'&CLIP(SELF.HyperLinkTarget)&' href="'&CLIP(SELF.HyperLink)&'"><<IMG src="' & CLIP('.\'&SELF.ImageNameG.GetName(Name:NoPath)) & '"  WIDTH="100%" HEIGHT="100%" ><</a>' & '<</SPAN>')
          END
       ELSE
          IF SELF.CreateSubDirectory
             SELF.Output.WriteLine('<<SPAN id="' & SELF.PosQ.Name & '">' & '<<IMG src="' & CLIP('..\'&SELF.OutNameG.GetName(Name:Name) &SELF.ImagesSubDirectory&'\'&SELF.ImageNameG.GetName(Name:NoPath)) & '" WIDTH="100%" HEIGHT="100%" >' & '<</SPAN>')
          ELSE
             SELF.Output.WriteLine('<<SPAN id="' & SELF.PosQ.Name & '">' & '<<IMG src="' & CLIP('.\'&SELF.ImageNameG.GetName(Name:NoPath)) & '" WIDTH="100%" HEIGHT="100%" >' & '<</SPAN>')
          END
       END
    END
    

HTMLGenerator.ProcessLine    PROCEDURE(*LineFormatGrp lf)
rect    GROUP(RectFormatGrp).
IsLine  BYTE
  CODE
    IF lf.Pos.Left = lf.Pos.Right 
      IsLine = LEFT_STYLE
    ELSIF lf.Pos.Top = lf.Pos.Bottom
      IsLine = TOP_STYLE
    END
    IF IsLine  
      IF lf.Pos.Left > lf.Pos.Right OR lf.Pos.Top > lf.Pos.Bottom
        ! needs swapping
        rect.Pos.Left = lf.Pos.Right
        rect.Pos.Right = lf.Pos.Left
        rect.Pos.Top = lf.Pos.Bottom
        rect.Pos.Bottom = lf.Pos.Top
      ELSE  
        rect.Pos = lf.Pos
      END  
      rect.Style.BorderColor = lf.Color
      rect.Style.BorderWidth = lf.Width
      SELF.ProcessRect(rect, IsLine)
    ELSE
      ! not possible to display in HTML ?
    END        

HTMLGenerator.ProcessLocation PROCEDURE(*PosGrp pPos,USHORT BorderWidth=0)
  CODE
    IF BorderWidth THEN
       SELF.PosQ.XPos   = (pPos.Left/39.35185)-((BorderWidth / 23.78121284185)/2)
       SELF.PosQ.YPos   = (pPos.Top/39.42652)-((BorderWidth / 23.78121284185)/2)
       SELF.PosQ.Width  = ((pPos.Right - pPos.Left)/39.35185)+(BorderWidth / 23.78121284185)
       SELF.PosQ.Height = ((pPos.Bottom - pPos.Top)/39.42652)+(BorderWidth / 23.78121284185)
    ELSE
       SELF.PosQ.XPos   = pPos.Left/39.35185
       SELF.PosQ.YPos   = pPos.Top/39.42652
       SELF.PosQ.Width  = (pPos.Right - pPos.Left)/39.35185
       SELF.PosQ.Height = (pPos.Bottom - pPos.Top)/39.42652
    END
    GET(SELF.PosQ, SELF.PosQ.XPos, SELF.PosQ.YPos, SELF.PosQ.Width, SELF.PosQ.Height)
    IF ERRORCODE()
      SELF.PosQ.Name = 'Position' & SELF.PosCnt
      SELF.PosCnt += 1
      ADD(SELF.PosQ)
    END  

HTMLGenerator.ProcessGroup      PROCEDURE(*GroupFormatGrp gfg, STRING Text)
lf    GROUP(LineFormatGrp).
  CODE
    SELF.ProcessString(gfg.header, Text)
    lf.Width    = gfg.LineWidth
    lf.Color    = gfg.LineColor
    lf.Pos.Left = gfg.Pos.Left
    lf.Pos.Top  = gfg.Pos.Top
    IF gfg.Pos.Left <> gfg.X1
      lf.Pos.Right = gfg.X1
      lf.Pos.Bottom = gfg.Pos.Top
      SELF.ProcessLine(lf)
    END  
    lf.Pos.Right  = gfg.Pos.Left
    lf.Pos.Bottom = gfg.Pos.Bottom
    SELF.ProcessLine(lf)
    lf.Pos.Left   = gfg.Pos.Left
    lf.Pos.Top    = gfg.Pos.Bottom
    lf.Pos.Right  = gfg.Pos.Right
    lf.Pos.Bottom = gfg.Pos.Bottom
    SELF.ProcessLine(lf)
    lf.Pos.Left   = gfg.Pos.Right
    lf.Pos.Top    = gfg.Pos.Top
    SELF.ProcessLine(lf)
    lf.Pos.Left   = gfg.X2
    lf.Pos.Bottom = gfg.Pos.Top
    SELF.ProcessLine(lf)

HTMLGenerator.ProcessRadio      PROCEDURE(*RadioFormatGrp rf, STRING Text)
  CODE
    SELF.ProcessString(rf.prompt, Text)
    SELF.ProcessLocation(rf.Outer.Pos)
    CLEAR(SELF.TextStyleQ)
    SELF.TextStyleQ.Size  = SELF.TextStyleQ.Size + 12
    SELF.TextStyleQ.Face  = 'Wingdings'
    SELF.TextStyleQ.Style = CHOOSE(rf.Outer.Style.BorderWidth > 1, FONT:Bold, 0)
    SELF.TextStyleQ.Color = rf.Outer.Style.BorderColor
    GET(SELF.TextStyleQ, SELF.TextStyleQ.Size,SELF.TextStyleQ.Style,SELF.TextStyleQ.Color,SELF.TextStyleQ.Face)
    IF ERRORCODE() <> 0
      SELF.TextStyleQ.Name = 'TextStyle' & SELF.TextStyleCnt
      SELF.TextStyleCnt += 1    
      ADD(SELF.TextStyleQ)
    END
    SELF.Output.WriteLine('<<SPAN class="' & SELF.TextStyleQ.Name & '" id="' & SELF.PosQ.Name & '">' & CHOOSE (NOT rf.Checked, CHR(161), CHR(164)) & '<</SPAN>')

HTMLGenerator.ProcessRect           PROCEDURE(*RectFormatGrp rf, BYTE IsLine)
  CODE
    SELF.ProcessLocation(rf.Pos,rf.Style.BorderWidth)
    CLEAR(SELF.RectStyleQ)
    SELF.RectStyleQ.IsLine = IsLine
    SELF.RectStyleQ.Color = rf.Style.BackgroundColor
    SELF.RectStyleQ.BorderColor = rf.Style.BorderColor
    SELF.RectStyleQ.BorderWidth = rf.Style.BorderWidth / 23.78121284185
    GET(SELF.RectStyleQ, SELF.RectStyleQ.Color, SELF.RectStyleQ.BorderColor, SELF.RectStyleQ.BorderWidth, SELF.RectStyleQ.IsLine)
    IF ERRORCODE() <> 0
      SELF.RectStyleQ.Name = 'RectStyle' & SELF.RectStyleCnt
      SELF.RectStyleCnt += 1
      ADD(SELF.RectStyleQ)
    END  
    SELF.Output.WriteLine('<<SPAN class="' & SELF.RectStyleQ.Name & '" id="' & SELF.PosQ.Name & '">' & '<</SPAN>')

HTMLGenerator.ProcessString    PROCEDURE(*StringFormatGrp tf, STRING text)
rf     GROUP(RectFormatGrp),AUTO.
pos    GROUP(PosGrp),AUTO.
  CODE
    IF tf.BackgroundColor
      CLEAR(rf)
      rf.Pos = tf.Pos
      rf.Style.BackgroundColor = tf.BackgroundColor
      SELF.ProcessRect(rf, FALSE)
    END
    pos.Top = tf.topText
    pos.Left = tf.leftText
    pos.Bottom = tf.Pos.Bottom
    pos.Right = tf.Pos.Right
    SELF.ProcessLocation(pos)
    CLEAR(SELF.TextStyleQ)
    SELF.TextStyleQ.Size = tf.Size
    SELF.TextStyleQ.Face = tf.Face
    SELF.TextStyleQ.Style = tf.Style
    SELF.TextStyleQ.Color = tf.Color
    GET(SELF.TextStyleQ, SELF.TextStyleQ.Size,SELF.TextStyleQ.Style,SELF.TextStyleQ.Color,SELF.TextStyleQ.Face)
    IF ERRORCODE()
      SELF.TextStyleQ.Name = 'TextStyle' & SELF.TextStyleCnt
      SELF.TextStyleCnt += 1    
      ADD(SELF.TextStyleQ)
    END
    IF SELF.IsHyperLink THEN
       SELF.IsHyperLink = False
       SELF.Output.WriteLine('<<SPAN class="' & SELF.TextStyleQ.Name & GetAlignStyle(text) & '" id="' & SELF.PosQ.Name & '"><<a'&CLIP(SELF.HyperLinkTarget)&' href="'&CLIP(SELF.HyperLink)&'">'& text & '<</a><</SPAN>')
    ELSE
       SELF.Output.WriteLine('<<SPAN class="' & SELF.TextStyleQ.Name & GetAlignStyle(text) & '" id="' & SELF.PosQ.Name & '">' & text & '<</SPAN>')
    END

HTMLGenerator.SetFileName              PROCEDURE(STRING pFileName)
 CODE
    SELF.Output.SetOriginalFileName(pFileName)

HTMLGenerator.ProcessComment           PROCEDURE(STRING Text,STRING pExtendControlAttr)
lHyperPrefix     CSTRING(MAXAttributeSize+1),AUTO
  CODE
    SELF.SkipControl = False
    IF SELF.Attribute.Extract(RepGen:ALL,TargetAttr:SKIP,pExtendControlAttr)
       SELF.SkipControl = True
    END
    IF SELF.Attribute.Extract(RepGen:HTML,TargetAttr:SKIP,pExtendControlAttr)
       SELF.SkipControl = True
    END
    SELF.IsHyperLink = False
    SELF.HyperLinkTarget = ''
    SELF.HyperLink = SELF.Attribute.Extract(RepGen:HTML,TargetAttr:Hyperlink,pExtendControlAttr)
    IF SELF.HyperLink THEN
       IF CLIP(SELF.HyperLink)=TargetAttr:Hyperlink THEN
          SELF.HyperLink = Text
       END
       IF CLIP(SELF.HyperLink) THEN
          lHyperPrefix     = SELF.Attribute.Extract(RepGen:HTML,TargetAttr:HyperlinkType,pExtendControlAttr)
          SELF.HyperLinkTarget = SELF.Attribute.Extract(RepGen:HTML,TargetAttr:HyperlinkTarget,pExtendControlAttr)
          IF CLIP(SELF.HyperLinkTarget) THEN
             SELF.HyperLinkTarget = ' target="'&CLIP(SELF.HyperLinkTarget)&'"'
          END
          SELF.HyperLink   = lHyperPrefix & SELF.HyperLink
          SELF.IsHyperLink = True
       END
    END

!If the control was hidden in runtime it replace the width and height with the real
HTMLGenerator.ProcessHiddenControlSize PROCEDURE(*StringFormatGrp pStrFormat,STRING pExtendControlAttr)
lOriginalRight LONG,AUTO
lOffset        LONG,AUTO
 CODE
    SELF.ProcessHiddenControlSize(pStrFormat.Pos,pExtendControlAttr)

HTMLGenerator.ProcessHiddenControlSize PROCEDURE(*PosGrp pPos,STRING pExtendControlAttr)
lWidth   LONG,AUTO
lHeight  LONG,AUTO
 CODE
       lWidth  = 0
       lHeight = 0
       lWidth  = SELF.Attribute.Extract(RepGen:ALL,TargetAttr:HideWidth,pExtendControlAttr)
       lHeight = SELF.Attribute.Extract(RepGen:ALL,TargetAttr:HideHeight,pExtendControlAttr)
       IF lWidth>0
           pPos.Right = pPos.Left + lWidth
       END
       IF lHeight>0
           pPos.Bottom= pPos.Top  + lHeight
       END

! ==============================================================================

HTMLReportGenerator.CONSTRUCT            PROCEDURE()
 CODE
    SELF.ImageFileQueue &= NEW(OutputFileQueue)


HTMLReportGenerator.DESTRUCT                             PROCEDURE()
 CODE
    FREE(SELF.ImageFileQueue)
    DISPOSE(SELF.ImageFileQueue)

HTMLReportGenerator.IReportGenerator.Init           PROCEDURE(<ErrorClass EC>)
  CODE


HTMLReportGenerator.IReportGenerator.AskProperties       PROCEDURE(BYTE Force=0)
LOC:FileName CSTRING(FILE:MaxFilePath),AUTO
i            UNSIGNED,AUTO
rt           BYTE,AUTO
  CODE
  SELF.Init()

  IF NOT CLIP(SELF.Output.GetFileName()) OR Force
     CLEAR (LOC:FileName)
     IF NOT FILEDIALOG ('Save as HTML File', LOC:FileName, 'HTM|*.HTM|HTML|*.HTML|All|*.*', FILE:LongName+FILE:Save+FILE:KeepDir)
        RETURN Level:Notify
     END

     i = LEN (LOC:FileName)

     LOOP WHILE i <> 0
       CASE VAL (LOC:FileName[i])
       OF VAL('\')
       OROF VAL(':')
         i = 0
       OROF VAL('.')
         BREAK
       END
       i -= 1
     END
     IF i = 0
        LOC:FileName = LOC:FileName & '.HTM'
     END

     SELF.OutNameG.Init ('ClarionReport', 'HTM')
     SELF.OutNameG.ExtractFileName (LOC:FileName)
     SELF.Output.SetOriginalFileName (LOC:FileName)

     IF SELF.CreateSubDirectory
        IF NOT SELF.SingleFileContent
           rt = CreateDirectory (SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name) & SELF.FilesSubDirectory & '\')
        END
        rt = CreateDirectory (SELF.OutNameG.GetName(Name:Path) & SELF.OutNameG.GetName(Name:Name) & SELF.ImagesSubDirectory & '\')
     END
  ELSE
     SELF.OutNameG.ExtractFileName (SELF.Output.GetFileName())
     IF SELF.CreateSubDirectory
        IF NOT SELF.SingleFileContent
           rt = CreateDirectory (SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:Name) & SELF.FilesSubDirectory & '\')
        END
        rt = CreateDirectory (SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:Name) & SELF.ImagesSubDirectory & '\')
     END
  END
  RETURN Level:Benign

HTMLReportGenerator.IReportGenerator.OpenDocument        PROCEDURE(UNSIGNED TotalPages)
  CODE
    SELF.PageNo = 0
    SELF.PosCnt = 1
    SELF.TextStyleCnt = 1
    SELF.RectStyleCnt = 1
    SELF.DocumentOpen = TRUE
    SELF.TotalPageNo  = TotalPages
    SELF.OutNameG.SetTotalPages(TotalPages)
    IF NOT SELF.OutputFileQueue &= NULL THEN
       FREE(SELF.OutputFileQueue)
          SELF.OutputFileQueue.FileName = SELF.OutNameG.GetName(Name:Full)
          ADD(SELF.OutputFileQueue)
    END
    IF SELF.SingleFileContent
       SELF.Output.OpenFile()
       SELF.Output.WriteLine('<<HTML>')
       SELF.FlushNavigation()      
    END
    RETURN Level:Benign
    
HTMLReportGenerator.IReportGenerator.CloseDocument  PROCEDURE
  CODE
    IF NOT SELF.DocumentOpen
      RETURN Level:Notify
    END
    SELF.DocumentOpen = FALSE
    IF SELF.SingleFileContent
       SELF.Output.WriteLine('<<script type="text/javascript">window.onload=function(){{pages='&SELF.TotalPageNo&';JAVASCRIPT:loadOptions();JAVASCRIPT:quickNav(''First'');}<</script>')     
       SELF.Output.WriteLine('<</body>')   
       SELF.Output.WriteLine('<<style type="text/css">')
       SELF.FlushStyleSheet()
       SELF.Output.WriteLine('<</style>')
       SELF.Output.WriteLine('<</HTML>')       
       SELF.Output.CloseFile()
    ELSE    
       SELF.FlushStyleSheet()
       IF SELF.TotalPageNo <> 0
          SELF.FlushNavigation()
          IF NOT SELF.OutputFileQueue &= NULL THEN
             SELF.OutputFileQueue.FileName = SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:NoPath)
             ADD(SELF.OutputFileQueue)
             IF SELF.CreateSubDirectory
                SELF.OutputFileQueue.FileName = SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:Name)&SELF.FilesSubDirectory&'\'& SELF.OutNameG.GetName(Name:Name)&'.CSS'
             ELSE
                SELF.OutputFileQueue.FileName = SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:Name)&'.CSS'
             END
             ADD(SELF.OutputFileQueue)
          END
       END
    END
    SELF.Output.SetFileName('')
    RETURN Level:Benign

HTMLReportGenerator.IReportGenerator.SupportPageProcessing PROCEDURE()
 CODE
    RETURN TRUE
    
HTMLReportGenerator.IReportGenerator.OpenPage PROCEDURE(STRING PageName)
 CODE
    RETURN Level:Benign
    
HTMLReportGenerator.IReportGenerator.StartPageProcess            PROCEDURE(SHORT pBoxLeft,SHORT pBoxTop,SHORT pBoxRight,SHORT pBoxBottom,STRING pPageName)
  CODE
    SELF.PageNo += 1
    IF SELF.SingleFileContent
       SELF.Output.WriteLine('<<div id="'&CLIP(SELF.NavPageText)&LEFT(CLIP(SELF.PageNo))&'" class="page">')
    ELSE
       IF SELF.CreateSubDirectory
          SELF.Output.SetFileName(SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:Name) &SELF.FilesSubDirectory&'\'& SELF.OutNameG.GetName(Name:NoPath,SELF.PageNo))
       ELSE
          SELF.Output.SetFileName(SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:NoPath,SELF.PageNo))
       END
       IF NOT SELF.OutputFileQueue &= NULL THEN
          IF SELF.CreateSubDirectory
             SELF.OutputFileQueue.FileName = SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:Name) &SELF.FilesSubDirectory&'\'& SELF.OutNameG.GetName(Name:NoPath,SELF.PageNo)
          ELSE
             SELF.OutputFileQueue.FileName = SELF.OutNameG.GetName(Name:Path,0) & SELF.OutNameG.GetName(Name:NoPath,SELF.PageNo)
          END
          ADD(SELF.OutputFileQueue)
       END
       SELF.Output.OpenFile()
       SELF.Output.WriteLine('<<HTML>')
       SELF.Output.WriteLine('<<HEAD>')
       SELF.Output.WriteLine('<<LINK rel="stylesheet" type="text/css" href="' & |
       SELF.OutNameG.GetName(Name:Name)&'.css'&|
                                    '">')
       SELF.Output.WriteLine('<</HEAD>')
       SELF.Output.WriteLine('<<BODY>')
    END

    SELF.PageOpen = TRUE
    SELF.BoxLeft = pBoxLeft
    SELF.BoxTop  = pBoxTop
    SELF.BoxRight = pBoxRight
    SELF.BoxBottom = pBoxBottom
    RETURN Level:Benign        

HTMLReportGenerator.IReportGenerator.ClosePage      PROCEDURE
  CODE
    IF NOT SELF.PageOpen
      RETURN Level:Notify
    END
    IF SELF.SingleFileContent
       SELF.Output.WriteLine('<</div>')
    ELSE
       SELF.Output.WriteLine('<</BODY>')
       SELF.Output.WriteLine('<</HTML>')
       SELF.Output.CloseFile()
    END
    IF ERRORCODE() <> 0
       RETURN Level:Notify
    END
    SELF.PageOpen = FALSE
    RETURN Level:Benign

HTMLReportGenerator.IReportGenerator.WhoAmI         PROCEDURE
  CODE
    RETURN RepGen:HTML  

HTMLReportGenerator.IReportGenerator.DisplayIcon         PROCEDURE()
 CODE
     RETURN '~EXP_HTM.ICO'

HTMLReportGenerator.IReportGenerator.DisplayName         PROCEDURE()
 CODE
     RETURN 'HTML'

HTMLReportGenerator.IReportGenerator.GetFileName              PROCEDURE()
 CODE
    RETURN SELF.Output.GetOriginalFileName()

HTMLReportGenerator.IReportGenerator.ProcessLine    PROCEDURE(*LineFormatGrp lf, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment('',pExtendControlAttr)
    IF NOT SELF.SkipControl
      SELF.ProcessLine(lf)
    END  

HTMLReportGenerator.IReportGenerator.ProcessCheck   PROCEDURE(*CheckFormatGrp cf, STRING Text, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment(Text,pExtendControlAttr)
    IF NOT SELF.SkipControl
      SELF.ProcessHiddenControlSize(cf.Prompt.Pos,pExtendControlAttr)
      SELF.ProcessCheck(cf, Text)
    END  

HTMLReportGenerator.IReportGenerator.ProcessEllipse    PROCEDURE(*EllipseFormatGrp ef, STRING pExtendControlAttr)
  CODE
  !NOT SUPPORTED
  ! can't implement in HTML + CSS

HTMLReportGenerator.IReportGenerator.ProcessArc      PROCEDURE(*ArcFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
 !NOT SUPPORTED

HTMLReportGenerator.IReportGenerator.ProcessChord      PROCEDURE(*ChordFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
 !NOT SUPPORTED

HTMLReportGenerator.IReportGenerator.ProcessImage      PROCEDURE(*ImageFormatGrp imgf, STRING iName, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment('',pExtendControlAttr)
    IF NOT SELF.SkipControl
      SELF.ProcessHiddenControlSize(imgf.Pos,pExtendControlAttr)
      SELF.ProcessImage(imgf, iName)
    END  

HTMLReportGenerator.IReportGenerator.ProcessGroup      PROCEDURE(*GroupFormatGrp gfg, STRING Text, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment(Text,pExtendControlAttr)
    IF NOT SELF.SkipControl
      SELF.ProcessHiddenControlSize(gfg.Pos,pExtendControlAttr)
      SELF.ProcessGroup(gfg, Text)
    END  

HTMLReportGenerator.IReportGenerator.ProcessPie        PROCEDURE(SliceFormatQueue sfq, *PosGrp Pos, STRING pExtendControlAttr)
  CODE
  !NOT SUPPORTED

HTMLReportGenerator.IReportGenerator.ProcessRadio      PROCEDURE(*RadioFormatGrp rf, STRING Text, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment(Text,pExtendControlAttr)
    IF NOT SELF.SkipControl
      SELF.ProcessHiddenControlSize(rf.Prompt,pExtendControlAttr)
      SELF.ProcessRadio(rf, Text)
    END  

HTMLReportGenerator.IReportGenerator.ProcessRectangle    PROCEDURE(*RectFormatGrp rf, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment('',pExtendControlAttr)
    IF NOT SELF.SkipControl
    ! Can't display rounded edges in HTML 
      SELF.ProcessHiddenControlSize(rf.Pos,pExtendControlAttr)
      SELF.ProcessRect(rf, FALSE)
    END  
  
HTMLReportGenerator.IReportGenerator.ProcessString    PROCEDURE(*StringFormatGrp tf, STRING text, STRING pExtendControlAttr)
  CODE
    SELF.ProcessComment(Text,pExtendControlAttr)
    IF NOT SELF.SkipControl
      SELF.ProcessHiddenControlSize(tf,pExtendControlAttr)
      SELF.ProcessString(tf, text)
    END  

HTMLReportGenerator.IReportGenerator.ProcessText     PROCEDURE(TextFormatQueue TextFormatQ, STRING pExtendControlAttr)
idx     UNSIGNED,AUTO
  CODE
  SELF.ProcessComment ('',pExtendControlAttr)
  IF NOT SELF.SkipControl
     LOOP idx = 1 TO RECORDS (TextFormatQ)
       GET(TextFormatQ, idx)
       SELF.ProcessHiddenControlSize(TextFormatQ.Format,pExtendControlAttr)
       SELF.ProcessString (TextFormatQ.Format, TextFormatQ.Text)
     END
  END

HTMLReportGenerator.IReportGenerator.ProcessBand     PROCEDURE(STRING type, BYTE start)
  CODE
  !NOT SUPPORTED

HTMLReportGenerator.IReportGenerator.ProcessPolygon  PROCEDURE(PointQueue pPointQueue, *StyleGrp pStyleGrp, STRING pExtendControlAttr)
  CODE
  !NOT SUPPORTED

HTMLReportGenerator.IReportGenerator.SetResultQueue      PROCEDURE(OutputFileQueue pOutputFile)
 CODE
    SELF.OutputFileQueue &= pOutputFile
    FREE(SELF.OutputFileQueue)

HTMLReportGenerator.IReportGenerator.SupportResultQueue  PROCEDURE()
 CODE
 RETURN True

HTMLReportGenerator.IReportGenerator.SupportPageParsing  PROCEDURE()
 CODE
 RETURN True

GetAlignStyle  PROCEDURE(STRING text)
  CODE
    IF LEFT(text) = text
      RETURN(' LeftStyle')
    ELSIF RIGHT(text) = text
      RETURN(' RightStyle')
    ELSE
      RETURN(' CenterStyle')
    END
