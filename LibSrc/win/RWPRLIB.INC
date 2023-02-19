! ReportWriter Print Engine DLL Interface

 OMIT('_End_RwEngineDllMode_Set',_RwEngineDllMode_Set)
_RwEngineDllMode      EQUATE(1)
_RwEngineDllMode_Set  EQUATE(1)
_End_RwEngineDllMode_Set

 OMIT('__EndOfInclude__',_ReportEnginePresent_)
_ReportEnginePresent_ EQUATE(1)


!!! <summary>
!!! List of image formats that can be used with ReportEngine2.ExportImage
!!! </summary>
ImageFormat       ITEMIZE(1),PRE(ImageFormat)
Bmp                 EQUATE
Png                 EQUATE
Jpeg                EQUATE
Emf                 EQUATE
Wmf                 EQUATE
Gif                 EQUATE
Tiff                EQUATE
                  END

!!! <summary>
!!! List of Jpeg image quality usable in ReportEngine2.ExportPdf
!!! </summary>
PdfJpegImageQuality ITEMIZE(1),PRE(PdfJpegImageQuality)
Lowest              EQUATE ! resulting Jpeg image quality is the lowest (the resulting PDF file size is the smallest).
Low                 EQUATE ! resulting Jpeg image quality is low (the resulting PDF file size is small).
Medium              EQUATE ! resulting Jpeg image quality is medium (the resulting PDF file size is also medium).
High                EQUATE ! resulting Jpeg image quality is high (the resulting PDF file size is big).
Highest             EQUATE ! resulting Jpeg image quality is the highest (the resulting PDF file size is the biggest).
	END
	
! Main Print Engine Class
!!! <summary>
!!! The ReportEngine class allows you to control the Report Writer Engine
!!! </summary>
ReportEngine      CLASS,TYPE,MODULE('RWPRLIB'),DLL(_RwEngineDllMode)
!region members
!region For internal use
Handle              LONG
engine              &IReportEngine,PRIVATE
!endregion
!region Helper variables (set before entry to PrintHook)
ReportName          STRING(FILE:MaxFilePath)
Report              &WINDOW
View                &VIEW
PagesToPreview      LONG                    ! 0  = no preview, -1 = all pages
!endregion
!endregion

!!! <summary>
!!! If the report parameter exists change to its directory and load it 
!!! and if the report was loaded set activeReport to true
!!! </summary>
!!! <param name="fullPathToReport">fully qualified path and name of the report library (repxl file or txr file)</param>
!!! <param name="password">the password used to encrypt the txr file</param>
!!! <returns>true if report library exists and was successfully loaded, otherwise false</returns>
LoadReportLibrary    PROCEDURE(STRING fullPathToReport, <STRING password>),BOOL,PROC,VIRTUAL

!!! <summary>
!!! Adds the list of reports found in the current Library to the passed queue
!!! </summary>
!!! <param name="returnedList">a queue that has the names of all reports in the library added to it</returns>
GetReportList        PROCEDURE(PrintPreviewFileQueue returnedList)

!!! <summary>
!!! Print the report
!!! </summary>
!!! <param name="rptname">The name of the report.  This must be one of the names returned by GetReportList</param>
!!! <returns>true if report printed without errors</returns>
PrintReport         PROCEDURE(STRING rptname),BOOL,PROC,VIRTUAL

!!! <summary>
!!! Deprecated.  This method does nothing and is here simply for backwards compatibility
!!! </summary>
UnloadReportLibrary PROCEDURE(),VIRTUAL

!!! <summary>
!!! The SetVariable method should be called to set a variable’s value before calling the PrintReport method.
!!! </summary>
!!! <param name="varname">The name of the variable in the report</param>
!!! <param name="value">The value the variable should be set to</param>
!!! <param name="description">An optional description to display for the variable when displayed in the variable dialog</param>
!!! <param name="description">An optional flag to indicate if the variable should be displayed or not.  The default value is FALSE</param>
SetVariable         PROCEDURE(STRING varname,STRING value, <STRING description>, BOOL display = FALSE),VIRTUAL

!!! <summary>
!!! Enables/Disables the print preview dialog.  If numPages is 0 or if this method is not called, then the dialog is not displayed.
!!! </summary>
!!! <param name="numPages">
!!! If a repxl library is loaded, then any value other than 0 will cause the preview dialog to display.
!!! If a txr library is loaded, then numPages of < 0 causes the preview dialog to display all pages, 0 indicates no preview, any other
!!! value causes that number of pages to display (up to the number of pages in the report)
!!!</param>
SetPreview          PROCEDURE(SIGNED numPages=-1),VIRTUAL

!!! <summary>
!!! Specify. which printer will be used when PrintReport is called.  This will override previous calls to UsePrinterDialog
!!! </summary>
SetPrinter          PROCEDURE(<STRING printdef>),VIRTUAL

!region Overrides for the library defaults for retrieving records
!!! <summary>
!!! Sets a filter based on the received filterstring
!!! </summary>
!!! <param name="filter">If a repxl library has been opened, then fields defined in the 
!!! filterString        must be enclosed in brackets
!!! i.e. [PRO:COST]
!!! the filter expression can use logical operators: AND, OR, NOT, LIKE
!!! the use of LIKE follows the form; [PRO:DESCRIPTION] Like \'c%\'
!!! *note the single quotes are escaped as \'
!!! If the filter uses a "Parameter Field" it must be enclosed in brackets and must
!!! include the "Parameters." prefix before its name,
!!! for example: [Parameters.DiscountAmount]
!!! </param>
SetReportFilter     PROCEDURE(STRING report, STRING filter)

!!! <summary>
!!! Sets a new sort order for the report, it first removes any existing sort order
!!! </summary>
!!! <param name="order">order is a string containing a comma delimited list of fully qualified
!!! fieldnames. Each fieldname may have a plus or minus sign appended to it, to indicate
!!! either an ascending or descending sort order. For example "CUS:COUNTRY-,ORD:INVOICENUMBER+"
!!! if neither plus or minus sign is appended the field is sorted ascending
!!! </param>
!!! 
SetReportOrder      PROCEDURE(STRING report,STRING order)
!endregion
!region Methods that are only valid if a txr library has been loaded
!!! <summary>
!!! Set the range of pages to print.  If not called, then all pages are printed
!!! </summary>
SetPageRange        PROCEDURE(LONG frompage=-1,LONG topage=-1),VIRTUAL

!!! <summary>
!!! Set the number of copies to print.  If not called, then one copy is printed
!!! </summary>
SetNumberOfCopies   PROCEDURE(LONG numcopies),VIRTUAL

!region NextPageNumber
!!! <summary>
!!! Set the page number that the next page will have.
!!! </summary>
SetNextPageNumber   PROCEDURE(LONG pagenum),VIRTUAL
!!! <summary>
!!! Set the page number that the next page will have.
!!! </summary>
GetNextPageNumber   PROCEDURE(),LONG,VIRTUAL
!endregion
!region OpenMode
!!! <summary>
!!! Set how the report engine should open the data files
!!! </summary>
SetOpenMode         PROCEDURE(SIGNED mode)

!!! <summary>
!!! Get how the report engine will open the data files
!!! </summary>
GetOpenMode         PROCEDURE(),SIGNED
!endregion

!!! <summary>
!!! Print Preview and Run hook
!!! </summary>
PrintHook           PROCEDURE(),SIGNED,VIRTUAL
!region Helper functions (called by PrintHook)
Reset               PROCEDURE(),LONG,VIRTUAL ! returns records to process
Next                PROCEDURE(),BYTE,VIRTUAL ! returns Level:Notify when finished
PrintAction         PROCEDURE(),VIRTUAL      ! called after next
EndReport           PROCEDURE(),VIRTUAL      ! call to close report
!endregion
!region User file hook
AttachOpenFile      PROCEDURE(STRING label),*FILE,VIRTUAL
ResolveVariableFilename PROCEDURE(STRING vname,*STRING value),SIGNED,VIRTUAL
ReadReportLibrary   PROCEDURE(*CSTRING buffer,USHORT count),SHORT,VIRTUAL !returns amount read
!endregion
!endregion
!region Methods that are only valid if a repxl library has been loaded
!!! <summary>
!!! When PrintReport is called a dialog will be displayed to allow the user to select which printer to use.  This will override previous calls to SetPrinter
!!! </summary>
UsePrinterDialog    PROCEDURE()


!region Export methods
!!! <summary>
!!! Exports the active report to CSV format
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param>
!!! <param name="runIt">if true start process to open CSV</param>
!!! <returns>true if report was successfully exported</returns>
ExportCSV           PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports the active report to HTML format
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param>
!!! <param name="runIt">if true start process to open HTML</param>
!!! <returns>true if report was successfully exported</returns>
ExportHTML          PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports the active report to RTF format
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param>
!!! <param name="runIt">if true start process to open RTF</param>
!!! <returns>true if report was successfully exported</returns>
ExportRTF           PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports the active report to specified image format
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param> 
!!! <param name="imageFormat">long with one of the values from the ImageFormat itemize list</param>
!!! <param name="runIt"> if true start process to open Image in default viewer</param>
!!! <returns>true if report was successfully exported</returns>
ExportImage         PROCEDURE(STRING report, STRING exportPathName, LONG imageFormat, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports active report to the specified file path in MHT format. 
!!! *note: this method overwrites any files present at the specified path 
!!! that have the same file name without any warnings.
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension </param>
!!! <param name="runIt"> if true start process to open Mht </param>
!!! <returns>true if report was successfully exported</returns>
ExportMht           PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports active report to the specified file path in Text format. 
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param>
!!! <param name="runIt"> if true start process to open Text file</param>
!!! <returns>true if report was successfully exported</returns>
ExportText          PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports active report to the specified file path in XLS format. 
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param> 
!!! <param name="runIt">if true start process to open XLS file</param> 
!!! <returns>true if report was successfully exported</returns>
ExportXls           PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports active report to PDF to the specified file path
!!! optionally opens PDF into default  viewer
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param> 
!!! <param name="runIt">if true start process to open PDF</param>      
!!! <returns>true if report was successfully exported</returns>
ExportPdf           PROCEDURE(STRING report, STRING exportPathName, BOOL runIt = FALSE),BOOL,PROC

!!! <summary>
!!! Exports active report to PDF to the specified file path
!!! using the user specified options
!!! optionally opens PDF into default viewer
!!! </summary>
!!! <param name="report">The name of the report to export</param>
!!! <param name="exportPathName">full path and filename with extension</param> 
!!! <param name="compress">true to compress the resulting PDF file; otherwise, false. The default is true</param> 
!!! <param name="app">an Application name</param> 
!!! <param name="author">string: author</param> 
!!! <param name="keywords">string which contains keywords</param> 
!!! <param name="subject">string: subject</param> 
!!! <param name="title">string: title</param> 
!!! <param name="imageQuality">an Long from the PdfJpegImageQuality ITEMIZE to specify the quality and the size of images when they are exported to PDF format.  The
!!!                            default is PdfJpegImageQuality:Medium </param> 
!!! <param name="pageRange">ranges are separated by commas. For example: "1,3,6-12"</param>        
!!! <param name="runIt">if true start process to open PDF</param>      
!!! <returns>true if report was successfully exported</returns>
ExportPdf           PROCEDURE(STRING report, STRING exportPathName, BOOL compress = TRUE, <STRING app>, <STRING author>, |
			                  <STRING keywords>, <STRING subject>, <STRING title>, LONG imageQuality = PdfJpegImageQuality:Medium, <STRING pageRange>, BOOL runIt),BOOL,PROC
!endregion
!endregion
Construct           PROCEDURE()
Destruct            PROCEDURE(),VIRTUAL
             END

__EndOfInclude__



