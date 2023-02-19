#TEMPLATE(SVCommunicationSupport, 'Communication Support'),FAMILY('ABC','CW20')
#!
#EXTENSION(SVCommunicationGlobal, 'Communication Support - Global'),APPLICATION(SVSendEmailReport(SVCommunicationSupport)),HLP('~TPLExtensionGlobalCommunications.htm')
#PREPARE
  #DECLARE(%GlobalMessageBodyEncodings),MULTI
  #DECLARE(%GlobalMessageBodyEncodingDesc,%GlobalMessageBodyEncodings)
  #CALL(%GlobalMessageBodyEncodingLoad)
#ENDPREPARE
#DISPLAY
#DISPLAY(' Email Support'),AT(3,,187), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
#DISPLAY
#DISPLAY('This template has added a Procedure Extension to each Report')
#DISPLAY('This Procedure allows Reports to be emailed from the Previewer.')
#DISPLAY('By default the functionality is OFF and needs to be')
#DISPLAY(' activated on the Report Procedure Extension.')
#PROMPT('Disable support for Emailing Report from Previewer?',CHECK),%GlobalDisable,DEFAULT(%False),AT(10)
#ENABLE(NOT %GlobalDisable AND %GlobalUseABCClasess)
  #BUTTON('Email Global Settings'),AT(10,,175)
    #BOXED('Sender Settings')
      #PROMPT('Email Server:',EDIT),%GlobalSenderEmailServer,REQ,DEFAULT('Your SMTP server'),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderEmailServer))
      #PROMPT('Server Port:',EDIT),%GlobalSenderServerPort,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderServerPort))
      #PROMPT('Use SSL',CHECK),%GlobalSenderServerSSL,DEFAULT(%True)
      #PROMPT('Login User Name:',EDIT),%GlobalSenderServerUserName,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderServerUserName))
      #PROMPT('Login Password:',EDIT),%GlobalSenderServerPassword,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderServerPassword))
      #PROMPT('Sender Email:',EDIT),%GlobalSenderEmail,REQ,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderEmail))
    #ENDBOXED
    #DISPLAY('Use the prefix ! to use a variable.')
    #PROMPT('Message Body Encoding',FROM(%GlobalMessageBodyEncodings,,%GlobalMessageBodyEncodingDesc)),%GlobalMessageBodyEncoding,DEFAULT('')
#ENDBUTTON
#ENDENABLE
  #BUTTON('SMTP SMS Global Settings'),AT(10,,175)
    #BOXED('Sender Settings')
      #PROMPT('SMS Server:',EDIT),%GlobalSenderSMSServer,REQ,DEFAULT('smtp.gmail.com'),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderSMSServer))
      #PROMPT('Server Port:',EDIT),%GlobalSenderSMSServerPort,DEFAULT('587'),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderSMSServerPort))
      #PROMPT('Use SSL',CHECK),%GlobalSenderSMSServerSSL,DEFAULT(%True)
      #PROMPT('Login User Name:',EDIT),%GlobalSenderSMSServerUserName,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderSMSServerUserName))
      #PROMPT('Login Password:',EDIT),%GlobalSenderSMSServerPassword,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderSMSServerPassword))
      #PROMPT('Sender Email:',EDIT),%GlobalSenderSMSEmail,REQ,DEFAULT(''),WHENACCEPTED(%SVExpresionEditor(%GlobalSenderSMSEmail))
    #ENDBOXED
    #DISPLAY('Use the prefix ! to use a variable.')
  #ENDBUTTON
#!
#AT(%CustomGlobalDeclarations),WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess)
  #ADD(%CustomGlobalDeclarationIncludes,'ClaRunExt.INC' )
#ENDAT
#AT(%BeforeGlobalData),WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess)
  #ADD(%CustomGlobalData,'ClaTalk')
  #SET(%CustomGlobalDataDeclaration,'ClaRunExtClass')
#ENDAT
#!
#AT(%ProcedureDeclarationsInsideGlobalMap),WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess)
#INSERT(%AskEmailAddressDeclaration)
#ENDAT
#AT(%ProgramProcedures),WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess)
#INSERT(%AskEmailAddressImplementation)
#ENDAT
#!
#CODE(SVSendEmail,'Send Email Code'),REQ(SVCommunicationGlobal),DESCRIPTION('Send an Email')
#ATSTART
  #IF(NOT %EmailOverrideGlobalSettings)
     #CALL(%ResetSettingsFromGlobal)
  #ENDIF
#ENDAT
#SHEET
  #TAB('General')
    #INSERT(%CurrentEmailOptionsPrompts)
  #ENDTAB
  #TAB('Server')
    #INSERT(%CurrentEmailServerOptionsPrompts)
  #ENDTAB
#ENDSHEET
#!
#!
 #IF(%LocalEmailErrorCode)
     %LocalEmailErrorCode = 0
 #ENDIF
     !If you need to send attachments then assign the file names separated by semi-colons to the variable lAttachedFileNames%ActiveTemplateInstance
     !lAttachedFileNames%ActiveTemplateInstance =
     #INSERT(%SendEmailCode,%ActiveTemplateInstance)
     #INSERT(%SendEmailCodeFunctionCall,%ActiveTemplateInstance)
 #IF(%LocalEmailErrorCode)
     %LocalEmailErrorCode = lEmailError%ActiveTemplateInstance
 #ENDIF
 #IF(%LocalEmailErrorMessage = 'Message')
     IF lEmailError%ActiveTemplateInstance
        MESSAGE('Error on sending the Message|Error#='&lEmailError%ActiveTemplateInstance,'Email Message')
     ELSE
        MESSAGE('Message was sent successfully!','Email Message')
     END
 #ENDIF
 #IF(%LocalEmailErrorMessage = 'ErrorMessage')
     IF lEmailError%ActiveTemplateInstance
        MESSAGE('Error reported on sending the email|error#='&lEmailError%ActiveTemplateInstance,'Email Message')
     END
 #ENDIF
#!
#!
#AT(%LocalDataAfterClasses),DESCRIPTION('Send Email Code support variables')
#INSERT(%SendEmailVariablesDeclaration,%ActiveTemplateInstance)
#ENDAT
#!
#CONTROL(SVSendEmailButton,'Send Email Button'),DESCRIPTION('Send an Email when clicked'),FIRST,REQ(SVCommunicationGlobal)
  CONTROLS
    BUTTON('Send &EMail'),USE(?SendEMail)
  END
#!
#ATSTART
  #DECLARE(%SendEMailControl)
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #SET(%SendEMailControl,%Control)
  #ENDFOR
  #IF(NOT %EmailOverrideGlobalSettings)
     #CALL(%ResetSettingsFromGlobal)
  #ENDIF
#ENDAT
#!
#SHEET
  #TAB('General')
    #INSERT(%CurrentEmailOptionsPrompts)
  #ENDTAB
  #TAB('Server')
    #INSERT(%CurrentEmailServerOptionsPrompts)
  #ENDTAB
#ENDSHEET
#!
#AT(%LocalDataAfterClasses),DESCRIPTION('Email button support variables')
#INSERT(%SendEmailVariablesDeclaration,%ActiveTemplateInstance)
#ENDAT
#!
#AT(%ControlEventHandling, %SendEMailControl, 'Accepted'),PRIORITY(8000),DESCRIPTION('Email Button Action')
 #IF(%LocalEmailErrorCode)
     %LocalEmailErrorCode = 0
 #ENDIF
#PRIORITY(100),DESCRIPTION('Send email function - parameters assignment')
     !If you need to send attachments assign the file names separated by semi-colons to the variable lAttachedFileNames%ActiveTemplateInstance
     !lAttachedFileNames%ActiveTemplateInstance =
     #INSERT(%SendEmailCode,%ActiveTemplateInstance)
#PRIORITY(6000),DESCRIPTION('Send email function call')
     #INSERT(%SendEmailCodeFunctionCall,%ActiveTemplateInstance)
 #IF(%LocalEmailErrorMessage = 'Message')
     IF lEmailError%ActiveTemplateInstance
        MESSAGE('Error reported on sending the email|error#='&lEmailError%ActiveTemplateInstance,'Email Message')
     ELSE
        MESSAGE('Message was sent successfully','Email Message')
     END
 #ENDIF
 #IF(%LocalEmailErrorMessage = 'ErrorMessage')
     IF lEmailError%ActiveTemplateInstance
        MESSAGE('Error reported on sending the email|error#='&lEmailError%ActiveTemplateInstance,'Email Message')
     END
 #ENDIF
 #IF(%LocalEmailErrorCode)
     %LocalEmailErrorCode = lEmailError%ActiveTemplateInstance
 #ENDIF
#ENDAT
#!
#EXTENSION(SVSendEmailReport,'Send Report Output by Email'),PROCEDURE,REQ(SVCommunicationGlobal),DESCRIPTION('Send Report Output by Email ('&%Populate&CHOOSE(%PopulateInPreview,' - In Preview','')&')')
#RESTRICT
  #IF(UPPER(%ProcedureTemplate)='REPORT')
    #ACCEPT
  #ELSE
    #REJECT
  #ENDIF
#ENDRESTRICT
  #DISPLAY(' Email Report Output'),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
  #BOXED,SECTION
    #BOXED,WHERE(%GlobalDisable OR NOT %GlobalUseABCClasess),AT(0,0)
      #DISPLAY('This option has been disabled at the global')
      #DISPLAY('level and is not available.')
    #ENDBOXED
    #BOXED,WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess AND NOT %EnablePrintPreview),AT(0,0)
      #DISPLAY('This option has been disabled because the')
      #DISPLAY('Report requires that Preview is ON.')
    #ENDBOXED
    #BOXED,WHERE(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview),AT(0,0)
        #SHEET
          #TAB('General')
      #PROMPT('Populate on Report',DROP('Always|Conditional|Never|Only When Output[OnlyReportOutput]')),%Populate,DEFAULT('Never')
      #BOXED,SECTION
        #BOXED,WHERE(%Populate='Never' AND NOT %GlobalDisable AND %GlobalUseABCClasess),AT(0,0)
          #DISPLAY('This option will not be available for this report.')
        #ENDBOXED
        #BOXED,WHERE(%Populate<>'Never' AND NOT %GlobalDisable AND %GlobalUseABCClasess),AT(0,0)
          #ENABLE(%Populate='Conditional')
            #PROMPT('Condition:',EDIT),%PopulateCondition,WHENACCEPTED(%SVExpresionEditor(%PopulateCondition))
          #ENDENABLE
        #ENDBOXED
      #ENDBOXED
      #ENABLE(%Populate<>'Never')
            #INSERT(%CurrentEmailOptionsPrompts)
            #INSERT(%AskForInformationMainPrompts)
            #PROMPT('Delete the generated files after sending',CHECK),%RemoveAttachmentAfterSend,DEFAULT(%False),AT(10)
      #ENDENABLE
          #ENDTAB
          #TAB('Send From Previewer')
            #ENABLE(NOT %GlobalDisable)
                #DISPLAY('Requires the PDF Output is activated')
                #PROMPT('Enable Preview Send Menu',CHECK),%PopulateInPreview,DEFAULT(%False),AT(10)
                #ENABLE(%PopulateInPreview),CLEAR
                #PROMPT('Delete generated files after sending',CHECK),%RemoveAttachmentAfterSendPreview,DEFAULT(%False),AT(10)
                    #INSERT(%AskForInformationPrompts)
                #ENDENABLE
                #ENABLE(%PopulateInPreview)
                #INSERT(%CurrentEmailAskOptionsPrompts)
                #ENDENABLE
            #ENDENABLE
          #ENDTAB
          #TAB('Server')
            #INSERT(%CurrentEmailServerOptionsPrompts)
          #ENDTAB
        #ENDSHEET
    #ENDBOXED
  #ENDBOXED
#!
#!
#!
#ATSTART
  #!IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
    #IF(NOT %EmailOverrideGlobalSettings)
      #CALL(%ResetSettingsFromGlobal)
    #ENDIF
  #!ENDIF
#ENDAT
#!
#! Send Report by Email after the report is printed or saved.
#AT(%WindowManagerMethodDataSection,'ProcessResultFiles','(OutputFileQueue OutputFile),BYTE'),PRIORITY(5000),WHERE(%Populate<>'Never')
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %Populate<>'Never')
lOutputFileIdx          LONG,AUTO
#INSERT(%SendEmailVariablesDeclaration,%ActiveTemplateInstance)
#ENDIF
#ENDAT
#AT(%WindowManagerMethodCodeSection,'ProcessResultFiles','(OutputFileQueue OutputFile),BYTE'),DESCRIPTION('Send Email'),PRIORITY(5000),WHERE(%Populate<>'Never')
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %Populate<>'Never')
 #IF(%Populate='Conditional')
IF %PopulateCondition THEN
 #ENDIF
 #IF(%Populate='OnlyReportOutput')
IF NOT SELF.ReportTarget &= NULL THEN
 #ENDIF
 #IF(%LocalEmailErrorCode)
  %LocalEmailErrorCode = 0
 #ENDIF
  IF RECORDS(OutputFile)
     #IF(%AskForInformationMain)
     #INSERT(%AskForInformationCodeMain,%ActiveTemplateInstance)
     IF lEmailError%ActiveTemplateInstance
     #ENDIF
     LOOP lOutputFileIdx = 1 TO RECORDS(OutputFile)
          GET(OutputFile,lOutputFileIdx)
          IF lOutputFileIdx = 1
             lAttachedFileNames%ActiveTemplateInstance = CLIP(OutputFile.Filename)
          ELSE
             lAttachedFileNames%ActiveTemplateInstance = lAttachedFileNames%ActiveTemplateInstance &';'& CLIP(OutputFile.Filename)
          END
     END
#ENDIF
#PRIORITY(100),DESCRIPTION('Send email function - parameters assignment')
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %Populate<>'Never')
     #IF(%AskForInformationMain)
        #INSERT(%SendEmailCodeAsk,%ActiveTemplateInstance)
     #ELSE
        #INSERT(%SendEmailCode,%ActiveTemplateInstance)
     #ENDIF
#ENDIF
#PRIORITY(6000),DESCRIPTION('Send email function call')
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %Populate<>'Never')
     #INSERT(%SendEmailCodeFunctionCall,%ActiveTemplateInstance)
     #IF(%RemoveAttachmentAfterSend)
     LOOP lOutputFileIdx = 1 TO RECORDS(OutputFile)
          GET(OutputFile,lOutputFileIdx)
          REMOVE(CLIP(OutputFile.Filename))
     END
     #ENDIF
 #IF(%LocalEmailErrorCode)
     %LocalEmailErrorCode = lEmailError%ActiveTemplateInstance
 #ENDIF
 #IF(%LocalEmailErrorMessage = 'Message')
     IF lEmailError%ActiveTemplateInstance
        MESSAGE('Error reported on sending the email|error#='&lEmailError%ActiveTemplateInstance,'Email Message')
     ELSE
        MESSAGE('Message was sent successfully','Email Message')
     END
 #ENDIF
 #IF(%LocalEmailErrorMessage = 'ErrorMessage')
     IF lEmailError%ActiveTemplateInstance
        MESSAGE('Error reported on sending the email|error#='&lEmailError%ActiveTemplateInstance,'Email Message')
     END
 #ENDIF
 #IF(%AskForInformationMain)
    END
 #ENDIF
  END
#IF(%Populate='Conditional' OR %Populate='OnlyReportOutput')
END
#ENDIF
#ENDIF
#ENDAT
#!
#!Send report from the Previewer Menu
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8505)
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %PopulateInPreview)
Previewer.AllowSendReport = TRUE
#ENDIF
#ENDAT
#!
#AT(%PreviewerMethodDataSection,'OnProcessFiles','(),BYTE'),PRIORITY(5000)
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %PopulateInPreview)
lOutputFileIdx    LONG,AUTO
lOutputFileName   CSTRING(FILE:MaxFileName),AUTO
#INSERT(%SendEmailVariablesDeclaration,%ActiveTemplateInstance)
#ENDIF
#ENDAT
#!
#AT(%PreviewerManagerMethodCodeSection,'OnProcessFiles','(),BYTE'),DESCRIPTION('Set Output for Email'),PRIORITY(4000)
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %PopulateInPreview)
IF ~SELF.TargetSelector &=NULL
   SELF.TargetSelector.SetDefaultOutputGenerator('PDF')		!Use the PDF as the default target
END
#ENDIF
#ENDAT
#AT(%PreviewerManagerMethodCodeSection,'OnProcessFiles','(),BYTE')
#PRIORITY(100),DESCRIPTION('Send email function parameters assignment')
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %PopulateInPreview)
 #IF(%LocalAskEmailErrorCode)
  %LocalAskEmailErrorCode = 0
 #ENDIF
IF ReturnValue = Level:Benign AND ~SELF.TargetSelector &=NULL AND SELF.ReportTarget.SupportResultQueue()=True AND SELF.TargetSelector.GetDefaultItemIndex() > 0
   IF RECORDS(SELF.OutputFileQueue)
     #IF(%AskForInformation)
     #INSERT(%AskForInformationCode,%ActiveTemplateInstance)
     IF lEmailError%ActiveTemplateInstance
     #ENDIF
        LOOP lOutputFileIdx = 1 TO RECORDS(SELF.OutputFileQueue)
             GET(SELF.OutputFileQueue,lOutputFileIdx)
             IF lOutputFileIdx = 1
                lAttachedFileNames%ActiveTemplateInstance = CLIP(SELF.OutputFileQueue.Filename)
             ELSE
                lAttachedFileNames%ActiveTemplateInstance = lAttachedFileNames%ActiveTemplateInstance &';'& CLIP(SELF.OutputFileQueue.Filename)
             END
        END
#ENDIF
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %PopulateInPreview)
     #IF(%AskForInformation)
        #INSERT(%SendEmailCodeAsk,%ActiveTemplateInstance)
     #ELSE
        #INSERT(%SendEmailCode,%ActiveTemplateInstance)
     #ENDIF
#ENDIF
#PRIORITY(6000),DESCRIPTION('Send email function call')
#IF(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview AND %PopulateInPreview)
     #INSERT(%SendEmailCodeFunctionCall,%ActiveTemplateInstance)
     #IF(%RemoveAttachmentAfterSendPreview)
        IF ~SELF.TargetSelector &=NULL
           LOOP lOutputFileIdx = 1 TO RECORDS(SELF.OutputFileQueue)
                GET(SELF.OutputFileQueue,lOutputFileIdx)
                REMOVE(SELF.OutputFileQueue.Filename)
           END
        END
     #ENDIF
 #IF(%LocalAskEmailErrorCode)
        %LocalAskEmailErrorCode = lEmailError%ActiveTemplateInstance
 #ENDIF
 #IF(%LocalAskEmailErrorMessage = 'Message')
        IF lEmailError%ActiveTemplateInstance
           MESSAGE('Error on sending the Message|Error#='&lEmailError%ActiveTemplateInstance,'Email Message')
        ELSE
           MESSAGE('Message was sent successfully!','Email Message')
        END
 #ENDIF
 #IF(%LocalAskEmailErrorMessage = 'ErrorMessage')
        IF lEmailError%ActiveTemplateInstance
           MESSAGE('Error on sending the Message|Error#='&lEmailError%ActiveTemplateInstance,'Email Message')
        END
 #ENDIF
 #IF(%AskForInformation)
     END
 #ENDIF
   END
END
#ENDIF
#ENDAT
#!
#!
#CONTROL(SVSendSMSButton,'Send SMS Button'),DESCRIPTION('Send an SMS when clicked'),FIRST,REQ(SVCommunicationGlobal)
  CONTROLS
    BUTTON('Send SMS'),USE(?SendSMS)
  END
#!
#!
#ATSTART
  #SET(%EmailOverrideGlobalSettings, %False)
  #CALL(%ResetSMSSettingsFromGlobal)
  #DECLARE(%SendSMSControl)
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #SET(%SendSMSControl,%Control)
  #ENDFOR
#ENDAT
#!
#SHEET
  #TAB('General')
    #INSERT(%CurrentSMSOptionsPrompts)
  #ENDTAB
  #TAB('Server'),WHERE(%False)
    #INSERT(%CurrentEmailServerOptionsPrompts)
  #ENDTAB
#ENDSHEET
#!
#AT(%LocalDataAfterClasses),DESCRIPTION('SMS Button support variables')
#INSERT(%SendSMSVariablesDeclaration,%ActiveTemplateInstance)
#ENDAT
#!
#AT(%ControlEventHandling, %SendSMSControl, 'Accepted'),PRIORITY(6000)
#PRIORITY(100),DESCRIPTION('Send SMS function - parameters assignment')
 #IF(%LocalSMSErrorCode)
     %LocalSMSErrorCode = 0
 #ENDIF
     #INSERT(%AssigningAllSendSMSVariables,%ActiveTemplateInstance)
#PRIORITY(6000),DESCRIPTION('Send SMS function call')
     #INSERT(%SendSMSCode,%ActiveTemplateInstance)
 #IF(%LocalSMSErrorCode)
     %LocalSMSErrorCode = lSMSError%ActiveTemplateInstance
 #ENDIF
 #IF(%LocalSMSErrorCode = 'Message')
     IF lSMSError%ActiveTemplateInstance
        MESSAGE('Error on sending the Message|Error#='&lSMSError%ActiveTemplateInstance,'SMS Message')
     ELSE
        MESSAGE('Message was sent successfully!','SMS Message')
     END
 #ENDIF
 #IF(%LocalSMSErrorCode = 'ErrorMessage')
     IF lSMSError%ActiveTemplateInstance
        MESSAGE('Error on sending the Message|Error#='&lSMSError,'SMS Message')
     END
 #ENDIF
#ENDAT
#!
#!
#!
#CODE(SVSendSMS,'Send SMS Code'),REQ(SVCommunicationGlobal),DESCRIPTION('Send an SMS')
#ATSTART
  #SET(%EmailOverrideGlobalSettings, %False)
  #CALL(%ResetSMSSettingsFromGlobal)
#ENDAT
#SHEET
  #TAB('General')
    #INSERT(%CurrentSMSOptionsPrompts)
  #ENDTAB
  #TAB('Server'),WHERE(%False)
    #INSERT(%CurrentEmailServerOptionsPrompts)
  #ENDTAB
#ENDSHEET
#!
 #IF(%LocalSMSErrorCode)
     %LocalSMSErrorCode = 0
 #ENDIF
     #INSERT(%AssigningAllSendSMSVariables,%ActiveTemplateInstance)
     #INSERT(%SendSMSCode,%ActiveTemplateInstance)
 #IF(%LocalSMSErrorCode)
     %LocalSMSErrorCode = lSMSError%ActiveTemplateInstance
 #ENDIF
 #IF(%LocalSMSErrorCode = 'Message')
     IF lSMSError%ActiveTemplateInstance
        MESSAGE('Error on sending the Message|Error#='&lSMSError%ActiveTemplateInstance,'SMS Message')
     ELSE
        MESSAGE('Message was sent successfully!','SMS Message')
     END
 #ENDIF
 #IF(%LocalSMSErrorCode = 'ErrorMessage')
     IF lSMSError%ActiveTemplateInstance
        MESSAGE('Error on sending the Message|Error#='&lSMSError,'SMS Message')
     END
 #ENDIF
#!
#!
#AT(%LocalDataAfterClasses),DESCRIPTION('Send SMS Code support variables')
#INSERT(%SendSMSVariablesDeclaration,%ActiveTemplateInstance)
#ENDAT
#!
#!
#!
#GROUP(%CurrentEmailOptionsPrompts)
 #BOXED('Email Options')
   #PROMPT('Email Recipient:',OPTFIELD),%LocalRecipientEmail,REQ
   #PROMPT('Email Subject:',OPTFIELD),%LocalMessageSubject
   #PROMPT('Message Text:',EDIT),%LocalMessageText,WHENACCEPTED(%SVExpresionEditor(%LocalMessageText))
   #PROMPT('Message Html:',EDIT),%LocalMessageHtml,WHENACCEPTED(%SVExpresionEditor(%LocalMessageHtml))
   #PROMPT('Request read receipt:',OPTFIELD),%LocalReceiveRequest,DEFAULT('False')
   #PROMPT('CC List',OPTFIELD),%LocalMessageCCList,MULTI('Email addresses in the format "display name" <name@host>')
   #PROMPT('BCC List',OPTFIELD),%LocalMessageBCCList,MULTI('Email addresses in the format "display name" <name@host>')
 #ENDBOXED
 #DISPLAY('Use the prefix ! to use a variable.')
 #PROMPT('Show internal error message?',CHECK),%LocalInternalErrorMessage,DEFAULT(%True),AT(10)
 #PROMPT('After Message Sent:',DROP('Show Error Message[ErrorMessage]|Show Message[Message]|Silent[Silent]')),%LocalEmailErrorMessage,DEFAULT('ErrorMessage')
 #PROMPT('Variable to assign error code:',OPTFIELD),%LocalEmailErrorCode,DEFAULT('')
#!
#GROUP(%CurrentEmailAskOptionsPrompts)
 #BOXED('Email Options')
   #PROMPT('Email Recipient:',OPTFIELD),%LocalAskRecipientEmail,REQ
   #PROMPT('Email Subject:',OPTFIELD),%LocalAskMessageSubject
   #PROMPT('Message Text:',EDIT),%LocalAskMessageText,WHENACCEPTED(%SVExpresionEditor(%LocalAskMessageText))
   #PROMPT('Message Html:',EDIT),%LocalAskMessageHtml,WHENACCEPTED(%SVExpresionEditor(%LocalAskMessageHtml))
   #PROMPT('Request read receipt:',OPTFIELD),%LocalAskReceiveRequest,DEFAULT('False')
   #PROMPT('CC List',OPTFIELD),%LocalAskMessageCCList,MULTI('Email addresses in the format "display name" <name@host>')
   #PROMPT('BCC List',OPTFIELD),%LocalAskMessageBCCList,MULTI('Email addresses in the format "display name" <name@host>')
 #ENDBOXED
 #DISPLAY('Use the prefix ! to use a variable.')
 #PROMPT('Show internal error message?',CHECK),%LocalAskInternalErrorMessage,DEFAULT(%True),AT(10)
 #PROMPT('After Message Send:',DROP('Show Error Message[ErrorMessage]|Show Message[Message]|Silent[Silent]')),%LocalAskEmailErrorMessage,DEFAULT('ErrorMessage')
 #PROMPT('Variable to assign error code:',OPTFIELD),%LocalAskEmailErrorCode,DEFAULT('')
#!
#GROUP(%CurrentSMSOptionsPrompts)
 #BOXED('SMS Options using SMTP')
   #PROMPT('Recipient Phone Number:',OPTFIELD),%LocalRecipientPhoneNumber,REQ
   #PROMPT('Carrier''s SMTP Format:',OPTFIELD),%LocalRecipientSMTPFormat,REQ
   #PROMPT('SMS Subject:',OPTFIELD),%LocalMessageSubject
   #PROMPT('Message Text:',EDIT),%LocalMessageText,WHENACCEPTED(%SVExpresionEditor(%LocalMessageText))
 #ENDBOXED
 #DISPLAY('Use the prefix ! to use a variable.')
 #PROMPT('Show internal error message?',CHECK),%LocalInternalErrorMessage,DEFAULT(%True),AT(10)
 #PROMPT('After Message Send:',DROP('Show Error Message[ErrorMessage]|Show Message[Message]|Silent[Silent]')),%LocalSMSErrorMessage,DEFAULT('ErrorMessage')
 #PROMPT('Variable to assign error code:',OPTFIELD),%LocalSMSErrorCode,DEFAULT('')
#!
#GROUP(%CurrentEmailServerOptionsPrompts)
 #BOXED('Server Settings')
  #PROMPT('Override global settings?',CHECK),%EmailOverrideGlobalSettings,DEFAULT(%False),AT(10)
  #ENABLE(%EmailOverrideGlobalSettings)
      #BUTTON('Email Global Settings'),AT(10,,175)
        #BOXED('Sender Settings')
          #PROMPT('Email Server:',EXPR),%LocalSenderEmailServer,REQ,DEFAULT('smtp.gmail.com')
          #PROMPT('Server Port:',EXPR),%LocalSenderServerPort,DEFAULT('')
          #PROMPT('Use SSL',CHECK),%LocalSenderServerSSL,DEFAULT(%True)
          #PROMPT('Login User Name:',EXPR),%LocalSenderServerUserName,DEFAULT('')
          #PROMPT('Login Password:',EXPR),%LocalSenderServerPassword,DEFAULT('')
          #PROMPT('Sender Email:',EXPR),%LocalSenderEmail,REQ,DEFAULT('')
        #ENDBOXED
        #DISPLAY('Use the prefix ! to use a variable.')
      #ENDBUTTON
  #ENDENABLE
 #ENDBOXED
#!
#GROUP(%AskForInformationPrompts)
 #PROMPT('Ask for Information',CHECK),%AskForInformation,DEFAULT(%True),AT(10)
 #ENABLE(%AskForInformation)
  #BOXED('Popup Email Info Window')
   #PROMPT('Ask for Recipient Information',CHECK),%AskForRecipient,DEFAULT(%True),AT(10)
   #ENABLE(%AskForInformation AND %AskForRecipient),CLEAR
     #PROMPT('Allow any Recipient Email',CHECK),%AskForAnyRecipient,DEFAULT(%True),AT(10)
     #PROMPT('Recipient Lookup Procedure',PROCEDURE),%AskForRecipientLookupProcedure
   #ENDENABLE
   #DISPLAY('The procedure must not have any parameters')
   #DISPLAY('and must return one string with the email address')
   #PROMPT('Ask for Subject',CHECK),%AskForEmailSubject,DEFAULT(%True),AT(10)
   #PROMPT('Ask for Email Text',CHECK),%AskForEmailText,DEFAULT(%True),AT(10)
  #ENDBOXED
 #ENDENABLE
#!
#GROUP(%AskForInformationMainPrompts)
 #PROMPT('Ask for Information',CHECK),%AskForInformationMain,DEFAULT(%False),AT(10)
 #ENABLE(%AskForInformationMain)
  #BOXED('Popup Email Info Window')
   #PROMPT('Ask for Recipient Information',CHECK),%AskForRecipientMain,DEFAULT(%True),AT(10)
   #ENABLE(%AskForInformationMain AND %AskForRecipientMain),CLEAR
     #PROMPT('Allow any Recipient Email',CHECK),%AskForAnyRecipientMain,DEFAULT(%True),AT(10)
     #PROMPT('Recipient Lookup Procedure',PROCEDURE),%AskForRecipientLookupProcedureMain
   #ENDENABLE
   #DISPLAY('The procedure must not have any parameters')
   #DISPLAY('and must return one string with the email address')
   #PROMPT('Ask for Subject',CHECK),%AskForEmailSubjectMain,DEFAULT(%True),AT(10)
   #PROMPT('Ask for Email Text',CHECK),%AskForEmailTextMain,DEFAULT(%True),AT(10)
  #ENDBOXED
 #ENDENABLE
#!
#GROUP(%AskForInformationCode,%subfix),AUTO
#IF(%AskForInformation)
 #DECLARE(%AskForInformationType,LONG)
 #SET(%AskForInformationType,0)
 #IF(%AskForRecipient)
     #IF(%AskForAnyRecipient)
        #SET(%AskForInformationType,%AskForInformationType + 1)
     #ENDIF
     #IF(%AskForRecipientLookupProcedure)
        #SET(%AskForInformationType,%AskForInformationType + 2)
     #ENDIF
 #ENDIF
 #IF(%AskForEmailSubject)
    #SET(%AskForInformationType,%AskForInformationType + 4)
 #ENDIF
 #IF(%AskForEmailText)
    #SET(%AskForInformationType,%AskForInformationType + 8)
 #ENDIF
 #IF(%AskForInformationType > 0)
#INSERT(%AssigningCurrentEmailAskVariables,%subfix)
lEmailError%subfix = SVAskEmailAddress(lRecipientEmail%subfix, lMessageSubject%subfix, lMessageText%subfix, %AskForInformationType%(CHOOSE(%AskForRecipientLookupProcedure,', '&%AskForRecipientLookupProcedure,',')))
 #ENDIF
#ENDIF
#!
#GROUP(%AskForInformationCodeMain,%subfix),AUTO
#IF(%AskForInformationMain)
 #DECLARE(%AskForInformationType,LONG)
 #SET(%AskForInformationType,0)
 #IF(%AskForRecipientMain)
     #IF(%AskForAnyRecipientMain)
        #SET(%AskForInformationType,%AskForInformationType + 1)
     #ENDIF
     #IF(%AskForRecipientLookupProcedureMain)
        #SET(%AskForInformationType,%AskForInformationType + 2)
     #ENDIF
 #ENDIF
 #IF(%AskForEmailSubjectMain)
    #SET(%AskForInformationType,%AskForInformationType + 4)
 #ENDIF
 #IF(%AskForEmailTextMain)
    #SET(%AskForInformationType,%AskForInformationType + 8)
 #ENDIF
 #IF(%AskForInformationType > 0)
#INSERT(%AssigningCurrentEmailVariables,%subfix)
lEmailError%subfix = SVAskEmailAddress(lRecipientEmail%subfix, lMessageSubject%subfix, lMessageText%subfix, %AskForInformationType%(CHOOSE(%AskForRecipientLookupProcedureMain,', '&%AskForRecipientLookupProcedureMain,',')))
 #ENDIF
#ENDIF
#!
#GROUP(%ResetSettingsFromGlobal)
#SET(%LocalSenderEmailServer,%GlobalSenderEmailServer)
#SET(%LocalSenderServerPort,%GlobalSenderServerPort)
#SET(%LocalSenderServerUserName,%GlobalSenderServerUserName)
#SET(%LocalSenderServerPassword,%GlobalSenderServerPassword)
#SET(%LocalSenderEmail,%GlobalSenderEmail)
#SET(%LocalSenderServerSSL,%GlobalSenderServerSSL)
#!
#GROUP(%ResetSMSSettingsFromGlobal)
#SET(%LocalSenderEmailServer,%GlobalSenderSMSServer)
#SET(%LocalSenderServerPort,%GlobalSenderSMSServerPort)
#SET(%LocalSenderServerUserName,%GlobalSenderSMSServerUserName)
#SET(%LocalSenderServerPassword,%GlobalSenderSMSServerPassword)
#SET(%LocalSenderEmail,%GlobalSenderSMSEmail)
#SET(%LocalSenderServerSSL,%GlobalSenderSMSServerSSL)
#!
#GROUP(%AssigningAllSendSMSVariables,%subfix)
#!
#INSERT(%AssigningGlobalVariablesSMS,%subfix)
#INSERT(%AssigningCurrentSMSVariables,%subfix)
#!
#GROUP(%AssigningGlobalVariables,%subfix)
lSenderEmailServer%subfix      = %(%SVStripPling(%LocalSenderEmailServer))
lSenderServerPort%subfix       = %(%SVStripPling(%LocalSenderServerPort))
lSenderServerSSL%subfix        = %(%SVBool(%LocalSenderServerSSL))
lShowInternalMessage%subfix    = %(%SVBool(%LocalInternalErrorMessage))
lSenderServerUserName%subfix   = %(%SVStripPling(%LocalSenderServerUserName))
lSenderServerPassword%subfix   = %(%SVStripPling(%LocalSenderServerPassword))
lSenderEmail%subfix            = %(%SVStripPling(%LocalSenderEmail))
lMessageBodyEncoding%subfix    = '%'GlobalMessageBodyEncoding'
#!
#GROUP(%AssigningGlobalVariablesSMS,%subfix)
lSenderEmailServer%subfix      = %(%SVStripPling(%LocalSenderEmailServer))
lSenderServerPort%subfix       = %(%SVStripPling(%LocalSenderServerPort))
lSenderServerSSL%subfix        = %(%SVBool(%LocalSenderServerSSL))
lShowInternalMessage%subfix    = %(%SVBool(%LocalInternalErrorMessage))
lSenderServerUserName%subfix   = %(%SVStripPling(%LocalSenderServerUserName))
lSenderServerPassword%subfix   = %(%SVStripPling(%LocalSenderServerPassword))
lSenderEmail%subfix            = %(%SVStripPling(%LocalSenderEmail))
lSMSAddress%subfix             = %(%SVStripPling(%LocalRecipientSMTPFormat))
#!lMessageBodyEncoding%subfix    = '%'GlobalMessageBodyEncoding'
#!
#GROUP(%AssigningCurrentEmailVariables,%subfix)
lRecipientEmail%subfix         = %(%SVStripPling(%LocalRecipientEmail))
lMessageSubject%subfix         = %(%SVStripPling(%LocalMessageSubject))
lMessageText%subfix            = %(%SVStripPling(%LocalMessageText))
lMessageHtml%subfix            = %(%SVStripPling(%LocalMessageHtml))
lReceiveRequest%subfix         = %(%SVStripPling(%LocalReceiveRequest))
 #FOR(%LocalMessageCCList)
IF lCCList%subfix
  lCCList%subfix = lCCList%subfix & ','
END
lCCList%subfix = lCCList%subfix & %(%SVStripPling(%LocalMessageCCList))
 #ENDFOR
 #FOR(%LocalMessageBCCList)
IF lBCCList%subfix
  lBCCList%subfix = lBCCList%subfix & ','
END
lBCCList%subfix = lBCCList%subfix & %(%SVStripPling(%LocalMessageBCCList))
 #ENDFOR
#!
#GROUP(%AssigningCurrentEmailAskVariables,%subfix)
lRecipientEmail%subfix         = %(%SVStripPling(%LocalAskRecipientEmail))
lMessageSubject%subfix         = %(%SVStripPling(%LocalAskMessageSubject))
lMessageText%subfix            = %(%SVStripPling(%LocalAskMessageText))
lMessageHtml%subfix            = %(%SVStripPling(%LocalAskMessageHtml))
lReceiveRequest%subfix         = %(%SVStripPling(%LocalAskReceiveRequest))
 #FOR(%LocalAskMessageCCList)
IF lCCList%subfix
  lCCList%subfix = lCCList%subfix & ','
END
lCCList%subfix = lCCList%subfix & %(%SVStripPling(%LocalAskMessageCCList))
 #ENDFOR
 #FOR(%LocalAskMessageBCCList)
IF lBCCList%subfix
  lBCCList%subfix = lBCCList%subfix & ','
END
lBCCList%subfix = lBCCList%subfix & %(%SVStripPling(%LocalAskMessageBCCList))
 #ENDFOR
#!
#GROUP(%AssigningCurrentSMSVariables,%subfix)
lRecipientPhoneNumber%subfix   = %(%SVStripPling(%LocalRecipientPhoneNumber))
lMessageSubject%subfix         = %(%SVStripPling(%LocalMessageSubject))
lMessageText%subfix            = %(%SVStripPling(%LocalMessageText))
#!
#GROUP(%SendSMSVariablesDeclaration,%subfix)
!Variables used by the SendSMS function
#INSERT(%SendServerCommonVariablesDeclaration,%subfix)
lSMSAddress%subfix             CSTRING(250)
lRecipientPhoneNumber%subfix   CSTRING(250)
lMessageSubject%subfix         CSTRING(250)
lMessageText%subfix            CSTRING(64000)
lSMSError%subfix               LONG
#!lMessageBodyEncoding%subfix    CSTRING(50)
#!
#GROUP(%SendEmailVariablesDeclaration,%subfix)
!Variables used by the SendEmail function
#INSERT(%SendServerCommonVariablesDeclaration,%subfix)
lRecipientEmail%subfix         CSTRING(250)
lReceiveRequest%subfix         LONG(0)
lMessageBodyEncoding%subfix    CSTRING(50)
lCCList%subfix                 CSTRING(64000)
lBCCList%subfix                CSTRING(64000)
#INSERT(%SendEmailCommonVariablesDeclaration,%subfix)
#!
#GROUP(%SendServerCommonVariablesDeclaration,%subfix)
lSenderEmail%subfix            CSTRING(250)
lSenderEmailServer%subfix      CSTRING(250)
lSenderServerPort%subfix       LONG(80)
lSenderServerUserName%subfix   CSTRING(250)
lSenderServerPassword%subfix   CSTRING(250)
lSenderServerSSL%subfix        LONG(0)
lShowInternalMessage%subfix    LONG(0)
lEmailErrorText%subfix         STRING(64000)
#!
#GROUP(%SendEmailCommonVariablesDeclaration,%subfix)
lMessageSubject%subfix         CSTRING(250)
lMessageText%subfix            CSTRING(64000)
lMessageHtml%subfix            CSTRING(64000)
lEmbeddedImageFileNames%subfix CSTRING(64000)
lAttachedFileNames%subfix      CSTRING(64000)
lEmailError%subfix             LONG(0)
#!
#!
#GROUP(%SendEmailCode,%subfix)
#INSERT(%SendEmailCodeAll,%subfix,%true)
#!
#GROUP(%SendEmailCodeAsk,%subfix)
#INSERT(%SendEmailCodeAll,%subfix,%false)
#!
#GROUP(%SendEmailCodeAll,%subfix,%assingLocal)
#INSERT(%AssigningGlobalVariables,%subfix)
#IF(%assingLocal)
#INSERT(%AssigningCurrentEmailVariables,%subfix)
#ENDIF
#!
#GROUP(%SendEmailCodeFunctionCall,%subfix)
lEmailError%subfix = ClaTalk.SendMail(lSenderEmailServer%subfix, |
                     lSenderServerUserName%subfix, |
                     lSenderServerPassword%subfix, |
                     lSenderServerPort%subfix, |
                     lSenderServerSSL%subfix, |
                     lShowInternalMessage%subfix, |
                     lReceiveRequest%subfix, |
                     lMessageBodyEncoding%subfix, |
                     lSenderEmail%subfix, |
                     lRecipientEmail%subfix, |
                     lSenderEmail%subfix, |
                     lMessageSubject%subfix, |
                     lMessageHtml%subfix, |
                     lembeddedImageFileNames%subfix, |
                     lMessageText%subfix, |
                     lattachedFileNames%subfix, |
                     lCCList%subfix, |
                     lBCCList%subfix, |
                     lEmailErrorText%subfix)
#!
#!
#GROUP(%SendSMSCode,%subfix)
lSMSError%subfix = ClaTalk.SendSMS(lSenderEmailServer%subfix, |
                           lSenderServerUserName%subfix, |
                           lSenderServerPassword%subfix, |
                           lSenderServerPort%subfix, |
                           lSenderServerSSL%subfix, |
                           lShowInternalMessage%subfix, |
                           lSenderEmail%subfix, |
                           lRecipientPhoneNumber%subfix, |
                           lSMSAddress%subfix, |
                           lMessageSubject%subfix, |
                           lMessageText%subfix, |
                           lEmailErrorText%subfix)
#!
#!
#GROUP(%AskEmailAddressDeclaration)
 SVLookupEmailAddressType(),STRING,TYPE
 SVAskEmailAddress(*CSTRING pRecipientEmail,*CSTRING pMessageSubject, *CSTRING pMessageText,LONG pLookupAddress, <SVLookupEmailAddressType pAddressLookupAddressProcedure>),BYTE
 SVAskEmailAddress(*CSTRING pRecipientEmail,*CSTRING pMessageSubject, *CSTRING pMessageText,LONG pLookupAddress, <SVLookupEmailAddressType pAddressLookupAddressProcedure>,STRING pWindowText, STRING pPROMPTRecipient, STRING pPROMPTSubject, STRING pPROMPTMessage, STRING pPROMPTSend, STRING pPROMPTClose),BYTE
#!
#GROUP(%AskEmailAddressImplementation)
SVAskEmailAddress PROCEDURE(*CSTRING pRecipientEmail,*CSTRING pMessageSubject, *CSTRING pMessageText,LONG pLookupAddress, <SVLookupEmailAddressType pAddressLookupAddressProcedure>)
 CODE
    IF OMITTED(5)
       RETURN SVAskEmailAddress (pRecipientEmail,pMessageSubject, pMessageText, pLookupAddress, ,'', '', '', '', '', '')
    ELSE
       RETURN SVAskEmailAddress (pRecipientEmail,pMessageSubject, pMessageText, pLookupAddress, pAddressLookupAddressProcedure,'', '', '', '', '', '')
    END

SVAskEmailAddress PROCEDURE(*CSTRING pRecipientEmail,*CSTRING pMessageSubject, *CSTRING pMessageText,LONG pLookupAddress, <SVLookupEmailAddressType  pAddressLookupAddressProcedure>,STRING pWindowText, STRING pPROMPTRecipient, STRING pPROMPTSubject, STRING pPROMPTMessage, STRING pPROMPTSend, STRING pPROMPTClose)
retVal BYTE
SVAskEmailAddressWindow WINDOW('Email Composing'),AT(,,363,102),CENTER,MODAL,GRAY, FONT('Segoe UI',9,,FONT:regular)
        PROMPT('Recipient email:'),AT(2,6,62),USE(?PROMPTRecipientEmail),RIGHT
        ENTRY(@s250),AT(68,4,265),USE(pRecipientEmail,, ?ENTRYRecipientEmail)
        BUTTON('...'),AT(336,4,19,13),USE(?LookupAddress),MSG('Lookup Address')
        PROMPT('Subject:'),AT(2,23,62),USE(?PROMPTSubject),RIGHT
        ENTRY(@s250),AT(68,22,265),USE(pMessageSubject,, ?ENTRYSubject)
        PROMPT('Message Text:'),AT(2,40,62),USE(?PROMPTMessage),RIGHT
        TEXT,AT(68,40,265,34),USE(pMessageText,, ?TEXTMessage),VSCROLL
        BUTTON('&Send'),AT(134,81,101,14),USE(?SendButton)
        BUTTON('&Cancel'),AT(280,81,46,14),USE(?CloseButton)
    END
askEmail   EQUATE(00000001b)!1
askLookup  EQUATE(00000010b)!2
askEmailLookup EQUATE(00000011b)!3
askSubject EQUATE(00000100b)!4
askMsgText EQUATE(00001000b)!8
 CODE
 IF pLookupAddress = 0
    RETURN true
 END
 retVal = FALSE
 OPEN(SVAskEmailAddressWindow)
 ACCEPT
    CASE EVENT()
    OF EVENT:OpenWindow
       IF CLIP(pWindowText)
          SVAskEmailAddressWindow{PROP:Text} = pWindowText
          IF CLIP(pPROMPTRecipient)
             ?PROMPTRecipientEmail{PROP:Text} = pPROMPTRecipient
          END
          IF CLIP(pPROMPTSubject)
             ?PROMPTSubject{PROP:Text} = pPROMPTSubject
          END
          IF CLIP(pPROMPTMessage)
             ?PROMPTMessage{PROP:Text} = pPROMPTMessage
          END
          IF CLIP(pPROMPTSend)
             ?SendButton{PROP:Text} = pPROMPTSend
          END
          IF CLIP(pPROMPTClose)
             ?CloseButton{PROP:Text} = pPROMPTClose
          END
       END

       IF NOT BAND(pLookupAddress,askEmail)
          ?ENTRYRecipientEmail{PROP:READONLY} = true
       END
       IF NOT BAND(pLookupAddress,askLookup) !OR OMITTED(5)
          HIDE(?LookupAddress)
          SVAskEmailAddressWindow{PROP:Width} = SVAskEmailAddressWindow{PROP:Width} - 22
       ELSE
          UNHIDE(?LookupAddress)
       END
       IF NOT BAND(pLookupAddress,askSubject)
          HIDE(?PROMPTSubject)
          HIDE(?ENTRYSubject)
          SVAskEmailAddressWindow{PROP:Height} = SVAskEmailAddressWindow{PROP:Height} - 18
          ?PROMPTMessage{PROP:Ypos} = ?PROMPTMessage{PROP:Ypos} - 18
          ?TEXTMessage{PROP:Ypos} = ?TEXTMessage{PROP:Ypos} - 18
          ?SendButton{PROP:Ypos} = ?SendButton{PROP:Ypos} - 18
          ?CloseButton{PROP:Ypos} = ?CloseButton{PROP:Ypos} - 18
       END
       IF NOT BAND(pLookupAddress,askMsgText)
          HIDE(?PROMPTMessage)
          HIDE(?TEXTMessage)
          SVAskEmailAddressWindow{PROP:Height} = SVAskEmailAddressWindow{PROP:Height} - 41
          ?SendButton{PROP:Ypos} = ?SendButton{PROP:Ypos} - 41
          ?CloseButton{PROP:Ypos} = ?CloseButton{PROP:Ypos} - 41
       END
    OF EVENT:Accepted
       CASE FIELD()
       OF ?LookupAddress
          IF BAND(pLookupAddress,askLookup) AND NOT OMITTED(5)
             pRecipientEmail = pAddressLookupAddressProcedure()
             DISPLAY()
             IF BAND(pLookupAddress,askSubject)
                SELECT(?ENTRYSubject)
             ELSE
                IF BAND(pLookupAddress,askMsgText)
                   SELECT(?ENTRYRecipientEmail)
                ELSE
                   SELECT(?SendButton)
                END
             END
          END
       OF ?SendButton
          retVal = TRUE
          CLOSE(SVAskEmailAddressWindow)
       OF ?CloseButton
          CLOSE(SVAskEmailAddressWindow)
       END
    END
 END
 RETURN retVal
#!
#GROUP(%GlobalMessageBodyEncodingLoad)
#ADD(%GlobalMessageBodyEncodings,'')
#SET(%GlobalMessageBodyEncodingDesc,'Default')
#ADD(%GlobalMessageBodyEncodings,'ASMO-708')
#SET(%GlobalMessageBodyEncodingDesc,'Arabic (ASMO 708)')
#ADD(%GlobalMessageBodyEncodings,'DOS-720')
#SET(%GlobalMessageBodyEncodingDesc,'Arabic (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-6')
#SET(%GlobalMessageBodyEncodingDesc,'Arabic (ISO)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-arabic')
#SET(%GlobalMessageBodyEncodingDesc,'Arabic (Mac)')
#ADD(%GlobalMessageBodyEncodings,'windows-1256')
#SET(%GlobalMessageBodyEncodingDesc,'Arabic (Windows)')
#ADD(%GlobalMessageBodyEncodings,'ibm775')
#SET(%GlobalMessageBodyEncodingDesc,'Baltic (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-4')
#SET(%GlobalMessageBodyEncodingDesc,'Baltic (ISO)')
#ADD(%GlobalMessageBodyEncodings,'windows-1257')
#SET(%GlobalMessageBodyEncodingDesc,'Baltic (Windows)')
#ADD(%GlobalMessageBodyEncodings,'ibm852')
#SET(%GlobalMessageBodyEncodingDesc,'Central European (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-2')
#SET(%GlobalMessageBodyEncodingDesc,'Central European (ISO)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-ce')
#SET(%GlobalMessageBodyEncodingDesc,'Central European (Mac)')
#ADD(%GlobalMessageBodyEncodings,'windows-1250')
#SET(%GlobalMessageBodyEncodingDesc,'Central European (Windows)')
#ADD(%GlobalMessageBodyEncodings,'EUC-CN')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Simplified (EUC)')
#ADD(%GlobalMessageBodyEncodings,'gb2312')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Simplified (GB2312)')
#ADD(%GlobalMessageBodyEncodings,'hz-gb-2312')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Simplified (HZ)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-chinesesimp')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Simplified (Mac)')
#ADD(%GlobalMessageBodyEncodings,'big5')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Traditional (Big5)')
#ADD(%GlobalMessageBodyEncodings,'x-Chinese-CNS')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Traditional (CNS)')
#ADD(%GlobalMessageBodyEncodings,'x-Chinese-Eten')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Traditional (Eten)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-chinesetrad')
#SET(%GlobalMessageBodyEncodingDesc,'Chinese Traditional (Mac)')
#ADD(%GlobalMessageBodyEncodings,'cp866')
#SET(%GlobalMessageBodyEncodingDesc,'Cyrillic (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-5')
#SET(%GlobalMessageBodyEncodingDesc,'Cyrillic (ISO)')
#ADD(%GlobalMessageBodyEncodings,'koi8-r')
#SET(%GlobalMessageBodyEncodingDesc,'Cyrillic (KOI8-R)')
#ADD(%GlobalMessageBodyEncodings,'koi8-u')
#SET(%GlobalMessageBodyEncodingDesc,'Cyrillic (KOI8-U)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-cyrillic')
#SET(%GlobalMessageBodyEncodingDesc,'Cyrillic (Mac)')
#ADD(%GlobalMessageBodyEncodings,'windows-1251')
#SET(%GlobalMessageBodyEncodingDesc,'Cyrillic (Windows)')
#ADD(%GlobalMessageBodyEncodings,'x-Europa')
#SET(%GlobalMessageBodyEncodingDesc,'Europa')
#ADD(%GlobalMessageBodyEncodings,'x-IA5-German')
#SET(%GlobalMessageBodyEncodingDesc,'German (IA5)')
#ADD(%GlobalMessageBodyEncodings,'ibm737')
#SET(%GlobalMessageBodyEncodingDesc,'Greek (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-7')
#SET(%GlobalMessageBodyEncodingDesc,'Greek (ISO)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-greek')
#SET(%GlobalMessageBodyEncodingDesc,'Greek (Mac)')
#ADD(%GlobalMessageBodyEncodings,'windows-1253')
#SET(%GlobalMessageBodyEncodingDesc,'Greek (Windows)')
#ADD(%GlobalMessageBodyEncodings,'ibm869')
#SET(%GlobalMessageBodyEncodingDesc,'Greek, Modern (DOS)')
#ADD(%GlobalMessageBodyEncodings,'DOS-862')
#SET(%GlobalMessageBodyEncodingDesc,'Hebrew (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-8-i')
#SET(%GlobalMessageBodyEncodingDesc,'Hebrew (ISO-Logical)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-8')
#SET(%GlobalMessageBodyEncodingDesc,'Hebrew (ISO-Visual)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-hebrew')
#SET(%GlobalMessageBodyEncodingDesc,'Hebrew (Mac)')
#ADD(%GlobalMessageBodyEncodings,'windows-1255')
#SET(%GlobalMessageBodyEncodingDesc,'Hebrew (Windows)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Arabic')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Arabic)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-CyrillicRussian')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Cyrillic Russian)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-CyrillicSerbianBulgarian')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Cyrillic Serbian-Bulgarian)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-DenmarkNorway')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Denmark-Norway)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-denmarknorway-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Denmark-Norway-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-FinlandSweden')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Finland-Sweden)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-finlandsweden-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Finland-Sweden-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-finlandsweden-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Finland-Sweden-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-france-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (France-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Germany')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Germany)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-germany-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Germany-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-GreekModern')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Greek Modern)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Greek')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Greek)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Hebrew')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Hebrew)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Icelandic')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Icelandic)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-icelandic-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Icelandic-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-international-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (International-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Italy')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Italy)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-italy-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Italy-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-JapaneseAndKana')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Japanese and Japanese Katakana)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-JapaneseAndJapaneseLatin')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Japanese and Japanese-Latin)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-JapaneseAndUSCanada')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Japanese and US-Canada)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-JapaneseKatakana')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Japanese katakana)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-KoreanAndKoreanExtended')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Korean and Korean Extended)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-KoreanExtended')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Korean Extended)')
#ADD(%GlobalMessageBodyEncodings,'CP870')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Multilingual Latin-2)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-SimplifiedChinese')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Simplified Chinese)')
#ADD(%GlobalMessageBodyEncodings,'X-EBCDIC-Spain')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Spain)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-spain-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Spain-Euro)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Thai')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Thai)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-TraditionalChinese')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Traditional Chinese)')
#ADD(%GlobalMessageBodyEncodings,'CP1026')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Turkish Latin-5)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-Turkish')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (Turkish)')
#ADD(%GlobalMessageBodyEncodings,'x-EBCDIC-UK')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (UK)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-uk-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (UK-Euro)')
#ADD(%GlobalMessageBodyEncodings,'ebcdic-cp-us')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (US-Canada)')
#ADD(%GlobalMessageBodyEncodings,'x-ebcdic-cp-us-euro')
#SET(%GlobalMessageBodyEncodingDesc,'IBM EBCDIC (US-Canada-Euro)')
#ADD(%GlobalMessageBodyEncodings,'ibm861')
#SET(%GlobalMessageBodyEncodingDesc,'Icelandic (DOS)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-icelandic')
#SET(%GlobalMessageBodyEncodingDesc,'Icelandic (Mac)')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-as')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Assamese')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-be')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Bengali')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-de')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Devanagari')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-gu')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Gujarathi')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-ka')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Kannada')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-ma')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Malayalam')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-or')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Oriya')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-pa')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Panjabi')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-ta')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Tamil')
#ADD(%GlobalMessageBodyEncodings,'x-iscii-te')
#SET(%GlobalMessageBodyEncodingDesc,'ISCII Telugu')
#ADD(%GlobalMessageBodyEncodings,'euc-jp')
#SET(%GlobalMessageBodyEncodingDesc,'Japanese (EUC)')
#ADD(%GlobalMessageBodyEncodings,'iso-2022-jp')
#SET(%GlobalMessageBodyEncodingDesc,'Japanese (JIS)')
#ADD(%GlobalMessageBodyEncodings,'iso-2022-jp')
#SET(%GlobalMessageBodyEncodingDesc,'Japanese (JIS-Allow 1 byte Kana - SO/SI)')
#ADD(%GlobalMessageBodyEncodings,'csISO2022JP')
#SET(%GlobalMessageBodyEncodingDesc,'Japanese (JIS-Allow 1 byte Kana)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-japanese')
#SET(%GlobalMessageBodyEncodingDesc,'Japanese (Mac)')
#ADD(%GlobalMessageBodyEncodings,'shift_jis')
#SET(%GlobalMessageBodyEncodingDesc,'Japanese (Shift-JIS)')
#ADD(%GlobalMessageBodyEncodings,'ks_c_5601-1987')
#SET(%GlobalMessageBodyEncodingDesc,'Korean')
#ADD(%GlobalMessageBodyEncodings,'euc-kr')
#SET(%GlobalMessageBodyEncodingDesc,'Korean (EUC)')
#ADD(%GlobalMessageBodyEncodings,'iso-2022-kr')
#SET(%GlobalMessageBodyEncodingDesc,'Korean (ISO)')
#ADD(%GlobalMessageBodyEncodings,'Johab')
#SET(%GlobalMessageBodyEncodingDesc,'Korean (Johab)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-korean')
#SET(%GlobalMessageBodyEncodingDesc,'Korean (Mac)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-3')
#SET(%GlobalMessageBodyEncodingDesc,'Latin 3 (ISO)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-15')
#SET(%GlobalMessageBodyEncodingDesc,'Latin 9 (ISO)')
#ADD(%GlobalMessageBodyEncodings,'x-IA5-Norwegian')
#SET(%GlobalMessageBodyEncodingDesc,'Norwegian (IA5)')
#ADD(%GlobalMessageBodyEncodings,'IBM437')
#SET(%GlobalMessageBodyEncodingDesc,'OEM United States')
#ADD(%GlobalMessageBodyEncodings,'x-IA5-Swedish')
#SET(%GlobalMessageBodyEncodingDesc,'Swedish (IA5)')
#ADD(%GlobalMessageBodyEncodings,'windows-874')
#SET(%GlobalMessageBodyEncodingDesc,'Thai (Windows)')
#ADD(%GlobalMessageBodyEncodings,'ibm857')
#SET(%GlobalMessageBodyEncodingDesc,'Turkish (DOS)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-9')
#SET(%GlobalMessageBodyEncodingDesc,'Turkish (ISO)')
#ADD(%GlobalMessageBodyEncodings,'x-mac-turkish')
#SET(%GlobalMessageBodyEncodingDesc,'Turkish (Mac)')
#ADD(%GlobalMessageBodyEncodings,'windows-1254')
#SET(%GlobalMessageBodyEncodingDesc,'Turkish (Windows)')
#ADD(%GlobalMessageBodyEncodings,'unicode')
#SET(%GlobalMessageBodyEncodingDesc,'Unicode')
#ADD(%GlobalMessageBodyEncodings,'unicodeFFFE')
#SET(%GlobalMessageBodyEncodingDesc,'Unicode (Big-Endian)')
#ADD(%GlobalMessageBodyEncodings,'utf-7')
#SET(%GlobalMessageBodyEncodingDesc,'Unicode (UTF-7)')
#ADD(%GlobalMessageBodyEncodings,'utf-8')
#SET(%GlobalMessageBodyEncodingDesc,'Unicode (UTF-8)')
#ADD(%GlobalMessageBodyEncodings,'us-ascii')
#SET(%GlobalMessageBodyEncodingDesc,'US-ASCII')
#ADD(%GlobalMessageBodyEncodings,'windows-1258')
#SET(%GlobalMessageBodyEncodingDesc,'Vietnamese (Windows)')
#ADD(%GlobalMessageBodyEncodings,'ibm850')
#SET(%GlobalMessageBodyEncodingDesc,'Western European (DOS)')
#ADD(%GlobalMessageBodyEncodings,'x-IA5')
#SET(%GlobalMessageBodyEncodingDesc,'Western European (IA5)')
#ADD(%GlobalMessageBodyEncodings,'iso-8859-1')
#SET(%GlobalMessageBodyEncodingDesc,'Western European (ISO)')
#ADD(%GlobalMessageBodyEncodings,'macintosh')
#SET(%GlobalMessageBodyEncodingDesc,'Western European (Mac)')
#ADD(%GlobalMessageBodyEncodings,'Windows-1252')
#SET(%GlobalMessageBodyEncodingDesc,'Western European (Windows)')
#!
#INCLUDE('SVFnGrp.TPW')