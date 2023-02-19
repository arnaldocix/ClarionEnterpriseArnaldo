                    MEMBER()

  INCLUDE('ClaMail.INC'),ONCE
  INCLUDE('ClaRunExt.INC'),ONCE
  INCLUDE('abutil.inc'),ONCE

CreateEmailList     PROCEDURE(EmailAddressList inList, SystemStringClass outStr)
i                     LONG,AUTO
recs                  LONG,AUTO
needQuote             BOOL,AUTO
  CODE
  outStr.Clean()
  recs = RECORDS(inList)
  LOOP i = 1 TO recs
    IF i <> 1
      outStr.Append(', ')
    END
    needQuote = FALSE
    GET(inList, i)
    IF inList.DisplayName
      IF inList.DisplayName[1] <> '"'
        outStr.Append('"')
        needQuote = TRUE
      END
      outStr.Append(CLIP(inList.DisplayName))
      IF needQuote
        outStr.Append('" ')
      ELSE
        outStr.Append(' ')
      END
    END
    needQuote = FALSE
    IF inList.DisplayName AND inList.emailAddress[1] <> '<'
      outStr.Append('<')
      needQuote = TRUE
    END
    outStr.Append(CLIP(inList.emailAddress))
    IF needQuote
      outStr.Append('>')
    END
  END

!region SMSMailClient
SMSMailClient.Construct   PROCEDURE()
  CODE
  SELF.claTalk &= NEW ClaRunExtClass()
  SELF.host &= NEW SystemStringClass()
  SELF.user &= NEW SystemStringClass()
  SELF.pwd &= NEW SystemStringClass()
  SELF.from &= NEW SystemStringClass()

SMSMailClient.Destruct    PROCEDURE()
  CODE
  DISPOSE(SELF.claTalk)
  DISPOSE(SELF.host)
  DISPOSE(SELF.user)
  DISPOSE(SELF.pwd)
  DISPOSE(SELF.from)

SMSMailClient.Setup   PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)
  CODE
  SELF.host.Str(SMTPServerHost)
  SELF.user.Str(SMTPLoginUserName)
  SELF.pwd.Str(SMTPLoginPassword)
  SELF.ssl = SMTPEnableSsl
  IF NOT OMITTED(SMTPPort)
    SELF.port = SMTPPort
  END

SMSMailClient.SetErrorReporting   PROCEDURE(BOOL showErrorMessage)
  CODE
  SELF.errorReporting = showErrorMessage
!endregion
!region SMSClient
SMSClient.Init  PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)
  CODE
  IF OMITTED(SMTPPort)
    PARENT.SetUp(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl)
  ELSE
    PARENT.SetUp(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl, SMTPPort)
  END

SMSClient.SendSMS   PROCEDURE(STRING toPhoneNumber, STRING toCarrier, STRING subject, STRING textMessage, <*STRING errMessage>)
  CODE
  RETURN SELF.claTalk.SendSMS(SELF.host.Str(), SELF.user.Str(), SELF.pwd.Str(), SELF.port, SELF.ssl, |
    SELF.errorReporting, SELF.from.Str(), toPhoneNumber, toCarrier, subject, textMessage, errMessage)
!endregion
!region MailClient
!region Initialization
MailClient.Construct PROCEDURE()
  CODE
  SELF.encoding &= NEW SystemStringClass()
  SELF.replyTo &= NEW SystemStringClass()

MailClient.Destruct  PROCEDURE()
  CODE
  DISPOSE(SELF.encoding)
  DISPOSE(SELF.replyTo)

MailClient.Init  PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)
  CODE
  IF OMITTED(SMTPPort)
    PARENT.SetUp(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl)
  ELSE
    PARENT.SetUp(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl, SMTPPort)
  END
  SELF.from.Str(fromAddress)
  SELF.ssl = SMTPEnableSsl

MailClient.SetEncoding   PROCEDURE(STRING encoding)
  CODE
  SELF.encoding.Str(encoding)

MailClient.SetReceivedRequest    PROCEDURE(BOOL wantReceivedNotice)
  CODE
  SELF.wantAReceivedNotice = wantReceivedNotice

MailClient.SetReplyTo    PROCEDURE(STRING replyAddress)
  CODE
  SELF.replyTo.Str(replyAddress)
!endregion

MailClient.SendHTMLMail  PROCEDURE(STRING toAddress, STRING subject, STRING htmlMessage, <STRING embeddedImageFileNames>, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>)
images                      SystemStringClass
attachments                 SystemStringClass
cc                          SystemStringClass
bcc                         SystemStringClass
  CODE
  IF NOT OMITTED(embeddedImageFileNames)
    images.str(embeddedImageFileNames)
  END
  IF NOT OMITTED(attachedFileNames)
    attachments.str(attachedFileNames)
  END
  IF NOT OMITTED(ccList)
    CreateEmailList(ccList, cc)
  END
  IF NOT OMITTED(bccList)
    CreateEmailList(bccList, bcc)
  END
  RETURN SELF.claTalk.SendMail(SELF.host.Str(), SELF.user.Str(), SELF.pwd.Str(), SELF.port, SELF.ssl, |
    SELF.errorReporting, SELF.wantAReceivedNotice, SELF.encoding.str(), |
    SELF.from.Str(), toAddress, SELF.replyTo.str(), CLIP(subject), CLIP(htmlMessage), |
    images.str(), '',  attachments.str(), cc.Str(), bcc.Str(), errMessage)


MailClient.SendMail  PROCEDURE(STRING toAddress, STRING subject, STRING message, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>)
attachments             SystemStringClass
cc                      SystemStringClass
bcc                     SystemStringClass
  CODE
  IF NOT OMITTED(attachedFileNames)
    attachments.str(attachedFileNames)
  END
  IF NOT OMITTED(ccList)
    CreateEmailList(ccList, cc)
  END
  IF NOT OMITTED(bccList)
    CreateEmailList(bccList, bcc)
  END
  RETURN SELF.claTalk.SendMail(SELF.host.Str(), SELF.user.Str(), SELF.pwd.Str(), SELF.port, SELF.ssl, |
    SELF.errorReporting, SELF.wantAReceivedNotice, SELF.encoding.str(), |
    SELF.from.Str(), toAddress, SELF.replyTo.str(), CLIP(subject), '', |
    '', CLIP(message), attachments.str(), cc.str(), bcc.str(), errMessage)
!endregion

!region exported functions
!region SMS
MakeSMSClient       PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)
ret &SMSClient
  CODE
  ret &= NEW SMSClient
  IF OMITTED(SMTPPort)
    ret.Init(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl)
  ELSE
    ret.Init(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl, SMTPPort)
  END
  RETURN ADDRESS(ret)

SendSMS             PROCEDURE(LONG SMSClientHandle, STRING toPhoneNumber, STRING  toCarrier, STRING  subject, STRING  textMessage, <*STRING errMessage>)
client &SMSClient
  CODE
  IF NOT SMSClientHandle
    RETURN 1
  END
  client &= (SMSClientHandle)
  RETURN client.SendSMS(toPhoneNumber, toCarrier, subject, textMessage, errMessage)

FreeSMSClient       PROCEDURE(LONG SMSClientHandle)
client &SMSClient
  CODE
  IF SMSClientHandle
    client &= (SMSClientHandle)
    DISPOSE(client)
  END
!endregion
!region Email
MakeMailClient           PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)
ret &MailClient
  CODE
  ret &= NEW MailClient
  IF OMITTED(SMTPPort)
    ret.Init(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl)
  ELSE
    ret.Init(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, fromAddress, SMTPEnableSsl, SMTPPort)
  END
  RETURN ADDRESS(ret)

SetEncoding         PROCEDURE(LONG MailClientHandle, STRING encoding)
client &MailClient
  CODE
  IF MailClientHandle
    client &= (MailClientHandle)
    client.SetEncoding(encoding)
  END

SetReceivedRequest  PROCEDURE(LONG MailClientHandle, BOOL wantReceivedNotice)
client &MailClient
  CODE
  IF MailClientHandle
    client &= (MailClientHandle)
    client.SetReceivedRequest(wantReceivedNotice)
  END

SetReplyTo          PROCEDURE(LONG MailClientHandle, STRING replyAddress)
client &MailClient
  CODE
  IF MailClientHandle
    client &= (MailClientHandle)
    client.SetReplyTo(replyAddress)
  END

SendHTMLMail        PROCEDURE(LONG MailClientHandle, STRING toAddress, STRING subject, STRING htmlMessage, <STRING embeddedImageFileNames>, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>)
client &MailClient
  CODE
  IF NOT MailClientHandle
    RETURN 1
  END
  client &= (MailClientHandle)
  RETURN client.SendHTMLMail(toAddress, subject, htmlMessage, embeddedImageFileNames, attachedFileNames, ccList, bccList, errMessage)

SendMail              PROCEDURE(LONG MailClientHandle, STRING toAddress, STRING subject, STRING message, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>)
client &MailClient
  CODE
  IF NOT MailClientHandle
    RETURN 1
  END
  client &= (MailClientHandle)
  RETURN client.SendMail(toAddress, subject, message, attachedFileNames, ccList, bccList, errMessage)

FreeMailClient      PROCEDURE(LONG MailClientHandle)
client &MailClient
  CODE
  IF MailClientHandle
    client &= (MailClientHandle)
    DISPOSE(client)
  END
!endregion

SetErrorReporting   PROCEDURE(LONG SMSMailClientHandle, BOOL showErrorMessage)
client &SMSMailClient
  CODE
  IF SMSMailClientHandle
    client &= (SMSMailClientHandle)
    client.SetErrorReporting(showErrorMessage)
  END

!endregion
