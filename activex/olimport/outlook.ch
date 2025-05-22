//////////////////////////////////////////////////////////////////////
//
//  OUTLOOK.CH
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//  Remarks:
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Automatically generated Header file.
//
// Program ID:     outlook.application
//
// Creation date:  24.08.2004
//
// Creation Tool:  CreateHeader.exe
//                 Copyright (c) Alaska Software. All rights reserved.
//
///////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef OUTLOOK_APPLICATION_HAEDER_DAEMON
#define OUTLOOK_APPLICATION_HAEDER_DAEMON
//OlActionCopyLike
#DEFINE olReply                                                                              0
#DEFINE olReplyAll                                                                           1
#DEFINE olForward                                                                            2
#DEFINE olReplyFolder                                                                        3
#DEFINE olRespond                                                                            4
//OlActionReplyStyle
#DEFINE olOmitOriginalText                                                                   0
#DEFINE olEmbedOriginalItem                                                                  1
#DEFINE olIncludeOriginalText                                                                2
#DEFINE olIndentOriginalText                                                                 3
#DEFINE olLinkOriginalItem                                                                   4
#DEFINE olUserPreference                                                                     5
#DEFINE olReplyTickOriginalText                                                           1000
//OlActionResponseStyle
#DEFINE olOpen                                                                               0
#DEFINE olSend                                                                               1
#DEFINE olPrompt                                                                             2
//OlActionShowOn
#DEFINE olDontShow                                                                           0
#DEFINE olMenu                                                                               1
#DEFINE olMenuAndToolbar                                                                     2
//OlAttachmentType
#DEFINE olByValue                                                                            1
#DEFINE olByReference                                                                        4
#DEFINE olEmbeddeditem                                                                       5
#DEFINE olOLE                                                                                6
//OlBusyStatus
#DEFINE olFree                                                                               0
#DEFINE olTentative                                                                          1
#DEFINE olBusy                                                                               2
#DEFINE olOutOfOffice                                                                        3
//OlDaysOfWeek
#DEFINE olSunday                                                                             1
#DEFINE olMonday                                                                             2
#DEFINE olTuesday                                                                            4
#DEFINE olWednesday                                                                          8
#DEFINE olThursday                                                                          16
#DEFINE olFriday                                                                            32
#DEFINE olSaturday                                                                          64
//OlDefaultFolders
#DEFINE olFolderDeletedItems                                                                 3
#DEFINE olFolderOutbox                                                                       4
#DEFINE olFolderSentMail                                                                     5
#DEFINE olFolderInbox                                                                        6
#DEFINE olFolderCalendar                                                                     9
#DEFINE olFolderContacts                                                                    10
#DEFINE olFolderJournal                                                                     11
#DEFINE olFolderNotes                                                                       12
#DEFINE olFolderTasks                                                                       13
#DEFINE olFolderDrafts                                                                      16
//OlDisplayType
#DEFINE olUser                                                                               0
#DEFINE olDistList                                                                           1
#DEFINE olForum                                                                              2
#DEFINE olAgent                                                                              3
#DEFINE olOrganization                                                                       4
#DEFINE olPrivateDistList                                                                    5
#DEFINE olRemoteUser                                                                         6
//OlEditorType
#DEFINE olEditorText                                                                         1
#DEFINE olEditorHTML                                                                         2
#DEFINE olEditorRTF                                                                          3
#DEFINE olEditorWord                                                                         4
//OlFlagStatus
#DEFINE olNoFlag                                                                             0
#DEFINE olFlagComplete                                                                       1
#DEFINE olFlagMarked                                                                         2
//OlFolderDisplayMode
#DEFINE olFolderDisplayNormal                                                                0
#DEFINE olFolderDisplayFolderOnly                                                            1
#DEFINE olFolderDisplayNoNavigation                                                          2
//OlFormRegistry
#DEFINE olDefaultRegistry                                                                    0
#DEFINE olPersonalRegistry                                                                   2
#DEFINE olFolderRegistry                                                                     3
#DEFINE olOrganizationRegistry                                                               4
//OlGender
#DEFINE olUnspecified                                                                        0
#DEFINE olFemale                                                                             1
#DEFINE olMale                                                                               2
//OlImportance
#DEFINE olImportanceLow                                                                      0
#DEFINE olImportanceNormal                                                                   1
#DEFINE olImportanceHigh                                                                     2
//OlInspectorClose
#DEFINE olSave                                                                               0
#DEFINE olDiscard                                                                            1
#DEFINE olPromptForSave                                                                      2
//OlItemType
#DEFINE olMailItem                                                                           0
#DEFINE olAppointmentItem                                                                    1
#DEFINE olContactItem                                                                        2
#DEFINE olTaskItem                                                                           3
#DEFINE olJournalItem                                                                        4
#DEFINE olNoteItem                                                                           5
#DEFINE olPostItem                                                                           6
#DEFINE olDistributionListItem                                                               7
//OlJournalRecipientType
#DEFINE olAssociatedContact                                                                  1
//OlMailingAddress
#DEFINE olNone                                                                               0
#DEFINE olHome                                                                               1
#DEFINE olBusiness                                                                           2
#DEFINE olOther                                                                              3
//OlMailRecipientType
#DEFINE olOriginator                                                                         0
#DEFINE olTo                                                                                 1
#DEFINE olCC                                                                                 2
#DEFINE olBCC                                                                                3
//OlMeetingRecipientType
#DEFINE olOrganizer                                                                          0
#DEFINE olRequired                                                                           1
#DEFINE olOptional                                                                           2
#DEFINE olResource                                                                           3
//OlMeetingResponse
#DEFINE olMeetingTentative                                                                   2
#DEFINE olMeetingAccepted                                                                    3
#DEFINE olMeetingDeclined                                                                    4
//OlMeetingStatus
#DEFINE olNonMeeting                                                                         0
#DEFINE olMeeting                                                                            1
#DEFINE olMeetingReceived                                                                    3
#DEFINE olMeetingCanceled                                                                    5
//OlNetMeetingType
#DEFINE olNetMeeting                                                                         0
#DEFINE olNetShow                                                                            1
#DEFINE olChat                                                                               2
//OlNoteColor
#DEFINE olBlue                                                                               0
#DEFINE olGreen                                                                              1
#DEFINE olPink                                                                               2
#DEFINE olYellow                                                                             3
#DEFINE olWhite                                                                              4
//OlObjectClass
#DEFINE olApplication                                                                        0
#DEFINE olNamespace                                                                          1
#DEFINE olFolder                                                                             2
#DEFINE olRecipient                                                                          4
#DEFINE olAttachment                                                                         5
#DEFINE olAddressList                                                                        7
#DEFINE olAddressEntry                                                                       8
#DEFINE olFolders                                                                           15
#DEFINE olItems                                                                             16
#DEFINE olRecipients                                                                        17
#DEFINE olAttachments                                                                       18
#DEFINE olAddressLists                                                                      20
#DEFINE olAddressEntries                                                                    21
#DEFINE olAppointment                                                                       26
#DEFINE olMeetingRequest                                                                    53
#DEFINE olMeetingCancellation                                                               54
#DEFINE olMeetingResponseNegative                                                           55
#DEFINE olMeetingResponsePositive                                                           56
#DEFINE olMeetingResponseTentative                                                          57
#DEFINE olRecurrencePattern                                                                 28
#DEFINE olExceptions                                                                        29
#DEFINE olException                                                                         30
#DEFINE olAction                                                                            32
#DEFINE olActions                                                                           33
#DEFINE olExplorer                                                                          34
#DEFINE olInspector                                                                         35
#DEFINE olPages                                                                             36
#DEFINE olFormDescription                                                                   37
#DEFINE olUserProperties                                                                    38
#DEFINE olUserProperty                                                                      39
#DEFINE olContact                                                                           40
#DEFINE olDocument                                                                          41
#DEFINE olJournal                                                                           42
#DEFINE olMail                                                                              43
#DEFINE olNote                                                                              44
#DEFINE olPost                                                                              45
#DEFINE olReport                                                                            46
#DEFINE olRemote                                                                            47
#DEFINE olTask                                                                              48
#DEFINE olTaskRequest                                                                       49
#DEFINE olTaskRequestUpdate                                                                 50
#DEFINE olTaskRequestAccept                                                                 51
#DEFINE olTaskRequestDecline                                                                52
#DEFINE olExplorers                                                                         60
#DEFINE olInspectors                                                                        61
#DEFINE olPanes                                                                             62
#DEFINE olOutlookBarPane                                                                    63
#DEFINE olOutlookBarStorage                                                                 64
#DEFINE olOutlookBarGroups                                                                  65
#DEFINE olOutlookBarGroup                                                                   66
#DEFINE olOutlookBarShortcuts                                                               67
#DEFINE olOutlookBarShortcut                                                                68
#DEFINE olDistributionList                                                                  69
#DEFINE olPropertyPageSite                                                                  70
#DEFINE olPropertyPages                                                                     71
#DEFINE olSyncObject                                                                        72
#DEFINE olSyncObjects                                                                       73
#DEFINE olSelection                                                                         74
#DEFINE olLink                                                                              75
#DEFINE olLinks                                                                             76
//OlOutlookBarViewType
#DEFINE olLargeIcon                                                                          0
#DEFINE olSmallIcon                                                                          1
//OlPane
#DEFINE olOutlookBar                                                                         1
#DEFINE olFolderList                                                                         2
#DEFINE olPreview                                                                            3
//OlRecurrenceState
#DEFINE olApptNotRecurring                                                                   0
#DEFINE olApptMaster                                                                         1
#DEFINE olApptOccurrence                                                                     2
#DEFINE olApptException                                                                      3
//OlRecurrenceType
#DEFINE olRecursDaily                                                                        0
#DEFINE olRecursWeekly                                                                       1
#DEFINE olRecursMonthly                                                                      2
#DEFINE olRecursMonthNth                                                                     3
#DEFINE olRecursYearly                                                                       5
#DEFINE olRecursYearNth                                                                      6
//OlRemoteStatus
#DEFINE olRemoteStatusNone                                                                   0
#DEFINE olUnMarked                                                                           1
#DEFINE olMarkedForDownload                                                                  2
#DEFINE olMarkedForCopy                                                                      3
#DEFINE olMarkedForDelete                                                                    4
//OlResponseStatus
#DEFINE olResponseNone                                                                       0
#DEFINE olResponseOrganized                                                                  1
#DEFINE olResponseTentative                                                                  2
#DEFINE olResponseAccepted                                                                   3
#DEFINE olResponseDeclined                                                                   4
#DEFINE olResponseNotResponded                                                               5
//OlSaveAsType
#DEFINE olTXT                                                                                0
#DEFINE olRTF                                                                                1
#DEFINE olTemplate                                                                           2
#DEFINE olMSG                                                                                3
#DEFINE olDoc                                                                                4
#DEFINE olHTML                                                                               5
#DEFINE olVCard                                                                              6
#DEFINE olVCal                                                                               7
//OlSensitivity
#DEFINE olNormal                                                                             0
#DEFINE olPersonal                                                                           1
#DEFINE olPrivate                                                                            2
#DEFINE olConfidential                                                                       3
//OlSortOrder
#DEFINE olSortNone                                                                           0
#DEFINE olAscending                                                                          1
#DEFINE olDescending                                                                         2
//OlTaskDelegationState
#DEFINE olTaskNotDelegated                                                                   0
#DEFINE olTaskDelegationUnknown                                                              1
#DEFINE olTaskDelegationAccepted                                                             2
#DEFINE olTaskDelegationDeclined                                                             3
//OlTaskOwnership
#DEFINE olNewTask                                                                            0
#DEFINE olDelegatedTask                                                                      1
#DEFINE olOwnTask                                                                            2
//OlTaskRecipientType
#DEFINE olUpdate                                                                             2
#DEFINE olFinalStatus                                                                        3
//OlTaskResponse
#DEFINE olTaskSimple                                                                         0
#DEFINE olTaskAssign                                                                         1
#DEFINE olTaskAccept                                                                         2
#DEFINE olTaskDecline                                                                        3
//OlTaskStatus
#DEFINE olTaskNotStarted                                                                     0
#DEFINE olTaskInProgress                                                                     1
#DEFINE olTaskComplete                                                                       2
#DEFINE olTaskWaiting                                                                        3
#DEFINE olTaskDeferred                                                                       4
//OlTrackingStatus
#DEFINE olTrackingNone                                                                       0
#DEFINE olTrackingDelivered                                                                  1
#DEFINE olTrackingNotDelivered                                                               2
#DEFINE olTrackingNotRead                                                                    3
#DEFINE olTrackingRecallFailure                                                              4
#DEFINE olTrackingRecallSuccess                                                              5
#DEFINE olTrackingRead                                                                       6
#DEFINE olTrackingReplied                                                                    7
//OlUserPropertyType
#DEFINE olText                                                                               1
#DEFINE olNumber                                                                             3
#DEFINE olDateTime                                                                           5
#DEFINE olYesNo                                                                              6
#DEFINE olDuration                                                                           7
#DEFINE olKeywords                                                                          11
#DEFINE olPercent                                                                           12
#DEFINE olCurrency                                                                          14
#DEFINE olFormula                                                                           18
#DEFINE olCombination                                                                       19
//OlWindowState
#DEFINE olMaximized                                                                          0
#DEFINE olMinimized                                                                          1
#DEFINE olNormalWindow                                                                       2
//OlSyncState
#DEFINE olSyncStopped                                                                        0
#DEFINE olSyncStarted                                                                        1
#endif //OUTLOOK_APPLICATION_HAEDER_DAEMON
