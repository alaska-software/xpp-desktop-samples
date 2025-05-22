//////////////////////////////////////////////////////////////////////
//
//  OWCCHART.CH
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   This file was generated with tlb2ch.exe
//   
//  Remarks:
//   Refer to the Xbase++ online documentation for the paremeter
//   profile of tlb2ch.exe
//   
//  Syntax:
//   tlb2ch.exe owc.chart > owcchart.ch
//  Return:
//   
//   
//////////////////////////////////////////////////////////////////////


#ifndef OWC_CHART_HAEDER_DAEMON
#define OWC_CHART_HAEDER_DAEMON
//ChartChartLayoutEnum
#DEFINE chChartLayoutAutomatic                                                               0
#DEFINE chChartLayoutHorizontal                                                              1
#DEFINE chChartLayoutVertical                                                                2
//LineStyleEnum
#DEFINE owcLineStyleNone                                                                     0
#DEFINE owcLineStyleAutomatic                                                                1
#DEFINE owcLineStyleSolid                                                                    2
#DEFINE owcLineStyleDash                                                                     3
#DEFINE owcLineStyleDot                                                                      4
#DEFINE owcLineStyleDashDot                                                                  5
#DEFINE owcLineStyleDashDotDot                                                               6
//LineWeightEnum
#DEFINE owcLineWeightHairline                                                                0
#DEFINE owcLineWeightThin                                                                    1
#DEFINE owcLineWeightMedium                                                                  2
#DEFINE owcLineWeightThick                                                                   3
//ChartScaleOrientationEnum
#DEFINE chScaleOrientationMinMax                                                             0
#DEFINE chScaleOrientationMaxMin                                                             1
//ChartScaleTypeEnum
#DEFINE chScaleTypeLinear                                                                    0
#DEFINE chScaleTypeLogarithmic                                                               1
//ChartAxisPositionEnum
#DEFINE chAxisPositionTop                                                                   -1
#DEFINE chAxisPositionBottom                                                                -2
#DEFINE chAxisPositionLeft                                                                  -3
#DEFINE chAxisPositionRight                                                                 -4
#DEFINE chAxisPositionRadial                                                                -5
#DEFINE chAxisPositionCircular                                                              -6
//ChartAxisTypeEnum
#DEFINE chCategoryAxis                                                                       0
#DEFINE chValueAxis                                                                          1
//ChartAxisCrossesEnum
#DEFINE chAxisCrossesAutomatic                                                               0
#DEFINE chAxisCrossesCustom                                                                  3
//UnderlineStyleEnum
#DEFINE owcUnderlineStyleNone                                                                0
#DEFINE owcUnderlineStyleSingle                                                              1
#DEFINE owcUnderlineStyleDouble                                                              2
#DEFINE owcUnderlineStyleSingleAccounting                                                    3
#DEFINE owcUnderlineStyleDoubleAccounting                                                    4
//ChartTickMarkEnum
#DEFINE chTickMarkAutomatic                                                                  0
#DEFINE chTickMarkNone                                                                       1
#DEFINE chTickMarkInside                                                                     2
#DEFINE chTickMarkOutside                                                                    3
#DEFINE chTickMarkCross                                                                      4
//ChartTitlePositionEnum
#DEFINE chTitlePositionAutomatic                                                             0
#DEFINE chTitlePositionTop                                                                   1
#DEFINE chTitlePositionBottom                                                                2
#DEFINE chTitlePositionLeft                                                                  3
#DEFINE chTitlePositionRight                                                                 4
//ChartLegendPositionEnum
#DEFINE chLegendPositionAutomatic                                                            0
#DEFINE chLegendPositionTop                                                                  1
#DEFINE chLegendPositionBottom                                                               2
#DEFINE chLegendPositionLeft                                                                 3
#DEFINE chLegendPositionRight                                                                4
//ChartWizardTypeEnum
#DEFINE chWizardFull                                                                         0
#DEFINE chWizardChartType                                                                    1
#DEFINE chWizardSourceData                                                                   2
//ChartDataLabelPositionEnum
#DEFINE chLabelPositionAutomatic                                                             0
#DEFINE chLabelPositionCenter                                                                1
#DEFINE chLabelPositionInsideEnd                                                             2
#DEFINE chLabelPositionInsideBase                                                            3
#DEFINE chLabelPositionOutsideEnd                                                            4
#DEFINE chLabelPositionOutsideBase                                                           5
#DEFINE chLabelPositionLeft                                                                  6
#DEFINE chLabelPositionRight                                                                 7
#DEFINE chLabelPositionTop                                                                   8
#DEFINE chLabelPositionBottom                                                                9
//ChartDimensionsEnum
#DEFINE chDimSeriesNames                                                                     0
#DEFINE chDimCategories                                                                      1
#DEFINE chDimValues                                                                          2
#DEFINE chDimYValues                                                                         3
#DEFINE chDimXValues                                                                         4
#DEFINE chDimOpenValues                                                                      5
#DEFINE chDimCloseValues                                                                     6
#DEFINE chDimHighValues                                                                      7
#DEFINE chDimLowValues                                                                       8
#DEFINE chDimBubbleValues                                                                    9
#DEFINE chDimRValues                                                                        10
#DEFINE chDimThetaValues                                                                    11
//ChartEndStyleEnum
#DEFINE chEndStyleNone                                                                       1
#DEFINE chEndStyleCap                                                                        2
//ChartErrorBarDirectionEnum
#DEFINE chErrorBarDirectionY                                                                 0
#DEFINE chErrorBarDirectionX                                                                 1
//ChartErrorBarIncludeEnum
#DEFINE chErrorBarIncludePlusValues                                                          0
#DEFINE chErrorBarIncludeMinusValues                                                         1
#DEFINE chErrorBarIncludeBoth                                                                2
//ChartErrorBarTypeEnum
#DEFINE chErrorBarTypeFixedValue                                                             0
#DEFINE chErrorBarTypePercent                                                                1
#DEFINE chErrorBarTypeCustom                                                                 2
//ChartErrorBarCustomValuesEnum
#DEFINE chErrorBarPlusValues                                                                12
#DEFINE chErrorBarMinusValues                                                               13
//ChartMarkerStyleEnum
#DEFINE chMarkerStyleNone                                                                    0
#DEFINE chMarkerStyleSquare                                                                  1
#DEFINE chMarkerStyleDiamond                                                                 2
#DEFINE chMarkerStyleTriangle                                                                3
#DEFINE chMarkerStyleX                                                                       4
#DEFINE chMarkerStyleStar                                                                    5
#DEFINE chMarkerStyleDot                                                                     6
#DEFINE chMarkerStyleDash                                                                    7
#DEFINE chMarkerStyleCircle                                                                  8
#DEFINE chMarkerStylePlus                                                                    9
//ChartTrendlineTypeEnum
#DEFINE chTrendlineTypeExponential                                                           0
#DEFINE chTrendlineTypeLinear                                                                1
#DEFINE chTrendlineTypeLogarithmic                                                           2
#DEFINE chTrendlineTypePolynomial                                                            3
#DEFINE chTrendlineTypePower                                                                 4
//ChartChartTypeEnum
#DEFINE chChartTypeCombo                                                                    -1
#DEFINE chChartTypeColumnClustered                                                           0
#DEFINE chChartTypeColumnStacked                                                             1
#DEFINE chChartTypeColumnStacked100                                                          2
#DEFINE chChartTypeBarClustered                                                              3
#DEFINE chChartTypeBarStacked                                                                4
#DEFINE chChartTypeBarStacked100                                                             5
#DEFINE chChartTypeLine                                                                      6
#DEFINE chChartTypeLineStacked                                                               8
#DEFINE chChartTypeLineStacked100                                                           10
#DEFINE chChartTypeLineMarkers                                                               7
#DEFINE chChartTypeLineStackedMarkers                                                        9
#DEFINE chChartTypeLineStacked100Markers                                                    11
#DEFINE chChartTypeSmoothLine                                                               12
#DEFINE chChartTypeSmoothLineStacked                                                        14
#DEFINE chChartTypeSmoothLineStacked100                                                     16
#DEFINE chChartTypeSmoothLineMarkers                                                        13
#DEFINE chChartTypeSmoothLineStackedMarkers                                                 15
#DEFINE chChartTypeSmoothLineStacked100Markers                                              17
#DEFINE chChartTypePie                                                                      18
#DEFINE chChartTypePieExploded                                                              19
#DEFINE chChartTypePieStacked                                                               20
#DEFINE chChartTypeScatterMarkers                                                           21
#DEFINE chChartTypeScatterLine                                                              25
#DEFINE chChartTypeScatterLineMarkers                                                       24
#DEFINE chChartTypeScatterLineFilled                                                        26
#DEFINE chChartTypeScatterSmoothLine                                                        23
#DEFINE chChartTypeScatterSmoothLineMarkers                                                 22
#DEFINE chChartTypeBubble                                                                   27
#DEFINE chChartTypeBubbleLine                                                               28
#DEFINE chChartTypeArea                                                                     29
#DEFINE chChartTypeAreaStacked                                                              30
#DEFINE chChartTypeAreaStacked100                                                           31
#DEFINE chChartTypeDoughnut                                                                 32
#DEFINE chChartTypeDoughnutExploded                                                         33
#DEFINE chChartTypeRadarLine                                                                34
#DEFINE chChartTypeRadarLineMarkers                                                         35
#DEFINE chChartTypeRadarLineFilled                                                          36
#DEFINE chChartTypeRadarSmoothLine                                                          37
#DEFINE chChartTypeRadarSmoothLineMarkers                                                   38
#DEFINE chChartTypeStockHLC                                                                 39
#DEFINE chChartTypeStockOHLC                                                                40
#DEFINE chChartTypePolarMarkers                                                             41
#DEFINE chChartTypePolarLine                                                                42
#DEFINE chChartTypePolarLineMarkers                                                         43
#DEFINE chChartTypePolarSmoothLine                                                          44
#DEFINE chChartTypePolarSmoothLineMarkers                                                   45
//ChartSizeRepresentsEnum
#DEFINE chSizeIsWidth                                                                        0
#DEFINE chSizeIsArea                                                                         1
//ChartDataSourceTypeEnum
#DEFINE chDataSourceTypeUnknown                                                              0
#DEFINE chDataSourceTypeSpreadsheet                                                          1
#DEFINE chDataSourceTypeRecordset                                                            2
#DEFINE chDataSourceTypePivotlist                                                            3
//ChartSelectionsEnum
#DEFINE chSelectionNone                                                                     -1
#DEFINE chSelectionAxis                                                                      0
#DEFINE chSelectionChart                                                                     1
#DEFINE chSelectionPlotArea                                                                  2
#DEFINE chSelectionDataLabels                                                                3
#DEFINE chSelectionErrorbars                                                                 4
#DEFINE chSelectionGridlines                                                                 5
#DEFINE chSelectionLegend                                                                    6
#DEFINE chSelectionLegendEntry                                                               7
#DEFINE chSelectionPoint                                                                     8
#DEFINE chSelectionSeries                                                                    9
#DEFINE chSelectionTitle                                                                    10
#DEFINE chSelectionTrendline                                                                11
#DEFINE chSelectionWebChart                                                                 12
//SheetCommandEnum
#DEFINE ssCalculate                                                                          0
#DEFINE ssInsertRows                                                                         2
#DEFINE ssInsertColumns                                                                      3
#DEFINE ssDeleteRows                                                                         4
#DEFINE ssDeleteColumns                                                                      5
#DEFINE ssCut                                                                                6
#DEFINE ssCopy                                                                               7
#DEFINE ssPaste                                                                              8
#DEFINE ssExport                                                                             9
#DEFINE ssUndo                                                                              10
#DEFINE ssSortAscending                                                                     11
#DEFINE ssSortDescending                                                                    12
#DEFINE ssFind                                                                              13
#DEFINE ssClear                                                                             14
#DEFINE ssAutoFilter                                                                        15
#DEFINE ssProperties                                                                        16
#DEFINE ssHelp                                                                              17
//SheetFindLookInEnum
#DEFINE ssFormulas                                                                           0
#DEFINE ssValues                                                                             1
//SheetFindLookAtEnum
#DEFINE ssPart                                                                               0
#DEFINE ssWhole                                                                              1
//SheetSearchOrderEnum
#DEFINE ssByColumns                                                                          0
#DEFINE ssByRows                                                                             1
//SheetSearchDirectionEnum
#DEFINE ssNext                                                                               0
#DEFINE ssPrevious                                                                           1
//SheetFreezePanesEnum
#DEFINE ssFreezeLeft                                                                         1
#DEFINE ssFreezeRight                                                                        2
#DEFINE ssFreezeTop                                                                          4
#DEFINE ssFreezeBottom                                                                       8
#DEFINE ssFreezeNone                                                                        16
//SheetSortOrderEnum
#DEFINE ssAscending                                                                          0
#DEFINE ssDescending                                                                         1
//SheetYesNoGuessEnum
#DEFINE ssGuess                                                                              0
#DEFINE ssYes                                                                                1
#DEFINE ssNo                                                                                 2
//SheetHAlignEnum
#DEFINE ssHAlignGeneral                                                                      0
#DEFINE ssHAlignLeft                                                                         1
#DEFINE ssHAlignCenter                                                                       2
#DEFINE ssHAlignRight                                                                        3
//SheetExportActionEnum
#DEFINE ssExportActionNone                                                                   0
#DEFINE ssExportActionOpenInExcel                                                            1
//SheetFilterFunction
#DEFINE ssFilterFunctionInclude                                                              1
#DEFINE ssFilterFunctionExclude                                                              2
//SheetVAlignEnum
#DEFINE ssVAlignBottom                                                                       0
#DEFINE ssVAlignCenter                                                                       1
#DEFINE ssVAlignTop                                                                          2
//SheetDirectionEnum
#DEFINE ssDown                                                                               0
#DEFINE ssToLeft                                                                             1
#DEFINE ssToRight                                                                            2
#DEFINE ssUp                                                                                 3
//ChartSeriesScalingsEnum
#DEFINE chCategoryScaling                                                                    0
#DEFINE chValueScaling                                                                       1
#DEFINE chValueScaling1                                                                      2
#DEFINE chValueScaling2                                                                      3
//ChartFillStyleEnum
#DEFINE chNone                                                                              -1
#DEFINE chSolid                                                                              0
//ChartColorIndexEnum
#DEFINE chColorAutomatic                                                                    -1
#DEFINE chColorNone                                                                         -2
//ChartDataGroupingFunctionEnum
#DEFINE chDataGroupingFunctionMinimum                                                        0
#DEFINE chDataGroupingFunctionMaximum                                                        1
#DEFINE chDataGroupingFunctionSum                                                            2
#DEFINE chDataGroupingFunctionAverage                                                        3
//ChartSeriesByEnum
#DEFINE chSeriesByRows                                                                       0
#DEFINE chSeriesByColumns                                                                    1
//ChartSpecialDataSourcesEnum
#DEFINE chDataLiteral                                                                       -1
#DEFINE chDataNone                                                                          -2
//ChartPivotDataReferenceEnum
#DEFINE chPivotColumns                                                                      -1
#DEFINE chPivotRows                                                                         -2
#DEFINE chPivotColAggregates                                                                -3
#DEFINE chPivotRowAggregates                                                                -4
//SheetBorderTypeEnum
#DEFINE ssEdgeAll                                                                            0
#DEFINE ssEdgeBottom                                                                         1
#DEFINE ssEdgeLeft                                                                           2
#DEFINE ssEdgeRight                                                                          3
#DEFINE ssEdgeTop                                                                            4
#DEFINE ssInsideHorizontal                                                                   5
#DEFINE ssInsideVertical                                                                     6
//SheetTriStateEnum
#DEFINE ssAuto                                                                              -1
#DEFINE ssOff                                                                                0
#DEFINE ssOn                                                                                 1
//ExpandBitmapTypeEnum
#DEFINE ecBitmapPlusMinus                                                                    0
#DEFINE ecBitmapUpDownArrow                                                                  1
#DEFINE ecBitmapOpenCloseFolder                                                              2
//DscRowsourceTypeEnum
#DEFINE dscTable                                                                             1
#DEFINE dscView                                                                              2
#DEFINE dscCommandText                                                                       3
#DEFINE dscProcedure                                                                         4
#DEFINE dscCommandFile                                                                       5
//DscFieldTypeEnum
#DEFINE dscParameter                                                                        -1
#DEFINE dscOutput                                                                            1
#DEFINE dscCalculated                                                                        2
#DEFINE dscGrouping                                                                          3
//DscTotalTypeEnum
#DEFINE dscNone                                                                              0
#DEFINE dscSum                                                                               1
#DEFINE dscAvg                                                                               2
#DEFINE dscMin                                                                               3
#DEFINE dscMax                                                                               4
#DEFINE dscCount                                                                             5
#DEFINE dscAny                                                                               6
#DEFINE dscStdev                                                                             7
//DscGroupOnEnum
#DEFINE dscNoGrouping                                                                       -1
#DEFINE dscEachValue                                                                         0
#DEFINE dscPrefix                                                                            1
#DEFINE dscYear                                                                              2
#DEFINE dscQuarter                                                                           3
#DEFINE dscMonth                                                                             4
#DEFINE dscWeek                                                                              5
#DEFINE dscDay                                                                               6
#DEFINE dscHour                                                                              7
#DEFINE dscMinute                                                                            8
#DEFINE dscInterval                                                                          9
//DscJoinTypeEnum
#DEFINE dscInnerJoin                                                                         1
#DEFINE dscLeftOuterJoin                                                                     2
#DEFINE dscRightOuterJoin                                                                    3
//DscPageRelTypeEnum
#DEFINE dscSublist                                                                           1
#DEFINE dscLookup                                                                            2
//DscObjectTypeEnum
#DEFINE dscobjUnknown                                                                       -1
#DEFINE dscobjSchemaRowsource                                                                1
#DEFINE dscobjSchemaField                                                                    2
#DEFINE dscobjSchemaRelationship                                                             4
#DEFINE dscobjRecordsetDef                                                                   8
#DEFINE dscobjPageRowsource                                                                 16
#DEFINE dscobjPageField                                                                     32
#DEFINE dscobjSublistRelationship                                                           64
#DEFINE dscobjLookupRelationship                                                           128
#DEFINE dscobjGroupingDef                                                                  256
#DEFINE dscobjDatamodel                                                                    512
#DEFINE dscobjPageRelatedField                                                            1024
#DEFINE dscobjParameterValue                                                              2048
#DEFINE dscobjSchemaRelatedField                                                          4096
#DEFINE dscobjSchemaParameter                                                             8192
//NavButtonEnum
#DEFINE navbtnMoveFirst                                                                      0
#DEFINE navbtnMovePrev                                                                       1
#DEFINE navbtnMoveNext                                                                       2
#DEFINE navbtnMoveLast                                                                       3
#DEFINE navbtnNew                                                                            4
#DEFINE navbtnDelete                                                                         5
#DEFINE navbtnSave                                                                           6
#DEFINE navbtnUndo                                                                           7
#DEFINE navbtnSortAscending                                                                  8
#DEFINE navbtnSortDescending                                                                 9
#DEFINE navbtnApplyFilter                                                                   10
#DEFINE navbtnToggleFilter                                                                  11
#DEFINE navbtnHelp                                                                          12
//DscDropTypeEnum
#DEFINE dscDefault                                                                           0
#DEFINE dscGrid                                                                              1
#DEFINE dscFields                                                                            2
//DscDropLocationEnum
#DEFINE dscAbove                                                                             1
#DEFINE dscWithin                                                                            2
#DEFINE dscBelow                                                                             3
//DscHyperlinkPartEnum
#DEFINE dschlDisplayedValue                                                                  0
#DEFINE dschlDisplayText                                                                     1
#DEFINE dschlAddress                                                                         2
#DEFINE dschlSubAddress                                                                      3
#DEFINE dschlScreenTip                                                                       4
#DEFINE dschlFullAddress                                                                     5
//DscLocationEnum
#DEFINE dscSystem                                                                           -1
#DEFINE dscClient                                                                            0
#DEFINE dscServer                                                                            1
//DscRecordsetTypeEnum
#DEFINE dscSnapshot                                                                          1
#DEFINE dscUpdatableSnapshot                                                                 2
//DscFetchTypeEnum
#DEFINE dscFull                                                                              1
#DEFINE dscParameterized                                                                     2
//DscAdviseTypeEnum
#DEFINE dscAdd                                                                               1
#DEFINE dscDelete                                                                            2
#DEFINE dscMove                                                                              3
#DEFINE dscLoad                                                                              4
#DEFINE dscChange                                                                            5
#DEFINE dscDeleteComplete                                                                    6
//FieldListConnectionTypeEnum
#DEFINE fltypeUnknown                                                                        0
#DEFINE fltypeMD                                                                             1
#DEFINE fltypePVT                                                                            2
#DEFINE fltypeRel                                                                            3
//FieldListObjectTypeEnum
#DEFINE flTables                                                                             1
#DEFINE flViews                                                                              2
#DEFINE flStoredProcedures                                                                   4
#DEFINE flCmdText                                                                            8
#DEFINE flSchemaDiagrams                                                                    16
#DEFINE flOLAPCube                                                                          32
#DEFINE flAll                                                                              255
//DaAttrEnum
#DEFINE daLength                                                                             1
#DEFINE daPrecision                                                                          2
#DEFINE daScale                                                                              3
//DaPivotLocationEnum
#DEFINE dplRowArea                                                                           1
#DEFINE dplColumnArea                                                                        2
#DEFINE dplFilterArea                                                                        3
#DEFINE dplDataArea                                                                          4
#DEFINE dplDetails                                                                           5
//FieldListRelationshipTypeEnum
#DEFINE flrelNoRel                                                                           0
#DEFINE flrelOneToMany                                                                       1
#DEFINE flrelManyToOne                                                                       2
#DEFINE flrelOneToOnePrimaryPrimary                                                          4
#DEFINE flrelOneToOnePrimaryForeign                                                          8
#DEFINE flrelUniqueConstraint                                                               16
#DEFINE flrelUniqueIndex                                                                    32
//SectTypeEnum
#DEFINE sectTypeNone                                                                         0
#DEFINE sectTypeCaption                                                                      1
#DEFINE sectTypeHeader                                                                       2
#DEFINE sectTypeFooter                                                                       3
#DEFINE sectTypeRecNav                                                                       4
//ProviderType
#DEFINE providerTypeUnknown                                                                  1
#DEFINE providerTypeRelational                                                               2
#DEFINE providerTypeMultidimensional                                                         3
//DefaultControlTypeEnum
#DEFINE ctlTypeTextBox                                                                       0
#DEFINE ctlTypeBoundHTML                                                                     1
//DataPageDesignerFlags
#DEFINE designFlagDontDelete                                                                 1
#DEFINE designFlagDontCleanup                                                                2
//PivotHAlignmentEnum
#DEFINE plHAlignAutomatic                                                                    0
#DEFINE plHAlignLeft                                                                         1
#DEFINE plHAlignCenter                                                                       2
#DEFINE plHAlignRight                                                                        3
//PivotFieldGroupOnEnum
#DEFINE plGroupOnEachValue                                                                   0
#DEFINE plGroupOnPrefixChar                                                                  1
#DEFINE plGroupOnYears                                                                       2
#DEFINE plGroupOnQtrs                                                                        3
#DEFINE plGroupOnMonths                                                                      4
#DEFINE plGroupOnWeeks                                                                       5
#DEFINE plGroupOnDays                                                                        6
#DEFINE plGroupOnHours                                                                       7
#DEFINE plGroupOnMinutes                                                                     8
#DEFINE plGroupOnSeconds                                                                     9
#DEFINE plGroupOnInterval                                                                   10
//PivotFieldSortDirectionEnum
#DEFINE plSortDirectionDefault                                                               0
#DEFINE plSortDirectionAscending                                                             1
#DEFINE plSortDirectionDescending                                                            2
//PivotFieldTypeEnum
#DEFINE plTypeRegular                                                                        1
#DEFINE plTypeCalculated                                                                     2
#DEFINE plTypeTime                                                                           3
#DEFINE plTypeTimeYears                                                                      4
#DEFINE plTypeTimeHalfYears                                                                  5
#DEFINE plTypeTimeQuarters                                                                   6
#DEFINE plTypeTimeMonths                                                                     7
#DEFINE plTypeTimeWeeks                                                                      8
#DEFINE plTypeTimeDays                                                                       9
#DEFINE plTypeTimeHours                                                                     10
#DEFINE plTypeTimeMinutes                                                                   11
#DEFINE plTypeTimeSeconds                                                                   12
#DEFINE plTypeTimeUndefined                                                                 13
#DEFINE plTypeUnknown                                                                       14
#DEFINE plTypeUserDefined                                                                   15
//PivotFieldFilterFunctionEnum
#DEFINE plFilterFunctionNone                                                                 0
#DEFINE plFilterFunctionInclude                                                              1
#DEFINE plFilterFunctionExclude                                                              2
//PivotTotalFunctionEnum
#DEFINE plFunctionSum                                                                        1
#DEFINE plFunctionCount                                                                      2
#DEFINE plFunctionMin                                                                        3
#DEFINE plFunctionMax                                                                        4
//PivotTotalTypeEnum
#DEFINE plTotalTypeIntrinsic                                                                 1
#DEFINE plTotalTypeUserDefined                                                               2
//PivotFieldSetOrientationEnum
#DEFINE plOrientationNone                                                                    0
#DEFINE plOrientationColumnAxis                                                              1
#DEFINE plOrientationRowAxis                                                                 2
#DEFINE plOrientationFilterAxis                                                              4
#DEFINE plOrientationDataAxis                                                                8
//PivotFieldSetTypeEnum
#DEFINE plFieldSetTypeTime                                                                   1
#DEFINE plFieldSetTypeOther                                                                  2
#DEFINE plFieldSetTypeUnknown                                                                3
#DEFINE plFieldSetTypeUserDefined                                                            4
//PivotViewTotalOrientationEnum
#DEFINE plTotalOrientationRow                                                                1
#DEFINE plTotalOrientationColumn                                                             2
//PivotExportActionEnum
#DEFINE plExportActionNone                                                                   0
#DEFINE plExportActionOpenInExcel                                                            1
//PivotViewReasonEnum
#DEFINE plViewReasonSelectionChange                                                          0
#DEFINE plViewReasonSubtotalsChange                                                          1
#DEFINE plViewReasonSystemColorChange                                                        2
#DEFINE plViewReasonQuery                                                                    3
#DEFINE plViewReasonFontNameChange                                                           4
#DEFINE plViewReasonFontSizeChange                                                           5
#DEFINE plViewReasonFontBoldChange                                                           6
#DEFINE plViewReasonFontItalicChange                                                         7
#DEFINE plViewReasonFontUnderlineChange                                                      8
#DEFINE plViewReasonMemberExpandedChange                                                     9
#DEFINE plViewReasonCellExpandedChange                                                      10
#DEFINE plViewReasonDetailRowHeightChange                                                   11
#DEFINE plViewReasonFieldDetailWidthChange                                                  12
#DEFINE plViewReasonFieldGroupedWidthChange                                                 13
#DEFINE plViewReasonViewDetailWidthChange                                                   14
#DEFINE plViewReasonFieldSetWidthChange                                                     15
#DEFINE plViewReasonTotalWidthChange                                                        16
#DEFINE plViewReasonForeColorChange                                                         17
#DEFINE plViewReasonBackColorChange                                                         18
#DEFINE plViewReasonAlignmentChange                                                         19
#DEFINE plViewReasonNumberFormatChange                                                      20
#DEFINE plViewReasonDetailTopChange                                                         21
#DEFINE plViewReasonDetailLeftChange                                                        22
#DEFINE plViewReasonTopChange                                                               23
#DEFINE plViewReasonLeftChange                                                              24
#DEFINE plViewReasonRightToLeftChange                                                       25
#DEFINE plViewReasonTotalOrientationChange                                                  26
#DEFINE plViewReasonDisplayOutlineChange                                                    27
#DEFINE plViewReasonFieldCaptionChange                                                      28
#DEFINE plViewReasonFieldSetCaptionChange                                                   29
#DEFINE plViewReasonLabelCaptionChange                                                      30
#DEFINE plViewReasonMemberCaptionChange                                                     31
#DEFINE plViewReasonTotalCaptionChange                                                      32
#DEFINE plViewReasonAllowFilteringChange                                                    33
#DEFINE plViewReasonAllowGroupingChange                                                     34
#DEFINE plViewReasonWidthChange                                                             35
#DEFINE plViewReasonHeightChange                                                            36
#DEFINE plViewReasonLabelVisibleChange                                                      37
#DEFINE plViewReasonDisplayToolbarChange                                                    38
#DEFINE plViewReasonMaxHeightChange                                                         39
#DEFINE plViewReasonMaxWidthChange                                                          40
#DEFINE plViewReasonAutoFitChange                                                           41
#DEFINE plViewReasonFieldExpandedChange                                                     42
#DEFINE plViewReasonCellsExpandedChange                                                     43
#DEFINE plViewReasonDetailMaxWidthChange                                                    44
#DEFINE plViewReasonDetailMaxHeightChange                                                   45
//PivotQueryReasonEnum
#DEFINE plQueryReasonInsertFieldSet                                                          0
#DEFINE plQueryReasonRemoveFieldSet                                                          1
#DEFINE plQueryReasonInsertTotal                                                             2
#DEFINE plQueryReasonRemoveTotal                                                             3
#DEFINE plQueryReasonAllowDetailsChange                                                      4
#DEFINE plQueryReasonSortDirectionChange                                                     5
#DEFINE plQueryReasonSortOnChange                                                            6
#DEFINE plQueryReasonSortOnScopeChange                                                       7
#DEFINE plQueryReasonFilterFunctionChange                                                    8
#DEFINE plQueryReasonFilterMemberChange                                                      9
#DEFINE plQueryReasonIsIncludedChange                                                       10
#DEFINE plQueryReasonExpressionChange                                                       11
#DEFINE plQueryReasonDisplayEmptyMembersChange                                              12
#DEFINE plQueryReasonTotalFunctionChange                                                    13
#DEFINE plQueryReasonUser                                                                   14
#DEFINE plQueryReasonDataSourceChange                                                       15
#DEFINE plQueryReasonDataMemberChange                                                       16
#DEFINE plQueryReasonGroupOnChange                                                          17
#DEFINE plQueryReasonUnknown                                                                18
#DEFINE plQueryReasonGroupOffsetChange                                                      19
#DEFINE plQueryReasonGroupIntervalChange                                                    20
#DEFINE plQueryReasonIsFilteredChange                                                       21
//PivotTableMemberExpandEnum
#DEFINE plMemberExpandAutomatic                                                              0
#DEFINE plMemberExpandAlways                                                                 1
#DEFINE plMemberExpandNever                                                                  2
//PivotTableReasonEnum
#DEFINE plPivotTableReasonTotalAdded                                                         0
#DEFINE plPivotTableReasonTotalDeleted                                                       1
#DEFINE plPivotTableReasonFieldSetAdded                                                      2
#DEFINE plPivotTableReasonFieldSetDeleted                                                    3
#DEFINE plPivotTableReasonFieldAdded                                                         4
#endif //OWC_CHART_HAEDER_DAEMON
