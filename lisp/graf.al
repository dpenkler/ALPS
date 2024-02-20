;;;-*- mode: emacs-lisp; -*-
;;;
;;; Graphics constants and functions
;;;
(require 'trig)
(a EZ_X                      1
 EZ_Y                      2
 EZ_WIDTH                  3
 EZ_HEIGHT                 4
 EZ_WIDTH_HINT             5
 EZ_HEIGHT_HINT            6
 EZ_BORDER_WIDTH           7
 EZ_BORDERWIDTH            EZ_BORDER_WIDTH
 EZ_BORDER_TYPE            8
 EZ_BORDER_STYLE           EZ_BORDER_TYPE
 EZ_PADX                   9
 EZ_PADY                   10
 EZ_IPADX                  11
 EZ_IPADY                  12
 EZ_STACKING               13
 EZ_ORIENTATION            EZ_STACKING
 EZ_SIDE                   14
 EZ_LABEL_POSITION         15
 EZ_LABELPOSITION          EZ_LABEL_POSITION
 EZ_JUSTIFICATION          16
 EZ_LABEL_JUSTIFICATION    EZ_JUSTIFICATION
 EZ_FILL_MODE              17
 EZ_PROPAGATE              18
 EZ_CALL_BACK              19
 EZ_CALLBACK               EZ_CALL_BACK
 EZ_FOREGROUND             20
 EZ_BACKGROUND             21
 EZ_BITMAP_FILE            22
 EZ_PIXMAP_FILE            EZ_BITMAP_FILE
 EZ_BITMAP_DATA            23   
 EZ_PIXMAP_DATA            24
 EZ_X_PIXMAP               25
 EZ_LABEL_PIXMAP           26
 EZ_LABELPIXMAP            EZ_LABEL_PIXMAP
 EZ_FONT_ID                27
 EZ_FONT_NAME              28
 EZ_TEXT_LINE_LENGTH       29
 EZ_SLIDER_LENGTH          30
 EZ_SLIDERLENGTH           EZ_SLIDER_LENGTH
 EZ_SLIDER_WIDTH           31
 EZ_SLIDERWIDTH            EZ_SLIDER_WIDTH
 EZ_SLIDER_BORDER_WIDTH    32
 EZ_SLIDER_BORDERWIDTH     EZ_SLIDER_BORDER_WIDTH
 EZ_SLIDER_RESOLUTION      33
 EZ_SLIDER_RANGE           34
 EZ_INDICATOR_SIZE_ADJUST  35
 EZ_IMAGE_FILE             36
 EZ_BUTTON_SHORTCUT        37
 EZ_SHORTCUT_KEY           EZ_BUTTON_SHORTCUT  
 EZ_EVENT_HANDLE           38
 EZ_EVENT_HANDLER          EZ_EVENT_HANDLE
 EZ_LABEL_STRING           39
 EZ_RESERVE_MENU_BUTTON    40
 EZ_BACKING_STORE          41
 EZ_EXPAND                 42  
 EZ_TRANSIENT              43
 EZ_SCROLLBAR_WIDTH        44
 EZ_SCROLLBAR_BORDER_WIDTH 45
 EZ_TEXT_SPACING           46
 EZ_UNDERLINE              47
 EZ_BUBBLE_HELP            48
 EZ_MOVABLE                49
 EZ_FREELABEL_MOVABLE      EZ_MOVABLE
 EZ_ITEM_MOVABLE           EZ_FREELABEL_MOVABLE
 EZ_INDICATOR_TYPE         50
 EZ_INDICATOR_COLOR        51
 EZ_TEXT_BACKGROUND        52  
 EZ_SLIDER_DISPLAY_VALUE   53
 EZ_HIGHLIGHT_FOREGROUND   54
 EZ_HIGHLIGHT_FG           EZ_HIGHLIGHT_FOREGROUND
 EZ_HIGHLIGHT_BACKGROUND   55
 EZ_HIGHLIGHT_BG           EZ_HIGHLIGHT_BACKGROUND
 EZ_ATTACH_PTR_DATA        56
 EZ_ATTACH_INT_DATA        57
 EZ_USER_PTR_DATA          EZ_ATTACH_PTR_DATA
 EZ_USER_INT_DATA          EZ_ATTACH_INT_DATA
 EZ_CLIENT_PTR_DATA        EZ_ATTACH_PTR_DATA   
 EZ_CLIENT_INT_DATA        EZ_ATTACH_INT_DATA
 EZ_DESTROY_CALLBACK       58
 EZ_EMBEDER                59
 EZ_IS_EMBEDER             EZ_EMBEDER
 EZ_DND_DRAG_CURSOR        60
 EZ_DND_BUBBLE_HELP        61
 EZ_SHAPED_WINDOW          62
 EZ_MOTION_CALLBACK        63

 EZ_CHECK_BUTTON_ON_VALUE  64
 EZ_CHECK_BUTTON_OFF_VALUE 65
 EZ_CHECK_BUTTON_ON_OFF    66
 EZ_RADIO_BUTTON_GROUP     67
 EZ_RADIO_BUTTON_VALUE     68
 EZ_OPTIONAL_HSCROLLBAR    69
 EZ_OPTIONAL_VSCROLLBAR    70
 EZ_FANCY_LISTBOX_TYPE     71
 EZ_FANCY_LIST_BOX_TYPE     EZ_FANCY_LISTBOX_TYPE
 EZ_FANCY_LISTBOX_COLUMNS  72
 EZ_FANCY_LIST_BOX_COLUMNS EZ_FANCY_LISTBOX_COLUMNS
 EZ_GLOB_PATTERN           73
 EZ_TEXT_WIDGET_EDITABLE   74
 EZ_ENTRY_STRING           75
 EZ_RETURN_VALUE           76
 EZ_SLIDER_INIT_VALUE      77
 EZ_OPTIONAL_ENTRY_REMEMBER_HISTORY   78
 EZ_OPTION_ENTRY_REMEMBER_HISTORY     EZ_OPTIONAL_ENTRY_REMEMBER_HISTORY
 EZ_OPTIONAL_ENTRY_HISTORY   78
 EZ_OPTION_ENTRY_HISTORY     EZ_OPTIONAL_ENTRY_HISTORY
 EZ_ENTRY_EDITABLE           79
 EZ_OPTIONAL_ENTRY_EDITABLE  EZ_ENTRY_EDITABLE
 EZ_OPTION_ENTRY_EDITABLE    EZ_ENTRY_EDITABLE
 EZ_ITEM_WIDGET_WIDGET     80
 EZ_RAW_RGB_DATA           81
 EZ_MENU_TEAR_OFF          82
 EZ_GRID_CELL_GEOMETRY     83
 EZ_GRID_CELL_PLACEMENT    84
 EZ_GRID_CONSTRAINS        85
 EZ_GRID_ROW_CONSTRAINS    86
 EZ_GRID_COLUMN_CONSTRAINS 87

 EZ_LOCATION               88
 EZ_SIZE                   89
 EZ_DIMENSION              EZ_SIZE
 EZ_GEOMETRY               90
 EZ_SIZE_HINT              91

 EZ_PADB                   92
 EZ_HIGHLIGHT_PAD          EZ_PADB
 EZ_OPAD                   EZ_PADB
 EZ_FOCUS_PAD              EZ_PADB

 EZ_NAME                   93

 EZ_LCD_NDIGITS            94
 EZ_LCD_FONT_SIZE          95
 EZ_LCD_BACKGROUND         96
 EZ_LCD_FOREGROUND         97

 EZ_LED_BACKGROUND         98
 EZ_LED_PIXEL_COLOR        99
 EZ_LED_WIDTH              100
 EZ_LED_HEIGHT             101
 EZ_LED_PIXEL_SIZE         102
 EZ_LED_COLOR_FUNCTION     103

 EZ_WMHINTS                104
 EZ_WM_HINTS               EZ_WMHINTS
 EZ_GEOMETRY_MANAGER       105
 EZ_CURSOR                 106
 EZ_CLASS                  107
 EZ_POPUP_NAME             108
 EZ_LABELED_ICON           109

 EZ_HSCROLL_INCREMENT      110
 EZ_VSCROLL_INCREMENT      111

 EZ_SMETER_VALUE           112
 EZ_SMETER_RANGE           113
 EZ_SMETER_STOP            114
 EZ_SMETER_STOPED          EZ_SMETER_STOP
 EZ_SMETER_BACKGROUND      115
 EZ_SMETER_FOREGROUND      116
 EZ_SMETER_FOREGROUND_N    117
 EZ_SMETER_SHOW_VALUE      118
 EZ_SMETER_VALUE_N         119
 EZ_SMETER_RANGE_N         120
 EZ_SMETER_STYLE           121
 EZ_SMETER_ORIENTATION     122

 EZ_FORGET_X               123
 EZ_FORGET_Y               124
 EZ_FORGET_W               125
 EZ_FORGET_H               126
 EZ_FORGET_LOCATION        127
 EZ_FORGET_POSITION        EZ_FORGET_LOCATION
 EZ_FORGET_SIZE            128
 EZ_FORGET_GEOMETRY        129

 EZ_WM_WINDOW_NAME         130
 EZ_WM_INITIAL_STATE       131	
 EZ_WM_ICON_NAME           132
 EZ_WM_ICON_XPM_FILE       133
 EZ_WM_ICON_PIXMAP         134
 EZ_WM_ICON_POSITION       135
 EZ_WM_POSITION_HINT       136
 EZ_WM_SIZE_HINT           137
 EZ_WM_MAX_SIZE_HINT       138
 EZ_WM_MIN_SIZE_HINT       139

 EZ_MARK_CLEAN             140

 EZ_HIGHLIGHT_MODE         141
 EZ_RUBBER_BAND            142
 EZ_BG_IMAGE_FILE          143
 EZ_BG_PIXMAP              144
 EZ_BG_IMAGE_FILE_B        145
 EZ_BG_PIXMAP_B            146
 EZ_DRAG_HANDLE            147
 EZ_DOCKABLE               148
 EZ_SLIDER_STYLE           149
 EZ_GRIP_BUTTON_STYLE      EZ_SLIDER_STYLE
 EZ_ROW_BG                 150
 EZ_ARROW_LABEL            151
 EZ_ARROW_TYPE             EZ_ARROW_LABEL
 EZ_INTERIOR_BORDER        152
 EZ_OPTIONAL_ENTRY_STYLE   153
 EZ_OPTION_ENTRY_STYLE     EZ_OPTIONAL_ENTRY_STYLE
 EZ_TILE_ORIGIN            154
 EZ_WM_IGNORE              155
 EZ_LABEL_SHADOW           156
 EZ_TERM_COLOR_N           157
 EZ_TERM_CURSOR_COLOR      158
 EZ_TERM_SAVE_LINES        159
 EZ_TERM_REVERSE_VIDEO     160
 EZ_SELECTION_BACKGROUND   161
 EZ_SELECTION_FOREGROUND   162
 EZ_RULER_DISPLAY_VALUE    163
 EZ_RULER_TICK_UNIT        164
 EZ_RULER_TICK_LENGTH      165
 EZ_RULER_POSITION         166
 EZ_RULER_TICK_SCALE       167
 EZ_RULER_TICK_OFFSET      168
 EZ_OVERRIDE_REDIRECT      169
 EZ_RESOURCES_HANDLE       170
 EZ_RESOURCES_HANDLER      EZ_RESOURCES_HANDLE
 EZ_SCROLL_TYPE            171
 EZ_SPIN_FUNCTION          172
 EZ_SPIN_VALUE             173
 EZ_LOCATOR_XY             174
 EZ_SPECIAL_EFFECT         175
 EZ_SHOW_TICK              176
 EZ_DIAL_RANGE             177
 EZ_DIAL_RESOLUTION        178
 EZ_DIAL_SPAN              179
 EZ_DIAL_DISPLAY_VALUE     180
 EZ_DIAL_STYLE             181
 EZ_DIAL_COLOR             182
 EZ_DIAL_VALUE             183
 EZ_CURSOR_NAME            184
 EZ_DND_DRAG_CURSOR_NAME   185
 EZ_SHAPE_FILE             186
 EZ_SHAPE_PIXMAP           187
 EZ_WRAP_TEXT              188
 EZ_BAR_EDITABLE           189
 EZ_BAR_ORIENTATION        190
 EZ_BAR_COLOR_N            191
 EZ_BAR_RANGE              192
 EZ_BAR_WIDTH              193
 EZ_HISTOGRAM_SAMPLE       194
 EZ_SEPARATOR_STYLE        195
 EZ_ROW_COLUMN_MAX_SIZE    196 
 EZ_RESOURCE_STRING        197
 EZ_SHEET_HEADER_SIZE      198
 EZ_SHEET_CELL_SIZE        199

 EZ_WIDGET_CONFIGURE_LAST_OPTION       199

; ***                   Border Styles                                 ***
 
 EZ_BORDER_NONE                  0
 EZ_BORDER_FLAT                  1
 EZ_BORDER_FRAMED_UP             2
 EZ_BORDER_RIDGE                 EZ_BORDER_FRAMED_UP
 EZ_BORDER_FRAMED_DOWN           3
 EZ_BORDER_GROOVE                EZ_BORDER_FRAMED_DOWN
 EZ_BORDER_UP                    4
 EZ_BORDER_RAISED                EZ_BORDER_UP
 EZ_BORDER_DOWN                  5
 EZ_BORDER_SUNKEN                EZ_BORDER_DOWN
 EZ_BORDER_NB_UP                 6
 EZ_BORDER_NB_DOWN               7
 EZ_BORDER_EMBOSSED              8
 EZ_BORDER_ENGRAVED              9
 EZ_BORDER_NB_UP_L               10
 EZ_BORDER_NB_DOWN_L             11
 EZ_BORDER_NB_UP_R               12
 EZ_BORDER_NB_DOWN_R             13
 EZ_BORDER_NB_UP_B               14
 EZ_BORDER_NB_DOWN_B             15
 EZ_BORDER_TB_UP                 16
 EZ_BORDER_TB_RAISED             16
 EZ_BORDER_TB_DOWN               17
 EZ_BORDER_TB_SUNKEN             17
 EZ_BORDER_LR_UP                 18
 EZ_BORDER_LR_RAISED             18
 EZ_BORDER_LR_DOWN               19
 EZ_BORDER_LR_SUNKEN             19
 EZ_BORDER_TB_FLAT               20
 EZ_BORDER_LR_FLAT               21
 EZ_BORDER_SHADOW                22
 EZ_BORDER_SHADOW3               23
 EZ_HSLIDER_BORDER               24
 EZ_VSLIDER_BORDER               25


 EZ_WIDGET_UNKNOWN                0
 EZ_WIDGET_FRAME                  1
 EZ_WIDGET_LABEL                  2
 EZ_WIDGET_NORMAL_BUTTON          3
 EZ_WIDGET_CHECK_BUTTON           4
 EZ_WIDGET_RADIO_BUTTON           5
 EZ_WIDGET_MENU_BUTTON            6
 EZ_WIDGET_MENU_SEPARATOR         7
 EZ_WIDGET_MENU_SUBMENU           8
 EZ_WIDGET_MENU_NORMAL_BUTTON     9
 EZ_WIDGET_MENU_CHECK_BUTTON      10
 EZ_WIDGET_MENU_RADIO_BUTTON      11
 EZ_WIDGET_VERTICAL_SLIDER        12
 EZ_WIDGET_HORIZONTAL_SLIDER      13
 EZ_VERTICAL_SLIDER               EZ_WIDGET_VERTICAL_SLIDER
 EZ_HORIZONTAL_SLIDER             EZ_WIDGET_HORIZONTAL_SLIDER
 EZ_WIDGET_ENTRY                  14
 EZ_WIDGET_3D_CANVAS              15
 EZ_WIDGET_VERTICAL_SCROLLBAR     16
 EZ_WIDGET_HORIZONTAL_SCROLLBAR   17
 EZ_WIDGET_ILIST_BOX              18
 EZ_WIDGET_ILISTBOX               EZ_WIDGET_ILIST_BOX
 EZ_WIDGET_LIST_BOX               19
 EZ_WIDGET_LISTBOX                EZ_WIDGET_LIST_BOX
 EZ_WIDGET_ITEXT                  20
 EZ_WIDGET_TEXT                   21
 EZ_WIDGET_NOTE_BOOK              22
 EZ_WIDGET_NOTEBOOK               EZ_WIDGET_NOTE_BOOK
 EZ_WIDGET_NB_PAGE                23
 EZ_WIDGET_NOTEBOOK_PAGE          EZ_WIDGET_NB_PAGE
 EZ_WIDGET_NW_LABEL               24
 EZ_WIDGET_PANE_HANDLE            25
 EZ_WIDGET_ARROW_BUTTON           26
 EZ_WIDGET_EXECUTOR               27
 EZ_WIDGET_OPTIONAL_ENTRY         28
 EZ_WIDGET_OPTION_ENTRY           EZ_WIDGET_OPTIONAL_ENTRY
 EZ_WIDGET_FILE_SELECTOR          29
 EZ_WIDGET_ITREE                  30
 EZ_WIDGET_TREE                   31
 EZ_WIDGET_LIST_TREE              EZ_WIDGET_TREE
 EZ_WIDGET_IFANCY_LIST_BOX        32
 EZ_WIDGET_IFANCY_LISTBOX         EZ_WIDGET_IFANCY_LIST_BOX 
 EZ_WIDGET_FANCY_LIST_BOX         33
 EZ_WIDGET_FANCY_LISTBOX          EZ_WIDGET_FANCY_LIST_BOX
 EZ_WIDGET_IWORK_AREA             34
 EZ_WIDGET_IWORKAREA              EZ_WIDGET_IWORK_AREA
 EZ_WIDGET_WORK_AREA              35
 EZ_WIDGET_WORKAREA               EZ_WIDGET_WORK_AREA
 EZ_WIDGET_RAW_XWINDOW            36
 EZ_WIDGET_TEAR_OFF               37
 EZ_WIDGET_ROW_COLUMN             38
 EZ_WIDGET_GRID_BAG               EZ_WIDGET_ROW_COLUMN
 EZ_WIDGET_LCD                    39
 EZ_WIDGET_LED                    40
 EZ_WIDGET_STATUS_METER           41
 EZ_WIDGET_MENU_BAR               42
 EZ_WIDGET_ITERM                  43
 EZ_WIDGET_TERM                   44
 EZ_WIDGET_HORIZONTAL_RULER       45
 EZ_WIDGET_VERTICAL_RULER         46
 EZ_WIDGET_SCROLL_BUTTON          47
 EZ_WIDGET_SPIN_BUTTON            48
 EZ_WIDGET_LOCATOR                49
 EZ_WIDGET_DIAL                   50
 EZ_WIDGET_GRADIENT_BAR           51
 EZ_WIDGET_HISTOGRAM              52
 EZ_WIDGET_SEPARATOR              53
 EZ_WIDGET_SPREAD_SHEET           54
 EZ_WIDGET_THUMB_WHEEL            55

 EZ_WIDGET_F56                    56
 EZ_WIDGET_F57                    57
 EZ_WIDGET_F58                    58
 EZ_WIDGET_F59                    59
 EZ_WIDGET_F60                    60
 EZ_WIDGET_F61                    61
 EZ_WIDGET_F62                    62
 EZ_WIDGET_F63                    63
 EZ_WIDGET_F64                    64
 EZ_WIDGET_F65                    65
 EZ_WIDGET_F66                    66
 EZ_WIDGET_F67                    67
 EZ_WIDGET_F68                    68
 EZ_WIDGET_F69                    69
 EZ_WIDGET_F70                    70
 EZ_WIDGET_F71                    71
 EZ_WIDGET_F72                    72
 EZ_WIDGET_F73                    73
 EZ_WIDGET_F74                    74
 EZ_WIDGET_F75                    75
 EZ_WIDGET_F76                    76
 EZ_WIDGET_F77                    77
 EZ_WIDGET_F78                    78
 EZ_WIDGET_F79                    79
 EZ_WIDGET_F80                    80
 EZ_WIDGET_F81                    81
 EZ_WIDGET_F82                    82
 EZ_WIDGET_F83                    83
 EZ_WIDGET_F84                    84
 EZ_WIDGET_F85                    85
 EZ_WIDGET_F86                    86
 EZ_WIDGET_F87                    87
 EZ_WIDGET_F88                    88
 EZ_WIDGET_F89                    89
 EZ_WIDGET_F90                    90
 EZ_WIDGET_F91                    91
 EZ_WIDGET_F92                    92
 EZ_WIDGET_F93                    93
 EZ_WIDGET_F94                    94
 EZ_WIDGET_F95                    95
 EZ_WIDGET_F96                    96
 EZ_WIDGET_F97                    97
 EZ_WIDGET_F98                    98
 EZ_WIDGET_F99                    99
 EZ_WIDGET_F100                    100
 EZ_WIDGET_F101                    101
 EZ_WIDGET_F102                    102
 EZ_WIDGET_F103                    103
 EZ_WIDGET_F104                    104
 EZ_WIDGET_F105                    105
 EZ_WIDGET_F106                    106
 EZ_WIDGET_F107                    107
 EZ_WIDGET_F108                    108
 EZ_WIDGET_F109                    109
 EZ_WIDGET_F110                    110
 EZ_WIDGET_F111                    111
 EZ_WIDGET_F112                    112
 EZ_WIDGET_F113                    113
 EZ_WIDGET_F114                    114
 EZ_WIDGET_F115                    115
 EZ_WIDGET_F116                    116
 EZ_WIDGET_F117                    117
 EZ_WIDGET_F118                    118
 EZ_WIDGET_F119                    119
 EZ_WIDGET_F120                    120
 EZ_WIDGET_F121                    121
 EZ_WIDGET_F122                    122
 EZ_WIDGET_F123                    123
 EZ_WIDGET_F124                    124

 EZ_WIDGET_POPUP_MENU              125
 EZ_WIDGET_MENU                    EZ_WIDGET_POPUP_MENU
 EZ_WIDGET_ITOPLEVEL               126
 EZ_WIDGET_ICON                    EZ_WIDGET_ITOPLEVEL         
 EZ_WIDGET_FREE_LABEL              EZ_WIDGET_ITOPLEVEL           
 EZ_WIDGET_EMBEDER                 127

;
 EZ_TREE                           0
 EZ_LIST_TREE                      1
 EZ_BRANCHED_LIST_TREE             2

; ***                   LED Widget actions                            ***

 EZ_LED_SCROLL_LEFT        1
 EZ_LED_SCROLL_RIGHT       2
 EZ_LED_SCROLL_UP          3
 EZ_LED_SCROLL_DOWN        4
 EZ_LED_SCROLL_CENTE_H     5
 EZ_LED_SCROLL_CENTER_V    6
 EZ_LED_SWEEP_LEFT         7
 EZ_LED_SWEEP_RIGHT        8
 EZ_LED_SWEEP_UP           9
 EZ_LED_SWEEP_DOWN         10
 EZ_LED_SWEEP_CENTER       11
 EZ_LED_SWEEP_CENTER_V     12
 EZ_LED_BOUNCE             13
 EZ_LED_BOUNCE_V           14
 EZ_LED_SHOW               15
 EZ_LED_BLINK              16
 EZ_LED_BLINK_SCROLL_LEFT  17
 EZ_LED_BLINK_SCROLL_RIGHT 18
 EZ_LED_SLEEP              19

; ***                   Widget Arrangements                           ***
;
; *  Orientation

 EZ_HORIZONTAL              1
 EZ_HORIZONTAL_LEFT         2
 EZ_HORIZONTAL_RIGHT        3
 EZ_HORIZONTAL_CENTER       4
 EZ_VERTICAL                5
 EZ_VERTICAL_TOP            6
 EZ_VERTICAL_BOTTOM         7
 EZ_VERTICAL_CENTER         8

; * Filling

 EZ_FILL_NONE               0
 EZ_FILL_HORIZONTALLY       1
 EZ_FILL_VERTICALLY         2
 EZ_FILL_BOTH               3

; * Allignment
 
 EZ_CENTER_ALIGNED          0
 EZ_LEFT_ALIGNED            1
 EZ_RIGHT_ALIGNED           2
 EZ_TOP_ALIGNED             3
 EZ_BOTTOM_ALIGNED          4

; * Label position, justification etc.

 EZ_CENTER                  0
 EZ_LEFT                    1
 EZ_RIGHT                   2
 EZ_TOP                     3
 EZ_BOTTOM                  4
 EZ_TOP_LEFT                5
 EZ_TOP_RIGHT               6
 EZ_BOTTOM_LEFT             7
 EZ_BOTTOM_RIGHT            8
 EZ_LEFT_2_RIGHT            9

 EZ_UP                      10
 EZ_DOWN                    11
 EZ_LEFT_RIGHT              12
 EZ_UP_DOWN                 13


; ***                   Indicator Types                               ***

 EZ_EMPTY_INDICATOR          0
 EZ_SQUARE_INDICATOR         1
 EZ_SUNKEN_SQUARE_INDICATOR  2
 EZ_DIAMOND_INDICATOR        3
 EZ_SUNKEN_DIAMOND_INDICATOR 4
 EZ_CIRCLE_INDICATOR         5
 EZ_CHECK_INDICATOR          6
 EZ_RECTANGLE_INDICATOR      7
 EZ_MENU_BUTTON_INDICATOR    8
 EZ_TICK_INDICATOR           9
 EZ_BALL_INDICATOR           10


; ***       Arrow Types (ArrowButton, Label, NWLabel)                 ***
 

 EZ_UP_TRIANGLE          1
 EZ_DOWN_TRIANGLE        2
 EZ_LEFT_TRIANGLE        3
 EZ_RIGHT_TRIANGLE       4
 EZ_UP_ARROW             5
 EZ_DOWN_ARROW           6
 EZ_LEFT_ARROW           7
 EZ_RIGHT_ARROW          8

 EZ_UP_TRIANGLE_BAR      9
 EZ_DOWN_TRIANGLE_BAR    10 
 EZ_LEFT_TRIANGLE_BAR    11
 EZ_RIGHT_TRIANGLE_BAR   12

 EZ_UP_ARROW_BAR         13
 EZ_DOWN_ARROW_BAR       14
 EZ_LEFT_ARROW_BAR       15
 EZ_RIGHT_ARROW_BAR      16

 EZ_DOUBLE_LEFT_TRIANGLE  17
 EZ_DOUBLE_RIGHT_TRIANGLE 18
 EZ_DOUBLE_UP_TRIANGLE    19
 EZ_DOUBLE_DOWN_TRIANGLE  20

 EZ_LEFT_RIGHT_ARROW     21
 EZ_UP_DOWN_ARROW        22

 EZ_LEFT_BAR_TRIANGLE    23
 EZ_RIGHT_BAR_TRIANGLE   24
 EZ_UP_BAR_TRIANGLE      25
 EZ_DOWN_BAR_TRIANGLE    26

 EZ_ELLIPSE              27
 EZ_RECTANGLE            28

 EZ_UP_TRIANGLE_UP       29
 EZ_UP_TRIANGLE_DOWN     30
 EZ_DOWN_TRIANGLE_UP     31
 EZ_DOWN_TRIANGLE_DOWN   32
 EZ_LEFT_TRIANGLE_UP     33
 EZ_LEFT_TRIANGLE_DOWN   34
 EZ_RIGHT_TRIANGLE_UP    35
 EZ_RIGHT_TRIANGLE_DOWN  36
; Drawing Mode #DM Begin/End
 EZ_LINES           1
 EZ_LINE_STRIP      2
 EZ_LINE_LOOP       3
 EZ_TRIANGLES       4
 EZ_TRIANGLE_STRIP  5
 EZ_TRIANGLE_FAN    6
 EZ_QUADS           7
 EZ_QUAD_STRIP      8
 EZ_POLYGON         9
 EZ_POINTS          10
 EZ_TMESH           11
 EZ_QMESH           12
;  For Enable()/Disable()
 EZ_LIGHT0          0
 EZ_LIGHT1          1
 EZ_LIGHT2          2
 EZ_LIGHT3          3
 EZ_LIGHT4          4
 EZ_LIGHT5          5
 EZ_LIGHT6          6
 EZ_LIGHT7          7
 EZ_LIGHTING        8
 EZ_NORMALIZE       9
 EZ_COLOR_MATERIAL  10
 EZ_CULL_FACE       11
 EZ_FOG             12
 EZ_DEPTH_TEST      13
 EZ_DITHER          14

; #PM

 EZ_FILL  0
 EZ_LINE  1
 EZ_POINT 2

;gmat and light

 EZ_FRONT                  0
 EZ_BACK                   1
 EZ_FRONT_AND_BACK         2
 EZ_EMISSION               1
 EZ_AMBIENT                2
 EZ_DIFFUSE                3
 EZ_SPECULAR               4
 EZ_SHININESS              5
 EZ_AMBIENT_AND_DIFFUSE    6
 EZ_COLOR_INDICES          7
 EZ_POSITION               8
 EZ_SPOT_DIRECTION         9
 EZ_SPOT_EXPONENT          10
 EZ_SPOT_CUTOFF            11
 EZ_CONSTANT_ATTENUATION   12
 EZ_LINEAR_ATTENUATION     13
 EZ_QUADRATIC_ATTENUATION  14

; For glmodel

 EZ_LIGHT_MODEL_AMBIENT      1
 EZ_LIGHT_MODEL_LOCAL_VIEWER 2
 EZ_LIGHT_MODEL_TWO_SIDE     3

; gsmodel shading model
 EZ_FLAT   0
 EZ_SMOOTH 1

 ; Button and Pointer events types.
 EZ_LEFT_BUTTON               1
 EZ_MIDDLE_BUTTON             2
 EZ_RIGHT_BUTTON              4
 EZ_BUTTON1                   EZ_LEFT_BUTTON
 EZ_BUTTON2                   EZ_MIDDLE_BUTTON
 EZ_BUTTON3                   EZ_RIGHT_BUTTON

 EZ_REDRAW                    256
 EZ_RESIZE                    257
 EZ_LEFT_BUTTON_PRESS         258
 EZ_MIDDLE_BUTTON_PRESS       259
 EZ_RIGHT_BUTTON_PRESS        260
 EZ_LEFT_BUTTON_RELEASE       261  
 EZ_MIDDLE_BUTTON_RELEASE     262
 EZ_RIGHT_BUTTON_RELEASE      263
 EZ_POINTER_MOTION            264
 EZ_KEY_PRESS                 265
 EZ_ENTER_WINDOW              266
 EZ_LEAVE_WINDOW              267

 EZ_CLIENT_MESSAGE            268
 EZ_PROPERTY_NOTIFY           269

 EZ_BUTTON1_PRESS             EZ_LEFT_BUTTON_PRESS
 EZ_BUTTON2_PRESS             EZ_MIDDLE_BUTTON_PRESS
 EZ_BUTTON3_PRESS             EZ_RIGHT_BUTTON_PRESS
 EZ_BUTTON1_RELEASE           EZ_LEFT_BUTTON_RELEASE
 EZ_BUTTON2_RELEASE           EZ_MIDDLE_BUTTON_RELEASE
 EZ_BUTTON3_RELEASE           EZ_RIGHT_BUTTON_RELEASE)

(require 'hex)

(a ColArr (cat (p [16 1] "#") ; From colarr in source
	       (hexdec (p [16 6] 
			  (tr (enc [16 16] (c (* 255  [  0.0  0.0  0.0  
							 1.0  1.0  1.0
							 1.0  0.0  0.0
							 1.0  0.5  0.0 
							 1.0  1.0  0.0
							 0.5  1.0  0.0
							 0.0  1.0  0.0
							 0.0  1.0  0.5  
							 0.0  1.0  1.0  
							 0.0  0.5  1.0  
							 0.0  0.0  1.0  
							 0.5  0.0  1.0  
							 1.0  0.0  1.0  
							 1.0  0.5  1.0  
							 1.0  0.5  0.5  
							 0.5  0.5  0.5]))))))))

(defun ColourString (X) (prog ((#IO 0))(aref ColArr X ())))


(setq Canvas (mkwdgt EZ_WIDGET_3D_CANVAS nil
			(list (list  EZ_ORIENTATION EZ_VERTICAL_BOTTOM)
			      (list  EZ_SIZE [512 512])
			      (list  EZ_BACKING_STORE 1))))

(defun Ginit2D () ; uses default canvas
  (gcanvas Canvas)
  (dispwdgt Canvas)
  (glinit)
  (gdisable EZ_DEPTH_TEST EZ_LIGHTING EZ_CULL_FACE )
  (proj nil)
  (gclear)
  )

(defun Ginit3D ()  ; uses default canvas
  (gcanvas Canvas)
  (dispwdgt Canvas)
  (glinit)
  (gsmodel EZ_SMOOTH)
  (proj t)
  (gclear)
  )
   
(defun FRAME () (graf(p [2 5] (aref #WN [1 2 2 1 1 3 3 4 4 3]))))
(defun DIAG  () (graf (aref #WN [3 4])) (graf (aref #WN [4 3])))
(defun AXES  () 
  (graf (p [2 2] (cat (tk 2 #WN) [0 0])))
  (graf (p [2 2] (cat [0 0] (tk -2 #WN)))))

(defun MARK (X Y) (graf (MMARK X Y)))

(defun MARK1 (X Y XO YO) 
  (let ((P (cat .5 {(- X XO) (+ X XO)} {(- Y YO) (+ Y YO)})))
    (list P (rot -1 [1 0] P))))

(defun MMARK (X Y) ; just produce mark coords
  (MARK1 X Y (* 0.01 (| (- (aref #WN 2)  (aref #WN 1))))
	 (* 0.01 (| (- (aref #WN 4)  (aref #WN 3))))))

(defun MP (X Y)
;;; make plot from list of list of vals with 1st and yth of each sublist
    (let ((D (len X))
	  (F (implod (mapcar 'car X)))
	  (V (implod (mapcar '(lambda (X) (nth Y X)) X))))
      (p (cat 2 D) (cat (l 10 F) (/ V (r 'c V)))))) ; log F, normalise V

(defun DBGRID (F) 
    (prog (#PP X J (R (- (aref #WN 4) (aref #WN 3))))
	  (setq #PP 0)
	  (setq F (rav  F))
	  (FOR I 1 (p F)
	       (setq X (l 10 (aref F I)))
	       (graf (p [2 2] (cat  (cat X X) (aref #WN [1 2]))))
	       (if (eq X (f (+ X 0.5)))
		   (prog ()
			 (graf (p [2 1] (cat X (aref #WN 3))))
			 (graf (cat (fmt (aref F I)) " Hz")))))
	  
	  (FOR I -10 10 
	       (a J (* I R))
	       (if (and (ge J (vdB (aref #WN 3)))
			(le J (vdB (aref #WN 4))))
		   (prog ()
			 (graf (p [2 2] (cat (aref #WN [1 2]) 
					     (p 2 (exp 10 (/ J 20))))))
			 (graf (p [2 1] (cat (aref #WN 1) (exp 10 (/ J 20)))))
			 (graf (cat (fmt J::1) " dB")))
		 ))))

(defun MK (X) (FOR I 0 (tk -1 (p X)) (apply MARK (explod (aref X () I)))))

(defun setwin (X) 
    (setq #WN {(r 'f (aref X 1 ()))  (r 'c (aref X 1 ()))
	       (r 'f (aref X 2 ()))  (r 'c (aref X 2 ()))}))

(defun axes () 
    (let* ((#IO 1) 
	   (XL (aref #WN 1))
	   (XH (aref #WN 2))
	   (YL (aref #WN 3))
	   (YH (aref #WN 4))
	   (XTL (/ (| (- XH XL)) 50))
	   (YTL (/ (| (- YH YL)) 50))
	   (XLI (/ XL 5))
	   (XHI (/ XH 5))
	   (YLI (/ YL 5))
	   (YHI (/ YH 5))
	   (XO (if (and (lt XL 0) (gt XH 0)) 0 XL))
	   (YO (if (and (lt YL 0) (gt YH 0)) 0 YL))
	   XLT XHT YLT YHT)
     (FOR I 1 5
	(a XLT (* XLI I) XHT (* XHI I) YLT (* YLI I) YHT (* YHI I))
	(graf 
	 (list 
	  (p [2 2] {XLT XLT (- YO YTL) (+ YO YTL)}) (vtr {XLT YO}) (fmt XLT)
	  (p [2 2] {XHT XHT (- YO YTL) (+ YO YTL)}) (vtr {XHT YO}) (fmt XHT)
	  (p [2 2] {(- XO XTL) (+ XO XTL) YLT YLT}) (vtr {XO YLT}) (fmt YLT)
	  (p [2 2] {(- XO XTL) (+ XO XTL) YHT YHT}) (vtr {XO YHT}) (fmt YHT)
	  )))
      (graf (p [2 2] {XL XH YO YO}))
      (graf (p [2 2] {XO XO YL YH}))
      (unless (and (zerop XO) (zerop YO)) (graf (vtr [0 0])) (graf (fmt 0::0)))))

(setq plot_clear t)

(defun plot (L) ; Note #DM and #WN are free in plot
  (prog
   ((#IO 1) (#PN 1) (#OP [0 0 0])  (#NF [1 -1]) X Wmin Wmax
    (Cevent ; Canvas event
     (fun '(lambda (E)
	     (let ((Widget (car  E))
		   (Event  (cadr E)))
	       (cond  ((eq Event EZ_BUTTON1_PRESS)
		       (print (list (caddr E) (car (cdddr E)))))
		      ))))))
   (cond ((atom L) (a L (list L))))
   (gcanvas Canvas)
   (dispwdgt Canvas)
   (glinit)
   (gdisable EZ_DEPTH_TEST  EZ_LIGHTING EZ_CULL_FACE)
   (cfgwdgt Canvas  (list (list EZ_EVENT_HANDLER Cevent)))
   (a L
      (mapcar
       '(lambda (X)
	       (if (onep (rank X))
		   (setq X (p {2 (p X)}  {(i (p X) ) X} )))
	       (if (or (ne 2 (rank X)) (ne 2 (aref (p X) 1)))
		   (error "bad shape"))
	       (a Wmin (cons {(r 'f (aref X 1 ()))  (r 'f (aref X 2 ()))} Wmin))
	       (a Wmax (cons {(r 'c (aref X 1 ()))  (r 'c (aref X 2 ()))} Wmax))
	       X)
       L))
   (a #WN (rav (tr (cat .5 (r 'f 1 (implod .5 Wmin))        ; window to fit all
			(r 'c 1 (implod .5 Wmax))))))
   (print #WN)
   (proj nil)
   (if plot_clear (gclear))
   (axes)
   (mapc '(lambda (X) (a #PN (+ 1 (| 15 #PN))) (graf X)) L)))


(defun Plot (X) ; plot a list of vectors
  (let ((#PN 1) (#DM 2) (#WN) (#OP [0 0 0]) (#NF [1 -1]) H L)
    (gcanvas Canvas)
    (gdisable EZ_DEPTH_TEST EZ_LIGHTING EZ_CULL_FACE) 
    (setq H (max (implod (mapcar max X))))
    (setq L (min (implod (mapcar min X))))
    (print (fmt "Window is " (setq #WN {-1 1 L H})))
    (proj nil)
    (dispwdgt Canvas)
    (glinit)
    (if plot_clear (gclear))
    (axes)
    (mapc '(lambda (X) (graf X) (incr #PN)) X)))


(de ORP (R) "Order points by closest neighbour"
    ;; R a 2xN array of unordered points of a polygon
    (let* ((#IO 1) 
	   (L (list 1)) (K 1) T (N (scalar (tk -1 (p R))))
	   (D '(lambda (I) (sqr (o '- (aref R I ()) (aref R I ())))))
	   (RR (sqrt (+ (D 1) (D 2)))) ;; array of distances between points
	   (M (r 'c (rav RR)))) ;; maximum distance
      (iset RR M (i N)) ;; make diagonal elements large
      (FOR I 2 N
	   (a T (ind (aref RR K ()) (min (aref RR K ())))) ;; index of nearest
	   (aset RR M T K) ;; remove point from consideration
	   (a K T)
	   (nconc L (list K)))
      (aref R () (implod L))))

(defun Proj (OP D X)
  (let* ((xc '(lambda (x) (aref (rav x) 1)))
	 (yc '(lambda (x) (aref (rav x) 2)))
	 (zc '(lambda (x) (aref (rav x) 3)))
	 (phi (- pi/2 (atan2 (zc OP) (sqrt (+ (sqr (xc OP)) (sqr (yc OP)))))))
	 (theta (atan2 (yc OP) (xc OP)))
	 (mu (sqrt (r '+ (sqr OP))))
	 (st (sin theta))
	 (ct (cos theta))
	 (sp (sin phi))
	 (cp (cos phi))
	 (tv (p [4 4] (implod (list (- st) (- (* ct cp)) (- (* ct sp)) 0
				    ct     (- (* st cp)) (- (* st sp)) 0
				    0      sp            (- cp)        0
				    0      0             mu            1))))
	 (tp (p [4 4] [1 0 0 0 0])))
    (aset tp (/ D) 3 4) ; perspective xform
;    (print tp)
;    (print tv)
;    (print D)
;    (print X)
    (setq X (. '+ '* (tr tv) X))
    (p (cat 2 (tk -1 (p X))) (cat (/ (* (aref X 1 ()) D) (aref X 3 ()))
				  (/ (* (aref X 2 ()) D) (aref X 3 ()))))))

(defun Id (N) (p {N N} {1 (p N 0)}))

(defun Rot2D (V A P) "
    V is a matrix of the 2 element column vectors of the endpoints of V
    A is the angle of rotation
    P is the column vector of the point around which to rotate V"
  (let* ((D {P P}) (S (sin A)) (C (cos A)) (M (p [2 2] {C S (- S) C})))
    (+ P (. '+ '* M (- V P)))))
  
(defun mkrotmat (AXIS ANGLE) ;build rotation matrix around std axis
  (let ((C (% 2 ANGLE)) (S (% 1 ANGLE)))
	(p [4 4] (implod
		  (cond ((equal AXIS 'Z) (list
					    C  S  0 0 
					 (- S) C  0 0
					    0  0  1 0
					    0  0  0 1))
			((equal AXIS 'Y) (list
					  C   0 (- S) 0
					  0   1    0  0
					  S   0    C  0
					  0   0    0  1))
			((equal AXIS 'X) (list
					  1    0  0 0
					  0    C  S 0
					  0 (- S) C 0
					  0    0  0 1))
			(t (error "mkrotmat:BadAxis")))))))


(defun RotMat (Angle V)
   (prog ((Anorm (r '+ (sqr V))) (#IO 1)  Norm
	  C C1 S X Y Z XX YY ZZ XY XZ YZ XYC1 XZC1 YZC1 XS YS ZS M)
	 (cond ((zerop Anorm) (return (Id 4)))) ;can't rotate around zero len v
	 (setq Norm (/ V Anorm)
	       C  (% 2 Angle)
	       C1 (- 1 C)
	       S  (% 1 Angle)
	       X     (aref V 1)     Y     (aref V 2)     Z     (aref V 3)
	       XX    (sqr X)        YY    (sqr Y)        ZZ    (sqr Z)
	       XY    (* X Y)        XZ    (* X Z)        YZ    (* Y Z)
	       XYC1  (* XY C1)      XZC1  (* XZ C1)      YZC1  (* YZ C1)
	       XS    (* X S)        YS    (* Y S)        ZS    (* Z S)
      
	       M (p [4 4] (implod (list
	       (+ C (* XX C1))   (+ XYC1 ZS)       (- XZ YS)        0
	       (- XYC1 ZS)       (+ C (* YY C1))   (+ YZC1 XS)      0
	       (+ XZC1 YS)       (- YZC1 XS)       (+ C (* ZZ C1))  0
	       0                 0                 0                1
	       ))))))

(defun EZProj (T X) 
  (princ (fmt "#OP " #OP::1 " #NF " #NF " NF " NF "T " T))
  (terpri)
  (. '+ '* (mktransmat T) X))

(defun CircOrd (X Y D) 
  { (* D (% 2 Y) (% 2 X))
    (* D (% 1 X))
    (* D (% 2 X) (% 1 Y))
    1 } )

(defun mkmesh (X Y D) 
 (let ((IL (p X)) (JL (p Y)) A B C)
  (setq A (p (implod (list 4 IL JL)) 0))
  (FOR I 1 IL 
    (FOR J 1 JL 
      (setq B (CircOrd (aref X I) (aref Y J) 1))
      (setq B (* B (>= (| B) #CT))) ; get rid of very small values
      (setq C (vtr B))
      (cond ((neql B C) (print (setq BUG (list B C I J))) (prin0 "shit") (wait 1)))
      (aset A B () I J)))
  A))

(defun mkgrid (N) 
  (let ((A (p {N N} (/ (i N) N))))
        (cat 1 (p {2 N N} (cat 1 (tr A) A))
	     (p {2 N N} (cat 1 (p {N N} 0) (p {N N} 1))))))

(defun GP () ; perspective demo
	(let ((G (list (mkmesh (i (% [-.5 .05 .5])) (% (i [0 0.05 2])) 2)))
	      (#DM EZ_QMESH) (#OP [2 2 2]))
	(genable EZ_CULL_FACE)
	(gdisable EZ_DEPTH_TEST)
	(proj t)
	(Gplay G)))

(defun GO (F) ;ortho
  (let ((#DM 2)
	(#OP [0 0 1])
	(#NF [1 -1])
	(#WN [-2 2 -2 2]))
    (proj nil)
    (genable EZ_CULL_FACE)
    (gdisable EZ_DEPTH_TEST)
    (F) 'ok))

(a GplayHook nil)
(defun Gplay (X) ; fool with X 
  (prog* (
	 (XFORM '(lambda (X Y) (mapcar '(lambda (Z) (if (nump Z) (. '+ '* Y Z) Z)) X)))
	 (SHOW  '(lambda (X) 
		   (let ((#OP O) (#NF NF)
			 (XM (mm (mm (mktransmat T) M) A))) ;;; apply translation last
		     ;(princl (fmt "#OP " #OP::1 " #NF " #NF " NF " NF " T " T))
		     (proj t) ;; need this if #OP or #NF have changed
		     (graf (XFORM X XM))
		     (gupdate))))
	 (INC (/ 8))
	 (ANG (% INC))
	 (O #OP)
	 (NF #NF)
	 (OINC [0 0 0.5])
	 (XINC (* INC [2 0 0]))
	 (YINC (* INC [0 2 0]))
	 (ZINC (* INC [0 0 2]))
	 (NINC [0.5 0])
	 (FINC [0   0.5])
	 (Z1 (mkrotmat 'Z ANG))
	 (Y1 (mkrotmat 'Y ANG))
	 (X1 (mkrotmat 'X ANG))
	 (z1 (mkrotmat 'Z (- ANG)))
	 (y1 (mkrotmat 'Y (- ANG)))
	 (x1 (mkrotmat 'X (- ANG)))
	 (T [0 0 0]);; Translation vector
	 (M (Id 4)) ;; Rotation
	 (A (Id 4)) ;; Amplitude
	 (R 0)
	 (D 32) ;; number of steps for R (repeat)
	 (#GB t) ;; smooth graphics
	 C (Run t))
    (gclear) 
    (print "SASFA")
    (SHOW X)
    (while Run
      (if (and GplayHook (nump (wait .02 nil (list #RF)))) (GplayHook X)
	(setq C (getc))
	;(print (list 'Got C))
	(if (or (null C) (eq C "q")) (a Run nil)
	  (cond ((eq C "X")  (setq M (mm M X1)))
		((eq C "Y")  (setq M (mm M Y1))) 
		((eq C "Z")  (setq M (mm M Z1))) 
		((eq C "x")  (setq M (mm M x1))) 
		((eq C "y")  (setq M (mm M y1))) 
		((eq C "z")  (setq M (mm M z1)))
		((eq C "A")  (setq O (+ O XINC))) 
		((eq C "a")  (setq O (- O XINC))) 
		((eq C "B")  (setq O (+ O YINC))) 
		((eq C "b")  (setq O (- O YINC))) 
		((eq C "C")  (setq O (+ O ZINC))) 
		((eq C "c")  (setq O (- O ZINC)))
		((eq C "D")  (setq D (+ D 2))) 
		((eq C "d")  (setq D (- D 2)))
		((eq C "N")  (setq NF (+ NF NINC)))
		((eq C "n")  (setq NF (- NF NINC)))
		((eq C "F")  (setq NF (+ NF FINC))) 
		((eq C "f")  (setq NF (- NF FINC)))
		((eq C "I")  (setq T (+ T XINC)))
		((eq C "i")  (setq T (- T XINC)))
		((eq C "J")  (setq T (+ T YINC)))
		((eq C "j")  (setq T (- T YINC)))
		((eq C "K")  (setq T (+ T ZINC)))
		((eq C "k")  (setq T (- T ZINC)))
		((eq C "M")  (setq M (* M 1.1)))
		((eq C "m")  (setq M (* M (/ 1.1))))
		((eq C "T")  (aset A (+ (aref A 3 3) .2) 3 3))
		((eq C "t")  (aset A (- (aref A 3 3) .2) 3 3))
		((eq C "L")  (genable EZ_LIGHTING))
		((eq C "l")  (gdisable EZ_LIGHTING))
		((eq C "0")  (setq #PM 0))
		((eq C "1")  (setq #PM 1))	
		((eq C "2")  (setq #PM 2))
		((eq C "P")  (genable EZ_CULL_FACE))
		((eq C "p")  (gdisable EZ_CULL_FACE))
		((eq C "S")  (gsmodel EZ_FLAT))
		((eq C "s")  (gsmodel EZ_SMOOTH))
		((eq C "r") (princ "enter repcount ") (setq R (read)) (rclr)
		 (let ((A (Id 4)) (A1 (aref A 3 3)) ;; local amplitude xform
		       (Amps (% 2 (% (/ (- (i D) #IO) (/ D 2)))))) ;; cos over 2pi in 12 steps
		   (proj t)
		   (a Amps (* Amps A1))
		   (FOR I 1 R
			(FOR J 1 D
			     (aset A (aref Amps J) 3 3)
			     (gclear) (SHOW X) (wait .05)))))
		((eq C "z") (princ "enter filename ") 
		 (gsave Canvas {(read) ".ppm"}))
		((equal C (chr 27)) (break))
		(t (print (list 'got C (num C) '?))))))
	  (gclear) (SHOW X))
   ))

(defun mm  (X Y) (. '+ '* X Y))

(defun Normalise (V) (/ V (sqrt (r '+ (sqr V)))))
(defun Norm   (X) (let ((S (r 'c (| X)))) (/ X S)))
(defun Norm2 (X)
  (let ((S X)) (while (gt (rank S) 0) (a S (r 'c (| S))))
  (/ X S)))


(defun CrossProd (U V) 
  (implod (list 
	   (- (* (aref U 2) (aref V 3)) (* (aref U 3) (aref V 2)))
	   (- (* (aref U 3) (aref V 1)) (* (aref U 1) (aref V 3))) 
	   (- (* (aref U 1) (aref V 2)) (* (aref U 2) (aref V 1))))))
 
(defun mktransmat (T) ; homogenous translation matrix
  (let ((M (Id 4))) (aset M T (i 3) 4) M))
  

(defun MkProj (Eye Focus T A R)
  (let* ((P (Id 4))
	 (#OP Eye)
	 (#OF Focus))
    (proj t)
    (mm P (mm (mktransmat T) (mkrotmat A R)))))

(defun Graf (X) (gclear) (graf X))

(defun PL (Eye Focus T A R X) 
  (prog ((Proj (MkProj Eye Focus T A R)))
	(Graf (mapcar '(lambda (X)  (tk (cat 3 (tk -1 (p X))) (mm Proj X))) X))))

(defun Rotate (X Axis Angle) 
;  (print (= (RotMat Angle Axis) (mkrotmat (k Axis "XYZ") Angle)))
  (mm (tr (RotMat Angle Axis)) X))

(defun Translate (X T) (mm (mktransmat T) X))

(defun GPO (Obj) 
  (gon)
  (prog ((AX "XYZ") (#WN [-2 2 -2 2]) AXIS)
	(FOR I 1 3
	     (FOR J 0 10
		  ;; (print (list (aref AX I) (/ (% J) 20)))
		  (setq AXIS (aref AX I))
		  (PL [2 2 2] [0 0 1] [0 0 0] AXIS (/ (% J) 10) Obj)
		; (wait 0.5)
		  ))))


(not (setq Cube (let ((Face (p [4 8] ;square CW viewed from z > 1
			  [-1 -1  1  1 -1  1  1 -1 ; x
			   -1  1  1 .9  1  1 -1 -1 ; y
			    1  1  1  1  1  1  1  1 ; z
			    1  1  1  1  1  1  1  1])))
	     (append (mapcar '(lambda (X) (Rotate Face [1 0 0] X)) 
			     (explod (% [0 .5 1 1.5])))
		     (mapcar '(lambda (X) (Rotate Face [0 1 0] X)) 
			     (explod (%  [.5 1.5 ])))))))

(defun Circ (R P N) (+ P (* R (o '% [2 1] (* (i N) (/ (% 2) N))))))

(defun SPatch (LA LO DA DO D) (print (tr (p [4 3] ;; Sphere patch
  (implod (mapcar 
	   '(lambda (X) { (* D (cos (cadr X)) (cos (car X)))
	                  (* D (sin (car X)))
			  (* D (cos (car X)) (sin (cadr X))) })
	     (list (list LA LO) 
		   (list (+ LA DA) LO)
		   (list (+ LA DA) (+ LO DO))
		   (list LA (+ LO DO)))))))))

(defun sstrip (Longitude) ; S and D free 
  (mapcar '(lambda (Latitude) (SPatch Latitude Longitude (% S) (% S) D))
           (explod (% (i (implod (list -.5 S .5)))))))

(defun mksphere (D S) (mapcan sstrip (explod (% (i (implod (list 0 S 2)))))))

(defun mkpoly (N) 
  (let ((P (% (i { 0 (/ 2 N) (- 2 (/ N))}))))
  (p {4 N} {(% 1 P) (% 2 P) (p N 0) (p N 1) } )))
				
;(a Sphere (mksphere 1 .1))

(defun sinc (X FS) (let* ((T (* FS (% X)))
			  (B (+ T (* (% 0.5) (= T 0))))
			  (C (+ T (= T 0))))
		     (/ (% 1 B) C)))

(defun SINC ()
  (prog (#PN T)
	(setq T (-  (i 256) 129))
	(FOR I 1 10 
	     (setq #PN I) 
	     (graf (sinc (/ T 8000) (/ 4000 I))))))

(defun CEXP (Z) (let ((R (exp (aref Z 1 ()))) (I (aref Z 2 ())))
		  (p (p Z) (cat (* R (% 2 I)) (* R (% 1 I))))))


(defun PS (F1 F2) 
  (prog* (I J (K 31) (L 64) (LL (* L 2))  (M 0) Y Z 
	    (R (p { 4  LL (+ 1 (* K 2))}  1))
	    (X (/ (- (i  LL) (+ L 1)) L)))
	(FOR I (- K) K
	     (setq M (+ M 1))
	     (setq J  (| (% 1 (% (/ I K)))))
	     (aset R X                                          1 () M)
	     (aset R (p LL (/ I K))                             2 () M)
	     (aset R (* J (sinc X (+ (* (- 1 J) F1) (* J F2)))) 3 () M))
	R))

(defun Rotoplots (N M) ;; (Rotoplots 6 4)
  (prog (TR L H P R (AX "XYZ")
	    (#PM EZ_LINES)
	    (#DM EZ_QUAD_STRIP)
	    (#OP [0 0 2])
	    (#OF [0 0 -1])
	    (#NF [1 -1])
	    (#WN [-1 1 -1 1]))
	(Ginit3D)
	(setq R (PS N M)) ; burn up some cycles
	(FOR I 1 3
	     (FOR J 0 40
		  ;(print (list (aref AX I) (/ (% J) 20)))
		  (setq TR (mkrotmat (aref AX I) (/ (% J) 20)))
		  (setq L (. '+ '* TR R))
		  (setq #PN (+ 1 (| 15 I)))
		  (gclear) (graf L) (gupdate) (wait .1)
		  ))))

(defun Pgraf (X)
  (genable EZ_DEPTH_TEST)
  (setq #DM EZ_POLYGON) (setq #PM 1) (setq #PN 1) (graf X))

(defun SP (X) 
  (let ((N (- (len X) 2)) (P (- (tk -1 (p (car X))) 1)) (K (i 3)))
    (FOR I 0 N
	 (FOR J 1 P
	      (graf (implod (list (aref (nth I X) K (cat J (+ J 1)))
				  (aref (nth (+ I 1) X) K(cat (+ J 1) J)))))
;	      (wait 1)
	      ))))
(defun TL () 
  (let ((G (mkmesh (% (i [-.5 .1 .5])) (% (i [0 0.25 2])) 5)))
    (gclear)
    (genable EZ_CULL_FACE EZ_DEPTH_TEST EZ_LIGHTING EZ_LIGHT0)
    (glmodel EZ_LIGHT_MODEL_LOCAL_VIEWER 1)
    (glmodel EZ_LIGHT_MODEL_TWO_SIDE 1)
    (setq #PM EZ_LINES)
    (setq #DM EZ_QMESH)
    (setq #OP [0 0 15])
    (setq #OF [0 0 0])
    (setq #NF [2 30])
    (setq #WN [-2 2 -2 2])
    (proj t)
    (light EZ_LIGHT0 EZ_POSITION [0 0 10 0])
    (gmat EZ_FRONT EZ_DIFFUSE [0.8 0.8 0 1])
    (gmat EZ_BACK  EZ_DIFFUSE [0 0.8 0.8 1])
    (Gplay (list (PS 6 4)
					; (mm (mktransmat [0 0 0]) G)
		 ))
    ))

(defun TCfn (F I S Scale)
  ;; Test complex function F on square  interval (re [-I I]) x (im [-I I])
  ;; S is the step length in the interval and Scale scales the result of
  ;; applying F over the interval.
  ;; (TCfn '! 3.5 .1 .5) gamma function
  ;; (TCfn carg 3.5 .05 .5) (carg defined in prims.al)
  ;; (TCfn '(lambda (X) (/ (sin (| X)) (| X))) 12 .2 12) circular wave
  ;; (TCfn mandc 3.5 .1 2) mandelbrot set (needs mandel.al)
  (let* ((#IO 1)
	 (N (* 2 I))
	 (R (i {0 S N}))
	 (SS (p 2 (p R))) ;; dimensions of the square in number of points
	 (M (p SS (- R I))) ;; centre the interval on 0
	 (K (+  M (* 0j1 (tr M)))) ;; complex interval
	 (Res (F K))
	 (Obj (p {4 SS} 1))
	 (#PM EZ_LINES)
	 (#DM EZ_QUAD_STRIP)
	 (#OP {0 0 (* 2 N)}) 
	 (#OF {0 0 (- N)})
	 (#NF {N (- N)})
	 (#WN {(- I) I (- I) I}))
    (aset Obj (o '% [9 11] K) [2 1] () ()) ;; split real and im to planes 2 & 1
    (aset Obj (f I (c (- I) (* Scale (re Res)))) 3 () ())
;   (aset Obj (<= 1 (dist (aref Obj 1 () ()) (aref Obj 2 () ()))) 3 () ())
    (gcanvas Canvas)
    (dispwdgt Canvas)
    (glinit)
    (genable EZ_CULL_FACE EZ_DEPTH_TEST EZ_LIGHTING EZ_LIGHT0)
    (glmodel EZ_LIGHT_MODEL_AMBIENT [1 1 1 0])
    (glmodel EZ_LIGHT_MODEL_LOCAL_VIEWER 1)
    (glmodel EZ_LIGHT_MODEL_TWO_SIDE 1) 
   
    (light EZ_LIGHT0 EZ_POSITION {0 0 N 0})
    (light EZ_LIGHT0 EZ_DIFFUSE {1 1 0 0})
    (gmat EZ_FRONT_AND_BACK EZ_DIFFUSE [0.5 0.5 0.5 1])
    (gmat EZ_BACK  EZ_DIFFUSE [0.2 0.2 0.2 1])
    (proj t)
    (gclear)
    (Gplay (list Obj))
    (hidewdgt Canvas)
))
