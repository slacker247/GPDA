/* Generated by wbuild
 * (generator version 3.1)
 */
#ifndef _XFWF_ENTRY_H
#define _XFWF_ENTRY_H
#include <Xfwf/Board.h>
_XFUNCPROTOBEGIN
typedef struct {
                       Boolean doit;
                       String text;
                       int length;
       } XfwfEntryCallbackStruct;

void  XfwfEntryClear(
#if NeedFunctionPrototypes
Widget
#endif
);
#ifndef XtNtext
#define XtNtext "text"
#endif
#ifndef XtCText
#define XtCText "Text"
#endif
#ifndef XtRString
#define XtRString "String"
#endif

#ifndef XtNactivate
#define XtNactivate "activate"
#endif
#ifndef XtCActivate
#define XtCActivate "Activate"
#endif
#ifndef XtRCallback
#define XtRCallback "Callback"
#endif

#ifndef XtNmodifyCallback
#define XtNmodifyCallback "modifyCallback"
#endif
#ifndef XtCModifyCallback
#define XtCModifyCallback "ModifyCallback"
#endif
#ifndef XtRCallback
#define XtRCallback "Callback"
#endif

#ifndef XtNafterModifyCallback
#define XtNafterModifyCallback "afterModifyCallback"
#endif
#ifndef XtCAfterModifyCallback
#define XtCAfterModifyCallback "AfterModifyCallback"
#endif
#ifndef XtRCallback
#define XtRCallback "Callback"
#endif

#ifndef XtNvalid
#define XtNvalid "valid"
#endif
#ifndef XtCValid
#define XtCValid "Valid"
#endif
#ifndef XtRString
#define XtRString "String"
#endif

#ifndef XtNmax
#define XtNmax "max"
#endif
#ifndef XtCMax
#define XtCMax "Max"
#endif
#ifndef XtRInt
#define XtRInt "Int"
#endif

#ifndef XtNecho
#define XtNecho "echo"
#endif
#ifndef XtCEcho
#define XtCEcho "Echo"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

#ifndef XtNfont
#define XtNfont "font"
#endif
#ifndef XtCFont
#define XtCFont "Font"
#endif
#ifndef XtRFontStruct
#define XtRFontStruct "FontStruct"
#endif

#ifndef XtNforeground
#define XtNforeground "foreground"
#endif
#ifndef XtCForeground
#define XtCForeground "Foreground"
#endif
#ifndef XtRPixel
#define XtRPixel "Pixel"
#endif

#ifndef XtNblinkingCursor
#define XtNblinkingCursor "blinkingCursor"
#endif
#ifndef XtCBlinkingCursor
#define XtCBlinkingCursor "BlinkingCursor"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

#ifndef XtNblinkInterval
#define XtNblinkInterval "blinkInterval"
#endif
#ifndef XtCBlinkInterval
#define XtCBlinkInterval "BlinkInterval"
#endif
#ifndef XtRInt
#define XtRInt "Int"
#endif

#ifndef XtNselectThreshold
#define XtNselectThreshold "selectThreshold"
#endif
#ifndef XtCSelectThreshold
#define XtCSelectThreshold "SelectThreshold"
#endif
#ifndef XtRInt
#define XtRInt "Int"
#endif

#ifndef XtNkillSelection
#define XtNkillSelection "killSelection"
#endif
#ifndef XtCKillSelection
#define XtCKillSelection "KillSelection"
#endif
#ifndef XtRBoolean
#define XtRBoolean "Boolean"
#endif

typedef struct _XfwfEntryClassRec *XfwfEntryWidgetClass;
typedef struct _XfwfEntryRec *XfwfEntryWidget;
externalref WidgetClass xfwfEntryWidgetClass;
_XFUNCPROTOEND
#endif /* _XFWF_ENTRY_H */
