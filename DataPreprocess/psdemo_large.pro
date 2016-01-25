; Property Sheet Demo

;

; This program contains these sections of code:

;

; (1) Definition of the IDLitTester class.

; (2) Methods for handling the user-defined data type.

; (3) Event handlers and main widget program.

 

;================================================

; (1) Definition of the IDLitTester class.

;------------------------------------------------

; IDLitTester

;

; Superclasses:

; IDLitComponent

;

; Subclasses:

; none

;

; Interfaces:

; IIDLProperty

;

; Intrinsic Methods:

; none (because it contains no objects)

 

;------------------------------------------------

; IDLitTester::Init

 

FUNCTION IDLitTester::Init, _REF_EXTRA = _extra

 

compile_opt idl2

 

; Initialize the superclass.

IF (self->IDLitComponent::Init() ne 1) THEN $

   RETURN, 0

 

; Create IDLitTester.

; Nothing to do, for now.

 

; Register properties.

;

; * Only registered properties will show up in the property sheet.

; * <identifier> must match self.<identifier>.

 

self->RegisterProperty, 'BOOLEAN', /BOOLEAN , $

   NAME = 'Boolean', DESCRIPTION = 'TRUE or FALSE'

 

self->RegisterProperty, 'COLOR', /COLOR, $

   NAME = 'Color', DESCRIPTION = 'Color (RGB)'

 

self->RegisterProperty, 'USERDEF', USERDEF = '', $

   NAME = 'User Defined', DESCRIPTION = 'User defined property'

 

self->RegisterProperty, 'NUMBER1', /INTEGER , $

   NAME = 'Integer', DESCRIPTION = 'Integer in [-100, 100]', $

   valid_range = [-100, 100]

 

self->RegisterProperty, 'NUMBER2', /FLOAT, $

   NAME = 'Floating Point', DESCRIPTION = 'Number trackbar', $

   valid_range = [-19.0D, 6.0D, 0.33333333333333D]

 

self->RegisterProperty, 'NUMBER3', /FLOAT, $

   NAME = 'Floating Point', $

   DESCRIPTION = 'Double in [-1.0, 1.0]', $

   valid_range = [-1.0D, 1.0D]

 

self->RegisterProperty, 'LINESTYLE', /LINESTYLE, $

   NAME = 'Line Style', DESCRIPTION = 'Line style'

 

self->RegisterProperty, 'LINETHICKNESS', /THICKNESS , $

   NAME = 'Line Thickness', $

   DESCRIPTION = 'Line thickness (pixels)'

 

self->RegisterProperty, 'STRINGOLA', /STRING , $

   NAME = 'String', DESCRIPTION = 'Just some text'

 

self->RegisterProperty, 'SYMBOL', /SYMBOL , $

   NAME = 'Symbol', DESCRIPTION = 'Symbol of some sort'

 

self->RegisterProperty, 'STRINGLIST', $

   NAME = 'String List', DESCRIPTION = 'Enumerated list', $

   enumlist = ['dog', 'cat', 'bat', 'rat', 'nat', $

   'emu', 'owl', 'pig', 'hog', 'ant']

 

; Set any property values.

self->SetProperty, _EXTRA = _extra

 

RETURN, 1

END

 

;------------------------------------------------

; IDLitTester::Cleanup

 

PRO IDLitTester::Cleanup

 

compile_opt idl2

 

self->IDLitComponent::Cleanup

 

END

 

;------------------------------------------------

; IDLitTester::GetProperty

;

; Implemention for IIDLProperty interface

 

PRO IDLitTester::GetProperty, $

   boolean = boolean, $

   color = color, $

   userdef = userdef, $

   font = font, $

   number1 = number1, $

   number2 = number2, $

   number3 = number3, $

   linestyle = linestyle, $

   linethickness = linethickness, $

   stringola = stringola, $

   stringlist = stringlist, $

   symbol = symbol, $

   _REF_EXTRA = _extra

 

compile_opt idl2

 

IF (arg_present(boolean)) THEN boolean = self.boolean

IF (arg_present(color)) THEN color = self.color

IF (arg_present(userdef)) THEN userdef = self.userdef

IF (arg_present(font)) THEN font = self.font

IF (arg_present(number1)) THEN number1 = self.number1

IF (arg_present(number2)) THEN number2 = self.number2

IF (arg_present(number3)) THEN number3 = self.number3

IF (arg_present(linestyle)) THEN linestyle = self.linestyle

IF (arg_present(linethickness)) $

   THEN linethickness = self.linethickness

IF (arg_present(stringola)) THEN stringola = self.stringola

IF (arg_present(stringlist)) THEN stringlist = self.stringlist

IF (arg_present(symbol)) THEN symbol = self.symbol

 

; Superclass' properties:

IF (n_elements(_extra) gt 0) THEN $

   self->IDLitComponent::GetProperty, _EXTRA = _extra

 

END

 

;------------------------------------------------

; IDLitTester::SetProperty

;

; Implementation for IIDLProperty interface

 

PRO IDLitTester::SetProperty, $

   boolean = boolean, $

   color = color, $

   userdef = userdef, $

   font = font, $

   number1 = number1, $

   number2 = number2, $

   number3 = number3, $

   linestyle = linestyle, $

   linethickness = linethickness, $

   stringola = stringola, $

   stringlist = stringlist, $

   symbol = symbol, $

   _REF_EXTRA = _extra

 

compile_opt idl2

 

IF (n_elements(boolean) ne 0) THEN self.boolean = boolean

IF (n_elements(color) ne 0) THEN self.color = color

IF (n_elements(userdef) ne 0) THEN self.userdef = userdef

IF (n_elements(font) ne 0) THEN self.font = font

IF (n_elements(number1) ne 0) THEN self.number1 = number1

IF (n_elements(number2) ne 0) THEN self.number2 = number2

IF (n_elements(number3) ne 0) THEN self.number3 = number3

IF (n_elements(linestyle) ne 0) THEN self.linestyle = linestyle

IF (n_elements(linethickness) ne 0) THEN $

   self.linethickness = linethickness

IF (n_elements(stringola) ne 0) THEN self.stringola = stringola

IF (n_elements(stringlist) ne 0) THEN self.stringlist = stringlist

IF (n_elements(symbol) ne 0) THEN self.symbol = symbol

 

self->IDLitComponent::SetProperty, _EXTRA = _extra

 

END

 

;------------------------------------------------

; IDLitTester__Define

 

PRO IDLitTester__Define

 

compile_opt idl2, hidden

 

struct = {$

   IDLitTester, $

   inherits IDLitComponent, $

   boolean:0L, $

   color:[0B,0B,0B], $

   userdef:"", $

   number1:0L, $

   number2:0D, $

   number3:0D, $

   linestyle:0L, $

   linethickness:0L, $

   stringola:"", $

   stringlist:0L, $

   symbol:0L $

   }

 

END

 

;================================================

; (2) Methods for handling the user-defined data type.

;------------------------------------------------

; UserDefEvent

;

; This procedure is just part of the widget code for

; the user defined property.

 

PRO UserDefEvent, e

 

IF (tag_names(e, /structure_name) eq 'WIDGET_BUTTON') $

   THEN BEGIN

 

   widget_control, e.top, get_uvalue = uvalue

   widget_control, e.id, get_uvalue = numb_ness

 

   propsheet = uvalue.propsheet

   component = uvalue.component

   identifier = uvalue.identifier

 

   ; Set the human readable value.

   component->SetPropertyAttribute, $

      identifier, userdef = numb_ness

 

   ; Set the real value of the component.

   component->SetPropertyByIdentifier, identifier, numb_ness

 

   WIDGET_CONTROL, propsheet, refresh_property = identifier

   PRINT, 'Changed: ', uvalue.identifier, ': ', numb_ness

   WIDGET_CONTROL, e.top, /destroy

 

ENDIF

 

END

 

;------------------------------------------------

; GetUserDefValue

;

; Creates widgets used to modify the user defined property's

; value. The value is actually set in UserDefEvent.

 

PRO GetUserDefValue, e

 

base = WIDGET_BASE(/row, title = 'Pick a Number', $

   /modal, group_leader = e.top)

 

one = WIDGET_BUTTON(base, value = 'one', uvalue = 'oneness')

two = WIDGET_BUTTON(base, value = 'two', uvalue = 'twoness')

six = WIDGET_BUTTON(base, value = 'six', uvalue = 'sixness')

ten = WIDGET_BUTTON(base, value = 'ten', uvalue = 'tenness')

 

; We will need this info when we set the value

WIDGET_CONTROL, base, $

   SET_UVALUE = {propsheet:e.id, $

   component:e.component, $

   identifier:e.identifier}

 

WIDGET_CONTROL, base, /REALIZE

 

XMANAGER, 'UserDefEvent', base, event_handler = 'UserDefEvent'

 

END

 

;================================================

; (3) Event handlers and main widget program.

;------------------------------------------------

;

; Event handling code for the main widget program and

; the main widget program.

 

;------------------------------------------------

; prop_event

;

; The property sheet generates an event whenever the user changes

; a value. The event holds the property's identifier and type, and

; an object reference to the component.

;

; Note: widget_control, e.id, get_value = objref also retrieves an

; object reference to the component.

 

PRO prop_event, e

 

IF (e.type eq 0) THEN BEGIN ; Value changed

 

; Get the value of the property identified by e.identifier.

 

   IF (e.proptype ne 0) THEN BEGIN

 

      ; Get the value from the property sheet.

      value = widget_info(e.id, property_value = e.identifier)

 

      ; Set the component's property's value.

      e.component->SetPropertyByIdentifier, e.identifier, $

         value

 

      ; Print the change in the component's property value.

      PRINT, 'Changed', e.identifier, ': ', value

   ENDIF ELSE BEGIN

 

      ; Use alternative means to get the value.

      GetUserDefValue, e

 

   ENDELSE

 

ENDIF ELSE BEGIN ; selection changed

 

   PRINT, 'Selected: ' + e.identifier

   r = e.component->GetPropertyByIdentifier(e.identifier, value)

   PRINT, ' Current Value: ', value

 

ENDELSE

 

END

 

;------------------------------------------------

; refresh_event 

 

PRO refresh_event, e

 

WIDGET_CONTROL, e.id, get_uvalue = uvalue

 

uvalue.o->SetProperty, boolean = 0L

uvalue.o->SetProperty, color = [255, 0, 46]

uvalue.o->SetPropertyAttribute, 'userdef', userdef = "Yeehaw!"

uvalue.o->SetProperty, number1 = 99L

uvalue.o->SetProperty, number2 = -13.1

uvalue.o->SetProperty, number3 = 6.5

uvalue.o->SetProperty, linestyle = 6L

uvalue.o->SetProperty, stringola = 'It worked!'

uvalue.o->SetProperty, stringlist = 6L

uvalue.o->SetProperty, symbol = 6L

 

uvalue.o->SetPropertyAttribute, 'Number1', sensitive = 1

uvalue.o->SetPropertyAttribute, 'Number2', sensitive = 1

 

WIDGET_CONTROL, uvalue.prop, $

   REFRESH_PROPERTY = ['boolean', 'color', 'userdef', $

   'number1', 'number2', 'number3', 'linestyle', $

   'stringola', 'stringlist', 'symbol']

 

END

 

;------------------------------------------------

; reload_event

 

PRO reload_event, e

 

WIDGET_CONTROL, e.id, GET_UVALUE = uvalue

 

LoadValues, uvalue.o

 

WIDGET_CONTROL, uvalue.prop, SET_VALUE = uvalue.o

 

update_state, e.top, 1

 

END

 

;------------------------------------------------

; hide_event 

 

PRO hide_event, e

 

WIDGET_CONTROL, e.id, get_uvalue = uvalue

 

uvalue.o->SetPropertyAttribute, 'color', HIDE

 

WIDGET_CONTROL, uvalue.prop, refresh_property = 'color'

 

END

 

;------------------------------------------------

; show_event 

 

PRO show_event, e

 

WIDGET_CONTROL, e.id, get_uvalue = uvalue

 

uvalue.o->SetPropertyAttribute, 'color', hide = 0

 

WIDGET_CONTROL, uvalue.prop, REFRESH_PROPERTY = 'color'

 

END

 

;------------------------------------------------

; clear_event 

 

PRO clear_event, e

update_state, e.top, 0

WIDGET_CONTROL, e.id, GET_UVALUE = uvalue

;WIDGET_CONTROL, uvalue.prop, SET_VALUE = SET_VALUE()

END

 

;------------------------------------------------

; psdemo_large_event

;

; Handles resize events for the property sheet demo program.

 

PRO psdemo_large_event, e

 

WIDGET_CONTROL, e.id, GET_UVALUE = base

geo_tlb = WIDGET_INFO(e.id, /GEOMETRY)

 

WIDGET_CONTROL, base.prop, $

SCR_XSIZE = geo_tlb.xsize - (2*geo_tlb.xpad), $

SCR_YSIZE = geo_tlb.ysize - (2*geo_tlb.ypad)

 

END

 

;------------------------------------------------

; sensitivity_event 

;

; Procedure to test sensitizing and desensitizing

 

PRO sensitivity_event, e

 

WIDGET_CONTROL, e.id, GET_UVALUE = uvalue, GET_VALUE = value

 

IF (value eq 'Desensitize') THEN b = 0 $

ELSE b = 1

 

uvalue.o->SetPropertyAttribute, 'Boolean', sensitive = b

uvalue.o->SetPropertyAttribute, 'Color', sensitive = b

uvalue.o->SetPropertyAttribute, 'UserDef', sensitive = b

uvalue.o->SetPropertyAttribute, 'Number1', sensitive = b

uvalue.o->SetPropertyAttribute, 'Number2', sensitive = b

uvalue.o->SetPropertyAttribute, 'Number3', sensitive = b

uvalue.o->SetPropertyAttribute, 'LineStyle', sensitive = b

uvalue.o->SetPropertyAttribute, 'LineThickness', sensitive = b

uvalue.o->SetPropertyAttribute, 'Stringola', sensitive = b

uvalue.o->SetPropertyAttribute, 'Symbol', sensitive = b

uvalue.o->SetPropertyAttribute, 'StringList', sensitive = b

 

WIDGET_CONTROL, uvalue.prop, $

   refresh_property = ['Boolean', 'Color', 'UserDef', $

   'Number1', 'Number2', 'Number3', 'LineStyle', $

   'LineThickness', 'Stringola', 'Symbol', 'StringList']

 

END

 

;------------------------------------------------

; LoadValues 

 

PRO LoadValues, o

 

o->SetProperty, boolean = 1L             ; 0 or 1

o->SetProperty, color = [200, 100, 50]   ; RGB

o->SetPropertyAttribute, 'userdef', userdef = "" 

; to be set later

o->SetProperty, number1 = 42L            ; integer

o->SetProperty, number2 = 0.0            ; double

o->SetProperty, number3 = 0.1            ; double

o->SetProperty, linestyle = 4L           ; 5th item (zero based)

o->SetProperty, linethickness = 4L       ; pixels

o->SetProperty, stringola = "This is a silly string."

o->SetProperty, stringlist = 3L          ; 4th item in list

o->SetProperty, symbol = 4L              ; 5th symbol in list


END

;------------------------------------------------

; quit_event 


PRO quit_event, e

  ;WIDGET_CONTROL, e.top, DESTROY

END


;------------------------------------------------

; update_state

 

PRO update_state, top, sensitive

 

WIDGET_CONTROL, top, GET_UVALUE = uvalue

 

FOR i = 0, n_elements(uvalue.b) - 1 do $

   WIDGET_CONTROL, uvalue.b[i], sensitive = sensitive

 

END

 

;------------------------------------------------

; psdemo_large

 

PRO psdemo_large

 

; Create and initialize the component.

 

o = OBJ_NEW('IDLitTester')

 

LoadValues, o

 

; Create some widgets.

 

base = WIDGET_BASE(/COLUMN, /TLB_SIZE_EVENT, $

   TITLE = 'Property Sheet Demo (Large)')

 

prop = WIDGET_PROPERTYSHEET(base, value = o, $

   YSIZE = 13, /FRAME, event_pro = 'prop_event')

 

b1 = WIDGET_BUTTON(base, value = 'Refresh', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'refresh_event')

 

b2 = WIDGET_BUTTON(base, value = 'Reload', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'reload_event')

 

b3 = WIDGET_BUTTON(base, value = 'Hide Color', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'hide_event')

 

b4 = WIDGET_BUTTON(base, value = 'Show Color', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'show_event')

 

b5 = WIDGET_BUTTON(base, value = 'Clear', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'clear_event')

 

b6 = WIDGET_BUTTON(base, value = 'Desensitize', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'sensitivity_event')

 

b7 = WIDGET_BUTTON(base, value = 'Sensitize', $

   uvalue = {o:o, prop:prop}, $

   event_pro = 'sensitivity_event')

 

b8 = WIDGET_BUTTON(base, value = 'Quit', $

EVENT_PRO = 'quit_event')

; Buttons that can't be pushed after clearing:

b = [b1, b3, b4, b5, b6, b7] 

 

; Activate the widgets.

 

WIDGET_CONTROL, base, SET_UVALUE = {prop:prop, b:b}, /REALIZE

 

XMANAGER, 'psdemo_large', base, /NO_BLOCK

 

END