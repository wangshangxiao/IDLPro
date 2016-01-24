PRO Progressbar_Example_Event, event

; Respond to program button events.

Widget_Control, event.id, Get_Value=buttonValue

CASE buttonValue OF

   'Start Loop (Normal)': BEGIN

         ; Create the progress bar.

      progressbar = Obj_New('progressbar',$
                             xsize=300,$
                             ysize=30,$
                              Color='red', Text='Loop Iteration 0')

         ; Place the progress bar on the display.

      progressbar -> Start

         ; Start the loop.

      count = 0
      FOR j=0, 1000 DO BEGIN

          IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.

               ; Did the user try to cancel the progress bar?

            IF progressbar->CheckCancel() THEN BEGIN
               ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
               progressbar -> Destroy ; Destroy the progress bar.
               RETURN
            ENDIF

               ; If user didn't cancel, update the progress bar. Update value
               ; must be between 0 and 100.

            progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
            count = count + 1
          ENDIF

          Wait, 0.01 ; This is where you would do something useful.
      ENDFOR

         ; Destroy the progress bar when you are finished with it.

      progressbar -> Destroy
      ENDCASE

   'Start Loop (Accept)': BEGIN

         ; Create the progress bar.

      progressbar = Obj_New('progressbar', Color='red', Text='Loop Iteration 0', /Accept)

         ; Place the progress bar on the display.

      progressbar -> Start

         ; Start the loop.

      count = 0
      FOR j=0, 1000 DO BEGIN

          IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.

               ; Did the user try to cancel the progress bar or did the user Accept?

            IF progressBar -> CheckButton(Accept=acceptButton) THEN BEGIN

               IF acceptButton THEN BEGIN

                  progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
                  ok = Dialog_Message('Final loop count is: '+ StrTrim(j,2))
                  BREAK

               ENDIF ELSE BEGIN

               ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
               progressbar -> Destroy ; Destroy the progress bar.
               RETURN
               ENDELSE

            ENDIF

               ; If user didn't cancel, update the progress bar. Update value
               ; must be between 0 and 100.

            progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
            count = count + 1
          ENDIF

          Wait, 0.01 ; This is where you would do something useful.
      ENDFOR

         ; Destroy the progress bar when you are finished with it.

      progressbar -> Destroy
      ENDCASE

   'Quit': Widget_Control, event.top, /Destroy

ENDCASE

END


PRO Progressbar_Example
tlb = Widget_Base(Column=1, Xoffset=200, Yoffset=200)
button = Widget_Button(tlb, Value='Start Loop (Normal)')
button = Widget_Button(tlb, Value='Start Loop (Accept)')
quiter = Widget_Button(tlb, Value='Quit')
Widget_Control, tlb, /Realize
XManager, 'progressbar_example', tlb, /No_Block
END