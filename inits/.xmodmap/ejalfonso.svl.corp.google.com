keycode 22 = BackSpace dead_acute
keycode 49 = grave asciitilde
keycode 65 = space underscore

clear lock
clear control
clear mod4

add control = Control_L Control_R Caps_Lock
remove mod1 = Super_L
add mod4 = Super_L Super_R
add mod3 = Hyper_L

! improve left-hand RSI
! keycode 66 = NoSymbol
keycode 66 = Control_L
keycode 37 = Super_L
keycode 108 = Super_L
keycode 105 = Hyper_L
!between R_Alt , R_Ctrl
keycode 135 = Super_L

!annoying pgup, pgdown accidental press on lenovo
keycode 112 = Control_L questiondown
keycode 117 = ediaeresis exclamdown

keycode 166 = NoSymbol
keycode 167 = NoSymbol

! bind unbound keycodes
keycode 120 = Prior NoSymbol Prior
keycode 132 = Next NoSymbol Next
keycode 118 = ntilde udiaeresis
keycode 119 = Delete emdash Delete

keycode 107 = Alt_L

keycode 133 = Control_L
remove mod4 = Control_L
add control = Control_L

! improve left-hand RSI
! keycode 64 = NoSymbol ! improve left-hand RSI
! keycode 64 = Alt_L

! Local Variables:
! comment-start: "!"
! compile-command: "xmodmap ${EMACS_COMPILATION_FILENAME}"
! End:
