`::Backspace
CapsLock::Control
+Space::Send {_}

#IfWinActive ahk_class ConsoleWindowClass
^v::SendInput {Raw}%clipboard%

Rwin::return
#k::!F4
