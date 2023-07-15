#Requires AutoHotkey v2.0

; Loop 9 {
;   Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /n"
; }

#1::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:0"
}

+#1::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:0 /maw"
}

#2::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:1"
}

+#2::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:1 /maw"
}

#3::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:2"
}

+#3::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:2 /maw"
}

#4::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:3"
}

+#4::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:3 /maw"
}

#5::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:4"
}

+#5::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:4 /maw"
}

#6::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:5"
}

+#6::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:5 /maw"
}

#7::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:6"
}

+#7::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:6 /maw"
}

#8::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:7"
}

+#8::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:7 /maw"
}

#9::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:8"
}

+#9::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:8 /maw"
}

#0::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:9"
}

+#0::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /gd:9 /maw"
}

#e::
{
    Run "explorer"
}

#t::
{
  Run "wt"
}

#Backspace::
{
  Run "wt --profile `"{8e510365-d81d-426c-9b28-dfb96a7a6a85}`""
}

#s::
{
    Run "explorer ms-screenclip:"
}

#+s::
{
    Run "explorer ms-screenclip:"
}

#d::
{
    Run "C:\Users\kress\AppData\Local\Programs\Microsoft VS Code\Code.exe"
}

#b::
{
    Run "D:\Users\kress\scoop\apps\thorium-avx2-np\current\thorium.exe --allow-outdated-plugins --disable-logging --disable-breakpad --enable-experimental-web-platform-features --new-canvas-2d-api"
}
