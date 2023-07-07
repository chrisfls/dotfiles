#Requires AutoHotkey v2.0

Loop 9 {
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /n"
}

^#1::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:0"
}

^+#1::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:0 /s:0"
}

^#2::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:1"
}

^+#2::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:1 /s:1"
}

^#3::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:2"
}

^+#3::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:2 /s:2"
}

^#4::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:3"
}

^+#4::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:3 /s:3"
}

^#5::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:4"
}

^+#5::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:4 /s:4"
}

^#6::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:5"
}

^+#6::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:5 /s:5"
}

^#7::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:6"
}

^+#7::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:6 /s:6"
}

^#8::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:7"
}

^+#8::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:7 /s:7"
}

^#9::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:8"
}

^+#9::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:8 /s:8"
}

^#0::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /s:9"
}

^+#0::
{
  Run "D:\Users\kress\scoop\apps\virtual-desktop\current\VirtualDesktopw.exe /maw:9 /s:9"
}
