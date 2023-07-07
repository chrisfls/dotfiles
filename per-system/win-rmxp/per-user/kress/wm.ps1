$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\wm.lnk")
$Link.TargetPath = (Resolve-Path "..\..\..\..\assets\wm\wm.ahk")
$Link.Arguments = ""
$Link.IconLocation = (Resolve-Path "..\..\..\..\assets\wm\wm.ahk")
$Link.WorkingDirectory = $env:HOME
$Link.Save()
