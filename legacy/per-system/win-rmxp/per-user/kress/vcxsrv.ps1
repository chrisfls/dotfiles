$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\VcXsrv.lnk")
$Link.TargetPath = "C:\Program Files\VcXsrv\vcxsrv.exe"
$Link.Arguments = ":0 -multiwindow -clipboard -wgl -ac"
$Link.IconLocation = "C:\Program Files\VcXsrv\vcxsrv.exe"
$Link.WorkingDirectory = "C:\Program Files\VcXsrv"
$Link.Save()
