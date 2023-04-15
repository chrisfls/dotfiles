$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\VcXsrv.lnk")
$Link.TargetPath = "$(scoop prefix vcxsrv)\vcxsrv.exe"
$Link.Arguments = ":0 -multiwindow -clipboard -wgl -ac"
$Link.IconLocation = "$(scoop prefix vcxsrv)\vcxsrv.exe"
$Link.WorkingDirectory = "$(scoop prefix vcxsrv)"
$Link.Save()
