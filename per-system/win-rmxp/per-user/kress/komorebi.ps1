$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\komorebi.lnk")
$Link.TargetPath = "$(scoop prefix komorebi)\komorebic.exe"
$Link.Arguments = "start --await-configuration"
$Link.IconLocation = "$(scoop prefix komorebi)\komorebic.exe"
$Link.WorkingDirectory = "$(scoop prefix komorebi)"
$Link.Save()
Copy-Item "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\komorebi.lnk" "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\komorebi.lnk"
Write-Host "Don't forget to setup KOMOREBI_CONFIG_HOME"
