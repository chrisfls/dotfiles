$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Emacs WSL.lnk")
$Link.TargetPath = "wslg"
$Link.Arguments = "-d Arch --cd `"~`" -- bash --login -c `"`$HOME/.nix-profile/bin/emacs`""
$Link.IconLocation = "$PSScriptRoot\emacs.ico"
$Link.WorkingDirectory = "$env:HOME"
$Link.Save()
