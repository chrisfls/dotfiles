$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Emacs.lnk")
$Link.TargetPath = "wslg"
$Link.Arguments = "-d Arch --cd `"~`" -- bash -c `"source ~/.bash_profile; `$HOME/.nix-profile/bin/emacs`""
$Link.IconLocation = "$PSScriptRoot\emacs.ico"
$Link.WorkingDirectory = "$env:HOME"
$Link.Save()
