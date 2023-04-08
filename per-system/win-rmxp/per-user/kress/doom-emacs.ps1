New-Item -ItemType SymbolicLink `
-Path "$env:HOME\.doom.d" `
-Target (Resolve-Path -Path "..\..\..\..\home\emacs\doom")

scoop install emacs ripgrep fd llvm

git clone https://github.com/hlissner/doom-emacs $HOME/.emacs.d
cd $HOME/.emacs.d
git checkout 22097b5a755a5b1d661e362a8441b61e37f777c9

doom install -! --no-config --no-hooks
doom sync

$url = "https://raw.githubusercontent.com/kress95/scoop-files/master/SeidChr.RunHiddenConsole.zip"
$output_name = "SeidChr.RunHiddenConsole"
Remove-Item -Path "$output_name.zip" -ErrorAction SilentlyContinue
Remove-Item -Path "$output_name.exe" -ErrorAction SilentlyContinue
Invoke-WebRequest $url -OutFile "$output_name.zip"
Expand-Archive -Force "$output_name.zip" -DestinationPath .
Move-Item -Force -Path "$output_name.exe" -Destination "$(scoop prefix git)\bin\bashw.exe"

$Wsh = New-Object -comObject WScript.Shell
$Link = $Wsh.CreateShortcut("$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Doom Emacs.lnk")
$Link.TargetPath = "$(scoop prefix git)\bin\bashw.exe"
$Link.Arguments = "-c runemacs"
$Link.IconLocation = "$(scoop prefix emacs)\bin\runemacs.exe"
$Link.WorkingDirectory = [Environment]::GetFolderPath("Desktop")
$Link.Save()
