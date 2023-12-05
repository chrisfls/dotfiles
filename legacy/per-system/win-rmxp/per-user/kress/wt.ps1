$target = "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json"
New-Item -ItemType SymbolicLink `
-Path $target `
-Target (Resolve-Path -Path "..\..\..\..\assets\Windows Terminal\settings.json")
