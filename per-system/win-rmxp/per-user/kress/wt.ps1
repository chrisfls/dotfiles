$target = "$(scoop prefix windows-terminal)\settings\settings.json"
New-Item -ItemType SymbolicLink `
-Path $target `
-Target (Resolve-Path -Path "..\..\..\..\assets\Windows Terminal\settings.json")
