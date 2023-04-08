$target = [Environment]::GetFolderPath('LocalApplicationData') + '\Microsoft\Windows Terminal\settings.json'
New-Item -ItemType SymbolicLink `
-Path $target `
-Target (Resolve-Path -Path "..\..\..\..\assets\Windows Terminal\settings.json")
