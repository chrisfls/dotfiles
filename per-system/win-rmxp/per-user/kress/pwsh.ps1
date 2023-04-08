$MyDocuments = [Environment]::GetFolderPath("MyDocuments")

New-Item -ItemType SymbolicLink `
-Path "$HOME\.config\Unset-Alias.ps1" `
-Target (Resolve-Path -Path "..\..\..\..\assets\PowerShell\Unset-Alias.ps1")

New-Item -ItemType SymbolicLink `
-Path "~/.config/starship.toml" `
-Target (Resolve-Path -Path "..\..\..\..\assets\PowerShell\starship.toml")

New-Item -ItemType SymbolicLink `
-Path "$MyDocuments\PowerShell\Microsoft.PowerShell_profile.ps1" `
-Target (Resolve-Path -Path "..\..\..\..\assets\PowerShell\Microsoft.PowerShell_profile.ps1")
