$MyDocuments = [Environment]::GetFolderPath("MyDocuments")

New-Item -ItemType SymbolicLink `
-Path "$MyDocuments\WindowsPowerShell\Unset-Alias.ps1" `
-Target (Resolve-Path -Path "..\..\..\..\assets\WindowsPowerShell\Unset-Alias.ps1")

New-Item -ItemType SymbolicLink `
-Path "$MyDocuments\WindowsPowerShell\Microsoft.PowerShell_profile.ps1" `
-Target (Resolve-Path -Path "..\..\..\..\assets\WindowsPowerShell\Microsoft.PowerShell_profile.ps1")
