$MyDocuments = [Environment]::GetFolderPath("MyDocuments")
New-Item -ItemType SymbolicLink -Path "$MyDocuments\PowerShell\Microsoft.PowerShell_profile.ps" -Target (Resolve-Path -Path "..\..\..\..\assets\PowerShell\Microsoft.PowerShell_profile.ps1")
