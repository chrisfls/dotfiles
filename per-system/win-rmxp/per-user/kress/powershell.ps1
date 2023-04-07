$MyDocuments = [Environment]::GetFolderPath("MyDocuments")
New-Item -ItemType SymbolicLink -Path "$MyDocuments\\WindowsPowerShell\Microsoft.PowerShell_profile.ps" -Target (Resolve-Path -Path "..\..\..\..\assets\\WindowsPowerShell\Microsoft.PowerShell_profile.ps1")
