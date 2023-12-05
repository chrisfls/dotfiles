New-Item -ItemType SymbolicLink `
-Path "$env:HOME\.gitconfig" `
-Target (Resolve-Path -Path "..\..\..\..\assets\git\.gitconfig")
New-Item -ItemType SymbolicLink `
-Path "$env:HOME\.gitlab.gitconfig" `
-Target (Resolve-Path -Path "..\..\..\..\assets\git\.gitlab.gitconfig")
