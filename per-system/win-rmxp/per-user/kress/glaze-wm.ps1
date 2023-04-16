New-Item -ItemType SymbolicLink `
-Path "$env:USERPROFILE\.glaze-wm\config.yaml" `
-Target (Resolve-Path -Path "..\..\..\..\assets\glaze-wm\config.yaml")
