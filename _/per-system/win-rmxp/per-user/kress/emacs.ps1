scoop install emacs # ripgrep fd llvm

New-Item -ItemType SymbolicLink `
-Path "$env:HOME\.emacs.d" `
-Target (Resolve-Path -Path "..\..\..\..\home\emacs\custom")
