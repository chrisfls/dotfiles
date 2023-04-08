# . $PSScriptRoot\Unset-Alias.ps1

Set-Alias g 'git'

# Shows navigable menu of all options when hitting Tab
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Autocompletion for arrow keys
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

function Prompt {
  $ESC = [char]27
  "`n$ESC[32m$(Get-Location)\$ESC[0m`r`n$("+"*(Get-Location -Stack).Count)$ESC[36mλ$ESC[0m "
}

function doom {
  .\make.ps1 @args
}

$bash = "$(scoop prefix git)\bin\bash.exe"
$doom = "$env:HOME\.emacs.d\bin\doom".Replace('\', '/').Replace("D:/", "/d/")
$emacs = "$(scoop prefix emacs)\bin\emacs.exe".Replace('\', '/').Replace("D:/", "/d/")

function doom {
  $arguments = $args -join ' '
  Invoke-Expression "$bash -c `"$doom $arguments`""
}

function emacs {
  $arguments = $args -join ' '
  Invoke-Expression "$bash -c `"$emacs $arguments`""
}

Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
