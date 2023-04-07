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
$doom = "$HOME\.emacs.d\bin\doom"
$emacs = "$(scoop prefix emacs)\bin\emacs.exe"

function doom {
  & $emacs -c "\"$doom\" @args"
}

function emacs {
  & $emacs -c "\"$emacs\" @args"
}

Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
