Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

# . $PSScriptRoot\Unset-Alias.ps1

Set-Alias g 'git'

# Shows navigable menu of all options when hitting Tab
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Autocompletion for arrow keys
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# function Prompt {
#   $ESC = [char]27
#   "`n$ESC[32m$(Get-Location)\$ESC[0m`r`n$("+"*(Get-Location -Stack).Count)$ESC[36mλ$ESC[0m "
# }

function emacs {
  $arguments = $args -join ' '
  bash -c "`$HOME/.nix-profile/bin/emacs $arguments"
}

Invoke-Expression (&starship init powershell)
