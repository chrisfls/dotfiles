Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

$PSModulePath = $env:PSModulePath
$SCOOP = $env:SCOOP
$env:PSModulePath = "$SCOOP\modules;$PSModulePath"

Set-Alias g 'git'
Set-Alias ib 'Invoke-Build'

# Shows navigable menu of all options when hitting Tab
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Autocompletion for arrow keys
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# function Prompt {
#   $ESC = [char]27
#   "`n$ESC[32m$(Get-Location)\$ESC[0m`r`n$("+"*(Get-Location -Stack).Count)$ESC[36mλ$ESC[0m "
# }

$prompt = ""

function Invoke-Starship-PreCommand {
    $current_location = $executionContext.SessionState.Path.CurrentLocation
    if ($current_location.Provider.Name -eq "FileSystem") {
        $ansi_escape = [char]27
        $provider_path = $current_location.ProviderPath -replace "\\", "/"
        $prompt = "$ansi_escape]7;file://${env:COMPUTERNAME}/${provider_path}$ansi_escape\"
    }
    $host.ui.Write($prompt)
}

function emacs {
  $arguments = $args -join ' '
  bash -c "source ~/.bash_profile; `$HOME/.nix-profile/bin/emacs $arguments"
}

Invoke-Expression (& {
    $hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
    (zoxide init --hook $hook powershell | Out-String)
})

Invoke-Expression (&starship init powershell)
