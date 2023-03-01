# copy this to the same folder as code.cmd on windows
$Env:VSCODE_DEV = $null
$Env:ELECTRON_RUN_AS_NODE = 1
& "$PSScriptRoot\..\Code.exe" "$PSScriptRoot\..\resources\app\out\cli.js" --ms-enable-electron-run-as-node $args