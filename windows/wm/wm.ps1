$url = "https://raw.githubusercontent.com/kress95/scoop-files/master/SeidChr.RunHiddenConsole.zip"
$output_name = "SeidChr.RunHiddenConsole"
Remove-Item -Path "$output_name.zip" -ErrorAction SilentlyContinue
Remove-Item -Path "$output_name.exe" -ErrorAction SilentlyContinue
Invoke-WebRequest $url -OutFile "$output_name.zip"
Expand-Archive -Force "$output_name.zip" -DestinationPath .

$prefix = "$(scoop prefix virtual-desktop)"

Move-Item -Force -Path "$output_name.exe" -Destination "$prefix\VirtualDesktopw.exe"
