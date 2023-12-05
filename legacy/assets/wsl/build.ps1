scoop reset gcc

Remove-Item git\git.exe -Force -EA SilentlyContinue 2>$null
gcc wslrun.c -o git\git.exe -D EXEC_NAME="`"git`""

Remove-Item node\node.exe -Force -EA SilentlyContinue 2>$null
gcc wslrun.c -o node\node.exe -D EXEC_NAME="`"node`""

Remove-Item node\npm.exe -Force -EA SilentlyContinue 2>$null
gcc wslrun.c -o node\npm.exe -D EXEC_NAME="`"npm`""
