scoop reset gcc

Remove-Item git.exe -Force -EA SilentlyContinue 2>$null
gcc wslrun.c -o git.exe -D EXEC_NAME="`"git`""

Remove-Item node.exe -Force -EA SilentlyContinue 2>$null
gcc wslrun.c -o node.exe -D EXEC_NAME="`"node`""

Remove-Item npm.exe -Force -EA SilentlyContinue 2>$null
gcc wslrun.c -o npm.exe -D EXEC_NAME="`"npm`""
