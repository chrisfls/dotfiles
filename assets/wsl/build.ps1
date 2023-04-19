scoop reset gcc

rm git.exe -Force -EA SilentlyContinue
gcc wslrun.c -o git.exe -D EXEC_NAME="`"git`""

rm node.exe -Force -EA SilentlyContinue
gcc wslrun.c -o node.exe -D EXEC_NAME="`"node`""

rm npm.exe -Force -EA SilentlyContinue
gcc wslrun.c -o npm.exe -D EXEC_NAME="`"npm`""
