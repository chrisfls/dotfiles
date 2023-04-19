scoop reset gcc

rm ./wsl/git.exe -Force
gcc wslrun.c -o ./wsl/wsl_git.exe -D EXEC_NAME="`"git`""

rm ./wsl/node.exe -Force
gcc wslrun.c -o ./wsl/node.exe -D EXEC_NAME="`"node`""

rm ./wsl/npm.exe -Force
gcc wslrun.c -o ./wsl/npm.exe -D EXEC_NAME="`"npm`""
