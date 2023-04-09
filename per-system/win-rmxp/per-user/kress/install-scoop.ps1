irm get.scoop.sh -outfile 'install.ps1'
$ScoopDir = 'D:\Users\kress\scoop'
$ScoopGlobalDir = 'D:\ProgramData\scoop'
.\install.ps1 -ScoopDir $ScoopDir -ScoopGlobalDir $ScoopGlobalDir -NoProxy

Write-Host "Now set:"
Write-Host "  SCOOP=`"$ScoopDir`""
Write-Host "  SCOOP_GLOBAL=`"$ScoopGlobalDir`""
