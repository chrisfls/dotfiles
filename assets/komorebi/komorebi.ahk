#NoEnv
#SingleInstance, Force
#Include %A_ScriptDir%\komorebic.lib.ahk
#Include %A_ScriptDir%\komorebi.generated.ahk

SendMode, Input
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%

ToggleMouseFollowsFocus()
WindowHidingBehaviour("cloak")
EnsureNamedWorkspaces(0, "6 7 8 9 0")
EnsureNamedWorkspaces(1, "1 2 3 4 5")
WatchConfiguration("enable")


NamedWorkspacePadding("1", 12)
NamedWorkspaceContainerPadding("1", 12)
NamedWorkspacePadding("2", 12)
NamedWorkspaceContainerPadding("2", 12)
NamedWorkspacePadding("3", 12)
NamedWorkspaceContainerPadding("3", 12)
NamedWorkspacePadding("4", 12)
NamedWorkspaceContainerPadding("4", 12)
NamedWorkspacePadding("5", 12)
NamedWorkspaceContainerPadding("5", 12)
NamedWorkspacePadding("6", 12)
NamedWorkspaceContainerPadding("6", 12)
NamedWorkspacePadding("7", 12)
NamedWorkspaceContainerPadding("7", 12)
NamedWorkspacePadding("8", 12)
NamedWorkspaceContainerPadding("8", 12)
NamedWorkspacePadding("9", 12)
NamedWorkspaceContainerPadding("9", 12)
NamedWorkspacePadding("0", 12)
NamedWorkspaceContainerPadding("0", 12)

CompleteConfiguration()

binding_mode := "normal"

;; #^ F12
#^R::
  binding_mode := "normal"
  Retile()
  return

;; #^+ F12
#^+R::
  binding_mode := "normal"
  ReloadConfiguration()
  return

#^+Escape::
  binding_mode := "normal"
  Stop()
  return

;; FOCUS WORKSPACE

#^1::
  binding_mode := "normal"
  FocusNamedWorkspace("1")
  return

;; #^ F2
#^2::
  binding_mode := "normal"
  FocusNamedWorkspace("2")
  return

;; #^ F3
#^3::
  binding_mode := "normal"
  FocusNamedWorkspace("3")
  return

;; #^ F4
#^4::
  binding_mode := "normal"
  FocusNamedWorkspace("4")
  return

;; #^ F5
#^5::
  binding_mode := "normal"
  FocusNamedWorkspace("5")
  return

;; #^ F6
#^6::
  binding_mode := "normal"
  FocusNamedWorkspace("6")
  return

;; #^ F7
#^7::
  binding_mode := "normal"
  FocusNamedWorkspace("7")
  return

;; #^ F8
#^8::
  binding_mode := "normal"
  FocusNamedWorkspace("8")
  return

;; #^ F9
#^9::
  binding_mode := "normal"
  FocusNamedWorkspace("9")
  return

;; #^ F10
#^0::
  binding_mode := "normal"
  FocusNamedWorkspace("0")
  return

;; #^ F14
#^Y::
  binding_mode := "normal"
  CycleWorkspace("previous")
  return

;; MOVE TO WORKSPACE

;; #^+ F1
#^+1::
  binding_mode := "normal"
  MoveToNamedWorkspace("1")
  FocusNamedWorkspace("1")
  return

;; #^+ F2
#^+2::
  binding_mode := "normal"
  MoveToNamedWorkspace("2")
  FocusNamedWorkspace("2")
  return

;; #^+ F3
#^+3::
  binding_mode := "normal"
  MoveToNamedWorkspace("3")
  FocusNamedWorkspace("3")
  return

;; #^+ F4
#^+4::
  binding_mode := "normal"
  MoveToNamedWorkspace("4")
  FocusNamedWorkspace("4")
  return

;; #^+ F5
#^+5::
  binding_mode := "normal"
  MoveToNamedWorkspace("5")
  FocusNamedWorkspace("5")
  return

;; #^+ F6
#^+6::
  binding_mode := "normal"
  MoveToNamedWorkspace("6")
  FocusNamedWorkspace("6")
  return

;; #^+ F7
#^+7::
  binding_mode := "normal"
  MoveToNamedWorkspace("7")
  FocusNamedWorkspace("7")
  return

;; #^+ F8
#^+8::
  binding_mode := "normal"
  MoveToNamedWorkspace("8")
  FocusNamedWorkspace("8")
  return

;; #^+ F9
#^+9::
  binding_mode := "normal"
  MoveToNamedWorkspace("9")
  FocusNamedWorkspace("9")
  return

;; #^+ F10
#^+0::
  binding_mode := "normal"
  MoveToNamedWorkspace("0")
  FocusNamedWorkspace("0")
  return

;; FOCUS STUFF

#^Left::
#^H::
  If (binding_mode = "normal")
  {
    CycleFocus("previous")
  }
  Else If (binding_mode = "stack")
  {
    Stack("left")
    binding_mode := "normal"
  }
  Else If (binding_mode = "resize")
  {
    ResizeAxis("horizontal", "decrease")
  }
  Return

#^Down::
#^J::
  If (binding_mode = "normal")
  {
    CycleStack("next")
  }
  Else If (binding_mode = "stack")
  {
    Stack("down")
    binding_mode := "normal"
  }
  Else If (binding_mode = "resize")
  {
    ResizeAxis("vertical", "increase")
  }
  Return

#^Up::
#^K::
  If (binding_mode = "normal")
  {
    CycleStack("previous")
  }
  Else If (binding_mode = "stack")
  {
    Stack("up")
    binding_mode := "normal"
  }
  Else If (binding_mode = "resize")
  {
    ResizeAxis("vertical", "decrease")
  }
  Return

#^Right::
#^F13::
  If (binding_mode = "normal")
  {
    CycleFocus("next")
  }
  Else If (binding_mode = "stack")
  {
    Stack("right")
    binding_mode := "normal"
  }
  Else If (binding_mode = "resize")
  {
    ResizeAxis("horizontal", "increase")
  }
  Return

;; MOVE STUFF

#^+Left::
#^+H::
  binding_mode := "normal"
  CycleMove("previous")
  Return

#^+Down::
#^+J::
  binding_mode := "normal"
  ; SendToMonitor(0)
  Return

#^+Up::
#^+K::
  binding_mode := "normal"
  ; SendToMonitor(1)
  Return

#^+Right::
#^+F13::
  binding_mode := "normal"
  CycleMove("next")
  Return

;; RESIZE STUFF

;; #^ F15
#^S::
  If (binding_mode = "resize")
  {
    binding_mode := "normal"
  }
  Else
  {
    binding_mode := "resize"
  }
  Return

;; ACTIONS

;; #^ F22
#^M::
  binding_mode := "normal"
  Minimize()
  Return

;; #^ F18
#^X::
  binding_mode := "normal"
  ToggleMonocle()
  Return

;; #^ F19
#^C::
  binding_mode := "normal"
  Close()
  Return

#^Space::
  ToggleWindowContainerBehaviour()
  Return
  
#^+Space::
  binding_mode := "normal"
  Unstack()
  Return

;; #^ F17
#^F::
  binding_mode := "normal"
  ToggleFloat()
  Return

;; #^ F20
#^V::
  If (binding_mode = "stack")
  {
    binding_mode := "normal"
  }
  Else
  {
    binding_mode := "stack"
  }
  Return

;; #^ F11
#^E::
  binding_mode := "normal"
  Run, explorer
  Return

#^BackSpace::
  binding_mode := "normal"
  Run, wt --profile "{8e510365-d81d-426c-9b28-dfb96a7a6a85}"
  Return

;; #^ F13
#^T::
  binding_mode := "normal"
  Run, wt
  Return

;; #^+ F15
#^+S::
  binding_mode := "normal"
  Run, explorer ms-screenclip:
  Return

;; #^ F16
#^D::
  binding_mode := "normal"
  Run, D:\Users\kress\scoop\apps\vscode\current\Code.exe
  Return

;; #^ F21
#^B::
  binding_mode := "normal"
  Run, D:\Users\kress\scoop\apps\thorium-avx2\current\thorium.exe --user-data-dir="D:\Users\kress\scoop\apps\thorium-avx2\current\User Data" --allow-outdated-plugins --disable-logging --disable-breakpad --enable-experimental-web-platform-features --new-canvas-2d-api
  Return

; TODO:
; Promote
; PromoteFocus
