!include "MUI2.nsh"

;--------------------------------
;General

Name "LambdaHack" # Name of the installer (usually the name of the application to install).

OutFile "LambdaHack_dev_windows-installer.exe" # Name of the installer's file.

InstallDir "$PROGRAMFILES\LambdaHack" # Default installing folder ($PROGRAMFILES is Program Files folder).

ShowInstDetails show # This will always show the installation details.

; Registry key to check for directory (so if you install again, it will
; overwrite the old one automatically)
  InstallDirRegKey HKLM "Software\LambdaHack" "Install_Dir"

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

; Request application privileges for Windows Vista
# RequestExecutionLevel admin

;--------------------------------
;Variables

  Var StartMenuFolder

;--------------------------------
;Interface Settings

!define MUI_ABORTWARNING # This will warn the user if he exits from the installer.

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY

  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\LambdaHack"
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

  !insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

  !insertmacro MUI_PAGE_INSTFILES

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Dummy Section" SecDummy

  SetOutPath "$INSTDIR"

  ;ADD YOUR OWN FILES HERE...
  File /r LambdaHackTheGame32\*

  ;Store installation folder
  WriteRegStr HKLM "Software\LambdaHack" "Install_Dir" $INSTDIR

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LambdaHack" "DisplayName" "LambdaHack"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LambdaHack" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LambdaHack" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LambdaHack" "NoRepair" 1
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application

    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
    CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecDummy ${LANG_ENGLISH} "A test section."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDummy} $(DESC_SecDummy)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete \r LambdaHackTheGame32
  # "$SMPROGRAMS\Example2\*.*"
  RMDir LambdaHackTheGame32

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder

  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"
  RMDir "$INSTDIR"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LambdaHack"
  DeleteRegKey /ifempty HKLM "Software\LambdaHack"

SectionEnd
