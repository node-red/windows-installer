; *****
; * Windows Installer for Node-RED
; * Definition file for the Inno Setup compiler.
; * Copyright 2023 Ralph Wetzel
; * License MIT
; * https://www.github.com/ralphwetzel/node-red-windows-installer
; *


; *****
; * PreProcessor setup:
; * We configure all constants via an INI file.
; * That's easier for maintenence rather than searching for things in the source code
; *

#define INIFile RemoveBackslash(SourcePath) + "\setup.ini"

#define VersionInfoURL ReadIni(INIFile, "installer", "url", "https://nodered.org")

; Node.js Default Version - that we propose to install if none is present
#define NodeVersionRecommended ReadIni(INIFile, "node", "recommended")

; comma-separated list of major versions numbers we offer for download
#define NodeVersions ReadIni(INIFILE, "node", "versions", NodeVersionRecommended)                                              

; URL to download node.js license from
#define NodeLicenseURL ReadIni(INIFILE, "node", "license")
#define NodeLicenseTmpFileName "node.license"

; Root URL to download node.js files from
#define NodeDownloadURL ReadIni(INIFILE, "node", "download", 'https://nodejs.org/dist')        

; URL to download Node-RED license from
#define REDLicenseURL ReadIni(INIFILE, "red", "license")
#define REDLicenseTmpFileName "red.license"                                             

; By default, we offer (for Node-RED) to install the dist-tag versions as known to npm.
; Additional versions may be defined here; duplicates don't matter!
#define REDAddVersions ReadIni(INIFile, "red", "versions", "")

; If npm is not installed, we cannot get the dist-tag versions.
; In that case, we try to get at least the 'latest' version from
; https://github.com/node-red/node-red/releases/latest
#define REDLatestTmpFileName "red.releases"    

; This is the lowest version number we offer for installation
#define REDMinVersion ReadIni(INIFile, "red", "min", "1.0")

; Count of parallel installations we provision for to manage
; We need this explicitely as there's no way to add dynamically to the [Run] section
#define REDProvisionCount ReadIni(INIFile, "red", "provision", "5")

; The root key for bookkeeping of the Node-RED installations we know of on this system
#define REDInstallationsRegRoot 'SOFTWARE\Node-RED\installations'

; py shall become something like '3.7.6'
#define py ReadIni(INIFile, "python", "version")
; for pth we extract the first two digits of py      
#define pth Copy(StringChange(py, '.', ''), 1, 2)
; md5 sum for the potential python installer files
#define PyMD5x86 ReadIni(INIFile, "python", "win32")
#define PyMD5x64 ReadIni(INIFile, "python", "amd64")

; the download link of the VS Studio Build Tools
; check as well: https://visualstudio.microsoft.com/downloads/
#define VSBuildToolsURL ReadIni(INIFile, "vs", "download", 'https://aka.ms/vs/17/release/vs_BuildTools.exe')
; 15 = 2015
#define VSBuildToolsMinVersion ReadIni(INIFile, "vs", "min", '15')

; URL to download vswhere from
#define VSWhereURL ReadIni(INIFILE, "vs", "where")

#define MyAppName "Node-RED"
#define MyAppVersion "> 3.0"
#define MyAppPublisher "The Node-RED community"
#define MyAppURL "https://nodered.org"
; #define MyAppExeName "MyProg.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{70D435D8-542E-4087-8E1C-D313404C7E9D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
DisableDirPage=yes
DisableWelcomePage=no
OutputBaseFilename="Node-RED Installer"
Compression=lzma
SolidCompression=yes
SetupLogging=yes
PrivilegesRequired=admin
VersionInfoCopyright={#ReadIni(INIFile, "installer", "copyright", "")}
VersionInfoDescription={#ReadIni(INIFile, "installer", "description", "")}
VersionInfoVersion={#ReadIni(INIFile, "installer", "version", "")}
WizardStyle=modern
WizardImageAlphaFormat=defined
; WizardImageBackColor=clWhite
WizardImageStretch=True
WizardImageFile="graphics\sidebar\Node RED Side Graphic - BMP.bmp"
LicenseFile="LICENSE"
SetupIconFile={#SourcePath}\icons\node-red-icons.ico
PrivilegesRequiredOverridesAllowed=dialog

; Installer Code Signing Data
; To create a test certificate: https://stackoverflow.com/questions/84847/how-do-i-create-a-self-signed-certificate-for-code-signing-on-windows
; To create a Secure String: $xxx = ConvertTo-SecureString plain-text-string -asPlainText -force
; to convert to Base64: certutil -encode .\ssCertInfo.pfx .\ssCertInfo.base64.txt
; Action: https://github.com/dlemstra/code-sign-action


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
; Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
; Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: "graphics\nodejs.bmp"; DestDir: "{tmp}"; DestName: "nodejs.bmp"; Flags: dontcopy
Source: "graphics\nrhex24.bmp"; DestDir: "{tmp}"; DestName: "node-red.bmp"; Flags: dontcopy
Source: "graphics\node-red-icon-small.bmp"; DestDir: "{tmp}"; DestName: "node-red-small.bmp"; Flags: dontcopy
; Source: "tools\vswhere.exe"; DestDir: "{tmp}"; DestName: "vswhere.exe"
Source: "bat\setup_loop.bat"; DestDir: "{tmp}"
Source: "icons\node-red-icons.ico"; DestDir: "{app}"; DestName: "red.ico"

[Icons]
; Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
; Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
; Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
; Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon
; Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchicon

[Messages]
english.WelcomeLabel1=Welcome to the%n[name] Setup Wizard
english.WelcomeLabel2=This will install Node-RED on your computer.%n%nInitially we check the version of Node.js installed is %1 or greater. We will try to install node %2 if none is found. Optionally you can choose to install node %3.%n%nIf necessary we will then remove the old core of Node-RED, before then installing the latest version. You can also optionally specify the version required.%n%nWe will finally try to run 'npm rebuild' to refresh any extra nodes you have installed that may have a native binary component. While this normally works ok, you need to check that it succeeds for your combination of installed nodes.%n%nIt is recommended that you close all other applications before continuing.
WizardReady=Final verification
ReadyLabel1=We just ran a final verification of your installation setup.
FinishedHeadingLabel=Completing the%n[name] Setup Wizard
TranslatorNote={#ReadIni(INIFile, "installer", "description", "")}%nVersion {#ReadIni(INIFile, "installer", "version", "")}%nCopyright © {#ReadIni(INIFile, "installer", "copyright", "")}%nSide graphic by Marcus J. Davies%n{#VersionInfoURL}
AboutSetupMenuItem=&About...
AboutSetupTitle=About {#ReadIni(INIFile, "installer", "description", "")}

[CustomMessages]
english.MSG_FAILED_FINISHED1=Setup failed to install Node-RED on your computer:
english.MSG_FAILED_FINISHED2=Sorry for this inconvenience!

[Run]

; main.error is the global Error flag
; Each step checks if this is set, and if (it is) returns False@Check!

; Check if Node.js should be uninstalled.
; Run the uninstall; GUID queried from Registry.
; Verify that no Node.js is known.
Filename: "msiexec"; \
  Parameters: "/norestart /quiet /uninstall {code:GetInstalledNodeGUID}"; \
  Flags: runascurrentuser; \
  StatusMsg: "Uninstalling Node.js v{code:GetNodeVersionMain|current}..."; \
  Check: RunCheck('rcsNodeUninstall', ''); \
  BeforeInstall: SetupRunConfig; \
  AfterInstall: Confirm('csNoNode', '');


; The next two run - in general - the same command.
; #1 Installs in silent mode
; #2 Shows the Node installer & hides our installer
; Only one of these will be executed - as switched by the Check function.
; We do not install the components addressed by flags 'NodeEtwSupport' & 'NodePerfCtrSupport'

; Check that Node.js should be installed
; Run the installer
; Verify that Node.js is ready on the system.
Filename: "msiexec"; \
  Parameters: "/i {tmp}\node.msi TARGETDIR=""C:\Program Files\nodejs\"" ADDLOCAL=""DocumentationShortcuts,EnvironmentPathNode,EnvironmentPathNpmModules,npm,NodeRuntime,EnvironmentPath"" /qn"; \
  Flags: runascurrentuser; \
  StatusMsg: "Installing Node.js v{code:GetNodeVersionMain|selected}..."; \
  Check: RunCheck('rcsNodeInstall', 'silent'); \
  BeforeInstall: SetupRunConfig; \
  AfterInstall: Confirm('csNode', ExpandConstant('{code:GetNodeVersionMain|selected}'));

Filename: "msiexec"; \
  Parameters: "/i {tmp}\node.msi TARGETDIR=""C:\Program Files\nodejs\"" ADDLOCAL=""DocumentationShortcuts,EnvironmentPathNode,EnvironmentPathNpmModules,npm,NodeRuntime,EnvironmentPath"" "; \
  Flags: runascurrentuser hidewizard; \
  StatusMsg: "Installing Node.js v{code:GetNodeVersionMain|selected}..."; \
  Check: RunCheck('rcsNodeInstall', 'show'); \
  BeforeInstall: SetupRunConfig; \
  AfterInstall: Confirm('csNode', ExpandConstant('{code:GetNodeVersionMain|selected}'));

Filename: "{tmp}\python_installer.exe"; \
  Parameters: "/quiet InstallAllUsers=1 PrependPath=1 Include_test=0 Include_launcher=0"; \
  Flags: runascurrentuser; \
  StatusMsg: "Installing Python..."; \
  Check: RunCheck('rcsPython', 'no'); \
  BeforeInstall: SetupRunConfig; \
  AfterInstall: Confirm('csPython', '');

Filename: "{tmp}\VSBT_installer.exe"; \
  Parameters: "--norestart --quiet --includeRecommended --add Microsoft.VisualStudio.Worlkload.VCTools"; \
  Flags: runascurrentuser; \
  StatusMsg: "Installing VisualStudio BuildTools..."; \
  Check: RunCheck('rcsVSBuildTools', ''); \
  BeforeInstall: SetupRunConfig;
  ; AfterInstall: Confirm('csVSBuildTools', '');

; This doesn't work when running the VS installer w/ --quiet
; Parameters: """WINDOWTITLE eq VISUAL STUDIO Installer"" setup.exe"; \
; As 'setup.exe' is quite ambigous, we've checked in PrepareInstallation that there
; wasn't another process with this IMAGENAME running! 

Filename: "{tmp}\setup_loop.bat"; \
  Parameters: """IMAGENAME eq setup.exe"" setup.exe"; \
  Flags: runasoriginaluser shellexec waituntilterminated runhidden; \
  StatusMsg: "Waiting for Visual Studio Installer to finish its job..."; \
  Check: RunCheck('rcsVSBuildTools', ''); \
  BeforeInstall: SetupRunConfig; \
  AfterInstall: Confirm('csVSBuildTools', '');

; There's a breaking change introduced - intentionally - in NPM9 (Node.js v18+) 
; that dis-allows non-standard config settings
; Thus the following doesn't work (anymore).
; Not sure about the consequences...
; Reference: https://github.com/npm/cli/issues/5852

; Filename: "npm"; \
;   Parameters: "config set python ""{code:GetPythonPath}"""; \
;   WorkingDir: "{code:GetNodeDataReg|path}"; \
;   Flags: runasoriginaluser shellexec waituntilterminated runhidden; \
;   StatusMsg: "Configuring Python path for Node.js..."; \
;   Check: RunCheck('rcsPythonConfig', ''); \
;   BeforeInstall: SetupRunConfig; \
;  AfterInstall: Confirm('csPythonConfig', '');

#define i

#sub RemoveRED
  Filename: "npm"; \
    Parameters: "uninstall {code:GetREDActionGlobal|{#i}} node-red"; \
    WorkingDir: "{code:GetREDActionPath|{#i}}"; \
    Flags: runascurrentuser shellexec waituntilterminated runhidden; \
    StatusMsg: "Removing {code:GetREDCurrentMsg|{#i}}..."; \
    Check: RunCheck('rcsREDRemove', '{#i}'); \
    BeforeInstall: SetupRunConfig; \
    AfterInstall: REDRemove('{#i}');
#endsub

#sub InstallRED
  Filename: "npm"; \
    Parameters: "install {code:GetREDActionGlobal|{#i}} node-red@{code:GetREDActionAction|{#i}}"; \
    WorkingDir: "{code:GetREDActionPath|{#i}}"; \
    Flags: runascurrentuser shellexec waituntilterminated runhidden; \
    StatusMsg: "Installing {code:GetREDActionMsg|{#i}}..."; \
    Check: RunCheck('rcsREDInstall', '{#i}'); \
    BeforeInstall: REDPrepare('{#i}'); \
    AfterInstall: REDFinalize('{#i}');
#endsub

#sub NPMRebuild
  Filename: "npm"; \
    Parameters: "rebuild"; \
    WorkingDir: "{code:GetREDActionPath|{#i}}"; \
    Flags: runascurrentuser shellexec waituntilterminated runhidden; \
    StatusMsg: "Rebuilding packages for {code:GetREDActionMsg|{#i}}..."; \
    Check: RunCheck('rcsREDInstall', '{#i}');
#endsub

#for {i = 0; i < Int(REDProvisionCount, 5); i++} RemoveRED
#for {i = 0; i < Int(REDProvisionCount, 5); i++} InstallRED
#for {i = 0; i < Int(REDProvisionCount, 5); i++} NPMRebuild

#undef i


[Code]

// All data managed by this installer is pushed into one huge record called 'main'.
// 'main' is of type rInstallerData.
// Dedicated records in 'main' carry information regarding Node.js, RED, Python, ...

type

  rNodeVersion = record
    key: string;
    sha: string;
    latest: string;
    msi: string;
    default: boolean;
    file: string;
  end;
  // TNodeVersionList = array of rNodeVersion;

  rREDVersion = record
    version: string;
    tag: string;
  end;

  TREDVersionArray = array of rREDVersion;

  rNodeData = record
    majors: array of integer;   // Supported major versions as read from the INI file
    versions: array of rNodeVersion;
    default: integer;
    selected: string;
    run_silent: boolean;
    install_tools: boolean;
    current: string;
    options: TStringList;
  end;

  rREDCalcData = record
    path: string;     // if defined, ensure empty directory & create package.json
    port: integer;
  end;

  rREDRegData = record
    name: string;
    path: string;
    version: string;
    icon: string;
    port: integer;
    autostart: string;
  end;

  sREDInstallationKind = (rikGlobal, rikPath, rikNew, rikVoid);

  rREDInstallation = record
    key: string;
    id: TObject;
    kind: sREDInstallationKind;
    name: string;
    // _line: integer;
    path: string;
    version: string;
    port: integer;
    action: string;
    autostart: boolean;
    icon: boolean;
    // final_path: string;
    calc: rREDCalcData;
    // add additional properties here!
    registry: rREDRegData;
  end;

  sREDListItemKind = (rlikNone, rlikAction, rlikPath, rlikGlobal, rlikPort, rlikLabel, rlikIcon, rlikAutostart);

  rREDListItem = record
    kind: sREDListItemKind;
    action: string;
    link: TObject;
  end;

  rRedData = record
    versions: array of rREDVersion;
    selected: string;
    current: string;
    installs: array of rREDInstallation;
    items: array of rREDListItem;
    npm: boolean;
    error: boolean;
    run: array of integer;    // index sequence of 'installs' to be installed. This eliminates the installs of 'rikVoid'. Populated @ UpdateReadyMemo. 
  end;

  rPageID = record
    node_license: TOutputMsgMemoWizardPage;
    node_version: TInputOptionWizardPage;
    red_license: TOutputMsgMemoWizardPage;
    red_version: TInputOptionWizardPage;
    red_action: TInputOptionWizardPage;
    download: TDownloadWizardPage;
  end;

  rInstallationError = record
    status: boolean; 
    msg: string;
  end;

  rPythonData = record
    version: string;      // proposed version (via .ini)
    npm: boolean;         // is npm aware of a python version?
    installed: string;    // installed version?
    path: string;
  end;

  rVSData = record
    version: string;
  end;

  rInstallerData = record
    node: rNodeData;
    red: rRedData;

    // As described in the documenttion, HKLM is going be set to HKEY_LOCAL_MACHINE_64 when running on 64bit systems 
    // *AND* "the system's processor architecture is included in the value of the ArchitecturesInstallIn64BitMode [Setup] section directive"
    // ArchitecturesInstallIn64BitMode [Setup] section yet is blank by default.
    // => HKLM - by default - always == HKEY_LOCAL_MACHINE
    // => set is as required: HKEY_LOCAL_MACHINE or HKEY_LOCAL_MACHINE_64
    HKLM: Integer;
    bit: String;

    pages: rPageID;
    error: rInstallationError;    // Global Installation Error Status
    python: rPythonData;
    vs: rVSData;

  end;

  sImageType = (imgNODE, imgRED, imgNONE);

var
  
  // Two additional controls
  REDLicenseAcceptedRadio: TRadioButton;
  REDLicenseNotAcceptedRadio: TRadioButton;

  // As described in the documenttion, HKLM is going be set to HKEY_LOCAL_MACHINE_64 when running on 64bit systems 
  // *AND* "the system's processor architecture is included in the value of the ArchitecturesInstallIn64BitMode [Setup] section directive"
  // ArchitecturesInstallIn64BitMode [Setup] section yet is blank by default.
  // => HKLM - by default - always == HKEY_LOCAL_MACHINE
  // => set it as required: HKEY_LOCAL_MACHINE or HKEY_LOCAL_MACHINE_64

  // The list of all node versions offered to install
  // nodeVersionSelectionOptions: TStringList;
  // the index the user choose to install
  // nodeVersionSelectionOptionsSelected: Integer;

  // cbHideNodeInstaller: TNewCheckBox;
  // cbInstallWindowsTools: TNewCheckBox;

  // The Node-RED version the user selected for installation
  // redVersionSelected: String;

  // This record holds all relevant data
  main: rInstallerData;

  // testPage: TInputOptionWizardPage;
  // testPage2: TInputOptionWizardPage;

// *****
// * Forward definition of some functions "exported" by other files

// nodedetector.iss
function detect_red_installations(var page: TOutputMarqueeProgressWizardPage): integer; forward;
function red_list(working_dir: string): string; forward;

// redactionpage.iss
function MakeRedActionPage(page: TInputOptionWizardPage): Boolean; forward;

// *
// *****


// *****
// * Support functions

procedure debug(message: string);
begin
  Log('[NRI] ' + message);
end;

{procedure debugInt(int: integer);
begin
  debug(IntToStr(int));
end;}

// pastebin.com/STcQLfKR
Function SplitString(const Value: string; Delimiter: string; Strings: TStrings): Boolean;
var
  S: string;
begin
  S := Value;
  if StringChangeEx(S, Delimiter, #13#10, True) > 0 then begin
    Strings.text := S;
    Result := True;
    Exit;
  end;
  Result := False;
end;

function OnDownloadProgress(const Url, FileName: String; const Progress, ProgressMax: Int64): Boolean;
begin
  if Progress = ProgressMax then begin
    Log(Format('Successfully downloaded file to {tmp}: %s', [FileName]));
  end;
  Result := True;
end;

function RightStr(S: string; C: Char; I: Integer): string;
begin
  Result := StringOfChar(C, I - Length(S)) + S;
end;

function SortAsInt(List: TStringList; Index1, Index2: Integer): Integer;
var
  i1, i2: Integer;
begin
  i1 := StrToInt(List[Index1]);
  i2 := StrToInt(List[Index2]);
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then Result := 1
  else
    Result := 0;
end;


// TStringList in Inno Setup flavour unfortunately does not support CustomSort
// Thus we have to detour via an Integer array sort by QuickSort
procedure QuickSort(var A: array of Integer; iLo, iHi: Integer) ;
var
  Lo, Hi, Pivot, T: Integer;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := A[(Lo + Hi) div 2];
  repeat
    while A[Lo] < Pivot do Inc(Lo) ;
    while A[Hi] > Pivot do Dec(Hi) ;
    if Lo <= Hi then
    begin
      T := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := T;
      Inc(Lo) ;
      Dec(Hi) ;
    end;
  until Lo > Hi;
  if Hi > iLo then QuickSort(A, iLo, Hi) ;
  if Lo < iHi then QuickSort(A, Lo, iHi) ;
end;


function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function RGB2TColor(const R, G, B: Byte): Integer;
begin
  Result := (Integer(R) or (Integer(G) shl 8) or (Integer(B) shl 16));
end;


procedure TColor2RGB(const Color: TColor; var R, G, B: Byte);
begin
  // convert hexa-decimal values to RGB
  R := Color and $FF;
  G := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
end;


function CompareVersions( checkVersion, compareVersion: string): integer;
var
  checkV, compV: TStringList;
  i, v1, v2, l1, l2: integer;
  msg: string;

begin

  checkV := TStringList.Create;
  compV := TStringList.Create;

  SplitString(checkVersion + '.', '.', checkV);
  SplitString(compareVersion + '.', '.', compV);

  l1 := checkV.Count;
  l2 := compV.Count;

  for i:= 0 to Max(l1, l2) - 1 do begin

    if l1 > i then begin
      v1 := StrToIntDef(checkV[i], -1);
    end else begin
      v1 := 0;
    end;
    
    if l2 > i then begin
      v2 := StrToIntDef(compV[i], -1);
    end else begin
      v2 := 0;
    end;

    // Only if both versions have a non-number part
    // compare those as strings
    // If only one has a non-number (e.g. '-beta.2') ammendment,
    // this version will be 'smaller' than the one without
    if ((v1 < 0) and (v2 < 0)) then begin
      Result := CompareStr(checkV[i], compV[i]);
      break;
    end;

    if v1 > v2 then begin
      Result := 1;
      break;
    end else if v1 < v2 then begin
      Result := -1;
      break;
    end;

    Result := 0;
  end;

  msg := '??';
  case Result of
    -1: msg := '<';
     0: msg := '=';
     1: msg := '>';
  end;
  // debug(checkVersion + ' ' + msg + ' ' + compareVersion);

end;

function GetVersion(version: String; index: Integer): Integer;
var
  vv: TStringList;
begin
  Result := -1;
  vv := TStringList.Create;

  if SplitString(version, '.', vv) then begin
    if vv.Count > index then begin
      Result := StrToIntDef(vv[index], 0);
    end;
  end;
end;

function GetVersionMajor(version: String): Integer;
begin
  Result := GetVersion(version, 0);
end;

function GetVersionMinor(version: String): Integer;
begin
  Result := GetVersion(version, 1);
end;

function GetVersionPatch(version: String): Integer;
begin
  Result := GetVersion(version, 2);
end;

procedure set_image(img: sImageType);
var
  image: string;
begin

  case img of
    imgNone: begin
      WizardForm.WizardSmallBitmapImage.Visible := False;
      Exit;
    end;
    imgNODE: image := 'nodejs.bmp';
    imgRED: image := 'node-red.bmp';
  else
    Exit;
  end;

  if not FileExists(ExpandConstant('{tmp}\' + image)) then ExtractTemporaryFile(image);
  WizardForm.WizardSmallBitmapImage.Bitmap.LoadFromFile(ExpandConstant('{tmp}\' + image));
  WizardForm.WizardSmallBitmapImage.Visible := True;

end;


function BoolToStr(bool: boolean): string;
begin
  if bool then
    Result:='true'
  else
    Result :='false';
end;

function StrToBool(str: string): boolean;
begin
  Result := str = 'true'
  Result := (str = 'yes') or Result;
end;


function isEmptyDir(dirName: String): Boolean;
var
  FindRec: TFindRec;
  FileCount: Integer;
begin
  Result := False;
  if FindFirst(dirName+'\*', FindRec) then begin
    try
      repeat
        if (FindRec.Name <> '.') and (FindRec.Name <> '..') then begin
          FileCount := 1;
          break;
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
      if FileCount = 0 then Result := True;
    end;
  end;
end;


// *****
// * Additional page to acknowledge Node-RED License
// * https://stackoverflow.com/questions/34592002/how-to-create-two-licensefile-pages-in-inno-setup
procedure CheckREDLicenseAccepted(Sender: TObject);
begin
  // Update Next button when user (un)accepts the license
  WizardForm.NextButton.Enabled := REDLicenseAcceptedRadio.Checked;
end;

function CloneLicenseRadioButton(Source: TRadioButton): TRadioButton;
begin
  Result := TRadioButton.Create(WizardForm);
  Result.Parent := main.pages.red_license.Surface;
  Result.Caption := Source.Caption;
  Result.Left := Source.Left;
  Result.Top := Source.Top;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.OnClick := @CheckREDLicenseAccepted;
end;

function CreateREDLicensePage(after: Integer): TOutputMsgMemoWizardPage;

begin
  
  Result :=
    CreateOutputMsgMemoPage(
      after, 'Node-RED ' + SetupMessage(msgWizardLicense),
      SetupMessage(msgLicenseLabel),
      SetupMessage(msgLicenseLabel3), '');

  // Shrink memo box to make space for radio buttons
  Result.RichEditViewer.Height := WizardForm.LicenseMemo.Height;

  // Clone accept/do not accept radio buttons for the second license
  REDLicenseAcceptedRadio :=
    CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
  REDLicenseNotAcceptedRadio :=
    CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);

  // Initially not accepted
  REDLicenseNotAcceptedRadio.Checked := True;

end;

// *
// *****

// *****
// ** RunCMD
// ** Execute a command with Windows cmd.exe

function RunCMD(Command, WorkingDir: string; var ResultArray: TArrayOfString): Boolean;

var
  bat, file, res: string;
  rc, i: integer;

  p1, p2: string;
  msg: string;
begin

  msg := '$> ' + Command;
  if Length(WorkingDir) > 0 then msg := msg + ' @ ' + WorkingDir;
  debug(msg);

  Result := False;
  
  // %PATH% is incomplete in cmd when we just use Exec, 
  // thus calling 'node' or 'npm' tells us "not found".
  // => Read the two registry keys thet both hold the PATH data
  // => to combine them and set PATH explicitely - for this cmd session!
  // (based on an idea I got from https://stackoverflow.com/a/32420542)
  p1:='';
  RegQueryStringValue(main.HKLM, 'System\CurrentControlSet\Control\Session Manager\Environment', 'Path', p1);
  p2:='';
  RegQueryStringValue(HKCU, 'Environment', 'Path', p2);

  res:= ExpandConstant('{tmp}\cmd_result.txt');

  bat := '';
  if (Length(p1) + Length(p2)) > 0 then
    bat := 'PATH=' + p1 + ';' + p2 + ';' + #13#10;
  bat := bat + command + ' > "' + res + '" 2>&1';

  file := ExpandConstant('{tmp}\run_cmd.bat');

  if SaveStringToFile(file, bat, False) then begin
    if Exec(file, '', WorkingDir, SW_HIDE, ewWaitUntilTerminated, rc) then begin
      if LoadStringsFromFile(res, ResultArray) then begin
        for i:=0 to GetArrayLength(ResultArray) -1 do begin
          debug('>> ' + ResultArray[i]);
        end;
        Result:=True;
      end;
    end else begin
      debug('>> ' + SysErrorMessage(rc));
    end;
  end;
end;



// *****
// ** NodeVersionSelectionPage
// **

var 
    _nodeVersion_cbHideInstaller: TNewCheckBox;
    _nodeVersion_cbInstallWindowsTools: TNewCheckBox;

procedure _nodeVersion_OnClickInstallBackground(Sender: TObject);
var
  status: boolean;
begin
  status := _nodeVersion_cbHideInstaller.Checked;
  _nodeVersion_cbInstallWindowsTools.Visible := status;
  main.node.run_silent := status
end;

procedure _nodeVersion_OnClickNodeVersion(Sender: TObject);
var
  index: Integer;
  nv: String;

begin

  index := main.pages.node_version.SelectedValueIndex;
  
  if index < main.node.options.Count then begin
    
    nv := main.node.options[index];
    main.node.selected := nv;

    _nodeVersion_cbHideInstaller.Visible := (Length(nv) > 0);
    _nodeVersion_cbInstallWindowsTools.Visible := ((Length(nv) > 0) and _nodeVersion_cbHideInstaller.Checked);
  end;

end;

function PrepareNodeVersionSelectionPage(): Boolean;
var

  sFlag: String;
  i, ii: Integer;

  _nvp: TInputOptionWizardPage;

  _possible: array of rNodeVersion;   // the Node.js versions we know of
  _options: TStringList;              // the Node.js versions we offer for download;
                                      // this may include '' for "Keep the current..."
  _current: string;

begin

  Result := False;

  // debug('PrepareNodeVersionSelectionPage');

  // Page to select a Node.js version
  _nvp := main.pages.node_version;
  if _nvp = nil then Exit;

  _nvp.SubCaptionLabel.Font.Style := [fsBold];
  _nvp.CheckListBox.OnClickCheck := @_nodeVersion_OnClickNodeVersion;

  // Additional checkboxes:
  _nodeVersion_cbHideInstaller := TNewCheckBox.Create(_nvp);
  with _nodeVersion_cbHideInstaller do begin
    Parent := _nvp.Surface;
    Top := _nvp.CheckListBox.Top + _nvp.CheckListBox.Height + ScaleY(8);
    Height := ScaleY(_nodeVersion_cbHideInstaller.Height);
    Left := _nvp.CheckListBox.Left;
    Width := _nvp.CheckListBox.Width;
    Caption := 'Run Node.js installer in the background.';
    Checked := True;
    OnClick := @_nodeVersion_OnClickInstallBackground;
  end;
  main.node.run_silent := True;

  _nodeVersion_cbInstallWindowsTools := TNewCheckBox.Create(_nvp);
  with _nodeVersion_cbInstallWindowsTools do begin
    Parent := _nvp.Surface;
    Top := _nodeVersion_cbHideInstaller.Top + _nodeVersion_cbHideInstaller.Height + ScaleY(8);
    Height := ScaleY(_nodeVersion_cbInstallWindowsTools.Height);
    Left := _nodeVersion_cbHideInstaller.Left;
    Width := _nodeVersion_cbHideInstaller.Width;
    Caption := 'Install Windows Tools for Native Node.js Modules - if necessary!';
    Checked := True;
    Enabled := True;
  end;
  main.node.install_tools := True;

  // Read the current node.js version from the registry
  _current := '';
  if RegKeyExists(main.HKLM, 'SOFTWARE\Node.js') then begin
    if RegQueryStringValue(main.HKLM, 'SOFTWARE\Node.js', 'Version', _current) = True then begin
      _nvp.SubCaptionLabel.Caption := 'Currently installed: Node.js ' + _current;
      debug('According Registry, Node.js v' + _current + ' is installed.');
    end;
  end;

  main.node.current := _current;

  // This list holds the versions numbers we offer for installation
  _options := TStringList.Create;

  _possible := main.node.versions;

  if _current = '' then begin
      _nvp.SubCaptionLabel.Caption := 'Currently there''s no Node.js version installed!';
  end else begin

    // Check if current version > Min version
    if CompareVersions(_current, _possible[0].key) > 0 then begin
        _nvp.Add('Keep CURRENT Node.js ' + _current);
        _options.Add('');
        
        // Set initial value to "Keep"
        _nvp.Values[0] := True;
    end;
  end;

  for i := 0 to GetArrayLength(_possible) - 1  do begin

    _options.Add(_possible[i].latest);

    ii := CompareVersions(_current, _possible[i].latest);
    if  ii > 0 then begin
      _nvp.Add('Change to v' + _possible[i].key + ' LTS Node.js version >> ' + _possible[i].latest + ' | NOT RECOMMENDED');
      continue;
    end else if ii = 0 then begin
      _nvp.Add('Re-Install v' + _possible[i].key + ' LTS Node.js version: ' + _possible[i].latest);
      continue;
    end;

    sFlag := '';
    if GetVersionMajor(_current) < GetVersionMajor(_possible[i].latest) then begin
      if GetVersionMajor(_possible[i].latest) = main.node.default then begin
        // debugInt(main.node.default);
        sFlag := ' | RECOMMENDED ';    
      end;
    end;
    
    _nvp.Add('Update to latest v' + _possible[i].key + ' LTS Node.js version >> ' + _possible[i].latest + sFlag);

    if Length(sFlag) > 0 then begin
        // if no version installed: Default to recommended version
        if _current = '' then
          _nvp.Values[_options.Count - 1] := True;
    end;
  end;

  main.node.options := _options;
  _nodeVersion_OnClickNodeVersion(nil);
  
  Result := True;

end;

// ** END
// ** NodeVersionSelectionPage
// *****

// *****
// ** Data Preparation Page
// ** (which isn't a true interactive page, but a Download & a Progress page inserted after wpWelcome)

function _redversion_insert_version(version, tag: String; red_versions: TREDVersionArray): TREDVersionArray;
var
  
  buffer: TREDVersionArray;
  ii, ibuffer, rvLength, cv: Integer;

begin

  // Do not accept versions that are lower than #REDMinVersion
  // debug('cv: ' + version + ' / {#REDMinVersion}');

  cv := CompareVersions(version, '{#REDMinVersion}');

  // debug('cv: ' + version + ' / {#REDMinVersion}' + ' = ' + IntToStr(cv));

  if cv < 0 then begin
    Result := red_versions;
    Exit;
  end;

  if Length(version) < 1 then begin
    Result := red_versions;
    Exit;
  end;

  rvLength := GetArrayLength(red_versions);
  SetArrayLength(buffer, rvLength + 1);

  if rvLength < 1 then begin
    buffer[0].version := version;
    buffer[0].tag := tag;
    Result := buffer;
    Exit;
  end;

  ibuffer:= 0;
  for ii:= 0 to rvLength - 1 do begin
    // debug(version + ' ? ' + red_versions[ii].version);

    if Length(version) > 0 then begin

      cv := CompareVersions(version, red_versions[ii].version);
    
      if cv = 0 then begin
        // if the version is already present,
        // don't overwrite
        version := '';

      end else if cv < 0 then begin
        buffer[ibuffer].version := version;
        buffer[ibuffer].tag := tag;
        ibuffer:=ibuffer+1;
        version := ''

      end;
    end;

    buffer[ibuffer].version := red_versions[ii].version;
    buffer[ibuffer].tag := red_versions[ii].tag;
    ibuffer:=ibuffer+1;

  end;

  if Length(version) > 0 then begin
    buffer[ibuffer].version := version;
    buffer[ibuffer].tag := tag;
    ibuffer:=ibuffer+1;
  end;

  // truncate in case version was already present!
  SetArrayLength(buffer, ibuffer);
  Result := buffer;

end;


function GetPythonPath(param: string): string; forward;
function GetVSBuildToolsVersion(): string; forward;
function GetNPMPythonConfig(): string; forward;

function RunDataPrepPage(): Boolean;

#define ProgressMax 10;

var
  _ppage: TOutputMarqueeProgressWizardPage;

  i, ii, having: integer;
  nvv: array of integer;  

  nv, tmp_file_name, line: string;
  check: boolean;

  parts, version: TStringList;
  _sha, _latest: string;

  file: array of string;

  majors: array of integer;
  
  // Node-RED
  res: array of string;
  rvv, splitres: TStringList;
  tag, rv: string;
  red_versions: TREDVersionArray;
  // current_version: string;
  
  msg: string;

begin

  _ppage := CreateOutputMarqueeProgressPage('Processing additional data...', 'We collect and analyze data to prepare the installation.');

  try
    // _ppage.SetProgress(_iProgress, {#ProgressMax});
    _ppage.Animate();
    _ppage.Show;

    // Get the Node versions, the Node msi download links & the SHA
    // This serves as well to verify that an internet connection is present.

    nvv := main.node.majors;

    debug('Trying to fetch version data of latest Node.js releases:');

    for i := 0 to GetArrayLength(nvv) - 1 do begin

      nv := IntToStr(nvv[i]);
      // debug(nv);
      // Download the SHA file
      tmp_file_name := ExpandConstant('{tmp}') + '\node' + nv + '.sha';
      // check := idpDownloadFile('https://nodejs.org/dist/latest-v' + nv + '.x/SHASUMS256.txt', tmp_file_name);      

      if not FileExists(tmp_file_name) then continue;
      _ppage.SetText('Extracting data for Node.js v' + nv, '');

      // Reset the data we expect to find
      // sha := '';
      // latest := '';
      // msi := '';

      _ppage.Animate();
      LoadStringsFromFile(tmp_file_name, file);
      
      for ii := 0 to GetArrayLength(file) - 1  do begin

        _ppage.Animate();

        // Example line in file:
        // 76102997f9084e1faa544693ad1daeef68394d46ae7e363ad8df1fa86896133f  node-v12.22.12-x64.msi
        line := file[ii];
        
        // find the line with '-x64.msi' or '-x86.msi' at the end
        if StringChangeEx(line, '-' + main.bit + '.msi', '', True) > 0 then begin
        
          parts := TStringList.Create; 

          // line - when found - looks now like this:  
          // 76102997f9084e1faa544693ad1daeef68394d46ae7e363ad8df1fa86896133f  node-v12.22.12
          if SplitString(line, '  ', parts) = True then begin
            
            if parts.Count = 2 then begin

              _sha := parts[0];
              line := parts[1];

              // assure that the rest starts with 'node-v'
              if StringChangeEx(line, 'node-v', '', True) > 0 then begin

                version := TStringList.Create;

                // line is now like 12.22.12
                // final check:
                // => we should have now a version of three elements
                // => at index 0 shall be the version number we started with
                if SplitString(line, '.', version) = True then begin
                  if ((version.Count = 3) and (version[0] = nv)) then
                    _latest := line;
                end;
              end;
            end;
          end;
        end;
      end;

      if ((Length(_sha) > 0) and (Length(_latest) > 0)) then begin

        having := GetArrayLength(main.node.versions);
        SetArrayLength(main.node.versions, having + 1);

        with main.node.versions[having] do begin
          key := nv;
          latest := _latest;
          sha := _sha;
          msi := 'node-v' + latest + '-' + main.bit + '.msi';
          file := tmp_file_name;
        end;

        debug('Latest Node.js @ v' + nv + ': ' + main.node.versions[having].latest);

        // re-construct the - now confirmed - 'majors' array
        having := GetArrayLength(majors);
        SetArrayLength(majors, having + 1);
        majors[having] := nvv[i];

      end;  
    end;

    // By now, this data is confirmed!
    main.node.majors := majors;

    having := GetArrayLength(main.node.versions);
    if having > 0 then begin
      Result := True;

    end else begin

      // Error and out!
      TaskDialogMsgBox('Error',
         'Failed to get Node.js download data.' + #13#10 + 'Please ensure that you''re connected to the Internet.',   
         mbCriticalError,
         MB_OK, [], 0);

      Result := False;
    end;

    if not Result then Exit;

    // Node-RED version processing
    // Att: The calls to npm are really slow!

    rvv := TStringList.Create;

    msg := 'Requesting Node-RED version info from npm';
    _ppage.SetText(msg + '...', '');
    debug(msg + ':');

    RunCMD('npm dist-tag ls node-red', '', res);

    // debug('npm dist-tag: ' + IntToStr(GetArrayLength(res)));

    for i:= 0 to GetArrayLength(res) - 1 do begin
      
      _ppage.Animate();

      line := res[i];

      // v1-maintenance: 1.3.7
      if SplitString(line, ': ', rvv) then begin
        tag:= Trim(rvv[0]);
        rv:= Trim(rvv[1]);
      end else
        continue;

      // debug(rv + ' | ' + tag);
      red_versions := _redversion_insert_version(rv, tag, red_versions);

    end;

    // debug('red_versions: ' + IntToStr(GetArrayLength(red_versions)));

    if GetArrayLength(red_versions) > 0 then begin
      main.red.npm := True;
    end else begin 

      // This either means Node-RED is EOL ... :(
      // ... or npm is not installed!
      // In this case we try to get at least the latest version info by querying the GitHub 'release/latest' page (already downloaded!)
      
      tmp_file_name := ExpandConstant('{tmp}\') + '{#REDLatestTmpFileName}';

      // debug(tmp_file_name);

      if FileExists(tmp_file_name) then begin

        _ppage.Animate();
        _ppage.SetText('Trying to extract the LATEST Node-RED version tag...', '');

        LoadStringsFromFile(tmp_file_name, file);
        check:= False;

        for i := 0 to GetArrayLength(file) - 1  do begin

          _ppage.Animate();

          line := file[i];
          // debug(line);
          
          parts := TStringList.Create;

          // We're looking for this fragment:
          // ... <meta property="og:url" content="/node-red/node-red/releases/tag/3.0.2" /> ...
          // This is VERY fragile ... !!
          if SplitString(line, '<meta property=', parts) then begin
            // debug(line);
            for ii:=0 to parts.Count - 1 do begin
              _ppage.Animate();
              line := parts[ii];
              // debug(line);
              if StringChangeEx(line, '"og:url" content="/node-red/node-red/releases/tag/', '', True) > 0 then begin
                // debug(line);
                splitres := TStringList.Create;
                if SplitString(line, '" />', splitres) then begin
                  line := splitres[0];
                  // debug(line);
                  red_versions := _redversion_insert_version(line, 'latest', red_versions);
                  check:=True;
                  break;
                end;
              end;
            end;

          end;

          if check then break;
          
        end;

      end;
    end;

    // Now merge the "Additional Versions" as defined in setup.ini
    // debug('{#REDAddVersions}');

    // Att: There's - with intention! - an addition ',' appended to REDAddVersions
    // SplitString only returns true, if the delimieter was found at least once!
    if SplitString('{#REDAddVersions},', ',', rvv) = True then begin
      for i := 0 to rvv.Count - 1 do begin
        rv := rvv.Strings[i];
        StringChangeEx(rv, ' ', '', True);
        // debug(rv);
        red_versions := _redversion_insert_version(rv, '', red_versions);
      end;
    end;

    for i:= 0 to GetArrayLength(red_versions) - 1 do begin
      debug(red_versions[i].version + ' / ' + red_versions[i].tag);
    end;

    detect_red_installations(_ppage);
    main.red.versions := red_versions;

    _ppage.SetText('Preparing the Node.js version selection page...', '');
    Result := PrepareNodeVersionSelectionPage() and Result;

    // _ppage.SetText('Preparing the Node-RED version selection page...', '');
    // Result := PrepareREDVersionSelectionPage() and Result;

    _ppage.SetText('Preparing the Node-RED installation setup page...', '');
    Result := MakeRedActionPage(main.pages.red_action) and Result;

    _ppage.SetText('Verifying the Python environment...', '');
    main.python.version := '{#py}';

    // Check if npm is already configured correctly
    if Length(main.node.current) > 0 then begin
      main.python.path := GetNPMPythonConfig();
      if Length(main.python.path) > 0 then begin
        main.python.npm := True;
      end;
    end;

    // Check if we have at least a python installation
    if Length(main.python.path) < 1 then
      main.python.path := GetPythonPath('');

    _ppage.SetText('Verifying the VisualStudio BuildTools setup...', '');
    // if not FileExists(ExpandConstant('{tmp}\vswhere.exe')) then ExtractTemporaryFile('vswhere.exe');

    // Check if vswhere is able to find an installation
    main.vs.version := GetVSBuildToolsVersion();

  finally
    _ppage.Hide();
  end;

end;


// ** END
// ** Data Prepartion Page
// *****

// *****
// ** Event Implementations
// **

function InitializeSetup(): boolean;

var
  i, ii, having: integer;
  node_versions: TStringList;
  nv: string;
  nvv: array of integer;
  default_version: string;

begin

  // check the bit status of this platform
  if IsWin64() = True then begin
    debug('We are going to install the 64bit version of Node.js.');
    main.bit := 'x64';
    main.HKLM := HKEY_LOCAL_MACHINE_64;
  end else begin
    main.bit := 'x86';
    main.HKLM := HKEY_LOCAL_MACHINE;
  end;

  default_version := '{#Trim(NodeVersionRecommended)}';
  main.node.default := -1;

  // There's one issue with the following logic:
  // We cannot test *NOW*, if the version given in the INI is actually available - by referencing back to the nodejs website
  // It might be though, that we offer a version here that doesn't exist.
  // We'll do this check later ... but here it might create comments!

  // Read the major Node.js versions - offered to install - from the INI
  node_versions := TStringList.Create;
  node_versions.Duplicates := dupIgnore

  // Verify that it's just a number (nothing else)
  // Att: Additional ',' appended to #nodeVersions - to satisfy SplitString!
  if SplitString('{#nodeVersions},', ',', node_versions) = True then begin
    for i := 0 to node_versions.Count - 1 do begin
      nv := node_versions.Strings[i];
      StringChangeEx(nv, ' ', '', True);
      try
        ii := StrToIntDef(nv, -1);
        if ii > 0 then begin
          having := GetArrayLength(nvv);
          SetArrayLength(nvv, having + 1);
          nvv[having] := ii;

          if nv = default_version then
            main.node.default := ii;

        end;
      except
        // That's fine as well...
      end;
    end;
  end;

  // Bail out - bail out!
  if GetArrayLength(nvv) < 1 then begin
    MsgBox('Looks like no information was given which' + #13#10 + 'versions of Node.js I should offer for installation.' + #13#10 + #13#10 + 'Stopping here...',
      mbCriticalError,
      MB_OK);
    Result:=False;
    Exit;
  end;

  // Now sort it ascending
  QuickSort(nvv, Low(nvv), High(nvv));

  // Keep this for later use
  main.node.majors := nvv;

  // check if default version is supported.
  // If not, let default version become min version
  if (main.node.default < 0) then begin
    main.node.default := nvv[0];
    debug('** Warning: INI-File defined ''' + default_version + ''' as recommended version - that yet is not in validated list of supported versions. Forced to v' + IntToStr(main.node.default) + '!');
  end;

  Result:=True;

end;


procedure InitializeWizard();
var
  
  i, having: Integer;
  msg: String;
  wf: TWizardForm;

begin

  // wpWelcome
  having := GetArrayLength(main.node.majors);
  if having > 0 then begin

    // Compose the message of the supported node.js versions
    msg := ''
    if having > 1 then begin
      for i := 0 to having - 2  do begin
        if Length(msg) > 0 then begin
          msg := msg + ', ';
        end;
        msg := msg + IntToStr(main.node.majors[i]);
      end;
    end;

    if Length(msg) > 0 then begin
      msg := msg + ' or ';
    end;

    msg := msg + IntToStr(main.node.majors[having - 1]) + ' LTS';
    
    wf := GetWizardForm();

    // https://github.com/jrsoftware/issrc/blob/main/Projects/MsgIDs.pas
    wf.WelcomeLabel2.Caption := FMTMessage(SetupMessage(msgWelcomeLabel2), [IntToStr(main.node.majors[0]), IntToStr(main.node.default), msg]);

  end;

end;

function NextButtonClick(CurPageID: Integer): Boolean;

var
  // bit, nv: String;
  check: Boolean;
  i: Integer;

  // node_versions: TStringList;
  // node_license_path, red_license_path: String;
  // lbl: TNewStaticText;

  having: integer;
  file, nv, msg: string;

  _dp: TDownloadWizardPage;

  red_image: string;

begin

  Result := True;
  red_image := 'node-red.bmp';

  if CurPageID = wpWelcome then begin
  end;

  if CurPageID = wpWelcome then begin

    check := FileExists(ExpandConstant('{tmp}\{#NodeLicenseTmpFileName}'));
    check := FileExists(ExpandConstant('{tmp}\{#REDLicenseTmpFileName}')) and check;
    check := FileExists(ExpandConstant('{tmp}\vswhere.exe')) and check;

    having := GetArrayLength(main.node.versions);
    if having > 0 then begin
      for i:=0 to having -1 do begin
        file := main.node.versions[i].file;
        if Length(file) > 0 then
          check := FileExists(file) and check;
      end;
    end else
      check := False;
    
    if not check then begin

      _dp := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), @OnDownloadProgress);

      set_image(imgRED);

      // Download Licenses
      _dp.Clear;

      _dp.Add('{#NodeLicenseURL}', '{#NodeLicenseTmpFileName}', '');
      _dp.Add('{#REDLicenseURL}', '{#REDLicenseTmpFileName}', '');
      _dp.Add('{#VSWhereURL}', 'vswhere.exe', '');

      // to run the backup option if npm is not present
      _dp.Add('https://github.com/node-red/node-red/releases/latest', '{#REDLatestTmpFileName}', '');

      // Download the SHA file for the defined Node.js versions
      for i := 0 to GetArrayLength(main.node.majors) - 1 do begin

        nv := IntToStr(main.node.majors[i]);

        file := 'node' + nv + '.sha';
        _dp.Add('{#NodeDownloadURL}/latest-v' + nv + '.x/SHASUMS256.txt', file, '');      

      end;

      // Don't add additional files here!
      // Move them before the SHASUMS256's !!

      _dp.Show;
      try
        try
          _dp.Download; // This downloads the files to {tmp}

          // Result := True;
        except
          if _dp.AbortedByUser then begin
              Log('Aborted by user.')
              Result := False;
          end else begin
            
            // Very likely the last file of the nodejs SHASUMS256's may not be found & 404 will be returned.
            // This (404) is ok though.
            msg := GetExceptionMessage();
            if StringChangeEx(msg, '404', '404', True) = 0 then begin
              SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
              Result := False;
            end;
          end;
        end;
      finally
        _dp.Hide;
      end;

      if not Result then Exit;

      // Adjust caption for Node.js License page ( = Standard License page )
      msg := PageFromID(wpLicense).Caption;
      PageFromID(wpLicense).Caption := 'Node.js ' + msg;

      // Additional page for the Node-RED license acceptance
      main.pages.red_license := CreateREDLicensePage(wpLicense);

      // Inject the licenses
      WizardForm.LicenseMemo.Lines.LoadFromFile(ExpandConstant('{tmp}\{#NodeLicenseTmpFileName}'));
      main.pages.red_license.RichEditViewer.Lines.LoadFromFile(ExpandConstant('{tmp}\{#REDLicenseTmpFileName}'));

      // Need to do this here, as the Wizard seems to change the Top of the
      // radio buttons on the License page after loading the license file.
      REDLicenseAcceptedRadio.Top := WizardForm.LicenseAcceptedRadio.Top;
      REDLicenseNotAcceptedRadio.Top := WizardForm.LicenseNotAcceptedRadio.Top;

      // Additional page to select to be installed Node.js version
      main.pages.node_version := CreateInputOptionPage(wpLicense,
        'Node.js Installation', 'Select the Node.js version you like to install.',
        '',
        True, False);

{      // Additional page to select to be installed Node-RED version
      main.pages.red_version := CreateInputOptionPage(main.pages.red_license.ID,
        'Node-RED Installation', 'Select the Node-RED version you like to install.',
        '',
        True, False);
}

      // Additional page to select to be installed Node-RED version
      main.pages.red_action := CreateInputOptionPage(main.pages.red_license.ID,
        'Node-RED Installation', 'Configure the Node-RED installation setup.',
        '',
        False, True);

      // pages will be initialized @ RunDataPrepPage
      Result := RunDataPrepPage();

      debug('Result:= ' + BoolToStr(Result));
      
    end;
  end;
  
  if main.pages.node_version <> nil then begin    
    if CurPageID = main.pages.node_version.ID then begin
      // run_silent & selected have a dedicated click event handler
      main.node.install_tools := _nodeVersion_cbInstallWindowsTools.Checked;
    end;
  end;
  
end;

function BackButtonClick(CurPageID: Integer): Boolean;
begin
  
  Result:=True;

  if main.pages.node_version <> nil then begin    
    if CurPageID = main.pages.node_version.ID then begin
      // run_silent & selected have a dedicated click event handler
      main.node.install_tools := _nodeVersion_cbInstallWindowsTools.Checked;
    end;
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
var

  wf: TWizardForm;

  node_image, red_image: String;
   
  // idNodeVersionSelection: integer;
  // idDownload: integer;
  // idREDLicense: integer;

begin

  wf := GetWizardForm();

  if CurPageID = wpWelcome then begin
  
    // wf.WelcomeLabel2.Caption = 
    

  end;

  node_image := 'nodejs.bmp';
  red_image := 'node-red.bmp';

  if FileExists(ExpandConstant('{tmp}\' + node_image)) = False then ExtractTemporaryFile(node_image);
  if FileExists(ExpandConstant('{tmp}\' + red_image)) = False then ExtractTemporaryFile(red_image);

  // Update Next button when user gets to second license page
  if main.pages.download <> nil then begin
    if CurPageID = main.pages.download.ID then begin
      // WizardForm.WizardSmallBitmapImage.Bitmap.LoadFromFile(ExpandConstant('{tmp}\' + red_image));
      WizardForm.WizardSmallBitmapImage.Visible := False;
    end;
  end;

  if CurPageID = wpLicense then begin
    set_image(imgNODE);
  end;

  if main.pages.node_version <> nil then begin
    if CurPageID = main.pages.node_version.ID then begin
      set_image(imgNODE);
    end;
  end;

    // Update Next button when user gets to second license page
  if main.pages.red_license <> nil then begin
    if CurPageID = main.pages.red_license.ID then begin
      set_image(imgRED);
      CheckREDLicenseAccepted(nil);
    end;
  end;

  if CurPageID = wpReady then begin
    // Title & ReadyLabel1 changed via 'Messages' section.
    wf.ReadyLabel.Caption := 'Click Install to continue with the installation, or click Back if you want to review or change any settings.';
    wf.ReadyMemo.Font.Name := 'Courier New';
    // wf.ReadyMemo.Font.Size := wf.ReadyMemo.Font.Size + 1;
    wf.ReadyMemo.Font.Style := [fsBold];
    wf.ReadyMemo.ScrollBars := ssVertical;

    wf.NextButton.Enabled := not main.red.error;

  end;

  // Customize FinishedPage in case of error.
  if CurPageID = wpFinished then begin
    if main.error.status then begin
      WizardForm.FinishedHeadingLabel.Caption := 'Node-RED Setup Error';
      WizardForm.FinishedLabel.Caption := ExpandConstant('{cm:MSG_FAILED_FINISHED1}') + #13#10#13#10 + '>> ' + main.error.msg + #13#10#13#10 + ExpandConstant('{cm:MSG_FAILED_FINISHED2}');
      WizardForm.FinishedLabel.AdjustHeight();

      wizardform.YesRadio.Top := WizardForm.FinishedLabel.Top + WizardForm.FinishedLabel.Height + ScaleY(8);
    end;  
  end;


end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  m: string;
  i, ii, l, f: integer;
  tag: integer;
  p, pp: string;
  c, cc: boolean;
  _global: boolean;

  _paths: TStringList;
  _error: boolean;

  _kind: sREDInstallationKind;

begin
  
  _paths:= TStringList.Create;
  _error := False;

  m:=     '*****************' + NewLine;
  m:= m + '***  Node.js  ***' + NewLine;
  m:= m + NewLine;

  if Length(main.node.current) > 0 then begin
    m:= m + 'Currently installed: ' + main.node.current + NewLine;
    if Length(main.node.selected) > 0 then begin
      m:= m + 'Changing to:         ' + main.node.selected + NewLine;
    end;
  end else begin
    m:= m + 'Currently NOT installed.' + NewLine;
    m:= m + 'Installing: ' + main.node.selected + NewLine;
  end;

  // m:= m + NewLine;

  if Length(main.node.selected) > 0 then begin
    if main.node.run_silent then begin
      m:= m + '+ Node.js installer will run in the background.' + NewLine;

      if main.node.install_tools then
        m:= m + '+ Additional Tools for Windows will be installed.' + NewLine;
    
    end;
  end;

  m:= m + NewLine;
  m:= m + NewLine;
  m:= m + '******************' + NewLine;
  m:= m + '***  Node-RED  ***' + NewLine;

  SetArrayLength(main.red.run, GetArrayLength(main.red.installs));

  l:=0;
  for i:= 0 to GetArrayLength(main.red.installs) - 1 do begin
    if main.red.installs[i].kind <> rikVoid then begin
      main.red.run[l] := i;
      l:= l + 1;
    end;
  end;

  SetArrayLength(main.red.run, l);

  _global:= False;
  tag := 0;

  for i:=0 to GetArrayLength(main.red.installs) - 1 do begin

    _kind := main.red.installs[i].kind;
    if _kind = rikVoid then continue;
    
    m:= m + NewLine;

    if l > 1 then begin
      tag := tag + 1;
      m:= m + '#' + IntToStr(tag) + ': ' + NewLine;
    end;

    if Length(main.red.installs[i].version) > 0 then begin
      m:= m + 'Currently installed: ' + main.red.installs[i].version + NewLine;
      c:= True;
    end else begin
      m:= m + 'NEW installation.' + NewLine;
      c:= False;
    end;

    if main.red.installs[i].action = 'remove' then begin
      m:= m + 'This installation will be REMOVED!' + NewLine;
    end else if Length(main.red.installs[i].action) < 1 then begin
      if ((_kind = rikGlobal) or (Length(main.red.installs[i].path) < 1)) then begin
        m:= m + '+ Global installation.' + NewLine;
        if not _global then begin
          _global := True;
        end else begin
          m:= m + '  *** ERROR: Another global installation configured!' + NewLine;
          _error := True;
        end;
      end;
    end else begin

      if c then begin
        m:= m + 'Changing to:         ' + main.red.installs[i].action + NewLine;
      end else begin
        m:= m + 'Installing: ' + main.red.installs[i].action + NewLine;
      end;

      if ((_kind = rikGlobal) or (Length(main.red.installs[i].path) < 1)) then begin
        m:= m + '+ Global installation.' + NewLine;
        if not _global then begin
          _global := True;
        end else begin
          m:= m + '  *** ERROR: Another global installation configured!' + NewLine;
          _error := True;
        end;
      end else begin

        pp := main.red.installs[i].path;

        if _kind = rikPath then begin 
          if DirExists(pp) and FileExists(pp + '\package.json') then begin
            // main.red.installs[i].calc.path := pp;
          end else begin
            _kind := rikNew;
          end;
        end;

        if _kind = rikNew then begin

          ii:= -1;
          p:= '';
          // lbl:= main.red.installs[i].name;
          // pp:= main.red.installs[i].path;
          debug('mri: ' + pp);

          repeat 
            
            // First Check:
            // Is another installation already using this path?
            // c := False if YES!
            f:=-1;
            c:= (_paths.IndexOf(pp) < 0);
            // if _paths.IndexOf(pp) > -1 then c := (f < 0);

            debug('c: ' + BoolToStr(c));

            if c then begin

              // Second Check:
              // We take it, if the directory doesn't exist.
              // cc := True => Dir does NOT exist!
              if not DirExists(pp) then begin
                cc:=True;
                break;
              end;

            end;

            // Final Check
            // Is this Dir empty?
            c:= isEmptyDir(pp) and c;

            if not c then begin

              // build a new path!
              ii:=ii+1;
              p:= 'Node-RED';
              if ii > 0 then
                p:= p + '(' + IntToStr(ii) + ')';
              
              pp:= main.red.installs[i].path + '\' + p;
              debug('pp: ' + pp);
            end;

          until c;

          main.red.installs[i].calc.path:= pp;

        end;

        _paths.Add(pp);

        m:= m + '+ Installation path: ' + pp + NewLine;
        if cc then 
          m:= m + '  + Directory will be created.' + NewLine;

      end;
    end;

    if main.red.installs[i].autostart then begin
      m:= m + '+ Creating entry in Autostart group.' + NewLine;
      if main.red.installs[i].port > 0 then 
        p := IntToStr(main.red.installs[i].port)
      else
        p := '(Default)';

      m:= m + '  + Port: ' + p + NewLine;
    end;

    if main.red.installs[i].icon then begin
      m:= m + '+ Creating Desktop icon.' + NewLine;
      if main.red.installs[i].port > 0 then 
        p := IntToStr(main.red.installs[i].port)
      else
        p := '(Default)';

      m:= m + '  + Port: ' + p + NewLine;
    end;

  end;

  main.red.error := _error;

{
  // UI Test only!
  m:= m + NewLine;
  m:= m + NewLine;

  m:= m + '********************************************' + NewLine;
  m:= m + '*** It is safe to press ''Install'' now.   ***' + NewLine;
  m:= m + '*** This version is for UI Test only.    ***' + NewLine;
  m:= m + '*** It does NOT install Node-RED!        ***' + NewLine;
  m:= m + '********************************************' + NewLine;
}
  Result := m;
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  i: integer;

  _dp: TDownloadWizardPage;

  _nv, _msi, _sha: string;

  res: TArrayOfString;
  md5: string;

begin

  _nv := main.node.selected;
  main.error.status := False;

  if Length(_nv) < 1 then begin
    Result := '';
    Exit;
  end;

  Result := 'Failed to prepare the installation: ' + #13#10 + #13#10;

  // Downloading the requested version of node.js
  _dp := CreateDownloadPage('Downloading the Node.js installer...', '', nil);
  _dp.Clear;

  _sha := '';
  for i:=0 to GetArrayLength(main.node.versions) - 1 do begin
    if main.node.versions[i].latest = _nv then begin
      _sha := main.node.versions[i].sha;
      _msi := main.node.versions[i].msi;
      break;
    end;
  end;

  if Length(_msi) < 1 then begin
    Result := Result + 'Node.js download file name is missing.';
    Exit;
  end;

  _dp.Add('{#NodeDownloadURL}/v' + main.node.selected + '/' + _msi, 'node.msi', _sha);      

  if main.node.install_tools then begin
    if Length(main.python.path) < 1 then begin
      // No python! Get python to install later.
      if main.bit = 'x64' then begin
        _dp.Add('https://www.python.org/ftp/python/{#py}/python-{#py}-amd64.exe', 'python_installer.exe', '');
      end else begin
        _dp.Add('https://www.python.org/ftp/python/{#py}/python-{#py}-win32.exe', 'python_installer.exe', '');
      end;
    end;
  end;

  if main.node.install_tools then begin
    if Length(main.vs.version) < 1 then begin
      // Need to download the BUilsTools!
      _dp.Add('{#VSBuildToolsURL}', 'VSBT_installer.exe', '');
    end;
  end;

  _dp.Show;
  try
    try
      _dp.Download; // This downloads the files to {tmp}
    except
      if _dp.AbortedByUser then begin
        Result := Result + 'File download aborted.';
        Exit;
      end else begin
        Result := Result + AddPeriod(GetExceptionMessage);
        Exit;
      end;
    end;
  finally
    _dp.Hide;
  end;

  if main.node.install_tools then begin
    if Length(main.python.path) < 1 then begin
      // Verify MD5 for downloaded python installer.
      try
        md5 := GetMD5OfFile(ExpandConstant('{tmp}\python_installer.exe'));
      except
        Result := Result + 'Failed to calculate MD5 Sum of downloaded Installer for Python.';
        Exit;
      end;

      if ((main.bit = 'x64') xor (md5 = '{#PyMD5x64}'))  or ((main.bit = 'x86') xor (md5 = '{#PyMD5x86}')) then begin
        DeleteFile(ExpandConstant('{tmp}\python_installer.exe'));
        Result := Result + 'Invalid MD5 sum calculated for Installer for Python (' + main.bit + '): ' + md5;
        Exit;
      end;
    end;
  end;

  if (main.node.run_silent and main.node.install_tools) then begin
    SetArrayLength(res, 0);
    RunCMD('tasklist /FI "IMAGENAME eq setup.exe" /FO Table /NH', '', res);
    if GetArrayLength(res) > 0 then begin
      for i:=0 to GetArrayLength(res) - 1 do begin
        // If another program w/ image name 'setup.exe' is running,
        // we cannot detect if the Visual Studio BuildTools Installer has finished it's job.
        if StringChangeEx(res[i], 'setup.exe', '', True) > 0 then begin
          Result := Result + 'Another installer seems to be running.' + #13#10 + 'Cannot install Native Build Tools for Node.js in the background.'
          Exit;
        end;
      end;
    end;
  end;

  Result := '';

end;


function GetPythonPath(param: string): string;
var
  res: array of string;
  parts: TStringList;
  i: integer;
begin
  // Check if we have a python installation
  SetArrayLength(res, 0);
  RunCMD('python -V', '', res);
  if GetArrayLength(res) = 1 then begin
    // Python 3.11.2
    parts := TStringList.Create();
    if SplitString(res[0], ' ', parts) then begin
      if parts[0] = 'Python' then begin
        i := CompareVersions(parts[1], '3.10');
        if i >= 0 then begin
          SetArrayLength(res, 0);
          RunCMD('where python', '', res);
          if GetArrayLength(res) > 0 then begin
            if FileExists(res[0]) then begin
              Result := res[0];
            end;
          end;
        end;
      end;
    end;
  end;
end;


function GetVSBuildToolsVersion(): string;
var
  res: array of string;
  version: TStringList;
  _btv: string;
begin
  // Check if vswhere is able to find an installation
  SetArrayLength(res, 0);
  RunCMD('vswhere -products Microsoft.VisualStudio.Product.BuildTools -property installationVersion', ExpandConstant('{tmp}'), res);
  if GetArrayLength(res) = 1 then begin
    version := TStringList.Create();
    if SplitString(res[0], '.', version) then begin
      if version.Count > 1 then begin
        if StrToIntDef(version[0], 0) > StrToIntDef('{#VSBuildToolsMinVersion}', 15) then begin   // 15 = 2015
          _btv := res[0];
          SetArrayLength(res, 0);
          RunCMD('vswhere -products Microsoft.VisualStudio.Product.BuildTools -property isComplete', ExpandConstant('{tmp}'), res);
          if GetArrayLength(res) = 1 then begin
            if res[0] = '1' then begin
              SetArrayLength(res, 0);
              RunCMD('vswhere -products Microsoft.VisualStudio.Product.BuildTools -property isLaunchable', ExpandConstant('{tmp}'), res);
              if GetArrayLength(res) = 1 then begin
                if res[0] = '1' then begin
                 Result := _btv;
                  debug('BuildTools found: ' + main.vs.version);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function GetNPMPythonConfig(): string;
var
  res: array of string;
begin
  // Check if npm is already configured correctly
  SetArrayLength(res, 0);
  RunCMD('npm config get python', '', res);
  if GetArrayLength(res) = 1 then begin
    if res[0] <> 'undefined' then begin
      if FileExists(res[0]) then begin
        Result := res[0];
      end;
    end;
  end;
end;



// Used while installing the node.js
// https://stackoverflow.com/questions/34336466/inno-setup-how-to-manipulate-progress-bar-on-run-section
procedure SetMarqueeProgress(Marquee: Boolean);
begin
  if Marquee then begin
    WizardForm.ProgressGauge.Style := npbstMarquee;
  end else begin
    WizardForm.ProgressGauge.Style := npbstNormal;
  end;
end;


// To be used for the lengthy status messages in the [Run] section
procedure SetupRunConfig();
begin
  SetMarqueeProgress(True);

  WizardForm.StatusLabel.WordWrap := True;
  WizardForm.StatusLabel.AdjustHeight();
end;

function GetNodeVersionMain(Param: string): string;
begin

  debug('gnv: ' + Param);

  if Param = 'current' then
    Result := main.node.current;

  if Param = 'selected' then
    Result := main.node.selected;

  debug('gnv: ' + Result);

end;

function GetInstalledNodeGUID(nv: string): string;
var
  _base: string;
  _keys: TArrayOfString;
  i: integer;
  _name: string;
  _nv: string;

begin
  _base := 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall';
  Result := '';

  if Length(nv) < 1 then nv := main.node.current;

  if RegGetSubkeyNames(main.HKLM, _base, _keys) then begin
    for i:= 0 to GetArrayLength(_keys) - 1 do begin
      if RegQueryStringValue(main.HKLM, _base + '\' + _keys[i], 'DisplayName', _name) = True then begin
        if _name = 'Node.js' then begin
          if RegQueryStringValue(main.HKLM, _base + '\' + _keys[i], 'DisplayVersion', _nv) = True then begin
            if _nv = nv then begin
              Result := _keys[i];
              break;
            end;
          end;
        end;
      end;
    end;
  end;

end;


{
// Check function to indicate if Node.js is currently installed
// mode = silent: Return true only when 'Run in Background' was selected
// mode = show; Return true only when 'Run in Background' was *NOT* selected
function CheckNodeInstall(mode: string): Boolean;
begin
  if main.error.status then Exit;
  // Result := (Length(main.node.selected) > 0) and (main.node.selected <> main.node.current);
  // We offer to re-install the current version!
  Result := Length(main.node.selected) > 0;

  if mode = 'silent' then
    Result := Result and main.node.run_silent;

  if mode = 'show' then 
    Result := Result and (not main.node.run_silent);

end;
}



// Each parameter MUST be string, integer or boolean.
// No SET type allowed here :(( !
function RunCheck(step, param: string): Boolean;
var
  bool: boolean;
  i: integer;
  str: string;
  _kind: sREDInstallationKind;

begin
  if main.error.status then Exit;

  if step = 'rcsNodeUninstall' then begin
    // We uninstall only if we shall install as well.
    if RunCheck('rcsNodeInstall', '') then
      Result := Length(GetInstalledNodeGUID(main.node.current)) > 0;
  end;

  if step = 'rcsNodeInstall' then begin
    Result := Length(main.node.selected) > 0;
    if param = 'silent' then
    // Return true only when 'Run in Background' was selected
      Result := Result and main.node.run_silent;
    if param = 'show' then
    // Return true only when 'Run in Background' was *NOT* selected
      Result := Result and (not main.node.run_silent);
  end;

  if step = 'rcsPython' then begin
    if Length(main.node.selected) < 1 then Exit;
    if not main.node.run_silent then Exit;
    if not main.node.install_tools then Exit;
    bool := StrToBool(param);
    if bool then begin
      Result := Length(main.python.path) > 0;
    end else begin
      Result := Length(main.python.path) = 0;
    end;
  end;

  if step = 'rcsVSBuildTools' then begin
    if Length(main.node.selected) < 1 then Exit;
    if not main.node.run_silent then Exit;
    if not main.node.install_tools then Exit;
    Result := Length(main.vs.version) = 0;
  end;

  if step = 'rcsPythonConfig' then begin
    if main.python.npm then Exit;
    Result := RunCheck('rcsPython', 'yes');
  end;

  if step = 'rcsREDRemove' then begin
    i := StrToIntDef(param, -1);
    if i < 0 then Exit;
    if not (i < GetArrayLength(main.red.run)) then Exit;
    i := main.red.run[i];
    _kind := main.red.installs[i].kind;
    if _kind = rikVoid then Exit;
    if _kind = rikNew then Exit;
    Result := Length(main.red.installs[i].action) > 0;
  end;

  if step = 'rcsREDInstall' then begin
    i := StrToIntDef(param, -1);
    if i < 0 then Exit;
    if not (i < GetArrayLength(main.red.run)) then Exit;
    i := main.red.run[i];
    if main.red.installs[i].kind = rikVoid then Exit;
    str := main.red.installs[i].action;
    if LowerCase(str) = 'remove' then Exit;
    Result := Length(str) > 0;
  end;

end;

{
// Check function to indicate if Node.js is currently installed and shall be uninstalled
function CheckNodeUninstall(): Boolean;
begin
  if main.error.status then Exit;
  if CheckNodeInstall('') then 
    Result := Length(GetInstalledNodeGUID(main.node.current)) > 0;
end;
}

function GetNodeVersionCall(msg: string): string;
var
  _page: TOutputMarqueeProgressWizardPage;
  res: array of string;
  _nv: string;

begin

  Result := '';
    _page := CreateOutputMarqueeProgressPage('Verifying Node.js installation', msg);

    try
      _page.Show();

      RunCMD('node --version', '', res);
      if GetArrayLength(res) = 1 then begin
        _nv := res[0];
        if StringChangeEx(_nv, 'v', '', True) = 1 then begin
          Result := _nv;
        end;
      end;

    finally
      _page.Hide();
    end;
end;

{
function GetNodeVersionReg(): string;
var
  _nv: string;
begin
  Result := ''
  if RegKeyExists(main.HKLM, 'SOFTWARE\Node.js') then begin
    if RegQueryStringValue(main.HKLM, 'SOFTWARE\Node.js', 'Version', _nv) = True then begin
      Result := _nv;
    end;
  end;
end;
}

function GetNodeDataReg(param: string): string;
var
  _data: string;
  _key: string;
begin
  Result := ''

  param := LowerCase(param);
  if param = 'version' then begin
    _key := 'Version'
  end else if param = 'path' then begin
    _key := 'InstallPath'
  end else begin
    Exit;
  end;

  if RegKeyExists(main.HKLM, 'SOFTWARE\Node.js') then begin
    if RegQueryStringValue(main.HKLM, 'SOFTWARE\Node.js', _key, _data) = True then begin
      Result := _data;
    end;
  end;
end;


// Each parameter MUST be string, integer or boolean.
// No SET type allowed here :(( !
procedure Confirm(step, param: string);
var
  _nvC: string;
  _nvR: string;
  _pyp: string;
  _vs: string;

begin

  if step = 'csNoNode' then begin
    _nvC := GetNodeVersionCall('Verifying that Node.js is not installed.');
    _nvR := GetNodeDataReg('version');

    if (Length(_nvC) > 0) or (Length(_nvR) > 0) then begin
      main.error.msg := 'Detected Node.js despite it should have been removed.';
    end;
  end;

  if step = 'csNode' then begin
    _nvC := GetNodeVersionCall('Verifying that Node.js is not installed.');
    _nvR := GetNodeDataReg('version');

    if not ((param = _nvC) and (_nvC = _nvR)) then begin
      main.error.msg := 'Failed to confirm that Node.js v' + param + ' is installed.'
    end;
  end;

  if step = 'csFile' then begin
    if not FileExists(param) then begin
      main.error.msg := 'File not found: ' + param;
    end;
  end;

  if step = 'csPython' then begin
    _pyp := GetPythonPath('');
    if Length(_pyp) < 1 then begin
      main.error.msg := 'Failed to confirm that Python is installed.'
    end else begin
      main.python.path := _pyp;
    end;
  end;

  if step = 'csVSBuildTools' then begin
    _vs := GetVSBuildToolsVersion();
    if Length(_vs) < 1 then begin
      main.error.msg := 'Failed to confirm that the VisualStudio BuildTools are installed.'
    end else begin
      main.vs.version := _vs;
    end;
  end;
  
  if step = 'csPythonConfig' then begin
    _pyp := GetNPMPythonConfig();
    if Length(_pyp) < 1 then begin
      main.error.msg := 'Failed to configure Node.js to know the Python path.'
    end;
  end;

  if Length(main.error.msg) > 0 then
    main.error.status := True;

end;

{
procedure ConfirmNoNode();
var
  _nvC: string;
  _nvR: string;

begin
  _nvC := GetNodeVersionCall('Verifying that Node.js is not installed.');
  _nvR := GetNodeVersionReg();

  if (Length(_nvC) > 0) or (Length(_nvR) > 0) then begin
    main.error.status := True;
    main.error.msg := 'Detected Node.js despite it should have been removed.';
  end;
end;


procedure ConfirmNode(nv: string);
var
  _nvC: string;
  _nvR: string;

begin
  _nvC := GetNodeVersionCall('Looking for Node.js v' + nv);
  _nvR := GetNodeVersionReg();

  if not ((_nvC = nv) and (_nvC = _nvR)) then begin
    main.error.status := True;
    main.error.msg := 'Failed to confirm that Node.js v' + nv + ' is installed.'
  end;
end;
}

function GetNodeInstallSilent(param: string): string;
begin
  if main.node.run_silent then Result := '/qn';
end;

function GetREDInstallsData(data: string; index: integer): string;
var
  p: string;

begin

  debug('GetREDInstallsData: ' + data + ' / ' + IntToStr(index));
  
  if index < 0 then Exit;
  if not (index < GetArrayLength(main.red.installs)) then Exit;

  if data = 'global' then begin
    // Explicitely - for an existing installation
    if main.red.installs[index].kind = rikGlobal then Result := '-g';
    // implicitey - for a new installation
    if Length(GetREDInstallsData('path', index)) < 1 then Result := '-g';

  end;

  if data = 'version' then Result := main.red.installs[index].version;
  if data = 'action' then Result := main.red.installs[index].action;

  if data = 'path' then begin
    // For new installations, the path was calculated.
    p := main.red.installs[index].calc.path;
    // In case this is an already existing installation
    // a new path will only be calculated if there was an issue with the given one.
    if main.red.installs[index].kind = rikPath then begin
      // if no path was calculated, take the given!
      if Length(p) < 1 then 
        p := main.red.installs[index].path;
    end;
    Result := p;

    debug('GetREDActionData: path / ' + p);
  end;

  if data = 'action' then Result := main.red.installs[index].action;

end;

// This functoin translates between the main.red.run index & the main.red.installs index
function GetREDRunData(data: string; index: integer): string;
begin
  if index < 0 then Exit;
  if not (index < GetArrayLength(main.red.run)) then Exit;
  Result := GetREDInstallsData(data, main.red.run[index]);
end;

function GetREDActionGlobal(param: string): string;
begin
  Result := GetREDRunData('global', StrToIntDef(param, -1));
end;

function GetREDActionVersion(param: string): string;
begin
  Result := GetREDRunData('version', StrToIntDef(param, -1));
end;

function GetREDActionAction(param: string): string;
begin
  Result := GetREDRunData('action', StrToIntDef(param, -1));
end;

function GetREDActionPath(param: string): string;
begin
  Result := GetREDRunData('path', StrToIntDef(param, -1));
end;

function GetREDCurrentMsg(param: string): string;
var
  _rv: string;
  _path: string;
  i: integer;

begin
  i := StrToIntDef(param, -1);
  if i < 0 then begin
    Result := 'GetREDCurrentMsg / ERROR: ' + param;
    Exit;
  end;

  _rv := GetREDRunData('version', i);
  _path := GetREDRunData('path', i);

  if Length(_path) < 1 then begin
    Result := 'globally installed Node-RED v' + _rv;
  end else begin
    Result := 'Node-RED v' + _rv + ' @ ' + _path;
  end;

end;

function GetREDActionMsg(param: string): string;
var
  _rv: string;
  _path: string;
  i: integer;

begin
  i := StrToIntDef(param, -1);
  if i < 0 then begin
    Result := 'GetREDActionMsg / ERROR: ' + param;
    Exit;
  end;

  _rv := GetREDRunData('action', i);
  _path := GetREDRunData('path', i);

  if Length(_path) < 1 then begin
    Result := 'globally installed Node-RED v' + _rv;
  end else begin
    Result := 'Node-RED v' + _rv + ' @ ' + _path;
  end;

end;


procedure REDPrepare(param: string);
var
  _path: string;
  _pkg: string;
  // _json: TJsonParser;
  // _json: string;
  index: integer;
  _kind: sREDInstallationKind;

  _key, k, _root: string;
  i: integer;
  error: boolean;

begin

  // We have no option to cancel the installation (step) at this stage.
  // Thus even if we know there's something wrong, we need to accept the incoming impact. 
  if main.error.status then Exit;
  index := StrToIntDef(param, -1);
  if index < 0 then Exit;
  if not (index < GetArrayLength(main.red.run)) then Exit;
  index := main.red.run[index];

  _kind := main.red.installs[index].kind;
  if _kind = rikVoid then Exit;

  _path := GetREDInstallsData('path', index);
  // if Length(_path) < 1 then Exit;

  // Start with Bookkeeping in the registry!
  // This ensures that - whatever happens - next time the installer runs we'll know where to look for a NR installation
  _key := main.red.installs[index].key;
  if Length(_key) < 1 then begin

    _root := AddBackslash('{#REDInstallationsRegRoot}');

    if main.red.installs[index].kind = rikGlobal then begin
      _key := _root + '0000';

    end else begin
      i := 0;
      repeat
        i := i + 1;
        k := '0000' + IntToStr(i);
        // Pascal Script strings begin with index '1'!
        _key := _root + '\' + Copy(k, Length(k) - 3, 4);
      until (RegKeyExists(main.HKLM, _key) xor (i < 10000));   // True if False!!

      // I'm aware that this will 'break' if 10.000+ keys are present!
      main.red.installs[index].key := _key;
    end;

  end;
  
  error := False;
  if not RegWriteStringValue(main.HKLM, _key, 'Path', _path) then error := True;
  
  // Additional properties follow
  if not RegWriteStringValue(main.HKLM, _key, 'Name', main.red.installs[index].name) then error := True;
  if not RegWriteStringValue(main.HKLM, _key, 'Port', IntToStr(main.red.installs[index].port)) then error := True;

  if error then debug('Failed to create registry enties for Node-RED installation @ ' + _path);

  if Length(_path) > 0 then begin
    
    // Prepare the installation directory
    if not DirExists(_path) then begin
      debug('Creating Node-RED installation directory @ ' + _path);
      if not CreateDir(_path) then Exit;    // this will create troubles...
    end;

    _pkg := _path + '\package.json';
    if not FileExists(_pkg) then begin
      debug('Creating minimal package.json @ ' + _pkg);
      SaveStringToFile(_pkg, '{}' + #13#10, False);  // no need to check for success here...
    end;
  
  end;

  SetupRunConfig();

end;


function CreateREDIcon(index: integer; path: string; folder: string): string;
var
  i: integer;
  _name: string;
  _port: string;
  _run: string;
  
begin
  i:=0;
  repeat
    _name := main.red.installs[index].name
    if Length(_name) < 1 then _name := 'Node-RED';
    if i > 0 then begin
      _name := _name + '(' + IntToStr(i) + ')';
    end;
    _name := AddBackslash(folder) + _name + '.lnk';
    i:=i+1;
  until FileExists(_name) xor True;

  if main.red.installs[index].port > 0 then
    _port := ' --port ' + IntToStr(main.red.installs[index].port) + ' ';

  // non-global install needs path to node.exe
  if Length(path) > 0 then begin
    _run := '"' + AddBackslash(GetNodeDataReg('path')) + 'node.exe"';
    // _run := _run + ' node_modules\node-red\red.js';
  end;

  if Length(main.red.installs[index].name) > 0 then 
    _port := _port + ' --title "' + main.red.installs[index].name + '" ';

  if Length(path) < 1 then begin

    try
      // we launch the global install with 'node-red'
      Result := CreateShellLink(_name, 'Run Node-RED', 'node-red', _port, '', ExpandConstant('{app}\red.ico'), 0, SW_SHOW);
    except
      Result := '';
    end;

  end else begin

    _port := _port + ' --userDir . ';

    try 
      Result := CreateShellLink(_name, 'Run Node-RED', _run, ' node_modules\node-red\red.js ' + _port, path, ExpandConstant('{app}\red.ico'), 0, SW_SHOW);
    except
      Result := '';
    end;

  end;
end;


procedure REDFinalize(param: string);
var
  _action: string;
  _path: string;
  p: string;
  error: boolean;
  _rv: string;
  res: array of string;
  index: integer;
  _kind: sREDInstallationKind;

  _global: boolean;
  _name: string;

begin

  if main.error.status then Exit;
  index := StrToIntDef(param, -1);
  if index < 0 then Exit;
  if not (index < GetArrayLength(main.red.run)) then Exit;
  index := main.red.run[index];

  _kind := main.red.installs[index].kind;
  if _kind = rikVoid then Exit;

  _action := main.red.installs[index].action;
  if Length(_action) < 1 then Exit;
  if LowerCase(_action) = 'remove' then Exit;

  // First: Let's confirm that there's 
  // > a NR installation
  // > with the correct version 
  // > @ the requested path (or globally!)

  _path := GetREDInstallsData('path', index);

  p := _path;
  if Length(_path) < 1 then begin

    _global := True;

    // Get the details of the global installation
    SetArrayLength(res, 0);
    if RunCMD('npm config get prefix', '', res) then begin
      if GetArrayLength(res) > 0 then p := res[0];
    end;
  end;

  _rv := red_list(p);
  main.error.status := True;
  if Length(_rv) > 0 then begin
    if main.red.installs[index].action = _rv then begin
      main.error.status := False;
    end;
  end;
  
  if main.error.status then begin
      main.error.msg := 'Failed to confirm that installation of ' + GetREDActionMsg(param) + ' was successful.';
      Exit;
  end;

  // if Length(_path) < 1 then Exit;

  // Next: Create the Desktop / Autostart entries
  error := false;
  if main.red.installs[index].icon then begin
    _name := CreateREDIcon(index, _path, ExpandConstant('{autodesktop}'));
    if Length(_name) > 0 then begin
      if RegWriteStringValue(main.HKLM, main.red.installs[index].key, 'Icon', _name) = False then begin
        error := true;
      end
    end;
  end;

  if main.red.installs[index].autostart then begin
    _name := CreateREDIcon(index, _path, ExpandConstant('{autostartup}'));
    if Length(_name) > 0 then begin
      if RegWriteStringValue(main.HKLM, main.red.installs[index].key, 'Autostart', _name) = False then begin
        error := true;
      end
    end;
  end;

  if error then begin
    main.error.status := true;
    main.error.msg := 'Failed to create icon entries for Node-RED installation @ ' + _path;
  end;

end;


procedure REDRemove(param: string);
var
  _path, p: string;
  res: array of string;
  _rv: string;
  index: integer;
  _kind: sREDInstallationKind;
  _icon: string;

begin

  if main.error.status then Exit;
  index := StrToIntDef(param, -1);
  if index < 0 then Exit;
  if not (index < GetArrayLength(main.red.run)) then Exit;
  index := main.red.run[index];

  _kind := main.red.installs[index].kind;
  if _kind = rikVoid then Exit;

  // Let's ensure that there is NO Node-RED installation at the given path (or globally)!
  _path := GetREDInstallsData('path', index);

  p := _path;
  if Length(_path) < 1 then begin
    SetArrayLength(res, 0);
    if RunCMD('npm config get prefix', '', res) then begin
      if GetArrayLength(res) > 0 then p := res[0];
    end;
  end;

  if Length(p) < 1 then begin
    main.error.status := True;
    main.error.msg := 'Failed to get path to check for presence of Node-RED installation.';
    Exit;
  end;
  
  _rv := red_list(p);
  if Length(_rv) > 0 then begin
    main.error.status := True;
    main.error.msg := 'Failed to confirm the removal of ' +  GetREDCurrentMsg(param) + '.';
  end;

  // We do not remove the Registry entries here!
  // Once the installer runs another time, it will detect any obsolete entries & remove those then.
  // This allows us to keep the sequence (in the registry) for this run - without puzzling around.

  // Try to remove the Desktop Icon
  _icon := main.red.installs[index].registry.icon;
  if Length(_icon) > 0 then DeleteFile(_icon);

  // Try to remove the Autostart Icon
  _icon := main.red.installs[index].registry.autostart;
  if Length(_icon) > 0 then DeleteFile(_icon);

end;


#include <.\iss\redactionpage.iss>
#include <.\iss\reddetector.iss>