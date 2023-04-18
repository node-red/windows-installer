#include <.\IDP_1.5.1\idp.iss>
; #include <.\amProgress.API.pas>

; Windows Installer for Node-RED
; Definition file for the Inno Setup compiler.
; Copyright 2023 Ralph Wetzel
; License MIT
; https://www.github.com/ralphwetzel/node-red-windows-installer

; We configure all constants via an INI file.
; That's easier for maintenence rather than searching for things in the source code
#define INIFile RemoveBackslash(SourcePath) + "\setup.ini"

; Node.js Default Version - that we propose to install if none is present
#define NodeVersionRecommended ReadIni(INIFile, "node", "recommended")

; comma-separated list of major versions numbers we offer for download
#define NodeVersions ReadIni(INIFILE, "node", "versions", NodeVersionRecommended)                                              

; URL to download node.js license from
#define NodeLicenseURL ReadIni(INIFILE, "node", "license")
#define NodeLicenseTmpFileName "node.license"
                                              
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
PrivilegesRequired=lowest
VersionInfoCopyright={#ReadIni(INIFile, "installer", "copyright", "")}
VersionInfoDescription={#ReadIni(INIFile, "installer", "description", "")}
VersionInfoVersion={#ReadIni(INIFile, "installer", "version", "")}
WizardStyle=modern
WizardImageAlphaFormat=defined
WizardImageBackColor=clWhite
WizardImageStretch=True
WizardImageFile="graphics\sidebar\Node RED Side Graphic - BMP.bmp"
LicenseFile="LICENSE"
SetupIconFile={#SourcePath}\icons\node-red-icons.ico

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

[Code]
#include <.\iss\forward.iss>

// https://stackoverflow.com/questions/20584263/how-to-install-node-js-in-custom-folder-silently-on-windows
// msiexec /i node-v6.11.2-x64.msi TARGETDIR="C:\Program Files\nodejs\" ADDLOCAL="NodePerfCtrSupport,NodeEtwSupport,DocumentationShortcuts,EnvironmentPathNode,EnvironmentPathNpmModules,npm,NodeRuntime,EnvironmentPath" /qn

// https://stackoverflow.com/questions/18506820/innosetup-how-to-pass-a-two-dimensional-string-array-to-a-function
type
  TNodeVersion = record
    key: string;
    sha: string;
    latest: string;
    msi: string;
    default: boolean;
    file: string;
  end;
  TNodeVersionList = array of TNodeVersion;

  TREDVersion = record
    version: string;
    tag: string;
  end;

  TREDVersionArray = array of TREDVersion;

  TNodeData = record
    majors: array of integer;   // Supported major versions as read from the INI file
    versions: array of TNodeVersion;
    default: integer;
    selected: string;
    run_silent: boolean;
    install_tools: boolean;
    current: string;
    options: TStringList;
  end;


  sREDInstallationKind = (rikGlobal, rikPath, rikNew, rikVoid);

  rREDInstallation = record
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
    final_path: string;
    // add additional properties here!
  end;

  // TREDAction = (raNone, raInstall, raRemove);

  // TREDInstallationAction = record
  //   index: integer;
  //   line: integer;
  //   action: TREDAction;
  //   installation: TREDInstallation;
  // end;

  sREDListItemKind = (rlikNone, rlikAction, rlikPath, rlikGlobal, rlikPort, rlikLabel, rlikIcon, rlikAutostart);

  rREDListItem = record
    kind: sREDListItemKind;
    action: string;
    link: TObject;
  end;

  TREDData = record
    versions: array of TREDVersion;
    selected: string;
    current: string;
    installs: array of rREDInstallation;
    items: array of rREDListItem;
    npm: boolean;
    error: boolean;
  end;

  // TREDTargetObject = record
  //  data: string;
  //  target: TREDInstallationAction;
  // end;

  TPageID = record
    node_license: TOutputMsgMemoWizardPage;
    node_version: TInputOptionWizardPage;
    red_license: TOutputMsgMemoWizardPage;
    red_version: TInputOptionWizardPage;
    red_action: TInputOptionWizardPage;
    download: TDownloadWizardPage;
  end;

  TInstallerData = record
    node: TNodeData;
    red: TREDData;

    // As described in the documenttion, HKLM is going be set to HKEY_LOCAL_MACHINE_64 when running on 64bit systems 
    // *AND* "the system's processor architecture is included in the value of the ArchitecturesInstallIn64BitMode [Setup] section directive"
    // ArchitecturesInstallIn64BitMode [Setup] section yet is blank by default.
    // => HKLM - by default - always == HKEY_LOCAL_MACHINE
    // => set is as required: HKEY_LOCAL_MACHINE or HKEY_LOCAL_MACHINE_64
    HKLM: Integer;
    bit: String;

    pages: TPageID;

  end;

  TImageType = (imgNODE, imgRED, imgNONE);

var
  
  // DownloadPage: TDownloadWizardPage;

  // List of <string> Node.js version numbers
  // Read from the INI file
  // nodeVersions: TStringList;

  // Node.js version we propose to install (if none is installed)
  // nodeVersionDefault: string;

  // Record to hold relevant data of the Node.js versions
  // we offer to install 
  nodeVersionDetails: TNodeVersionList;

  // Node-RED License Acknowledgement Page
  // REDLicensePage: TOutputMsgMemoWizardPage;
  REDLicenseAcceptedRadio: TRadioButton;
  REDLicenseNotAcceptedRadio: TRadioButton;

  // NodeVersionSelectionPage: TInputOptionWizardPage;
  REDVersionSelectionPage: TInputOptionWizardPage;

  // As described in the documenttion, HKLM is going be set to HKEY_LOCAL_MACHINE_64 when running on 64bit systems 
  // *AND* "the system's processor architecture is included in the value of the ArchitecturesInstallIn64BitMode [Setup] section directive"
  // ArchitecturesInstallIn64BitMode [Setup] section yet is blank by default.
  // => HKLM - by default - always == HKEY_LOCAL_MACHINE
  // => set it as required: HKEY_LOCAL_MACHINE or HKEY_LOCAL_MACHINE_64

  // The list of all node versions offered to install
  nodeVersionSelectionOptions: TStringList;
  // the index the user choose to install
  nodeVersionSelectionOptionsSelected: Integer;

  // cbHideNodeInstaller: TNewCheckBox;
  // cbInstallWindowsTools: TNewCheckBox;

  // The Node-RED version the user selected for installation
  redVersionSelected: String;

  // This record holds all relevant data
  main: TInstallerData;

  testPage: TInputOptionWizardPage;
  testPage2: TInputOptionWizardPage;

procedure debug(message: string);
begin
  Log('[NR] ' + message);
end;

procedure debugInt(int: integer);
begin
  debug(IntToStr(int));
end;


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

function CompareVersions( checkVersion, compareVersion: String): Integer;
var
  checkV, compV: TStringList;
  i, v1, v2, l1, l2: Integer;

begin

  checkV := TStringList.Create;
  compV := TStringList.Create;

  SplitString(checkVersion + '.', '.', checkV);
  SplitString(compareVersion + '.', '.', compV);

  l1 := checkV.Count;
  l2 := compV.Count;

  debug(IntToStr(Max(l1, l2)));

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

  debug(IntToStr(Result));

end;

function GetVersion(version: String; index: Integer): Integer;
var
  vv: TStringList;
  v: String;
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

procedure set_image(img: TImageType);
var
//  node_image: string;
//  red_image: string;
  image: string;
begin

//  node_image := 'nodejs.bmp';
//  red_image := 'node-red.bmp';

//  if img = imgNONE then begin
//    WizardForm.WizardSmallBitmapImage.Visible := False;
//    Exit;
//  end;

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

function GetREDInstallationIndex(link: TObject): integer;
var
  i: integer;
  _length: integer;

begin
  Result := -1;
  for i:=0 to GetArrayLength(main.red.installs) - 1 do begin
    if main.red.installs[i].id = link then begin
      Result := i;
      break;
    end;
  end;
  debug('GetRED: ' + IntToStr(Result));
end;

function BoolToStr(bool: boolean): string;
// StrToBool is already defined in idp.iss ! 
begin
  if bool then
    Result:='true'
  else
    Result :='false';
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

// Additional page to acknowledge Node-RED License
// https://stackoverflow.com/questions/34592002/how-to-create-two-licensefile-pages-in-inno-setup
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
// var
  // IndependenceFileName: string;
  // IndependenceFilePath: string;

begin
  
  Result :=
    CreateOutputMsgMemoPage(
      after, 'Node-RED ' + SetupMessage(msgWizardLicense),
      SetupMessage(msgLicenseLabel),
      SetupMessage(msgLicenseLabel3), '');

  // Shrink memo box to make space for radio buttons
  Result.RichEditViewer.Height := WizardForm.LicenseMemo.Height;

  // Load Node-RED LIcense
  // Loading ex-post, as Lines.LoadFromFile supports UTF-8,
  // contrary to LoadStringFromFile.
  // REDLicensePage.RichEditViewer.Lines.LoadFromFile(ExpandConstant('{tmp}\{#REDLicenseTmpFileName}'));

  // Clone accept/do not accept radio buttons for the second license
  REDLicenseAcceptedRadio :=
    CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
  REDLicenseNotAcceptedRadio :=
    CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);

  // REDLicenseAcceptedRadio.Top := 5;
  // REDLicenseNotAcceptedRadio.Top := REDLicenseAcceptedRadio.Top + REDLicenseAcceptedRadio.Height + 5;

  // Customize captions
  // IndependenceAcceptedRadio.Caption := 'I acknowledge this statement.'
  // IndependenceNotAcceptedRadio.Caption := 'I do not acknowledge this statement.'

  // Initially not accepted
  REDLicenseNotAcceptedRadio.Checked := True;

end;

// *****
// ** RunCMD
// ** Execute a command with Windows cmd.exe

function RunCMD(Command, WorkingDir: string; var ResultArray: TArrayOfString): Boolean;

var
  bat, file, res: String;
  rc, i: Integer;

  p1, p2: String;
begin

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
          debug(ResultArray[i]);
        end;
        Result:=True;
      end;
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

  _possible: array of TNodeVersion;   // the Node.js versions we know of
  _options: TStringList;              // the Node.js versions we offer for download;
                                      // this may include '' for "Keep the current..."
  _current: string;

begin

  Result := False;

  debug('PrepareNodeVersionSelectionPage');

  // Page to select a Node.js version
  _nvp := main.pages.node_version;
  if _nvp = nil then Exit;

  debug('PrepareNodeVersionSelectionPage1');

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
    Caption := 'Install Windows Tools for Native Node.js Modules.';
    Checked := True;
    Enabled := True;
  end;
  main.node.install_tools := True;

  // Read the current node.js version from the registry
  _current := '';
  if RegKeyExists(main.HKLM, 'SOFTWARE\Node.js') then begin
    if RegQueryStringValue(main.HKLM, 'SOFTWARE\Node.js', 'Version', _current) = True then begin
      _nvp.SubCaptionLabel.Caption := 'Currently installed: Node.js ' + _current;
    end;
  end;

  main.node.current := _current;

  // This list holds the versions numbers we offer for installation
  _options := TStringList.Create;

  _possible := main.node.versions;

  debug('PrepareNodeVersionSelectionPage2');

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

    debug(_possible[i].latest);
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
        debugInt(main.node.default);
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
// ** REDVersionSelectionPage
// **

// var 
//    _redVersion_page: TInputOptionWizardPage;



function PrepareREDVersionSelectionPage(): Boolean;

var
  res: TArrayOfString;  // this exact type is mandatory here! 

  splitres: TStringList;

  red_versions, red_buffer: TREDVersionArray;

  i, ii, ibuffer, rvLength: Integer;
  tag, rv, line: String;
  rvv: TStringList;

  prefix: String;

  current_version: String;

  _rvp: TInputOptionWizardPage;

begin

  Result := False;

  // Page to select a Node.js version
  _rvp := main.pages.red_version;
  if _rvp = nil then Exit;

  _rvp.SubCaptionLabel.Font.Style := [fsBold];

  current_version := main.red.current;

  if current_version = '' then begin
    _rvp.SubCaptionLabel.Caption := 'Currently there''s no Node-RED version installed!';

  end else begin
    _rvp.SubCaptionLabel.Caption := 'Currently installed: Node-RED v' + current_version;
    
  end;

  rvLength := GetArrayLength(main.red.versions);
  for i := 0 to rvLength - 1 do begin

    // From Hi to Lo
    rv := main.red.versions[rvLength - 1 - i].version;
    tag := main.red.versions[rvLength - 1 - i].tag;
    
    ii := CompareVersions(current_version, rv);

    line := tag;
    if Length(line) > 0 then
      line := ' | ' + line;

    if ii > 0 then begin

      _rvp.Add('Install Node-RED v' + rv + ANSIUpperCase(line));
      continue;
    end else if ii = 0 then begin
      if Length(line) > 0 then
        line := line + ', ';
      _rvp.Add('Keep Node-RED v' + rv + ANSIUpperCase(line + 'current'));
      _rvp.Values[i] := True;
    end else if ii < 0 then begin
      _rvp.Add('Update to Node-RED v' + rv + ANSIUpperCase(line));
    end;

    if Length(current_version) < 1 then
      if tag = 'latest' then
        _rvp.Values[i] := True;

  end;

  Result := True;

end;

// ** END
// ** REDVersionSelectionPage
// *****

// *****
// ** Data Preparation Page
// ** (which isn't a true interactive page,
// ** but a Download & a Progress page inserted after wpWelcome)

function _redversion_insert_version(version, tag: String; red_versions: TREDVersionArray): TREDVersionArray;
var
  
  buffer: TREDVersionArray;
  ii, ibuffer, rvLength, cv: Integer;

begin

  // Do not accept versions that are lower than #REDMinVersion
  debug('cv: ' + version + ' / {#REDMinVersion}');

  cv := CompareVersions(version, '{#REDMinVersion}');

  debug('cv: ' + version + ' / {#REDMinVersion}' + ' = ' + IntToStr(cv));

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
    debug(version + ' ? ' + red_versions[ii].version);

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


function RunDataPrepPage(): Boolean;

#define ProgressMax 10;

var
  _ppage: TOutputMarqueeProgressWizardPage;
  _iProgress: Integer;

  i, ii, having: integer;
  nvv: array of integer;  

  nv, tmp_file_name, line: string;
  check: boolean;

  parts, version: TStringList;
  _sha, _latest: string;

  file: array of string;

  majors: array of integer;
  new_version: TNodeVersion;

  // Node-RED
  res: array of string;
  rvv, splitres: TStringList;
  tag, rv: string;
  red_versions: TREDVersionArray;
  // current_version: string;
  prefix: string;

begin

  _ppage := CreateOutputMarqueeProgressPage('Processing additional data...', 'We collect and analyze data to prepare the installation.');

  try
    // _ppage.SetProgress(_iProgress, {#ProgressMax});
    _ppage.Animate();
    _ppage.Show;

    // Get the Node versions, the Node msi download links & the SHA
    // This serves as well to verify that an internet connection is present.

    nvv := main.node.majors;

    for i := 0 to GetArrayLength(nvv) - 1 do begin

      nv := IntToStr(nvv[i]);
      debug(nv);
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

        debug(main.node.versions[having].latest);

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

    _ppage.SetText('Requesting Node-RED version info from npm...', '');
    RunCMD('npm dist-tag ls node-red', '', res);

    debug('npm dist-tag: ' + IntToStr(GetArrayLength(res)));

    for i:= 0 to GetArrayLength(res) - 1 do begin
      
      _ppage.Animate();

      line := res[i];

      // v1-maintenance: 1.3.7
      if SplitString(line, ': ', rvv) then begin
        tag:= Trim(rvv[0]);
        rv:= Trim(rvv[1]);
      end else
        continue;

      debug(rv + ' | ' + tag);
      red_versions := _redversion_insert_version(rv, tag, red_versions);

    end;

    debug('red_versions: ' + IntToStr(GetArrayLength(red_versions)));

    if GetArrayLength(red_versions) > 0 then begin
      main.red.npm := True;
    end else begin 

      // This either means Node-RED is EOL ... :(
      // ... or npm is not installed!
      // In this case we try to get at least the latest version info by querying the GitHub 'release/latest' page (already downloaded!)
      
      tmp_file_name := ExpandConstant('{tmp}\') + '{#REDLatestTmpFileName}';

      debug(tmp_file_name);

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
            debug(line);
            for ii:=0 to parts.Count - 1 do begin
              _ppage.Animate();
              line := parts[ii];
              debug(line);
              if StringChangeEx(line, '"og:url" content="/node-red/node-red/releases/tag/', '', True) > 0 then begin
                debug(line);
                splitres := TStringList.Create;
                if SplitString(line, '" />', splitres) then begin
                  line := splitres[0];
                  debug(line);
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
    debug('{#REDAddVersions}');

    // Att: There's - with intention! - an addition ',' appended to REDAddVersions
    // SplitString only returns true, if the delimieter was found at least once!
    if SplitString('{#REDAddVersions},', ',', rvv) = True then begin
      for i := 0 to rvv.Count - 1 do begin
        rv := rvv.Strings[i];
        StringChangeEx(rv, ' ', '', True);
        debug(rv);
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
    debug('Node.js Recommended Version definition: ''' + default_version + ''' not in validated list of supported versions. Forced to v' + IntToStr(main.node.default) + '!');
  end;

  Result:=True;

end;


procedure InitializeWizard();
var
  
  i, having: Integer;
  msg: String;

  wf: TWizardForm;

  _lb: TNewCheckListBox;

  tp2: TInputQueryWizardPage;

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

  // if CurPageID = wpWelcome then
  //    Exit;

  if CurPageID = wpWelcome then begin

    check := FileExists(ExpandConstant('{tmp}\{#NodeLicenseTmpFileName}'));
    check := FileExists(ExpandConstant('{tmp}\{#REDLicenseTmpFileName}')) and check;

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

      // to run the backup option if npm is not present
      _dp.Add('https://github.com/node-red/node-red/releases/latest', '{#REDLatestTmpFileName}', '');

      // Download the SHA file for the defined Node.js versions
      for i := 0 to GetArrayLength(main.node.majors) - 1 do begin

        nv := IntToStr(main.node.majors[i]);

        file := 'node' + nv + '.sha';
        _dp.Add('https://nodejs.org/dist/latest-v' + nv + '.x/SHASUMS256.txt', file, '');      

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

    if main.red.error then begin
      wf.NextButton.Enabled := False;
      debug('wpReady: error');
    end;

  end;

end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  m, mm: string;
  i, ii, l, f: integer;
  p, pp: string;
  c, cc: boolean;
  _lbl: string;
  _global: boolean;

  _paths: TStringList;
  _error: boolean;

begin
  
  _paths:= TStringList.Create;
  _error := False;

  m:=     '*****************' + NewLine;
  m:= m + '***  Node.js  ***' + NewLine;
  m:= m + NewLine;

  if Length(main.node.current) > 0 then begin
    m:= m + 'Currently installed: ' + main.node.current + NewLine;
    m:= m + 'Changing to:         ' + main.node.selected + NewLine;
  end else begin
    m:= m + 'Currently NOT installed.' + NewLine;
    m:= m + 'Installing: ' + main.node.selected + NewLine;
  end;

  // m:= m + NewLine;

  if main.node.run_silent then begin
    m:= m + '+ Node.js installer will run in the background.' + NewLine;

    if main.node.install_tools then
      m:= m + '+ Additional Tools for Windows will be installed.' + NewLine;
  
  end;

  m:= m + NewLine;
  m:= m + NewLine;
  m:= m + '******************' + NewLine;
  m:= m + '***  Node-RED  ***' + NewLine;

  l:=0;
  for i:= 0 to GetArrayLength(main.red.installs) - 1 do begin
    if main.red.installs[i].kind <> rikVoid then
      l:= l + 1;
  end;

  _global:= False;

  for i:=0 to GetArrayLength(main.red.installs) - 1 do begin

    if main.red.installs[i].kind = rikVoid then continue;
    
    m:= m + NewLine;

    if l > 1 then begin
      m:= m + '#' + IntToStr(i+1) + ': ' + NewLine;
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
      // ??
    end else begin

      if c then begin
        m:= m + 'Changing to:         ' + main.red.installs[i].action + NewLine;
      end else begin
        m:= m + 'Installing: ' + main.red.installs[i].action + NewLine;
      end;

      if Length(main.red.installs[i].path) < 1 then begin
        m:= m + '+ Global installation.' + NewLine;
        if not _global then begin
          _global := True;
        end else begin
          m:= m + '  *** ERROR: Another global installation configured!' + NewLine;
          _error := True;
        end;
      end else begin

        ii:= -1;
        p:= '';
        // lbl:= main.red.installs[i].name;
        pp:= main.red.installs[i].path;
        debug('mri: ' + main.red.installs[i].path);

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

        _paths.Add(pp);

        m:= m + '+ Installation path: ' + pp + NewLine;
        if cc then 
          m:= m + '  + Directory will be created.' + NewLine;

        main.red.installs[i].final_path:=pp;

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

  if _error then begin
    debug('_error');
    main.red.error := True;
  end;


  // UI Test only!
  m:= m + NewLine;
  m:= m + NewLine;

  m:= m + '********************************************' + NewLine;
  m:= m + '*** It is safe to press ''Install'' now.   ***' + NewLine;
  m:= m + '*** This version is for UI Test only.    ***' + NewLine;
  m:= m + '*** It does NOT install Node-RED!        ***' + NewLine;
  m:= m + '********************************************' + NewLine;

  Result := m;
end;


#include <.\iss\redactionpage.iss>
#include <.\iss\reddetector.iss>