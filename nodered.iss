#include <.\IDP_1.5.1\idp.iss>

; Windows Installer for Node-RED
; Definition file for the Inno Setup compiler.
; Copyright 2023 Ralph Wetzel
; License MIT
; https://www.github.com/ralphwetzel/node-red-windows-installer

; We configure all constants via an INI file.
; That's easier for maintenence rather than searching for things in the source code
#define INIFile RemoveBackslash(SourcePath) + "\setup.ini"

; node.js Download page
; #define node ReadIni(INIFile, "node", "download")

; Node.js Default Version - that we propose to install if none is present
#define NodeDefaultVersion ReadIni(INIFile, "node", "default_version")

; min version we accept
#define NodeMinVersion ReadIni(INIFile, "node", "min_version")

; comma-separated list of major versions numbers we offer for download
#define NodeVersions ReadIni(INIFILE, "node", "versions", NodeDefaultVersion)                                              



#define MyAppName "Node-RED"
#define MyAppVersion "3.1"
#define MyAppPublisher "The Node-RED community"
#define MyAppURL "https://nodered.org"
#define MyAppExeName "MyProg.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{70D435D8-542E-4087-8E1C-D313404C7E9D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
DisableWelcomePage=no
OutputBaseFilename=Node-REDInstaller
Compression=lzma
SolidCompression=yes
SetupLogging=yes
PrivilegesRequired=lowest
WizardSmallImageFile=C:\Users\Ralph\Projekte\node-red-windows-installer\node-red-icon.bmp
WizardStyle=modern
WizardImageAlphaFormat=defined
WizardImageBackColor=clWhite
WizardImageStretch=True

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
Source: "C:\Program Files (x86)\Inno Setup 6\Examples\MyProg.exe"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[Messages]
english.WelcomeLabel1=Welcome to the%n[name] Setup Wizard
english.WelcomeLabel2=This will install Node-RED on your computer.%n%nInitially we check the version of Node.js installed is %1 or greater. We will try to install node %2 if none is found. Optionally you can choose to install node %3.%n%nIf necessary we will then remove the old core of Node-RED, before then installing the latest version. You can also optionally specify the version required.%n%nWe will finally try to run 'npm rebuild' to refresh any extra nodes you have installed that may have a native binary component. While this normally works ok, you need to check that it succeeds for your combination of installed nodes.%n%nIt is recommended that you close all other applications before continuing.

[Code]
// https://stackoverflow.com/questions/20584263/how-to-install-node-js-in-custom-folder-silently-on-windows
// msiexec /i node-v6.11.2-x64.msi TARGETDIR="C:\Program Files\nodejs\" ADDLOCAL="NodePerfCtrSupport,NodeEtwSupport,DocumentationShortcuts,EnvironmentPathNode,EnvironmentPathNpmModules,npm,NodeRuntime,EnvironmentPath" /qn

// https://stackoverflow.com/questions/18506820/innosetup-how-to-pass-a-two-dimensional-string-array-to-a-function
type
  TNodeVersion = record
    key: string;
    sha: string;
    latest: string;
    msi: string;
  end;
  TNodeVersionList = array of TNodeVersion;

var
  
  DownloadPage: TDownloadWizardPage;

  // List of <string> Node.js version numbers
  // Read from the INI file
  nodeVersions: TStringList;

  // Node.js version we propose to install (if none is installed)
  // nodeVersionDefault: string;

  // Record to hold relevant data of the Node.js versions
  // we offer to install 
  nodeVersionDetails: TNodeVersionList;

procedure debug(message: string);
begin
  Log('[NR] ' + message);
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

// First Event that fires
function InitializeSetup(): Boolean;
var
  bit, nv, sha, latest, msi, line, msg: String;
  check, error: Boolean;
  i, ii, having: Integer;

  // node_versions: TStringList;

  tmp_file_name: string;
  file: array of string;

  parts, version: TStringList;

  // nodeVersion: TNodeVersion;

  wf: TWizardForm;

begin

  // nodeLatest := TStringList.Create;
  // nodeSHA := TStringList.Create;
  // nodeMSI := TStringList.Create;

  // check the bit status of this platform
  if IsWin64() = True then begin
    debug('We are going to install the 64bit version of Python.');
    bit := 'x64';
  end else
    bit := 'x86';

  // Get the Node versions, the Node msi download links & the SHA
  // This serves as well to verify that an internet connection is present.

  try
    // get the latest of the major node versions we are interested in
    nodeVersions := TStringList.Create;
    debug('{#nodeVersions}');
    if SplitString('{#nodeVersions}', ',', nodeVersions) = True then begin
      for i := 0 to nodeVersions.Count - 1 do begin
        nv := nodeVersions.Strings[i];
        StringChangeEx(nv, ' ', '', True);
        nodeVersions.Strings[i] := nv;
        debug(nodeVersions.Strings[i]);

        // Download the SHA file
        tmp_file_name := ExpandConstant('{tmp}') + '\node' + nv + '.check';
        check := idpDownloadFile('https://nodejs.org/dist/latest-v' + nv + '.x/SHASUMS256.txt', tmp_file_name);      

        if check = False then Continue;

        // Reset the data we expect to fiind
        sha := '';
        latest := '';
        msi := '';

        LoadStringsFromFile(tmp_file_name, file);
        
        for ii := 0 to GetArrayLength(file) - 1  do begin

          // Example line in file:
          // 76102997f9084e1faa544693ad1daeef68394d46ae7e363ad8df1fa86896133f  node-v12.22.12-x64.msi
          line := file[ii];
          
          // find the line with '-x64.msi' or '-x86.msi' at the end
          if StringChangeEx(line, '-' + bit + '.msi', '', True) > 0 then begin
          
            parts := TStringList.Create; 

            // line - when found - looks now like this:  
            // 76102997f9084e1faa544693ad1daeef68394d46ae7e363ad8df1fa86896133f  node-v12.22.12
            if SplitString(line, '  ', parts) = True then begin
              
              if parts.Count = 2 then begin

                sha := parts[0];
                line := parts[1];

                // assure that the rest starts with 'node-v'
                if StringChangeEx(line, 'node-v', '', True) > 0 then begin

                  version := TStringList.Create;

                  // line is now like 12.22.12
                  // final check: we should have now a version of three elements
                  if SplitString(line, '.', version) = True then begin
                    if version.Count = 3 then
                      latest := line;
                  end;
                end;
              end;
            end;
          end;
        end;

        if ((Length(sha) > 0) and (Length(latest) > 0)) then begin

          having := GetArrayLength(nodeVersionDetails);
          SetArrayLength(nodeVersionDetails, having + 1);

          nodeVersionDetails[having].key := nv;
          nodeVersionDetails[having].latest := latest;
          nodeVersionDetails[having].sha := sha;
          nodeVersionDetails[having].msi := 'node-v' + latest + '-' + bit + '.msi';
        
        end;  
      end;

      having := GetArrayLength(nodeVersionDetails);
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

    end;
  finally
  end;
end;




procedure InitializeWizard();
var
  iResultCode: Integer;
  TmpFileName: String;

  node_versions: TStringList;
  i, having: Integer;
  nv, bit, msg: String;

  check: Boolean;

  wf: TWizardForm;

begin

  DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), @OnDownloadProgress);


  debug('Test');
  TmpFileName := ExpandConstant('{tmp}') + '\versions.txt';
  Exec(ExpandConstant('{cmd}'), '/C node --version && npm --version > "' + TmpFileName + '"', '', SW_SHOW, ewWaitUntilTerminated, iResultCode);
  debug('result: ' + IntToStr(iResultCode));

  having := GetArrayLength(nodeVersionDetails);
  if having > 0 then begin

    msg := ''
    if having > 1 then begin
      for i := 0 to having - 2  do begin
        if Length(msg) > 0 then begin
          msg := msg + ', ';
        end;
        msg := msg + nodeVersionDetails[i].key
      end;
    end;

    if Length(msg) > 0 then begin
      msg := msg + ' and '
    end;

    msg := msg + nodeVersionDetails[having - 1].key + ' LTS';
    
    wf := GetWizardForm();

    // https://github.com/jrsoftware/issrc/blob/main/Projects/MsgIDs.pas
    wf.WelcomeLabel2.Caption := FMTMessage(SetupMessage(msgWelcomeLabel2), ['{#NodeMinVersion}', '{#NodeDefaultVersion}', msg]);

  end;

end;

function NextButtonClick(CurPageID: Integer): Boolean;

var
  bit, nv: String;
  check: Boolean;
  i: Integer;

  node_versions: TStringList;

begin
  if CurPageID = wpWelcome then begin


  end else if CurPageID = wpReady then begin
    DownloadPage.Clear;
    // Use AddEx to specify a username and password
    DownloadPage.Add('https://jrsoftware.org/download.php/is.exe', 'innosetup-latest.exe', '');
    DownloadPage.Add('https://jrsoftware.org/download.php/iscrypt.dll', 'ISCrypt.dll', '2f6294f9aa09f59a574b5dcd33be54e16b39377984f3d5658cda44950fa0f8fc');
    DownloadPage.Show;
    try
      try
        DownloadPage.Download; // This downloads the files to {tmp}
        Result := True;
      except
        if DownloadPage.AbortedByUser then
          Log('Aborted by user.')
        else
          SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
        Result := False;
      end;
    finally
      DownloadPage.Hide;
    end;
  end else
    Result := True;
end;

procedure CurPageChanged(CurPageID: Integer);
var

  wf: TWizardForm;
   
begin

  wf := GetWizardForm();

  if CurPageID = wpWelcome then begin
  
    // wf.WelcomeLabel2.Caption = 
    

  end;
end;
