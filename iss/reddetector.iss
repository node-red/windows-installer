[Code]

procedure SetText(var page: TOutputMarqueeProgressWizardPage; text: string);
begin
    page.SetText(text, '');
end;

function red_list(working_dir: string): string;
var
  res: array of string;
  rv: string;
  splitres: TStringList;

begin

  Result := '';
  if not DirExists(working_dir) then Exit;

  SetArrayLength(res, 0);
  if RunCMD('npm list node-red', working_dir, res) then begin
    if GetArrayLength(res) > 1 then begin
      rv := res[1];
      // -- node-red@3.0.2 extraneous
      splitres := TStringList.Create
      if SplitString(rv, ' ', splitres) then begin
        if splitres.Count > 1 then begin
          rv := splitres[1];
          // node-red@3.0.2
          if StringChangeEx(rv, 'node-red@', '', True) = 1 then
            Result := rv;
        end;
      end;
    end;
  end;

end;


procedure query_registry(key: string; index: integer);
var
  data: string;

begin

  // Read additional data from registry
  RegQueryStringValue(main.HKLM, key, 'Name', main.red.installs[index].name);
  RegQueryStringValue(main.HKLM, key, 'Icon', main.red.installs[index].registry.icon);
  RegQueryStringValue(main.HKLM, key, 'Autostart', main.red.installs[index].registry.autostart);

  if RegQueryStringValue(main.HKLM, key, 'Port', data) = True then 
    main.red.installs[index].port := StrToIntDef(data, 0);
      
  main.red.installs[index].icon := Length(main.red.installs[index].registry.icon) > 0;
  main.red.installs[index].autostart := Length(main.red.installs[index].registry.autostart) > 0;

end;


function detect_global_red(var page: TOutputMarqueeProgressWizardPage): integer;
var
  res: array of string;
  rv: string;
  i: integer;
  _path: string;
  _key: string;
begin

  _key := AddBackslash('{#REDInstallationsRegRoot}') + '0000';
  
  Result := 0;
  // current_version := '';
  SetArrayLength(res, 0);
  page.SetText('Requesting local npm prefix definition...', '');
  if RunCMD('npm config get prefix', '', res) then begin
    if GetArrayLength(res) > 0 then begin
      _path := res[0];
      page.SetText('Looking for global Node-RED installation...','');
      rv := red_list(_path);

      if Length(rv) > 0 then begin
        i:= GetArrayLength(main.red.installs);
        SetArrayLength(main.red.installs, i+1);
        with main.red.installs[i] do begin
          key := _key;
          kind := rikGlobal;
          version := rv;
          path := _path;
          id := TObject.Create;
        end;

        query_registry(_key, i);

        Result := 1;
      end;
    end;
  end;

end;


function detect_path_red(var page: TOutputMarqueeProgressWizardPage; path: string): integer;
var
  rv: string;
  i, having: integer;

  _subkeys: array of string;

  regKey: string;

  _path: string;
  check: boolean;
  _key: string;

begin

  Result := 0;

  regKey := '{#REDInstallationsRegRoot}'

  // check registry for node-RED key
  // enumerate registered installations
  // check for package.json
  // call npm list 

  page.SetText('Querying registry for known Node-RED installations...','');
  if RegKeyExists(main.HKLM, regKey) then begin
    if RegGetSubkeyNames(main.HKLM, regKey, _subkeys) then begin
      for i:=0 to GetArrayLength(_subkeys) - 1 do begin

        // '0000' reserved for global installation
        if StrToIntDef(_subkeys[i], -1) < 1 then continue;

        check := False;
        _key := regKey + '\' + _subkeys[i];
        if RegQueryStringValue(main.HKLM, _key, 'Path', _path) = True then begin
          page.SetText('Verifying Node-RED installation path: ' + _path, '');
          if DirExists(_path) then begin
            rv := red_list(_path);

            if Length(rv) > 0 then begin
              having:= GetArrayLength(main.red.installs);
              SetArrayLength(main.red.installs, having+1);
              with main.red.installs[having] do begin
                key := _key;
                kind := rikPath;
                version := rv;
                path := _path;
                id := TObject.Create;
              end;

              // Additional Data
              query_registry(_key, having);

              having:= GetArrayLength(main.red.installs);
              Result := Result + 1;
              check := True;
            end;
          end;
        end;

         // no Node-RED installation in path!
        if not check then
          RegDeleteKeyIncludingSubkeys(main.HKLM, regKey + '\' + _subkeys[i])

      end;
    end;
  end;
end;


function detect_red_installations(var page: TOutputMarqueeProgressWizardPage): integer;
var
    i, ii: integer;

begin

  i:=0;
  i:= i + detect_global_red(page);
  i:= i + detect_path_red(page, '');

  for ii:=0 to i - 1 do begin
    debug('found: ' + main.red.installs[ii].version);
  end;

  Result := i;  

end;

