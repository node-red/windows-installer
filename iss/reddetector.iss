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

function detect_global_red(var page: TOutputMarqueeProgressWizardPage): integer;
var
  res: array of string;
  rvv, splitres: TStringList;
  tag, rv: string;
  prefix: string;
  i: integer;
  _path: string;

begin

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
          kind := rikGlobal;
          version := rv;
          path := _path;
          id := TObject.Create;
        end;
        Result := 1;
      end;
    end;
  end;

end;

function detect_path_red(var page: TOutputMarqueeProgressWizardPage; path: string): integer;
var
  res: array of string;
  rvv, splitres: TStringList;
  tag, rv: string;
  i, having: integer;

  installs: array of string;

  regKey: string;

  _path: string;
  check: boolean;

begin

  Result := 0;

  regKey := 'SOFTWARE\Node-RED\installations'

  // check registry for node-RED key
  // enumerate registered installations
  // check for package.json
  // call npm list 

  page.SetText('Querying registry for known Node-RED installations...','');
  if RegKeyExists(main.HKLM, regKey) then begin
    if RegGetSubkeyNames(main.HKLM, regKey, installs) then begin
      for i:=0 to GetArrayLength(installs) - 1 do begin
        check := False;
        if RegQueryStringValue(main.HKLM, regKey + '\' + installs[i], 'Path', _path) = True then begin
          page.SetText('Verifying Node-RED installation path: ' + _path, '');
          if DirExists(_path) then begin
            rv := red_list(_path);

            if Length(rv) > 0 then begin
              having:= GetArrayLength(main.red.installs);
              SetArrayLength(main.red.installs, having+1);
              with main.red.installs[having] do begin
                kind := rikPath;
                version := rv;
                path := _path;
                id := TObject.Create;
              end;
              having:= GetArrayLength(main.red.installs);
              Result := Result + 1;
              check := True;


              // We should either get some more details here 
              // from a package.json or from the registry


            end;
          end;
        end;

         // no Node-RED installation in path!
        if not check then
          RegDeleteKeyIncludingSubkeys(main.HKLM, regKey + '\' + installs[i])

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

