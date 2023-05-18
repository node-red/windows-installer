#include <.\image_button.iss>

[code]

var

  // _add: TNewButton;
  // _remove: TNewButton;
 //_note: TLabel;
  _clicked_line: integer;
  // _nr: TNewButton;



  // _actions: array of TREDInstallationAction;

  // list of all the actions we put into the checklistbox
  // the index of an action in this array **must** match the index of the item shown in the CheckListBox!
  // _action_items: array of TREDListItem;

// procedure WizardFormResize(Sender: TObject); forward;

// procedure OnMM(Sender: TObject; Shift: TShiftState; X, Y: Integer); forward;


// *****
// ** Work around this BUG!
// ** https://stackoverflow.com/questions/31167028/inno-setup-components-graphical-refresh-issue
// **
// ** As soon as the FontStyle of an item is changed (in whatever! way)
// ** this item will not be drawn correctly when it's selected & the CheckListBox
// ** looses the focus.
// **
// ** There seems to be no workaround than to move ItemIndex to an item with unmodified FontStyle!
// **
// ** FocusMonitorProc will be called by SetTimer, invoked in MakeRedActionPage

function SetTimer(hWnd: LongWord; nIDEvent, uElapse: LongWord; 
  lpTimerFunc: LongWord): LongWord; external 'SetTimer@user32.dll stdcall';

var
  LastFocusedControl: TWinControl;
  
procedure FocusMonitorProc(H: LongWord; Msg: LongWord; IdEvent: LongWord; Time: LongWord);

var
  fs: TFontStyles;
  box: TNewCheckListBox;
  index: integer;
begin

  // if WizardForm.CurPageID <> main.pages.red_action.ID then Exit;

  box := main.pages.red_action.CheckListBox;

  if not (WizardForm.ActiveControl = box) then begin
    if LastFocusedControl = box then begin

      index := box.ItemIndex;
      fs := box.ItemFontStyle[index];
      if not (fs = []) then
        box.ItemIndex := index + 1;

    end;
  end;

  LastFocusedControl := WizardForm.ActiveControl;

end;

// When an installation setup dataset is created (either for an existing installation or representing a new one),
// a new TObject is created as 'id' property.
// This 'id' is used to link between entries in the CheckListBox & main.red.installs

function GetREDInstallationIndex(link: TObject): integer;
var
  i: integer;

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

function _add_installation(index: integer): Boolean; forward;
function _add_new_installation(): Boolean; forward;
procedure _make_headline(); forward;

function _set_remove_button_state(index: integer): boolean;
var
  _remove: TNewButton;
  // _state: boolean;
  i, ii: integer;

begin
  _remove := TNewButton(main.pages.red_action.FindComponent('redaction_remove'));

  Result := true;
  if index < 0 then begin
    index := main.pages.red_action.CheckListBox.ItemIndex;
  end;

  if not (index < 0) then begin
    if index < GetArrayLength(main.red.items) then begin
      Result := (GetREDInstallationIndex(main.red.items[index].link) >= 0);
    end;
  end;

  // Check if there's more than 1 valid entry.
  ii:=0;
  for i:=0 to GetArrayLength(main.red.installs) -1 do begin
    if main.red.installs[i].kind <> rikVoid then begin
      index:=i;
      ii:=ii+1;
    end;
  end;

  // If so: In case it's only one entry: Enable only if this is not a rikNew installation!
  if ii = 1 then begin
    Result := (main.red.installs[index].kind <> rikNew) and Result;
  end else begin
    Result := (ii > 1) and Result;
  end;

  _remove.Enabled := Result;

end;


procedure _on_click(Sender: TObject);
var
  index: integer;
  box: TNewCheckListBox;
  // _remove: TNewButton;
  k: sREDListItemKind;
  _inst_index: integer;

begin
  box := TNewCheckListBox(Sender);
  index := box.ItemIndex;

  _clicked_line := index;

  // _remove := TNewButton(main.pages.red_action.FindComponent('redaction_remove'));
  // _remove.Enabled := (GetREDInstallationIndex(main.red.items[index].link) >= 0);
  _set_remove_button_state(index);

  k := main.red.items[index].kind;
  _inst_index := GetREDInstallationIndex(main.red.items[index].link);

  if k = rlikAction then begin
    main.red.installs[_inst_index].action := main.red.items[index].action;
  end;

  if k = rlikAutostart then begin
    main.red.installs[_inst_index].autostart := (box.State[index] = cbChecked);
  end;

  if k = rlikIcon then begin
    main.red.installs[_inst_index].icon := (box.State[index] = cbChecked);
  end;

  if k = rlikGlobal then begin
    if box.State[index] = cbChecked then begin
      main.red.installs[_inst_index].path := '';
    end;
  end;

  if k = rlikPath then begin
    if box.State[index] = cbChecked then begin
      main.red.installs[_inst_index].path := main.red.items[index].action;
    end;
  end;

end;

function _run_label_input_box(_label: string): string; forward;
function _run_port_input_box(port: integer): integer; forward;

procedure _on_dblclick(Sender: TObject);
var
  index: integer;
  box: TNewCheckListBox;

  _label: string;
  _new_label: string;

  _port: integer;
  _path: string;

  k: sREDListItemKind;
  _inst_index: integer;

begin
  box := TNewCheckListBox(Sender);
  index := box.ItemIndex;

  k := main.red.items[index].kind;
  _inst_index := GetREDInstallationIndex(main.red.items[index].link);

  if _inst_index < 0 then Exit;

  if k = rlikLabel then begin

    _label := main.red.installs[_inst_index].name;
    if Length(_label) < 1 then
      _label := 'Node-RED';

    _new_label := _run_label_input_box(_label);

    if _label <> _new_label then begin
      _label := _new_label;

      main.red.installs[_inst_index].name := _label;

      if Length(_label) < 1 then
        _label := 'Node-RED';

      box.ItemCaption[index] := '#' + main.red.items[index].action + ': ' + _label;
    end;

  end;

  if k = rlikPort then begin

    // debug(IntToStr(li.target.installation.port));
    
    _port := _run_port_input_box(main.red.installs[_inst_index].port);
    if _port <> main.red.installs[_inst_index].port then begin
      
      // need to use _action_item[] here, as li seems to be (only) a copy.
      main.red.installs[_inst_index].port := _port;

      if _port = 0 then
        _label := '(Default)'
      else
        _label := IntToStr(_port);
      
      box.ItemCaption[index] := 'Port for Autostart & Desktop Icon: ' + _label;
    
    end;

  end;

  if k = rlikPath then begin

    _path := main.red.items[index].action;

    if BrowseForFolder(
      'Select the installation directory for this Node-RED installation.',
      _path, True) then begin

{
      if DirExists(_path) then begin
        if (not isEmptyDir(_path)) then begin
          MsgBox('Selected directory: ' + #13#10 + #13#10 + '        ' + _path + #13#10 + #13#10
            'This directory is not empty.' + #13#10 +
            'Please choose another - empty - directory!',
            mbError, MB_OK);
          _path := '';
        end;
      end;
}

      if Length(_path) > 0 then begin
        main.red.items[index].action := _path;
        box.ItemCaption[index] := 'Custom path: ' + _path;
        main.red.installs[_inst_index].path := _path;
      end;

    end;
  end;

end;

procedure _on_add_red(Sender: TObject);
var
  _label: string;

  i: integer;

  _path: string;
  _ppage: TOutputMarqueeProgressWizardPage;
  rv: string;
  _error: integer;

begin
  // box := TNewCheckListBox(Sender);
  // index := box.ItemIndex;

  // li := _action_items[index];
  // debug(li.target.installation.name);

  if not BrowseForFolder(
      'Select the installation directory of an existing Node-RED installation.',
      _path, True) then Exit;

  rv := '';
  if DirExists(_path) then begin
    _ppage := CreateOutputMarqueeProgressPage('Verifying installation path...', 'We check if there''s a Node-RED installation at the given path.');
    _ppage.SetText('Verifying: ' + _path, '');

    try
      // _ppage.SetProgress(_iProgress, {#ProgressMax});
      _ppage.Animate();
      _ppage.Show;
      rv := red_list(_path);
    finally
      _ppage.Hide();
    end;
  end;

  if Length(rv) < 1 then begin
    MsgBox(
      'No Node-RED installation found @ ' + _path,
      mbInformation, MB_OK);
      Exit;
  end;

  _error := 0;
  
  for i:= 0 to GetArrayLength(main.red.installs) - 1 do begin
    if main.red.installs[i].path = _path then begin
      _error := 1;
      break;
    end;
  end;

  // debugInt(i);

  if _error > 0 then begin

    _label := main.red.installs[i].name;
    if Length(_label) < 1 then
      _label := 'Node-RED';

    MsgBox(
      'We found a Node-RED installation @ ' + _path + #13#10 + #13#10 +
      'This yet is already managed by #' + IntToStr(i) + ': ' + _label + '.' + #13#10 + #13#10 +
      'Thus we did not add another installation setup.',
      mbError, MB_OK);
    
    Exit;

  end;
  
  i:= GetArrayLength(main.red.installs);
  SetArrayLength(main.red.installs, i+1);
  with main.red.installs[i] do begin
    kind := rikPath;
    version := rv;
    path := _path;
    id := TObject.Create;
  end;

  _add_installation(i);
  _make_headline();

end;

procedure _make_headline();
var
  _length, i: integer;
  _found: integer;
  _new: integer;

  _label: string;

  k: sREDInstallationKind;

begin

  _length := GetArrayLength(main.red.installs);

  for i:= 0 to GetArrayLength(main.red.installs) - 1 do begin
    k:= main.red.installs[i].kind
    if k = rikNew then begin
      _new := _new + 1
    end else if k <> rikVoid then begin
      _found := _found + 1;
    end;
  end;

  if _found > 0 then begin
    _label := IntToStr(_found) + ' current ';
    if _found > 1 then
      _label := _label + 'installations'
    else
      _label := _label + 'installation';

    _label := _label + ' found'
  end;

  if _new > 0 then begin

    if Length(_label) > 0 then
      _label := _label + ' / '; 

    _label := _label + IntToStr(_new) + ' new ';
    if _new > 1 then
      _label := _label + 'installations'
    else
      _label := _label + 'installation';

    _label := _label + ' projected:'
  end;

  with main.pages.red_action.SubCaptionLabel do begin
    Caption := _label;
    Font.Style := [fsBold];
  end;

end;

function _run_label_input_box(_label: string): string;
var
  _form: TSetupForm;
  _edit: TNewEdit;
  _ok, _cancel: TNewButton;
  _width: integer;

  box: TNewCheckListBox;
  index: integer;
  fs: TFontStyles;

begin

  _form := CreateCustomForm();
  try
    with _form do begin
      ClientWidth := ScaleX(256);
      ClientHeight := ScaleY(128);
      Caption := 'Label this installation';
    end;

    _edit := TNewEdit.Create(_form);
    with _edit do begin
      Parent := _form;
      Top := ScaleY(10);
      Left := ScaleX(10);
      Width := _form.ClientWidth - ScaleX(2 * 10);
      Height := ScaleY(23);
      Anchors := [akLeft, akTop, akRight];
      Text := _label;
    end;

    _ok := TNewButton.Create(_form);
    with _ok do begin
      Parent := _form;
      Caption := 'OK';
      Left := _form.ClientWidth - ScaleX(75 + 6 + 75 + 10);
      Top := _form.ClientHeight - ScaleY(23 + 10);
      Height := ScaleY(23);
      Anchors := [akRight, akBottom]
      ModalResult := mrOk;
      Default := True;
    end;

    _cancel := TNewButton.Create(_form);
    with _cancel do begin
      Parent := _form;
      Caption := 'Cancel';
      Left := _form.ClientWidth - ScaleX(75 + 10);
      Top := _form.ClientHeight - ScaleY(23 + 10);
      Height := ScaleY(23);
      Anchors := [akRight, akBottom]
      ModalResult := mrCancel;
      Cancel := True;
    end;

    _width := _form.CalculateButtonWidth([_ok.Caption, _cancel.Caption]);
    _ok.Width := _width;
    _cancel.Width := _width;

    _form.ActiveControl := _edit;
    { Keep the form from sizing vertically since we don't have any controls which can size vertically }
    _form.KeepSizeY := True;
    { Center on WizardForm. Without this call it will still automatically center, but on the screen }
    _form.FlipSizeAndCenterIfNeeded(True, WizardForm, False);

    // BoldItem-not-drawn-when-focus-lost-bug ... workaround
    box := main.pages.red_action.CheckListBox;
    index := box.ItemIndex;

    fs := box.ItemFontStyle[index];
    if not (fs = []) then
      box.ItemIndex := index + 1;

    if _form.ShowModal() = mrOk then
      Result := _edit.Text
    else
      Result := _label;

    box.ItemIndex := index;

  finally
    _form.Free();
  end;
end;

procedure _port_input_box_radio_default_checked(Sender: TObject);
var
  _form: TSetupForm;
  _radio: TNewRadioButton;
  _ok: TNewButton;

begin
  _radio := TNewRadioButton(Sender);
  _form := TSetupForm(_radio.Parent);
  _ok := TNewButton(_form.FindComponent('_port_input_ok'));
  _ok.Enabled := True;
end;


procedure _port_input_box_port_on_change(Sender: TObject);
var
  _form: TSetupForm;
  _edit: TNewEdit;
  _text: string;
  _port: integer;
  _ok: TNewButton;
  _radio: TNewRadioButton;

begin
  _edit := TNewEdit(Sender);
  _text := _edit.Text;
  _form := TSetupForm(_edit.Parent);
  _ok := TNewButton(_form.FindComponent('_port_input_ok'));
  _radio := TNewRadioButton(_form.FindComponent('_port_input_radio_port'));

  _radio.Checked := True;

  _port := StrToIntDef(_edit.Text, -1);
  if ((_port < 0) or (_port > 65535)) then begin
    _edit.Font.Color := clRed;
    _ok.Enabled := False;
  end else begin
    _edit.Font.Color := clBlack;
    _ok.Enabled := True;
  end;
end;

function _run_port_input_box(port: integer): integer;
var
  _form: TSetupForm;
  _edit: TNewEdit;
  _radio_default, _radio_port: TNewRadioButton;
  _ok, _cancel: TNewButton;
  _width: integer;
  _info: TNewStaticText;
  
begin

  _form := CreateCustomForm();
  try
    with _form do begin
      ClientWidth := ScaleX(256);
      ClientHeight := ScaleY(128);
      Caption := 'Port for Autostart & Desktop Icon';
    end;

    _radio_default := TNewRadioButton.Create(_form)
    with _radio_default do begin
      Parent := _form;
      Top := ScaleY(8);
      Left := ScaleX(8);
      Width := _form.ClientWidth - ScaleX(2 * 8);
      Height := ScaleY(Height);
      Anchors := [akLeft, akTop, akRight];
      Caption := '(Default) - as defined in ''settings.js''.'
      OnClick := @_port_input_box_radio_default_checked;
    end;

    _radio_port := TNewRadioButton.Create(_form)
    with _radio_port do begin
      Parent := _form;
      Top := _radio_default.Top + _radio_default.Height + ScaleY(8);
      Left := ScaleX(8);
      Width := ScaleX(80);
      Height := ScaleY(Height);
      Anchors := [akLeft, akTop, akRight];
      Caption := 'Custom Port:'
      Name := '_port_input_radio_port'
    end;

    _edit := TNewEdit.Create(_form);
    with _edit do begin
      Parent := _form;
      Top := _radio_port.Top;
      Left := _radio_port.Left + _radio_port.Width + ScaleX(8);
      Width := _form.ClientWidth - Left - ScaleX(8);
      Height := ScaleY(Height);
      Anchors := [akLeft, akTop, akRight];
      // Text := IntToStr(port);
      OnChange := @_port_input_box_port_on_change;
    end;

    _info := TNewStaticText.Create(_form);
    with _info do begin
      Parent := _form;
      Top := _radio_port.Top + _radio_port.Height + ScaleY(4);
      Left := _edit.Left; // ScaleX(8 + 16);
      Width := _edit.Width; // _form.ClientWidth - Left - ScaleX(8);
      Height := ScaleY(Height * 2);
      Anchors := [akLeft, akTop, akRight];
      WordWrap := True;
      Font.Style := [fsBold];
      Caption := 'Please ensure that this port is free to be used by Node-RED.';
    end;

    _ok := TNewButton.Create(_form);
    with _ok do begin
      Parent := _form;
      Caption := 'OK';
      Left := _form.ClientWidth - ScaleX(75 + 6 + 75 + 10);
      Top := _form.ClientHeight - ScaleY(23 + 10);
      Height := ScaleY(23);
      Anchors := [akRight, akBottom]
      ModalResult := mrOk;
      Default := True;
      Name := '_port_input_ok'
    end;

    _cancel := TNewButton.Create(_form);
    with _cancel do begin
      Parent := _form;
      Caption := 'Cancel';
      Left := _form.ClientWidth - ScaleX(75 + 10);
      Top := _form.ClientHeight - ScaleY(23 + 10);
      Height := ScaleY(23);
      Anchors := [akRight, akBottom]
      ModalResult := mrCancel;
      Cancel := True;
    end;

    _width := _form.CalculateButtonWidth([_ok.Caption, _cancel.Caption]);
    _ok.Width := _width;
    _cancel.Width := _width;

    _form.ActiveControl := _edit;
    { Keep the form from sizing vertically since we don't have any controls which can size vertically }
    _form.KeepSizeY := True;
    { Center on WizardForm. Without this call it will still automatically center, but on the screen }
    _form.FlipSizeAndCenterIfNeeded(True, WizardForm, False);

    if port > 0 then begin
      _edit.Text := IntToStr(port);
      _radio_port.Checked := True;
      _form.ActiveControl := _radio_port;
    end else begin
      _edit.Text := '1880';
      _radio_default.Checked := True;
      _form.ActiveControl := _radio_default;
    end; 

    if _form.ShowModal() = mrOk then begin
      if _radio_default.Checked then
        Result := 0
      else
        Result := StrToInt(_edit.Text);
    end else
      Result := port;

  finally
    _form.Free();
  end;
end;

procedure _on_add_new(Sender: TObject);
begin
  _add_new_installation();
end;

{
procedure move_action_record(var from_record: TREDInstallationAction; var to_record: TREDInstallationAction);
begin
  to_record := from_record;
end;


procedure move_action_item_record(var from_record: TREDListItem; var to_record: TREDListItem);
begin
  to_record := from_record;
end;
}

const
  LB_DELETESTRING = $182;

procedure _on_click_remove(Sender: TObject);
var
  index: integer;
  box: TNewCheckListBox;
  line: integer;
  action: string;
  
  fs: TFontStyles;

  i, ii: integer;
  _label: string;

  _id: TObject;
  _inst_index: integer;

begin
  box := main.pages.red_action.CheckListBox;
  index := box.ItemIndex;

{
  debugInt(index);

  _ai := _action_items[index].target.index;
  line := _action_items[index].target.line;

  debugInt(_ai);
  debugInt(line);
}

  _id := main.red.items[index].link;
  _inst_index := GetREDInstallationIndex(_id);

  debug('_inst_index: ' + IntToStr(_inst_index));

  line := -1;
  for i:=0 to GetArrayLength(main.red.items) - 1 do begin
    if box.ItemLevel[i] <> 0 then continue;

    if GetREDInstallationIndex(main.red.items[i].link) = _inst_index then begin
      line := i;
      break;
    end;
  end;

  if line < 0 then Exit;

  if not (main.red.installs[_inst_index].kind = rikNew) then begin

    repeat
      action := '';
      if main.red.items[line].kind = rlikAction then begin
        action := main.red.items[line].action;
      end;
      line := line + 1
    until action = 'remove';

    line := line - 1;
    box.CheckItem(line, coCheck);
    main.red.installs[_inst_index].action := 'remove';

    // work around this BUG!
    // https://stackoverflow.com/questions/31167028/inno-setup-components-graphical-refresh-issue
    fs := box.ItemFontStyle[index];
    if not (fs = []) then begin
      box.ItemIndex := index + 1;
    end;

    Exit;

  end;
  
  if GetArrayLength(main.red.installs) = 1 then begin
    MsgBox('Please cancel the installation if you do not want to continue!', mbError, IDOK);
    Exit;
  end;

  // LB_DELETESTRING := $182;

  // Remove all entries of this installation from the CheckListBox
  index := line;
  while GetArrayLength(main.red.items) > index do begin
    if main.red.items[index].link = _id then begin
      SendMessage(box.Handle, LB_DELETESTRING, line, 0);
      index := index + 1;
    end else begin
      break;
    end;
  end;

  debug('index: ' + IntToStr(index));

  // index now points to the first item of the next installation!

  if line > 0 then begin
    // there's still an empty line...
    line := line - 1;
    SendMessage(box.Handle, LB_DELETESTRING, line, 0);
  end;

  // debug('_action_items: ' + IntToStr(GetArrayLength(_action_items)));

  // restructure _action_items
  ii:=0;
  for i:=0 to GetArrayLength(main.red.items) - 1 do begin
    debug('i/ii: ' + IntToStr(i) + '/' + IntToStr(ii));
    if i < line then begin
      ii:=ii+1;
      continue;
    end else if i >= index then begin
      // move_action_item_record(_action_items[i], _action_items[ii]);
      main.red.items[ii] := main.red.items[i];
      ii:=ii+1;
    end;
  end;

  SetArrayLength(main.red.items, ii);

  debug('main.red.items: ' + IntToStr(GetArrayLength(main.red.items)));

  // finally: care for main.red.installations
  {
  ii:=0;
  for i:=0 to GetArrayLength(main.red.actions) - 1 do begin
    debug('i/ii: ' + IntToStr(i) + '/' + IntToStr(ii));
    if i < _ai then begin
      ii:=ii+1;
      continue;
    end else if i = _ai then begin
      continue;
    end else begin
      // move things up one step...
      move_action_record(main.red.actions[i], main.red.actions[ii]);
      ii:=ii+1;
    end;
  end;
  
  SetArrayLength(main.red.actions, ii);
  }
  main.red.installs[_inst_index].kind := rikVoid;
  main.red.installs[_inst_index].id := nil;

  _set_remove_button_state(-1);
  _make_headline();
  Exit;

  // The next sequence used to change the labels of the listed installations
  // to get a contious count without gaps.
  // Kept here in case we think this creates a better user experience - sometimes in the future...

  ii:=0;
  for i:=0 to GetArrayLength(main.red.items) - 1 do begin
    if box.ItemLevel[i] = 0 then begin

      if main.red.items[i].kind = rlikNone then continue;

      index := GetREDInstallationIndex(main.red.items[i].link);

      if index > -1 then begin

        _label := main.red.installs[index].name;

        if Length(_label) < 1 then
          _label := 'Node-RED';

        box.ItemCaption[i] := '#' + IntToStr(ii + 1) + ': ' + _label;
        main.red.items[i].action := IntToStr(ii + 1);

        ii:=ii+1;
      end;

    end;
  end;

  _make_headline();

end;

function _create_red_action_layout(page: TInputOptionWizardPage): Boolean;
var
  box: TNewCheckListBox;

  _add: TNewButton;
  _remove: TNewButton;

  _red: TNewButton;
  _icon: string;

  _bitmap: TBitmap;
  _imageList: THandle;
  _buttonImageList: BUTTON_IMAGELIST;

  _note: TLabel;

begin

    // _page := main.pages.red_action;
  if page = nil then begin
    Result := False;
    Exit;
  end;

  Result := True;

  box := page.CheckListBox;

  with box do begin
    ShowLines := True;
    WantTabs := False;
    Flat := True;
  end;

  _add := TNewButton.Create(page);
  with _add do begin
    Name := 'redaction_add';
    Parent := page.Surface;
    Caption := '+';
    Left := page.SurfaceWidth - ScaleX(Height);
    Top := box.Top;
    Width := ScaleX(Height);
    Height := ScaleY(Height);
    Anchors := [akTop, akRight];
    Hint := 'Add additional installation';
    ShowHint := True;
    Font.Size := Font.Size + 2;
    OnClick := @_on_add_new;
  end;

  _remove := TNewButton.Create(page);
  with _remove do begin
    Name := 'redaction_remove';
    Parent := page.Surface;
    Caption := '-';
    Left := _add.Left;
    Top := _add.Top + _add.Height + ScaleY(8);
    Width := ScaleX(Height);
    Height := ScaleY(Height);
    Anchors := [akTop, akRight];
    Hint := 'Remove selected installation';
    ShowHint := True;
    Font.Size := Font.Size + 4;
    Enabled := False;
    OnClick := @_on_click_remove;
  end;
  
  _note := TLabel.Create(page)
  with _note do begin
    Name := 'redaction_note';
    Parent := page.Surface;
    Left := box.Left;
    Top := page.SurfaceHeight - ScaleY(Height);
    Width := _add.Left - ScaleX(8);
    Height := ScaleY(Height);
    Anchors := [akLeft, akRight, akBottom];
    Caption := '* DoubleClick to edit.';
    Font.Color := clGray;
  end;

  _red := TNewButton.Create(page);
  with _red do begin
    Name := 'redaction_red';
    Parent := page.Surface;
    Caption := '';
    Left := _add.Left;
    Top := _remove.Top + _remove.Height + ScaleY(24);
    Width := ScaleX(Height);
    Height := ScaleY(Height);
    Anchors := [akTop, akRight];
    Hint := 'Add existing Node-RED installation';
    ShowHint := True;
    Enabled := main.red.npm;
    OnClick := @_on_add_red;
  end;

  // Manage the icon for the _red button
  _icon := 'node-red-small.bmp';
  if not FileExists(ExpandConstant('{tmp}\' + _icon)) then ExtractTemporaryFile(_icon);

  _imageList := ImageList_Create(32, 32, ILC_COLOR32, 1, 1);

  _bitmap := TBitmap.Create();
  _bitmap.LoadFromFile(ExpandConstant('{tmp}\' + _icon));

  ImageList_Add(_imageList, _bitmap.Handle, 0);

  _buttonImageList.himl := _imageList;
  _buttonImageList.uAlign := BUTTON_IMAGELIST_ALIGN_CENTER;

  SendSetImageListMessage(_red.Handle, BCM_SETIMAGELIST, 0, _buttonImageList);

  box.Width := _add.Left - ScaleX(8);
  box.Height := _note.Top - box.Top - ScaleY(8);
  box.Anchors := [akLeft, akTop, akRight, akBottom];

  box.OnClick := @_on_click;
  box.OnDblClick := @_on_dblclick;

  Result := True;
end;

procedure _add_action_item(kind: sREDListItemKind; action: string; id: TObject; check: integer);
var
  _length: integer;

begin

  _length := GetArrayLength(main.red.items);
  SetArrayLength(main.red.items, _length + 1);

  main.red.items[_length].kind := kind;
  main.red.items[_length].action := action;
  main.red.items[_length].link := id;
  // check => not used!

end;

function _add_actions(box: TNewCheckListBox; var installation: rREDInstallation; level: byte; show_header: boolean): integer;

var
  rvl: integer;
  i, ii: integer;
  cv, rv, tag: string;
  index: integer;

  _id: TObject;
begin

  _id := installation.id;

  if show_header then begin
    index := box.AddGroup('Action', '', level, nil);
    _add_action_item(rlikNone, '', _id, index);
    box.ItemFontStyle[index] := [fsBold];
    level := level + 1;
  end;

  cv := installation.version;
  rvl := GetArrayLength(main.red.versions);
  tag := '';

  // only if there's an installation (providing a version number)
  if Length(cv) > 0 then begin

    for i := 0 to rvl - 1 do begin
      rv := main.red.versions[i].version;
      if CompareVersions(cv, rv) = 0 then begin
        tag := main.red.versions[i].tag;
        break;
      end;
    end;

    index := box.AddRadioButton('Keep Node-RED v' + cv, ANSIUpperCase(tag), level, False, True, nil);
    _add_action_item(rlikAction, '', _id, index);

    box.Checked[index] := True;

    // 'Change' group label only if there's an installation
    index := box.AddRadioButton('Change', '', level, False, True, nil);
    _add_action_item(rlikNone, '', _id, index);

  end else begin
    // Compensate for the +1 in later lines!
    level := level - 1;
  end;

  for i := 0 to rvl - 1 do begin

    // From Hi to Lo
    rv := main.red.versions[rvl - 1 - i].version;
    tag := main.red.versions[rvl - 1 - i].tag;

    ii := CompareVersions(cv, rv);

    if ((ii > 0) or (Length(cv) < 1)) then begin
      index := box.AddRadioButton('Install Node-RED v' + rv, ANSIUpperCase(tag), level+1, False, True, nil);
      _add_action_item(rlikAction, rv, _id, index);

      if Length(cv) < 1 then begin
        if ANSILowerCase(tag) = 'latest' then begin
          box.Checked[index] := True;
          main.red.installs[GetREDInstallationIndex(_id)].action := rv;
        end;
      end;

      continue;
    end else if ii = 0 then begin
      continue;
    end else if ii < 0 then begin
        index := box.AddRadioButton('Update to Node-RED v' + rv, ANSIUpperCase(tag), level+1, False, True, nil);
        _add_action_item(rlikAction, rv, _id, index);
    end;

  end;

  if Length(cv) > 0 then begin
    index := box.AddRadioButton('Remove Node-RED v' + cv, '', level, False, True, nil);
    _add_action_item(rlikAction, 'remove', _id, index);
  end;

  Result := index;

end;

{
procedure _copy_tredinstallation_record(const source: TREDInstallation; var target: TREDInstallation);
begin

  // deep copy!
  target.kind := source.kind;
  target.path := source.path;
  target.version := source.version;
  target.port := source.port;
  target.name := source.name;

end;
}

function _add_details(box: TNewCheckListBox; var installation: rREDInstallation; level: byte; show_header: boolean): integer;
var
  index: integer;

  _id: TObject;

begin

  _id := installation.id;

  if show_header then begin
    index := box.AddGroup('Details', '', level, nil);
    _add_action_item(rlikNone, '', _id, index);

    box.ItemFontStyle[index] := [fsBold];
    level := level + 1;
  end;

  index := box.AddGroup('Version: ' + installation.version, '', level, nil);
  _add_action_item(rlikNone, '', _id, index);

  if installation.kind = rikGlobal then begin
    index := box.AddGroup('Global installation @ ' + installation.path, '', level, nil);
  end else begin
    index := box.AddGroup('Path: ' + installation.path, '', level, nil);
  end;
  _add_action_item(rlikNone, '', _id, index);

  Result := index;
end;

function _add_config(box: TNewCheckListBox; var installation: rREDInstallation; level: byte; show_header: boolean): integer;
var
  index: integer;
  // _inst: TREDInstallation;
  _label: string;
  _port: integer;
  _path: string;

  _id: TObject;
  id: integer;

begin

  _id := installation.id;

  if show_header then begin
    index := box.AddGroup('Configuration', '', level, nil);
    _add_action_item(rlikNone, '', _id, index);

    box.ItemFontStyle[index] := [fsBold];
    level := level + 1;
  end;

  _label := installation.name;
  if Length(_label) < 1 then
    _label := 'Node-RED';

  index := box.AddCheckBox('Create Desktop Icon', '', level, installation.icon, True, False, False, nil);
  _add_action_item(rlikIcon, '', _id, index);

  index := box.AddCheckBox('Add to Autostart group', '', level, installation.autostart, True, False, False, nil);
  _add_action_item(rlikAutostart, '', _id, index);

  if installation.kind = rikNew then begin

    _path := ExpandConstant('{commonpf32}');

    // Design decision:
    // If the selected directory is empty, we install into this directory
    // If it is not, we'll create a subdirectory 'Node-RED', if necessary postfixed by '(x)' & install there

    index := box.AddGroup('Installation path', '', level, nil);
    _add_action_item(rlikNone, '', _id, index);

    index := box.AddRadioButton('Install with ''npm -global'' flag.', '', level+1, False, True, nil);
    _add_action_item(rlikGlobal, '', _id, index);

    id:= GetREDInstallationIndex(_id);
    if id = 0 then begin
      box.Checked[index] := True;
      main.red.items[index].action := BoolToStr(True);
      installation.path := '';
      // Optionally: Run through all _actions and check if there's already a global installation! 
    end;

    index := box.AddRadioButton('Custom path: ' + _path, '*', level+1, False, True, nil);
    _add_action_item(rlikPath, _path, _id, index);
    if id > 0 then begin
      box.Checked[index] := True;
      main.red.items[index].action := _path;
      installation.path := _path;
    end;

  end;

  _port := installation.port;
  if _port > 0 then
    _label := IntToStr(_port)
  else
    _label := '(Default)';
    _port := 0;
  
  index := box.AddGroup('Port for Autostart & Desktop Icon: ' + _label , '*', level, nil);
  _add_action_item(rlikPort, '', _id, index);

  Result := index;
end;

function _add_installation(index: integer): Boolean;
var

  box: TNewCheckListBox;
  line: integer;
  _empty: rREDInstallation;
  _label: string;

  ii: integer;
  k: sREDInstallationKind;

begin

  if index < 0 then Exit;
  if index >= GetArrayLength(main.red.installs) then Exit;

  k := main.red.installs[index].kind;
  if k = rikVoid then Exit;

  box := main.pages.red_action.CheckListBox;
  line := GetArrayLength(main.red.items);

  Result := True;
  main.red.installs[index].id := TObject.Create;

  if line > 0 then begin

    // an empty REDInstallation
    // ... to provide a mean to return rikVoid on an empty line
    _empty.id := TObject.Create;
    _empty.kind := rikVoid;

    line := box.AddGroup('', '', 0, nil);
    _add_action_item(rlikNone, '', _empty.id, line);

  end;

  ii:= index + 1;

// With those next lines, the number in the label of an installation
// used to be calculated only based on the non-rikVoid entries.
// This created a continous count.
// Commented as we think it's better when one installation keeps it's label number for the liefetime of an setup run.
// Left here in if we change our mind at some point in time...

{
  ii:=0;
  for i:=0 to index do begin
    if main.red.installs[i].kind <> rikVoid then begin
      ii:=ii+1;
    end;
  end;
}

  _label := main.red.installs[index].name;
  if Length(_label) < 1 then 
    _label := 'Node-RED';

  line := box.AddGroup('#' + IntToStr(ii) + ': ' + _label, '*', 0, nil);

  _add_action_item(rlikLabel, IntToStr(ii), main.red.installs[index].id, line);

  box.ItemFontStyle[line] := [fsBold];

  if k = rikNew then begin
    line := box.AddGroup('New installation', '', 1, nil);
    _add_action_item(rlikNone, '', main.red.installs[index].id, line);
  end else begin
    line := _add_details(box, main.red.installs[index], 1, False);
  end;
  line := _add_actions(box, main.red.installs[index], 1, True);
  line := _add_config(box, main.red.installs[index], 1, True);

  _set_remove_button_state(-1);

end;


function _fill_current_installations(page: TInputOptionWizardPage): Boolean;
var
  i: integer;
  _length: integer;

begin

  Result := True;
  _length := GetArrayLength(main.red.installs);

  for i:= 0 to _length - 1 do begin
    Result := _add_installation(i) and Result;
  end;

  _make_headline();

end;

function _add_new_installation(): Boolean;
var
  _length: integer;

begin

  _length := GetArrayLength(main.red.installs);
  SetArrayLength(main.red.installs, _length + 1);

  main.red.installs[_length].kind := rikNew;
  Result := _add_installation(_length);

  _make_headline();

end;

function MakeRedActionPage(page: TInputOptionWizardPage): Boolean;
begin

  Result := False;

  // this is for bookkeeping of the actions connected to the lines in the CheckListBox
  SetArrayLength(main.red.items, 0);

  if not _create_red_action_layout(page) then Exit;

  if GetArrayLength(main.red.installs) > 0 then begin
    _fill_current_installations(page);
  end else begin
    _add_new_installation();
  end;

  // Set up 50ms timer to monitor the focus
  SetTimer(0, 0, 50, CreateCallback(@FocusMonitorProc));

  Result := True;
end;
