[Code]

// This is an adaptation of
// https://stackoverflow.com/questions/35129746/how-to-create-an-image-button-in-inno-setup

function ImageList_Add(ImageList: THandle; Image, Mask: THandle): Integer;
  external 'ImageList_Add@Comctl32.dll stdcall';
function ImageList_Create(CX, CY: Integer; Flags: UINT;
  Initial, Grow: Integer): THandle;
  external 'ImageList_Create@Comctl32.dll stdcall';

const
  IMAGE_BITMAP = 0;
  LR_LOADFROMFILE = $10;
  ILC_COLOR32 = $20;
  BCM_SETIMAGELIST = $1600 + $0002;
  BUTTON_IMAGELIST_ALIGN_CENTER = $04;

type
  BUTTON_IMAGELIST = record
    himl: THandle;
    margin: TRect;
    uAlign: UINT;
  end;

function SendSetImageListMessage(
  Wnd: THandle; Msg: Cardinal; WParam: Cardinal;
  var LParam: BUTTON_IMAGELIST): Cardinal;
  external 'SendMessageW@User32.dll stdcall';
