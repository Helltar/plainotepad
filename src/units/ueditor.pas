unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Graphics, Dialogs,
  ATSynEdit;

type

  { TColorTheme }

  TColorTheme = (cream, dark, white);

  { TEditor }

  TEditor = class
  private
    synEdit: TATSynEdit;
    FFileModified: boolean;
    procedure addTextToParentCaption(AText: string);
    procedure setTextToParentCaption(AText: string);
    procedure synEditChange(Sender: TObject);
    procedure updateCreamColorTheme();
    procedure updateDarkColorTheme();
    procedure updateParentCaption();
    procedure updateWhiteColorTheme();
  public
    constructor Create(AOwner: TATSynEdit);
    function getCurrentFileName(): string;
    function isNotNewFile(): boolean;
    function openFile(fileName: string): boolean;
    function saveCurrentFile(): boolean;
    function saveFile(fileName: string): boolean;
    procedure setColorTheme(colorTheme: TColorTheme);
    property fileModified: boolean read FFileModified;
  end;

implementation

uses
  uLogger, uConsts;

resourcestring
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';
  TITLE_READONLY = '%s - ReadOnly';

{ TEditor }

constructor TEditor.Create(AOwner: TATSynEdit);
begin
  synEdit := AOwner;
  synEdit.OnChange := @synEditChange;
  synEdit.Colors.TextSelFont := clBlack;
end;

procedure TEditor.synEditChange(Sender: TObject);
begin
  FFileModified := True;

  if isNotNewFile() then
    addTextToParentCaption('*');
end;

procedure TEditor.addTextToParentCaption(AText: string);
begin
  if not AText.IsEmpty then
    AText := ' ' + AText;

  synEdit.Parent.Caption := ExtractFileName(getCurrentFileName()) + AText;
end;

procedure TEditor.setTextToParentCaption(AText: string);
begin
  synEdit.Parent.Caption := AText;
end;

procedure TEditor.updateParentCaption;
begin
  addTextToParentCaption('');
end;

function TEditor.isNotNewFile: boolean;
begin
  Result := not getCurrentFileName().IsEmpty;
end;

function TEditor.openFile(fileName: string): boolean;
var
  errMsg: string;

begin
  Result := False;

  if not FileExists(fileName) then
    Exit;

  try
    synEdit.LoadFromFile(fileName);

    FFileModified := False;

    if not FileIsReadOnly(fileName) then
      updateParentCaption()
    else
      setTextToParentCaption(Format(TITLE_READONLY, [ExtractFileName(fileName)]));

    Result := True;
  except
    setTextToParentCaption(APP_NAME);
    errMsg := Format(ERROR_OPEN_FILE, [fileName]);
    synEdit.Text := errMsg;
    addLog(errMsg, False);
  end;
end;

function TEditor.saveFile(fileName: string): boolean;
begin
  Result := False;

  try
    synEdit.SaveToFile(fileName);

    FFileModified := False;
    updateParentCaption();

    Result := True;
  except
    addLog(Format(ERROR_SAVE_FILE, [fileName]));
  end;
end;

function TEditor.saveCurrentFile: boolean;
begin
  Result := saveFile(getCurrentFileName());
end;

function TEditor.getCurrentFileName: string;
begin
  Result := synEdit.FileName;
end;

procedure TEditor.setColorTheme(colorTheme: TColorTheme);
begin
  case colorTheme of
    cream: updateCreamColorTheme();
    white: updateWhiteColorTheme();
    dark: updateDarkColorTheme();
  end;
end;

procedure TEditor.updateWhiteColorTheme;
begin
  with synEdit.Colors do
  begin
    GutterBG := clWhite;
    GutterCaretBG := $f5f5f5;
    GutterFoldBG := clWhite;
    GutterFont := clSilver;

    MarginRight := clSilver;

    TextBG := clWhite;
    TextFont := $00222222;
    TextSelBG := $EEEEEE;

    MinimapTooltipBG := TextSelBG;
    MinimapBorder := MarginRight;
    MinimapTooltipBorder := MinimapBorder;
  end;
end;

procedure TEditor.updateDarkColorTheme;
begin
  with synEdit.Colors do
  begin
    GutterBG := $001e1e1e;
    GutterCaretBG := $002e2e2e;
    GutterFoldBG := $001e1e1e;
    GutterFont := $00606060;

    MarginRight := $00303030;

    TextBG := $001e1e1e;
    TextFont := clWhite;
    TextSelBG := clSilver;

    MinimapTooltipBG := $333333;
    MinimapBorder := MarginRight;
    MinimapTooltipBorder := $555555;
  end;
end;

procedure TEditor.updateCreamColorTheme;
begin
  with synEdit.Colors do
  begin
    GutterBG := clCream;
    GutterCaretBG := clCream;
    GutterFoldBG := clCream;
    GutterFont := $00DDDDDD;

    MarginRight := $00DDDDDD;

    TextBG := clCream;
    TextFont := $00222222;
    TextSelBG := $EEEEEE;

    MinimapTooltipBG := TextSelBG;
    MinimapBorder := MarginRight;
    MinimapTooltipBorder := MinimapBorder;
  end;
end;

end.
