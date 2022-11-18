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
    currentColorTheme: TColorTheme;
    synEdit: TATSynEdit;
    function getCurrentFileName(): string;
    function GetFileModified: boolean;
    procedure synEditChange(Sender: TObject);
    procedure updateParentCaption();
    procedure updateParentCaption(addedText: string);
  public
    constructor Create(AOwner: TATSynEdit);
    function isNotNewFile(): boolean;
    function openFile(fileName: string): boolean;
    function saveCurrentFile(): boolean;
    function saveFile(fileName: string): boolean;
    procedure setColorTheme(colorTheme: TColorTheme);
    property fileModified: boolean read GetFileModified;
  end;

implementation

uses
  uLogger;

resourcestring
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';

{ TEditor }

constructor TEditor.Create(AOwner: TATSynEdit);
begin
  synEdit := AOwner;
  synEdit.OnChange := @synEditChange;
  synEdit.Colors.TextSelFont := clBlack;
end;

procedure TEditor.synEditChange(Sender: TObject);
begin
  if isNotNewFile() then
    updateParentCaption(' *');
end;

procedure TEditor.updateParentCaption(addedText: string);
begin
  synEdit.Parent.Caption := ExtractFileName(getCurrentFileName()) + addedText;
end;

procedure TEditor.updateParentCaption;
begin
  updateParentCaption('');
end;

function TEditor.isNotNewFile: boolean;
begin
  Result := not getCurrentFileName().IsEmpty;
end;

function TEditor.openFile(fileName: string): boolean;
begin
  Result := False;

  if not FileExists(fileName) then
    Exit;

  try
    synEdit.LoadFromFile(fileName);

    updateParentCaption();

    Result := True;
  except
    addLog(Format(ERROR_OPEN_FILE, [fileName]));
  end;
end;

function TEditor.saveFile(fileName: string): boolean;
begin
  Result := False;

  try
    synEdit.SaveToFile(fileName);
    synEdit.Update();

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

function TEditor.GetFileModified: boolean;
begin
  Result := synEdit.Modified;
end;

procedure TEditor.setColorTheme(colorTheme: TColorTheme);
begin
  currentColorTheme := colorTheme;

  case colorTheme of
    cream:
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
      end;

    white:
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
      end;

    dark:
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
      end;
  end;
end;

end.
