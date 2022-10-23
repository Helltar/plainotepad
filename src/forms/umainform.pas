unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls, Menus, StdActns, SynEdit;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFullscreen: TAction;
    actClose: TAction;
    actSaveFile: TAction;
    actionList: TActionList;
    edtCopy: TEditCopy;
    edtCut: TEditCut;
    edtDelete: TEditDelete;
    edtPaste: TEditPaste;
    edtSelectAll: TEditSelectAll;
    editUndo: TEditUndo;
    miUndo: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    miDelete: TMenuItem;
    pmMain: TPopupMenu;
    saveDialog: TSaveDialog;
    miSeparator: TMenuItem;
    miSeparator1: TMenuItem;
    miSeparator2: TMenuItem;
    synEdit: TSynEdit;
    procedure actFullscreenExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private
    appConfigFile: string;
    function saveFile(): boolean;
    function showFileChangeDialog(): TModalResult;
    procedure createDefaultConfigFile();
    procedure loadFormConfig();
    procedure loadSynEditConfig();
    procedure saveConfig();
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uConfig, uEditor;

resourcestring
  CAPTION_FILE_CHANGED = 'File changed';
  MSG_SAVE_CHANGES = 'Save the changes?';

var
  editor: TEditor;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  appConfigDir: string;

begin
  editor := TEditor.Create(synEdit);

  appConfigDir := GetAppConfigDir(False);
  appConfigFile := appConfigDir + APP_CONFIG_FILE_NAME;

  if not DirectoryExists(appConfigDir) then
    if CreateDir(appConfigDir) then
      createDefaultConfigFile();

  loadFormConfig();
  loadSynEditConfig();

  if ParamCount > 0 then
    editor.openFile(ParamStr(1));
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(editor);
  saveConfig();
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if editor.fileModified then
    case showFileChangeDialog() of
      mrYes:
        if not saveFile() then
          Exit;
      mrCancel: Exit;
    end;

  editor.openFile(FileNames[0]);
end;

procedure TfrmMain.loadFormConfig;
var
  config: TConfig;

begin
  try
    config := TConfig.Create(appConfigFile);

    with config do
    begin
      if (formLeft + formTop) > 0 then
      begin
        Left := formLeft;
        Top := formTop;
      end
      else
        Position := poScreenCenter;

      Height := formHeight;
      Width := formWidth;

      case colorTheme of
        COLOR_THEME_DARK:
        begin
          Color := $001e1e1e;
          editor.setColorTheme(dark);
        end;

        COLOR_THEME_WHITE:
        begin
          Color := clWhite;
          editor.setColorTheme(white);
        end;
      end;

      if highlighter then
        case colorTheme of
          COLOR_THEME_DARK: editor.setHighlighterColorTheme(dark);
          COLOR_THEME_WHITE: editor.setHighlighterColorTheme(white);
          else
            editor.setHighlighterColorTheme(default);
        end;
    end;
  finally
    FreeAndNil(config);
  end;
end;

procedure TfrmMain.loadSynEditConfig;
var
  config: TConfig;

begin
  try
    config := TConfig.Create(appConfigFile);

    with synEdit do
    begin
      Font.Name := config.fontName;
      Font.Size := config.fontSize;

      Gutter.Parts[1].Visible := config.lineNumbers; // SynGutterLineNumber
      Gutter.Parts[3].Visible := config.lineNumbers; // SynGutterSeparator
      RightEdge := config.rightEdge;

      BorderSpacing.Left := config.borderSpaceLeft;
      BorderSpacing.Right := config.borderSpaceRight;
      BorderSpacing.Top := config.borderSpaceTop;
      BorderSpacing.Bottom := config.borderSpaceBottom;

      if config.scrollBars then
        ScrollBars := ssAutoBoth
      else
        ScrollBars := ssNone;
    end;
  finally
    FreeAndNil(config);
  end;
end;

procedure TfrmMain.saveConfig;
var
  config: TConfig;

begin
  try
    config := TConfig.Create(appConfigFile);

    with config do
    begin
      formLeft := Left;
      formTop := Top;
      formHeight := Height;
      formWidth := Width;
    end;
  finally
    FreeAndNil(config);
  end;
end;

procedure TfrmMain.createDefaultConfigFile;
var
  config: TConfig;

begin
  try
    config := TConfig.Create(appConfigFile);
    config.createDefaultConfigFile();
  finally
    FreeAndNil(config);
  end;
end;

function TfrmMain.saveFile: boolean;
begin
  Result := False;

  if editor.isNotNewFile() then
    Result := editor.saveCurrentFile()
  else
  if saveDialog.Execute then
    Result := editor.saveFile(saveDialog.FileName);
end;

function TfrmMain.showFileChangeDialog: TModalResult;
begin
  Result := MessageDlg(CAPTION_FILE_CHANGED, MSG_SAVE_CHANGES, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TfrmMain.actSaveFileExecute(Sender: TObject);
begin
  saveFile();
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if editor.fileModified then
    case showFileChangeDialog() of
      mrYes: saveFile();
      mrCancel: CanClose := False;
    end;
end;

procedure TfrmMain.actFullscreenExecute(Sender: TObject);
begin
  if WindowState <> wsFullScreen then
    WindowState := wsFullScreen
  else
    WindowState := wsNormal;
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
  Close;
end;

end.
