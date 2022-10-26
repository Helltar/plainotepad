unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ActnList, StdCtrls, Menus, StdActns, SynEdit, LCLIntf,
  uEditor, uConfig;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFullscreen: TAction;
    actClose: TAction;
    actHtmlExport: TAction;
    actSaveFile: TAction;
    actionList: TActionList;
    edtCopy: TEditCopy;
    edtCut: TEditCut;
    edtDelete: TEditDelete;
    edtPaste: TEditPaste;
    edtSelectAll: TEditSelectAll;
    editUndo: TEditUndo;
    miSaveAs: TMenuItem;
    miHtmlExport: TMenuItem;
    miSettings: TMenuItem;
    miFullscreen: TMenuItem;
    miView: TMenuItem;
    miDelete: TMenuItem;
    miAbout: TMenuItem;
    miQuit: TMenuItem;
    miHelp: TMenuItem;
    miEdit: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    miUndo: TMenuItem;
    miFile: TMenuItem;
    miSaveFile: TMenuItem;
    pmMain: TPopupMenu;
    saveDialog: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator8: TMenuItem;
    synEdit: TSynEdit;
    procedure actFullscreenExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actHtmlExportExecute(Sender: TObject);
    procedure actHtmlExportUpdate(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actSaveFileUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure miAboutClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
  private
    editor: TEditor;
    function openFile(fileName: string): boolean;
    function saveFile(): boolean;
    function showFileChangeDialog(): TModalResult;
    procedure saveConfig();
    procedure loadFormConfig();
    procedure loadEditorConfig();
    procedure loadSynEditConfig();
  public
    appConfigFile: string;
    config: TConfig;
    procedure updateConfig();
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uConsts, uAboutForm, uSettingsForm;

resourcestring
  CAPTION_FILE_CHANGED = 'File changed';
  MSG_SAVE_CHANGES = 'Save the changes?';

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  appConfigDir: string;

begin
  appConfigDir := GetAppConfigDir(False);
  appConfigFile := appConfigDir + APP_CONFIG_FILE_NAME;

  if not DirectoryExists(appConfigDir) then
    CreateDir(appConfigDir);

  config := TConfig.Create(appConfigFile);
  editor := TEditor.Create(synEdit);

  loadFormConfig();
  loadEditorConfig();

  if ParamCount > 0 then
    openFile(ParamStr(1));

  saveDialog.InitialDir := GetUserDir;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  saveConfig();
  FreeAndNil(editor);
  FreeAndNil(config);
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  openFile(FileNames[0]);
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  with TfrmAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.miSaveAsClick(Sender: TObject);
begin
  if saveDialog.Execute then
    editor.saveFile(saveDialog.FileName);
end;

procedure TfrmMain.miSettingsClick(Sender: TObject);
begin
  with TfrmSettings.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

function TfrmMain.openFile(fileName: string): boolean;
begin
  Result := False;

  if editor.fileModified then
    case showFileChangeDialog() of
      mrYes:
        if not saveFile() then
          Exit;
      mrCancel: Exit;
    end;

  Result := editor.openFile(fileName);
end;

procedure TfrmMain.loadFormConfig;
begin
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
  end;
end;

procedure TfrmMain.loadEditorConfig;
begin
  with synEdit do
    with config do
    begin
      Font.Name := fontName;
      Font.Size := fontSize;

      Gutter.Parts[1].Visible := lineNumbers; // SynGutterLineNumber
      Gutter.Parts[3].Visible := lineNumbers; // SynGutterSeparator
      synEdit.RightEdge := rightEdge;

      BorderSpacing.Left := borderSpaceLeft;
      BorderSpacing.Right := borderSpaceRight;
      BorderSpacing.Top := borderSpaceTop;
      BorderSpacing.Bottom := borderSpaceBottom;

      if scrollBars then
        synEdit.ScrollBars := ssAutoBoth
      else
        synEdit.ScrollBars := ssNone;

      case colorTheme of
        COLOR_THEME_CREAM:
        begin
          Self.Color := clCream;
          editor.setColorTheme(cream);
        end;

        COLOR_THEME_DARK:
        begin
          Self.Color := $001e1e1e;
          editor.setColorTheme(dark);
        end;

        COLOR_THEME_WHITE:
        begin
          Self.Color := clWhite;
          editor.setColorTheme(white);
        end;
      end;

      editor.enableHighlighter(highlighter);
    end;
end;

procedure TfrmMain.loadSynEditConfig;
begin

end;

procedure TfrmMain.updateConfig;
begin
  loadEditorConfig();
end;

procedure TfrmMain.saveConfig;
begin
  with config do
  begin
    formLeft := Left;
    formTop := Top;
    formHeight := Height;
    formWidth := Width;
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

procedure TfrmMain.actSaveFileUpdate(Sender: TObject);
begin
  actSaveFile.Enabled := editor.fileModified;
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

procedure TfrmMain.actHtmlExportExecute(Sender: TObject);
begin
  editor.exportToHtml();
end;

procedure TfrmMain.actHtmlExportUpdate(Sender: TObject);
begin
  actHtmlExport.Enabled := editor.isHighlighterUsed();
end;

end.
