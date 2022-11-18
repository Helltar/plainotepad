unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus, StdActns, LCLIntf,

  ATSynEdit, ATSynEdit_Globals, ATStringProc, ATSynEdit_Adapter_EControl, ATSynEdit_Carets,
  ATSynEdit_Bookmarks, ATSynEdit_Export_HTML, ec_SyntAnal, ec_proc_lexer,

  uConfig, uEditor;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFullscreen: TAction;
    actClose: TAction;
    actHtmlExport: TAction;
    actOpenFile: TAction;
    actSaveFile: TAction;
    actionList: TActionList;
    synEdit: TATSynEdit;
    edtCopy: TEditCopy;
    edtCut: TEditCut;
    edtDelete: TEditDelete;
    edtPaste: TEditPaste;
    edtSelectAll: TEditSelectAll;
    editUndo: TEditUndo;
    miOpenFile: TMenuItem;
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
    openDialog: TOpenDialog;
    pmMain: TPopupMenu;
    saveDialog: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    procedure actFullscreenExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actHtmlExportExecute(Sender: TObject);
    procedure actHtmlExportUpdate(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
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
    procedure initComponents();
    procedure initEditor();
    procedure loadEditorConfig();
    procedure loadFormConfig();
    procedure saveConfig();
  public
    appConfigFile: string;
    config: TConfig;
    procedure updateConfig();
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uConsts, uAboutForm, uSettingsForm, uLogger, uUtils;

resourcestring
  CAPTION_FILE_CHANGED = 'File changed';
  MSG_SAVE_CHANGES = 'Save the changes?';
  ERROR_MK_CONFIG_DIR = 'Configuration directory could not be created, editor settings will not be saved';

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  appConfigDir: string;

begin
  appConfigDir := GetAppConfigDir(False);
  appConfigFile := appConfigDir + APP_CONFIG_FILE_NAME;

  if not DirectoryExists(appConfigDir) then
    if not CreateDir(appConfigDir) then
      addLog(ERROR_MK_CONFIG_DIR);

  config := TConfig.Create(appConfigFile);
  editor := TEditor.Create(synEdit);

  loadFormConfig();
  initComponents();

  initEditor();
  loadEditorConfig();

  if ParamCount > 0 then
    openFile(ParamStr(1));
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
    if editor.saveFile(saveDialog.FileName) then
      editor.openFile(saveDialog.FileName);
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

    if fullScreen then
      WindowState := wsFullScreen;
  end;
end;

procedure TfrmMain.loadEditorConfig;
begin
  with synEdit do
    with config do
    begin
      Font.Name := fontName;
      Font.Size := fontSize;

      Gutter[Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible := lineNumbers;

      if wordWrap then
        OptWrapMode := cWrapOn
      else
        OptWrapMode := cWrapOff;

      if rightEdge = 0 then
        // todo: tmp. OptMarginRight := 10000
        synEdit.OptMarginRight := 10000
      else
        synEdit.OptMarginRight := rightEdge;

      BorderSpacing.Left := borderSpaceLeft;
      BorderSpacing.Right := borderSpaceRight;
      BorderSpacing.Top := borderSpaceTop;
      BorderSpacing.Bottom := borderSpaceBottom;

      if scrollBars then
      begin
        synEdit.OptScrollbarsNew := True;
        synEdit.OptScrollStyleHorz := aessAuto;
        synEdit.OptScrollStyleVert := aessAuto;
      end
      else
      begin
        synEdit.OptScrollbarsNew := False;
        synEdit.OptScrollStyleHorz := aessHide;
        synEdit.OptScrollStyleVert := aessHide;
      end;

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
    end;
end;

procedure TfrmMain.initEditor;
begin
  synEdit.Gutter[synEdit.Gutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks)].Visible := False;
  synEdit.PopupText := pmMain;
end;

procedure TfrmMain.initComponents;
begin
  saveDialog.InitialDir := GetUserDir;
  openDialog.InitialDir := GetUserDir;
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
    fullScreen := WindowState = wsFullScreen;
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
  CanClose := False;

  if editor.fileModified then
    case showFileChangeDialog() of
      mrYes:
        if not saveFile() then
          Exit;
      mrCancel: Exit;
    end;

  CanClose := True;
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
  //editor.exportToHtml();
end;

procedure TfrmMain.actHtmlExportUpdate(Sender: TObject);
begin
  //actHtmlExport.Enabled := editor.isHighlighterUsed();
end;

procedure TfrmMain.actOpenFileExecute(Sender: TObject);
begin
  if openDialog.Execute then
    openFile(openDialog.FileName);
end;

end.
