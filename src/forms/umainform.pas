unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus, LCLIntf,
  ATSynEdit, ATSynEdit_Globals, ATSynEdit_Commands,
  uConfig, uEditor, uSettingsForm;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFullscreen: TAction;
    actClose: TAction;
    actNewWindow: TAction;
    actNewFile: TAction;
    actShowMenubar: TAction;
    actSettings: TAction;
    actSaveFileAs: TAction;
    actOpenFile: TAction;
    actSaveFile: TAction;
    actionList: TActionList;
    miNewWindow: TMenuItem;
    miNewFile: TMenuItem;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miAbout: TMenuItem;
    miFullscreen: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    miUndo: TMenuItem;
    miEditDelete: TMenuItem;
    miShowMenubar: TMenuItem;
    miEdit: TMenuItem;
    miView: TMenuItem;
    miHelp: TMenuItem;
    miOpenFile: TMenuItem;
    miSaveFile: TMenuItem;
    miSaveFileAs: TMenuItem;
    miSettings: TMenuItem;
    miClose: TMenuItem;
    separator1: TMenuItem;
    separator4: TMenuItem;
    separator5: TMenuItem;
    separator6: TMenuItem;
    separator3: TMenuItem;
    separator2: TMenuItem;
    Separator7: TMenuItem;
    synEdit: TATSynEdit;
    openDialog: TOpenDialog;
    saveDialog: TSaveDialog;
    procedure actCloseExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actFullscreenExecute(Sender: TObject);
    procedure actNewWindowExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actSaveFileAsExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actSaveFileUpdate(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowMenubarExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure miAboutClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miEditDeleteClick(Sender: TObject);
  private
    function openFile(fileName: string): boolean;
    function saveFile(): boolean;
    function checkFileModifiedStatus(): boolean;
    procedure closeFile();
    procedure initComponents();
    procedure initEditor();
    procedure loadFormConfig();
    procedure saveConfig();
    procedure updateSaveDialogTitle();
    procedure initialSetup();
  public
    config: TConfig;
    editor: TEditor;
    procedure loadEditorConfig(ASynEdit: TATSynEdit; AEditor: TEditor; const changeParentColor: boolean = True);
    procedure updateConfig();
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uConsts, uAboutForm, uLogger, uUtils, uFileChangedDialog;

resourcestring
  ERROR_MK_CONFIG_DIR = 'Configuration directory could not be created, editor settings will not be saved';
  TITLE_SAVE_FILE_AS = 'Save file as';

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;

begin
  initialSetup();

  config := TConfig.Create(getConfigDir() + APP_CONFIG_FILE_NAME);
  editor := TEditor.Create(synEdit);

  loadFormConfig();
  initComponents();

  initEditor();
  loadEditorConfig(synEdit, editor);

  if ParamCount > 0 then
  begin
    openFile(ParamStr(1));

    if ParamCount > 1 then
      for i := 2 to ParamCount do
        runProcess(ParamStr(0), ParamStr(i));
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  saveConfig();
  FreeAndNil(editor);
  FreeAndNil(config);
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;

begin
  openFile(FileNames[0]);

  for i := 1 to Length(FileNames) - 1 do
    runProcess(ParamStr(0), FileNames[i]);
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

procedure TfrmMain.miCutClick(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_ClipboardCut, cInvokeMenuContext);
end;

procedure TfrmMain.miCopyClick(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_ClipboardCopy, cInvokeMenuContext);
end;

procedure TfrmMain.miPasteClick(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_ClipboardPaste, cInvokeMenuContext);
end;

procedure TfrmMain.miSelectAllClick(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_SelectAll, cInvokeMenuContext);
end;

procedure TfrmMain.miUndoClick(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_Undo, cInvokeMenuContext);
end;

procedure TfrmMain.miEditDeleteClick(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_TextDeleteSelection, cInvokeMenuContext);
end;

function TfrmMain.openFile(fileName: string): boolean;
begin
  Result := False;

  if checkFileModifiedStatus() then
    Result := editor.openFile(fileName);
end;

function TfrmMain.saveFile: boolean;
begin
  Result := False;

  if editor.isNotNewFile() and not FileIsReadOnly(editor.getCurrentFilename()) then
    Result := editor.saveCurrentFile()
  else
  if saveDialog.Execute then
  begin
    updateSaveDialogTitle();
    Result := editor.saveFile(saveDialog.FileName);
  end;
end;

procedure TfrmMain.closeFile;
begin
  if checkFileModifiedStatus() then
    editor.closeFile();
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

    if not showMenubar then
      Menu := nil;
  end;
end;

procedure TfrmMain.loadEditorConfig(ASynEdit: TATSynEdit; AEditor: TEditor; const changeParentColor: boolean);
begin
  with ASynEdit do
    with config do
    begin
      Font.Name := fontName;
      Font.Size := fontSize;

      Gutter[Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible := lineNumbers;
      OptMinimapVisible := miniMap;

      case mouseMiddleClickAction of
        0: OptMouseMiddleClickAction := mcaPaste;
        1: OptMouseMiddleClickAction := mcaScrolling;
        else
          OptMouseMiddleClickAction := mcaPaste;
      end;

      if wordWrap then
        OptWrapMode := cWrapOn
      else
        OptWrapMode := cWrapOff;

      if rightEdge = 0 then
        OptMarginRight := 10000
      else
        OptMarginRight := rightEdge;

      BorderSpacing.Left := borderSpaceLeft;
      BorderSpacing.Right := borderSpaceRight;
      BorderSpacing.Top := borderSpaceTop;

      if borderSpaceBottom > 0 then
        BorderSpacing.Bottom := borderSpaceBottom
      else
        BorderSpacing.Bottom := 1; // fullscreen mouse-select down-scroll

      if scrollBars then
      begin
        synEdit.OptScrollbarsNew := nonSystemScrollBars;
        OptScrollStyleHorz := aessAuto;
        OptScrollStyleVert := aessAuto;
      end
      else
      begin
        OptScrollStyleHorz := aessHide;
        OptScrollStyleVert := aessHide;
      end;

      AEditor.highlighter := highlighter;
      AEditor.setColorTheme(colorTheme);

      if changeParentColor then
        ASynEdit.Parent.Color := Colors.TextBG;

      Update();
    end;
end;

procedure TfrmMain.initEditor;
begin
  synEdit.Gutter[synEdit.Gutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks)].Visible := False;
end;

procedure TfrmMain.initComponents;
begin
  saveDialog.InitialDir := GetUserDir;
  openDialog.InitialDir := GetUserDir;
end;

procedure TfrmMain.updateConfig;
begin
  loadEditorConfig(synEdit, editor);
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
    showMenubar := Assigned(Menu);
  end;
end;

procedure TfrmMain.updateSaveDialogTitle;
var
  filename: string;

begin
  filename := editor.getCurrentFilename();
  saveDialog.Title := TITLE_SAVE_FILE_AS;

  if not filename.IsEmpty then
    saveDialog.Title := ExtractFileName(filename) + ' - ' + TITLE_SAVE_FILE_AS;
end;

procedure TfrmMain.initialSetup;
var
  dirLexlib, dirThemes: string;

begin
  if not DirectoryExists(getConfigDir()) then
    if not CreateDir(getConfigDir()) then
    begin
      addLog(ERROR_MK_CONFIG_DIR);
      Exit;
    end;

  dirLexlib := getConfigDir() + DIR_LEXLIB;

  if not DirectoryExists(dirLexlib) then
  begin
    copyResToDir(RES_SYNT_ANALYZERS, dirLexlib);
    unzipArchive(dirLexlib + RES_SYNT_ANALYZERS.ToLower, dirLexlib);
    DeleteFile(dirLexlib + RES_SYNT_ANALYZERS.ToLower);
  end;

  dirThemes := getConfigDir() + DIR_COLOR_SCHEMES;

  if not DirectoryExists(dirThemes) then
  begin
    copyResToDir(COLOR_THEME_DARK + FILE_EXT_COLOR_SCHEME, dirThemes);
    copyResToDir(COLOR_THEME_CREAM + FILE_EXT_COLOR_SCHEME, dirThemes);
  end;
end;

function TfrmMain.checkFileModifiedStatus: boolean;
begin
  Result := False;

  if editor.fileModified then
    with TdlgFileChanged.Create(Self, editor.getCurrentFilename()) do
      try
        ShowModal;
        case dlgResult of
          dlgResOk:
            if not saveFile() then
              Exit;
          dlgResCancel: Exit;
        end;
      finally
        Free;
      end;

  Result := True;
end;

procedure TfrmMain.actSaveFileExecute(Sender: TObject);
begin
  saveFile();
end;

procedure TfrmMain.actSaveFileUpdate(Sender: TObject);
begin
  actSaveFile.Enabled := editor.fileModified;
end;

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  if not Assigned(frmSettings) then
    frmSettings := TfrmSettings.Create(Self);

  frmSettings.ShowModal;
end;

procedure TfrmMain.actShowMenubarExecute(Sender: TObject);
begin
  if Assigned(Menu) then
    Menu := nil
  else
    Menu := mmMain;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := checkFileModifiedStatus();
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actNewFileExecute(Sender: TObject);
begin
  closeFile();
end;

procedure TfrmMain.actFullscreenExecute(Sender: TObject);
begin
  if WindowState <> wsFullScreen then
  begin
    {$IfDef MSWINDOWS}
    BorderStyle := bsNone;
    {$EndIf}

    WindowState := wsFullScreen;
  end
  else
  begin
    {$IfDef MSWINDOWS}
    BorderStyle := bsSizeable;
    {$EndIf}

    WindowState := wsNormal;
  end;
end;

procedure TfrmMain.actNewWindowExecute(Sender: TObject);
begin
  runProcess(ParamStr(0));
end;

procedure TfrmMain.actOpenFileExecute(Sender: TObject);
begin
  if openDialog.Execute then
    openFile(openDialog.FileName);
end;

procedure TfrmMain.actSaveFileAsExecute(Sender: TObject);
begin
  updateSaveDialogTitle();

  if saveDialog.Execute then
    if editor.saveFile(saveDialog.FileName) then
      editor.openFile(saveDialog.FileName);
end;

end.
