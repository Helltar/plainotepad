unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, LCLIntf, ExtCtrls, ComCtrls, StdCtrls,
  ATSynEdit, ATSynEdit_Globals, ATSynEdit_Commands,
  ATSynEdit_Finder, ATSynEdit_Carets,
  { --- }
  uConfig, uEditor, uSettingsForm;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFullscreen: TAction;
    actClose: TAction;
    actCut: TAction;
    actCopy: TAction;
    actDelete: TAction;
    actSearch: TAction;
    actFindPrevious: TAction;
    actFindNext: TAction;
    actUndo: TAction;
    actPaste: TAction;
    actSelectAll: TAction;
    actNewWindow: TAction;
    actNewFile: TAction;
    actShowMenubar: TAction;
    actSettings: TAction;
    actSaveFileAs: TAction;
    actOpenFile: TAction;
    actSaveFile: TAction;
    actionList: TActionList;
    btnFindNext: TButton;
    btnFindPrevious: TButton;
    btnCloseSearch: TButton;
    edtFind: TEdit;
    miSearch: TMenuItem;
    miFindNext: TMenuItem;
    miFindPrevious: TMenuItem;
    Separator11: TMenuItem;
    miShortcuts: TMenuItem;
    miOpenRecent: TMenuItem;
    miShowToolBar: TMenuItem;
    miCreateDesktopEntry: TMenuItem;
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
    pnlSearch: TPanel;
    pnlEditor: TPanel;
    separator1: TMenuItem;
    Separator10: TMenuItem;
    separator4: TMenuItem;
    separator5: TMenuItem;
    separator6: TMenuItem;
    separator3: TMenuItem;
    separator2: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    stEditor: TStatusBar;
    synEdit: TATSynEdit;
    openDialog: TOpenDialog;
    saveDialog: TSaveDialog;
    tbEditor: TToolBar;
    tbtnNewFile: TToolButton;
    tbtnOpenFile: TToolButton;
    tbtnSaveFile: TToolButton;
    tbtnSaveFileAs: TToolButton;
    tbtnSeparator: TToolButton;
    tbtnNewWindow: TToolButton;
    timFileModifiedCheck: TTimer;
    ToolButton1: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actFindNextUpdate(Sender: TObject);
    procedure actFindPreviousUpdate(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actFullscreenExecute(Sender: TObject);
    procedure actNewWindowExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSaveFileAsExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actSaveFileUpdate(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowMenubarExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnFindPreviousClick(Sender: TObject);
    procedure btnCloseSearchClick(Sender: TObject);
    procedure edtFindChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure miShortcutsClick(Sender: TObject);
    procedure miCreateDesktopEntryClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miShowToolBarClick(Sender: TObject);
    procedure synEditChangeCaretPos(Sender: TObject);
    procedure timFileModifiedCheckTimer(Sender: TObject);
  private
    currentFileAge: longint;
    editorFinder: TATEditorFinder;
    function checkFileModifiedStatus(): boolean;
    function openFile(fileName: string): boolean;
    function saveFile(): boolean;
    procedure addFilenameToRecent(const filename: string);
    procedure closeFile();
    procedure doFind(ANext: boolean);
    procedure initComponents();
    procedure initEditor();
    procedure initialSetup();
    procedure initRecentFilesItems();
    procedure initSynEdit(ASynEdit: TATSynEdit);
    procedure loadFormConfig();
    procedure miClearRecentListClick(Sender: TObject);
    procedure miRecentFileClick(Sender: TObject);
    procedure updateSaveDialogTitle();
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
  uConsts, uAboutForm, uLogger, uUtils,
  uFileChangedDialog, uFileChangedOnDiskDialog;

resourcestring
  CAPTION_CLEAR_LIST = 'Clear List';
  DESKTOP_ENTRY_CREATED = 'Desktop entry created';
  ERROR_MK_CONFIG_DIR = 'Configuration directory could not be created, editor settings will not be saved';
  ERROR_WHEN_SAVE_FILE = 'Error when save file: %s';
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

  editorFinder := TATEditorFinder.Create;
  editorFinder.Editor := synEdit;
  editorFinder.StrFind := edtFind.Text;

  if ParamCount > 0 then
  begin
    openFile(ExpandFileName(ParamStr(1)));

    if ParamCount > 1 then
      for i := 2 to ParamCount do
        runProcess(ParamStr(0), ParamStr(i));
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(editorFinder);
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

procedure TfrmMain.miShortcutsClick(Sender: TObject);
var
  dirHelp: string;

begin
  dirHelp := getConfigDir() + DIR_HELP;

  if not DirectoryExists(dirHelp) then
    copyResToDir(RES_SHORTCUTS_HTML, dirHelp);

  OpenURL(dirHelp + RES_SHORTCUTS_HTML.ToLower);
end;

procedure TfrmMain.miCreateDesktopEntryClick(Sender: TObject);
begin
  if createDesktopEntry() then
    addLog(DESKTOP_ENTRY_CREATED, True, False);
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

procedure TfrmMain.miShowToolBarClick(Sender: TObject);
begin
  if tbEditor.Visible then
    tbEditor.Visible := False
  else
    tbEditor.Visible := True;

  config.showToolbar := tbEditor.Visible;
end;

procedure TfrmMain.synEditChangeCaretPos(Sender: TObject);
var
  x, y: string;

begin
  x := IntToStr(synEdit.Carets[0].PosX + 1);
  y := IntToStr(synEdit.Carets[0].PosY + 1);
  stEditor.Panels.Items[0].Text := y + ':  ' + x + '      ';
end;

procedure TfrmMain.timFileModifiedCheckTimer(Sender: TObject);
var
  filename: string;

begin
  if not synEdit.Focused then
    Exit;

  if not editor.isNotNewFile() then
    Exit;

  filename := editor.getCurrentFilename();

  if not FileExists(filename) then
  begin
    editor.fileModified := True;
    Exit;
  end;

  if FileAge(filename) <> currentFileAge then
    with TdlgFileChangedOnDisk.Create(Self, filename) do
      try
        ShowModal;

        case dlgResult of
          dlgResReload:
          begin
            editor.openFile(filename);
            currentFileAge := FileAge(filename);
          end;

          dlgResIgnore: timFileModifiedCheck.Enabled := False;
        end;
      finally
        Free;
      end;
end;

procedure TfrmMain.miRecentFileClick(Sender: TObject);
begin
  openFile(TMenuItem(Sender).Hint);
end;

procedure TfrmMain.miClearRecentListClick(Sender: TObject);
begin
  if DeleteFile(getConfigDir + APP_RECENT_FILES_FILENAME) then
  begin
    miOpenRecent.Enabled := False;
    miOpenRecent.Clear;
  end;
end;

function TfrmMain.openFile(fileName: string): boolean;
begin
  Result := False;

  if checkFileModifiedStatus() then
    Result := editor.openFile(fileName);

  if Result then
  begin
    addFilenameToRecent(fileName);
    currentFileAge := FileAge(fileName);
    timFileModifiedCheck.Enabled := True;
  end;
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

  if Result then
  begin
    currentFileAge := FileAge(editor.getCurrentFilename());
    timFileModifiedCheck.Enabled := True;
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
      WindowState := wsFullScreen
    else
    if wsMaximized then
      WindowState := Forms.wsMaximized;

    if not showMenubar then
      Menu := nil;

    tbEditor.Visible := showToolbar;
    miShowToolBar.Checked := showToolbar;
    edtFind.Font.Name := fontName;
    edtFind.Font.Size := fontSize;
  end;

  {$IfDef MSWINDOWS}
  miCreateDesktopEntry.Visible := False;
  Separator8.Visible := False;
  {$EndIf}

  initRecentFilesItems();
end;

procedure TfrmMain.initSynEdit(ASynEdit: TATSynEdit);
begin
  with ASynEdit do
  begin
    OptBorderVisible := False;
    OptCaretVirtual := False;
    OptFoldEnabled := False;
    OptMouse2ClickOpensURL := False;
    OptNumbersStyle := TATEditorNumbersStyle.All;
    OptRulerVisible := False;
    OptShowCurLine := True;
    OptShowMouseSelFrame := False;
    OptShowScrollHint := True;
    OptShowURLs := False;
    OptSpacingY := 0;
    OptTabSize := 4;
    OptTabSpaces := True;
    OptUnprintedEnds := False;

    PopupGutterFold := TPopupMenu.Create(Self);
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
      OptUnprintedVisible := unprintedVisible;
      OptSavingForceFinalEol := appendNewline;

      case mouseMiddleClickAction of
        0: OptMouseMiddleClickAction := TATEditorMiddleClickAction.Paste;
        1: OptMouseMiddleClickAction := TATEditorMiddleClickAction.Scrolling;
        else
          OptMouseMiddleClickAction := TATEditorMiddleClickAction.Paste;
      end;

      if wordWrap then
        OptWrapMode := TATEditorWrapMode.ModeOn
      else
        OptWrapMode := TATEditorWrapMode.ModeOff;

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
        OptScrollStyleHorz := TATEditorScrollbarStyle.Auto;
        OptScrollStyleVert := TATEditorScrollbarStyle.Auto;
      end
      else
      begin
        OptScrollStyleHorz := TATEditorScrollbarStyle.Hide;
        OptScrollStyleVert := TATEditorScrollbarStyle.Hide;
      end;

      AEditor.highlighter := highlighter;
      AEditor.setColorTheme(colorTheme);

      if changeParentColor then
        ASynEdit.Parent.Color := Colors.TextBG;

      stEditor.Color := Colors.TextBG;
      stEditor.Font.Color := Colors.TextFont;
      pnlSearch.Color := Colors.TextBG;

      initSynEdit(ASynEdit);

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
  pnlEditor.AnchorSideTop.Control := tbEditor;
end;

procedure TfrmMain.updateConfig;
begin
  loadEditorConfig(synEdit, editor);
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

procedure TfrmMain.doFind(ANext: boolean);
var
  aChanged: boolean;

begin
  edtFind.Font.Color := clBlack;
  edtFind.Color := clWhite;

  if editorFinder.StrFind = '' then
    Exit;

  editorFinder.OptFromCaret := ANext;

  if editorFinder.DoAction_FindOrReplace(False, False, aChanged, True) then
  begin
    edtFind.Color := $00BDE7AB;

    synEdit.DoGotoPos(Point(editorFinder.MatchEdPos.X, editorFinder.MatchEdPos.Y),
      Point(editorFinder.MatchEdEnd.X, editorFinder.MatchEdEnd.Y), 5, 2, True, TATEditorActionIfFolded.Unfold);
  end
  else
  begin
    edtFind.Font.Color := clWhite;
    edtFind.Color := $005050F0;
  end;

  edtFind.Update();
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

procedure TfrmMain.addFilenameToRecent(const filename: string);
var
  list: TStringList;
  recentFilesConfig: string;

begin
  try
    list := TStringList.Create;

    with list do
    begin
      recentFilesConfig := getConfigDir + APP_RECENT_FILES_FILENAME;

      try
        if FileExists(recentFilesConfig) then
          LoadFromFile(recentFilesConfig);

        if Text.IndexOf(filename) > -1 then
          Exit;

        if Count > 10 then
        begin
          Insert(0, ExtractFileName(filename) + '=' + filename);
          Delete(Count - 1);
        end
        else
          AddPair(ExtractFileName(filename), filename);

        SaveToFile(recentFilesConfig);
      except
        // todo: Exception at 00000000004653BA: EFCreateError
      end;

      initRecentFilesItems();
    end;
  finally
    FreeAndNil(list);
  end;
end;

procedure TfrmMain.initRecentFilesItems;
var
  i: integer;
  filename: string;
  recentFileslist, list: TStringList;
  mItem: TMenuItem;

begin
  filename := getConfigDir + APP_RECENT_FILES_FILENAME;

  if not FileExists(filename) then
    Exit;

  if miOpenRecent.Count > 0 then
    miOpenRecent.Clear;

  try
    recentFileslist := TStringList.Create;
    list := TStringList.Create;

    with recentFileslist do
    begin
      LoadFromFile(filename);

      for i := 0 to Count - 1 do
        if FileExists(ValueFromIndex[i]) then
        begin
          mItem := TMenuItem.Create(miOpenRecent);
          mItem.Caption := Names[i] + ' --> ' + ValueFromIndex[i];
          mItem.Hint := ValueFromIndex[i];
          mItem.OnClick := @miRecentFileClick;
          miOpenRecent.Add(mItem);

          list.AddPair(Names[i], ValueFromIndex[i]);
        end;

      if list.Count > 0 then
      begin
        mItem := TMenuItem.Create(miOpenRecent);
        mItem.Caption := '-';
        miOpenRecent.Add(mItem);

        mItem := TMenuItem.Create(miOpenRecent);
        mItem.Caption := CAPTION_CLEAR_LIST;
        mItem.OnClick := @miClearRecentListClick;
        miOpenRecent.Add(mItem);
      end;

      try
        list.SaveToFile(filename);
      except
        addLog(Format(ERROR_WHEN_SAVE_FILE, [filename]));
      end;
    end;
  finally
    FreeAndNil(recentFileslist);
    FreeAndNil(list);
  end;

  miOpenRecent.Enabled := miOpenRecent.Count > 0;
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

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_SelectAll, TATCommandInvoke.MenuContext);
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

  config.showMenubar := Assigned(Menu);
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_Undo, TATCommandInvoke.MenuContext);
end;

procedure TfrmMain.btnFindNextClick(Sender: TObject);
begin
  editorFinder.OptBack := False;
  DoFind(True);
end;

procedure TfrmMain.btnFindPreviousClick(Sender: TObject);
begin
  editorFinder.OptBack := True;
  DoFind(True);
end;

procedure TfrmMain.btnCloseSearchClick(Sender: TObject);
begin
  pnlEditor.AnchorSideTop.Control := tbEditor;
  pnlSearch.Enabled := False;
  pnlSearch.Visible := False;
end;

procedure TfrmMain.edtFindChange(Sender: TObject);
begin
  btnFindNext.Enabled := edtFind.Text <> '';
  btnFindPrevious.Enabled := btnFindNext.Enabled;
  editorFinder.StrFind := edtFind.Text;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    with config do
    begin
      formLeft := Left;
      formTop := Top;
      formHeight := Height;
      formWidth := Width;
      wsMaximized := WindowState = Forms.wsMaximized;
    end;
  except
    // todo: Exception at 00000000004653BA: EFCreateError
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := checkFileModifiedStatus();
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_ClipboardCopy, TATCommandInvoke.MenuContext);
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_ClipboardCut, TATCommandInvoke.MenuContext);
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_TextDeleteSelection, TATCommandInvoke.MenuContext);
end;

procedure TfrmMain.actFindNextUpdate(Sender: TObject);
begin
  actFindNext.Enabled := editorFinder.StrFind <> '';
end;

procedure TfrmMain.actFindPreviousUpdate(Sender: TObject);
begin
  actFindPrevious.Enabled := actFindNext.Enabled;
end;

procedure TfrmMain.actSearchExecute(Sender: TObject);
begin
  with pnlEditor do
    if AnchorSideTop.Control = pnlSearch then
      AnchorSideTop.Control := tbEditor
    else
      AnchorSideTop.Control := pnlSearch;

  pnlSearch.Enabled := not pnlSearch.Enabled;
  pnlSearch.Visible := not pnlSearch.Visible;

  if pnlSearch.Visible then
    edtFind.SetFocus;
end;

procedure TfrmMain.actFindPreviousExecute(Sender: TObject);
begin
  editorFinder.OptBack := True;
  DoFind(True);
end;

procedure TfrmMain.actFindNextExecute(Sender: TObject);
begin
  editorFinder.OptBack := False;
  DoFind(True);
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

  config.fullScreen := WindowState = wsFullScreen;
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

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  synEdit.DoCommand(cCommand_ClipboardPaste, TATCommandInvoke.MenuContext);
end;

procedure TfrmMain.actSaveFileAsExecute(Sender: TObject);
begin
  updateSaveDialogTitle();

  if saveDialog.Execute then
    if editor.saveFile(saveDialog.FileName) then
      editor.openFile(saveDialog.FileName);
end;

end.
