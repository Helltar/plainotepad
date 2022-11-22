unit uSettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Forms, Graphics, Dialogs,
  StdCtrls, Spin, ComCtrls, ColorBox, ExtCtrls, FileUtil;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnSelectFont: TButton;
    btnOk: TButton;
    btnCreateDesktopEntry: TButton;
    cbHighlighter: TCheckBox;
    cbLineNumbers: TCheckBox;
    cbScrollBars: TCheckBox;
    cbNonSystemScrollBars: TCheckBox;
    cbMiniMap: TCheckBox;
    cbWordWrap: TCheckBox;
    cmbEditColorTheme: TComboBox;
    cmbLexers: TComboBox;
    cmbMouseMiddleClickAction: TComboBox;
    cmbColorTheme: TComboBox;
    edtFontName: TEdit;
    fontDialog: TFontDialog;
    gbBorderSpace: TGroupBox;
    lblTheme: TLabel;
    lblLexer: TLabel;
    lblMouseMiddleClickAction: TLabel;
    lblRightMargin: TLabel;
    lblBottomSpace: TLabel;
    lblLeftSpace: TLabel;
    lblRightSpace: TLabel;
    lblTopSpace: TLabel;
    lblColorTheme: TLabel;
    lblFont: TLabel;
    pcSettings: TPageControl;
    sbLexerColors: TScrollBox;
    sbMainColors: TScrollBox;
    scGeneral: TScrollBox;
    seFontSize: TSpinEdit;
    seRightMargin: TSpinEdit;
    seBottomSpace: TSpinEdit;
    seLeftSpace: TSpinEdit;
    seRightSpace: TSpinEdit;
    seTopSpace: TSpinEdit;
    Splitter1: TSplitter;
    tsThemeEditor: TTabSheet;
    tsGeneral: TTabSheet;
    tsBorders: TTabSheet;
    procedure btnCreateDesktopEntryClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure cbHighlighterChange(Sender: TObject);
    procedure cbLineNumbersChange(Sender: TObject);
    procedure cbMiniMapChange(Sender: TObject);
    procedure cbNonSystemScrollBarsChange(Sender: TObject);
    procedure cbScrollBarsChange(Sender: TObject);
    procedure cbWordWrapChange(Sender: TObject);
    procedure cmbColorThemeChange(Sender: TObject);
    procedure cmbEditColorThemeChange(Sender: TObject);
    procedure cmbLexersChange(Sender: TObject);
    procedure cmbMouseMiddleClickActionChange(Sender: TObject);
    procedure controlEditingDone(Sender: TObject);
    procedure edtFontNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lexerColorBoxChange(Sender: TObject);
    procedure seBottomSpaceChange(Sender: TObject);
    procedure seFontSizeChange(Sender: TObject);
    procedure seLeftSpaceChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
    procedure seRightSpaceChange(Sender: TObject);
    procedure seTopSpaceChange(Sender: TObject);
    procedure themeColorBoxChange(Sender: TObject);
  private
    function getColorThemeFilename(): string;
    function getLexerFilename(): string;
    function getSelectedColorThemeName(): string;
    function getSelectedLexerName(): string;
    procedure initColorEditControls();
    procedure initControls(const filename: string; AParent: TWinControl; onChangeEvent: TNotifyEvent);
    procedure initThemeEditControls();
    procedure loadColorThemes();
    procedure loadLexersList();
    procedure updateSelectedLexer();
  end;

var
  frmSettings: TfrmSettings;

implementation

uses
  uMainForm, uConsts, uUtils, uLogger;

resourcestring
  DESKTOP_ENTRY_CREATED = 'Desktop entry created';
  ERROR_LOAD_CONFIG_FILE = 'Error when load a color theme config file: %s';
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  pcSettings.ActivePageIndex := 0;

  with config do
  begin
    cbHighlighter.Checked := highlighter;
    cbLineNumbers.Checked := lineNumbers;
    cbMiniMap.Checked := miniMap;
    cbNonSystemScrollBars.Checked := nonSystemScrollBars;
    cbNonSystemScrollBars.Enabled := scrollBars;
    cbScrollBars.Checked := scrollBars;
    cbWordWrap.Checked := wordWrap;
    edtFontName.Text := fontName;
    seBottomSpace.Value := borderSpaceBottom;
    seFontSize.Value := fontSize;
    seLeftSpace.Value := borderSpaceLeft;
    seRightMargin.Value := rightEdge;
    seRightSpace.Value := borderSpaceRight;
    seTopSpace.Value := borderSpaceTop;
  end;

  with cmbMouseMiddleClickAction do
    if config.mouseMiddleClickAction <= Items.Count then
      ItemIndex := config.mouseMiddleClickAction
    else
      ItemIndex := 0;

  loadColorThemes();

  with cmbColorTheme do
    if Items.Count > 0 then
    begin
      if Items.IndexOf(config.colorTheme) >= 0 then
        ItemIndex := Items.IndexOf(config.colorTheme)
      else
        ItemIndex := 0;

      cmbEditColorTheme.Items := Items;
      cmbEditColorTheme.ItemIndex := ItemIndex;
    end;

  {$IFDEF MSWINDOWS}
  btnCreateDesktopEntry.Visible := False;
  {$ENDIF}

  loadLexersList();
  initThemeEditControls();
  initColorEditControls();
end;

procedure TfrmSettings.btnSelectFontClick(Sender: TObject);
begin
  if fontDialog.Execute then
  begin
    edtFontName.Text := fontDialog.Font.Name;
    seFontSize.Value := fontDialog.Font.Size;
  end;
end;

procedure TfrmSettings.cbHighlighterChange(Sender: TObject);
begin
  config.highlighter := cbHighlighter.Checked;
end;

procedure TfrmSettings.cbLineNumbersChange(Sender: TObject);
begin
  config.lineNumbers := cbLineNumbers.Checked;
end;

procedure TfrmSettings.cbMiniMapChange(Sender: TObject);
begin
  config.miniMap := cbMiniMap.Checked;
end;

procedure TfrmSettings.cbNonSystemScrollBarsChange(Sender: TObject);
begin
  config.nonSystemScrollBars := cbNonSystemScrollBars.Checked;
end;

procedure TfrmSettings.cbScrollBarsChange(Sender: TObject);
begin
  config.scrollBars := cbScrollBars.Checked;
  cbNonSystemScrollBars.Enabled := cbScrollBars.Checked;
end;

procedure TfrmSettings.cbWordWrapChange(Sender: TObject);
begin
  config.wordWrap := cbWordWrap.Checked;
end;

procedure TfrmSettings.cmbColorThemeChange(Sender: TObject);
begin
  config.colorTheme := getSelectedColorThemeName();
  cmbEditColorTheme.ItemIndex := cmbColorTheme.ItemIndex;
end;

procedure TfrmSettings.cmbEditColorThemeChange(Sender: TObject);
begin
  cmbColorTheme.ItemIndex := cmbEditColorTheme.ItemIndex;
  initThemeEditControls();
  loadLexersList();
  initColorEditControls();
end;

procedure TfrmSettings.cmbLexersChange(Sender: TObject);
begin
  initColorEditControls();
end;

procedure TfrmSettings.cmbMouseMiddleClickActionChange(Sender: TObject);
begin
  config.mouseMiddleClickAction := cmbMouseMiddleClickAction.ItemIndex;
end;

procedure TfrmSettings.edtFontNameChange(Sender: TObject);
begin
  config.fontName := edtFontName.Text;
  frmMain.updateConfig();
end;

procedure TfrmSettings.controlEditingDone(Sender: TObject);
begin
  frmMain.updateConfig();
end;

procedure TfrmSettings.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettings.btnCreateDesktopEntryClick(Sender: TObject);
begin
  if createDesktopEntry() then
    addLog(DESKTOP_ENTRY_CREATED);
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;

  updateSelectedLexer();
end;

procedure TfrmSettings.lexerColorBoxChange(Sender: TObject);
var
  filename: string;

begin
  filename := getLexerFilename();

  with TStringList.Create do
    try
      try
        LoadFromFile(filename);
      except
        addLog(Format(ERROR_OPEN_FILE, [filename]));
      end;

      Values[TLabel(TColorBox(Sender).AnchorSideTop.Control).Caption] := ColorToString(TColorBox(Sender).Selected);

      try
        SaveToFile(filename);
      except
        addLog(Format(ERROR_SAVE_FILE, [filename]));
      end;

      editor.setColorTheme(getSelectedColorThemeName());
    finally
      Free;
    end;
end;

procedure TfrmSettings.themeColorBoxChange(Sender: TObject);
var
  filename: string;

begin
  filename := getColorThemeFilename();

  with TStringList.Create do
    try
      try
        LoadFromFile(filename);
      except
        addLog(Format(ERROR_OPEN_FILE, [filename]));
      end;

      Values[TLabel(TColorBox(Sender).AnchorSideTop.Control).Caption] := ColorToString(TColorBox(Sender).Selected);

      try
        SaveToFile(filename);
      except
        addLog(Format(ERROR_SAVE_FILE, [filename]));
      end;

      editor.setColorTheme(getSelectedColorThemeName());
    finally
      Free;
    end;
end;

procedure TfrmSettings.seBottomSpaceChange(Sender: TObject);
begin
  config.borderSpaceBottom := seBottomSpace.Value;
end;

procedure TfrmSettings.seFontSizeChange(Sender: TObject);
begin
  config.fontSize := seFontSize.Value;
end;

procedure TfrmSettings.seLeftSpaceChange(Sender: TObject);
begin
  config.borderSpaceLeft := seLeftSpace.Value;
end;

procedure TfrmSettings.seRightMarginChange(Sender: TObject);
begin
  config.rightEdge := seRightMargin.Value;
end;

procedure TfrmSettings.seRightSpaceChange(Sender: TObject);
begin
  config.borderSpaceRight := seRightSpace.Value;
end;

procedure TfrmSettings.seTopSpaceChange(Sender: TObject);
begin
  config.borderSpaceTop := seTopSpace.Value;
end;

function TfrmSettings.getLexerFilename: string;
begin
  Result := GetAppConfigDir(False) + DIR_COLOR_SCHEMES + getSelectedColorThemeName() +
    DirectorySeparator + DIR_COLOR_SCHEMES_LEXERS + getSelectedLexerName();
end;

function TfrmSettings.getColorThemeFilename: string;
begin
  Result := GetAppConfigDir(False) + DIR_COLOR_SCHEMES + getSelectedColorThemeName() + DirectorySeparator + COLOR_SCHEME_MAIN_FILE;
end;

function TfrmSettings.getSelectedColorThemeName: string;
begin
  Result := 'null';

  with cmbEditColorTheme do
    if Items.Count > 0 then
      if ItemIndex >= 0 then
        Result := Items[ItemIndex];
end;

function TfrmSettings.getSelectedLexerName: string;
begin
  Result := 'null';

  with cmbLexers do
    if Items.Count > 0 then
      if ItemIndex >= 0 then
        Result := Items[ItemIndex];
end;

procedure TfrmSettings.initColorEditControls();
begin
  initControls(getLexerFilename(), sbLexerColors, @lexerColorBoxChange);
end;

procedure TfrmSettings.initThemeEditControls;
begin
  initControls(getColorThemeFilename(), sbMainColors, @themeColorBoxChange);
end;

procedure TfrmSettings.initControls(const filename: string; AParent: TWinControl; onChangeEvent: TNotifyEvent);
var
  configList: TStringList;
  i: integer;

begin
  if AParent.ControlCount > 0 then
    for i := AParent.ControlCount - 1 downto 0 do
      AParent.Controls[i].Free;

  configList := TStringList.Create;

  try
    try
      configList.LoadFromFile(filename);

      for i := 0 to configList.Count - 1 do
        with TLabel.Create(AParent) do
        begin
          Parent := AParent;
          Caption := configList.Names[i];

          if i <> 0 then
          begin
            AnchorSideTop.Control := AParent.Controls[i - 1];
            AnchorSideTop.Side := asrBottom;
          end
          else
            AnchorSideTop.Control := AParent;

          AnchorSideLeft.Control := AParent;
          BorderSpacing.Around := 24;
        end;

      for i := 0 to configList.Count - 1 do
        with TColorBox.Create(AParent) do
        begin
          Anchors := [akTop, akRight];
          AnchorSideRight.Control := AParent;
          AnchorSideRight.Side := asrBottom;
          AnchorSideTop.Control := AParent.Controls[i];
          AnchorSideTop.Side := asrCenter;
          BorderSpacing.Around := 24;
          Parent := AParent;
          OnChange := onChangeEvent;
          Style := [cbPrettyNames, cbStandardColors, cbExtendedColors, cbCustomColor, cbCustomColors];
          Selected := StringToColor(configList.ValueFromIndex[i]);
          Width := 200;
        end;
    except
      addLog(Format(ERROR_LOAD_CONFIG_FILE, [filename]));
    end;
  finally
    FreeAndNil(configList);
  end;
end;

procedure TfrmSettings.loadLexersList;
var
  filesList: TStringList;
  lexersDir: string;
  i, selectedIndex: integer;

begin
  lexersDir := GetAppConfigDir(False) + DIR_COLOR_SCHEMES + getSelectedColorThemeName() + DirectorySeparator + DIR_COLOR_SCHEMES_LEXERS;

  if not DirectoryExists(lexersDir) then
    Exit;

  selectedIndex := cmbLexers.ItemIndex;
  cmbLexers.Items.Clear;

  filesList := FindAllFiles(lexersDir, '*', False);

  for i := 0 to filesList.Count - 1 do
    cmbLexers.Items.Add(ExtractFileName(filesList[i]));

  FreeAndNil(filesList);

  if selectedIndex >= 0 then
    cmbLexers.ItemIndex := selectedIndex
  else
    updateSelectedLexer();
end;

procedure TfrmSettings.updateSelectedLexer;
begin
  with cmbLexers do
    if not editor.lexerName.IsEmpty then
      ItemIndex := Items.IndexOf(editor.lexerName)
    else
      ItemIndex := 0;
end;

procedure TfrmSettings.loadColorThemes;
var
  searchRec: TSearchRec;
  themeDir, themesDir: string;

begin
  themesDir := GetAppConfigDir(False) + DIR_COLOR_SCHEMES;

  if FindFirst(themesDir + '*', faDirectory, searchRec) = 0 then
  begin
    repeat
      with searchRec do
        if (Attr and faDirectory) <> 0 then
          if (Name <> '.') and (Name <> '..') then
          begin
            themeDir := themesDir + Name + DirectorySeparator;

            if FileExists(themeDir + COLOR_SCHEME_MAIN_FILE) then
              if DirectoryExists(themeDir + DIR_COLOR_SCHEMES_LEXERS) then
                cmbColorTheme.Items.Add(Name);
          end;
    until FindNext(searchRec) <> 0;

    FindClose(searchRec);
  end;
end;

end.
