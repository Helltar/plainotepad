unit uColorThemeEditorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniFiles,
  StdCtrls, ExtCtrls, ColorBox, FileUtil, LCLIntf, ActnList,
  ATSynEdit, ec_SyntAnal,
  uEditor;

type

  { TfrmColorThemeEditor }

  TfrmColorThemeEditor = class(TForm)
    actCopy: TAction;
    actDel: TAction;
    actlMain: TActionList;
    btnDelete: TButton;
    btnOk: TButton;
    btnCopy: TButton;
    synEdit: TATSynEdit;
    cmbEditColorTheme: TComboBox;
    cmbLexers: TComboBox;
    lblLexer: TLabel;
    lblTheme: TLabel;
    sbLexerColors: TScrollBox;
    sbMainColors: TScrollBox;
    splHoriz: TSplitter;
    splVert: TSplitter;
    procedure actCopyExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actDelUpdate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cmbEditColorThemeChange(Sender: TObject);
    procedure cmbLexersChange(Sender: TObject);
    procedure colorBoxChange(Sender: TObject; const iniFileSection: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lexerColorBoxChange(Sender: TObject);
    procedure themeColorBoxChange(Sender: TObject);
  private
    editor: TEditor;
    function getColorThemeFilename(): string;
    function getSelectedColorThemeName(): string;
    function getSelectedLexerName(): string;
    procedure initColorEditControls();
    procedure initThemeEditControls();
    procedure loadEditorConfiig();
    procedure loadLexersList();
    procedure updateControls();
    procedure updateSelectedLexer();
    procedure updateEditorLexer();
  end;

implementation

uses
  uLogger, uConsts, uMainForm, uSettingsForm, uUtils;

resourcestring
  COLOR_THEME_ALREADY_EXISTS = 'A color theme with this name already exists';
  ERROR_COPY_FILE = 'Error when copy a file: %s';
  ERROR_LOAD_CONFIG_FILE = 'Error when load a color theme config file: %s';
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';
  TITLE_TYPE_NEW_NAME = 'Type in a new name';

{$R *.lfm}

{ TfrmColorThemeEditor }

procedure TfrmColorThemeEditor.FormCreate(Sender: TObject);
begin
  if (Screen.Height + Screen.Width) > 1400 then
  begin
    Height := Screen.Height - 200;
    Width := Screen.Width - 200;
  end;

  editor := TEditor.Create(synEdit);

  cmbEditColorTheme.Items := frmSettings.cmbColorTheme.Items;
  cmbEditColorTheme.ItemIndex := frmSettings.cmbColorTheme.ItemIndex;

  updateControls();

  cmbLexers.Enabled := cmbLexers.Items.Count > 0;
end;

procedure TfrmColorThemeEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(editor);
end;

procedure TfrmColorThemeEditor.cmbEditColorThemeChange(Sender: TObject);
begin
  updateControls();
  synEdit.Update;
end;

procedure TfrmColorThemeEditor.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmColorThemeEditor.actCopyExecute(Sender: TObject);
var
  newName, filename: string;

begin
  if InputQuery(getSelectedColorThemeName(), TITLE_TYPE_NEW_NAME, False, newName) then
  begin
    if newName.IsEmpty then
      Exit;

    filename := getConfigDir() + DIR_COLOR_SCHEMES + newName + FILE_EXT_COLOR_SCHEME;

    if not FileExists(filename) then
    begin
      if CopyFile(getColorThemeFilename(), filename) then
      begin
        updateControls();
        synEdit.Update;
        frmSettings.loadColorThemes();
        cmbEditColorTheme.Items := frmSettings.cmbColorTheme.Items;
        cmbEditColorTheme.ItemIndex := cmbEditColorTheme.Items.IndexOf(newName);
      end
      else
        addLog(Format(ERROR_COPY_FILE, [filename]));
    end
    else
      addLog(COLOR_THEME_ALREADY_EXISTS);
  end;
end;

procedure TfrmColorThemeEditor.actDelExecute(Sender: TObject);
begin
  DeleteFile(getColorThemeFilename());
  frmSettings.loadColorThemes();
  cmbEditColorTheme.Items := frmSettings.cmbColorTheme.Items;
  cmbEditColorTheme.ItemIndex := 0;
  updateControls();
  synEdit.Update;
end;

procedure TfrmColorThemeEditor.actDelUpdate(Sender: TObject);
begin
  actDel.Enabled := (getSelectedColorThemeName() <> COLOR_THEME_CREAM) and (getSelectedColorThemeName() <> COLOR_THEME_DARK);
end;

procedure TfrmColorThemeEditor.cmbLexersChange(Sender: TObject);
begin
  updateEditorLexer();
  initColorEditControls();
end;

procedure TfrmColorThemeEditor.FormShow(Sender: TObject);
begin
  updateSelectedLexer();
end;

procedure TfrmColorThemeEditor.lexerColorBoxChange(Sender: TObject);
begin
  colorBoxChange(Sender, COLOR_SCHEME_CONFIG_SECTION_LEXER + getSelectedLexerName());
end;

procedure TfrmColorThemeEditor.themeColorBoxChange(Sender: TObject);
begin
  colorBoxChange(Sender, COLOR_SCHEME_CONFIG_SECTION_MAIN);
end;

procedure TfrmColorThemeEditor.colorBoxChange(Sender: TObject; const iniFileSection: string);
begin
  with TIniFile.Create(getColorThemeFilename()) do
    try
      WriteString(iniFileSection, TLabel(TColorBox(Sender).AnchorSideTop.Control).Caption, ColorToString(TColorBox(Sender).Selected));
    finally
      Free;
    end;

  editor.setColorTheme(getSelectedColorThemeName());
  synEdit.Update();
end;

function TfrmColorThemeEditor.getColorThemeFilename: string;
begin
  Result := getConfigDir() + DIR_COLOR_SCHEMES + getSelectedColorThemeName() + FILE_EXT_COLOR_SCHEME;
end;

function TfrmColorThemeEditor.getSelectedColorThemeName: string;
begin
  Result := 'null';

  with cmbEditColorTheme do
    if Items.Count > 0 then
      if ItemIndex >= 0 then
        Result := Items[ItemIndex];
end;

function TfrmColorThemeEditor.getSelectedLexerName: string;
begin
  Result := 'null';

  with cmbLexers do
    if Items.Count > 0 then
      if ItemIndex >= 0 then
        Result := Items[ItemIndex];
end;

procedure TfrmColorThemeEditor.initColorEditControls;
var
  i: integer;
  syntAnalyzer: TecSyntAnalyzer;

begin
  if sbLexerColors.ControlCount > 0 then
    for i := sbLexerColors.ControlCount - 1 downto 0 do
      sbLexerColors.Controls[i].Free;

  syntAnalyzer := editor.editorHighlighter.findAnalyzer(getSelectedLexerName());

  if not Assigned(syntAnalyzer) then
    Exit;

  for i := 0 to syntAnalyzer.Formats.Count - 1 do
    with TLabel.Create(sbLexerColors) do
    begin
      AutoSize := True;
      Parent := sbLexerColors;
      Caption := syntAnalyzer.Formats.Items[i].DisplayName;

      if i <> 0 then
      begin
        AnchorSideTop.Control := sbLexerColors.Controls[i - 1];
        AnchorSideTop.Side := asrBottom;
      end
      else
        AnchorSideTop.Control := sbLexerColors;

      AnchorSideLeft.Control := sbLexerColors;
      BorderSpacing.Around := 24;
    end;

  for i := 0 to syntAnalyzer.Formats.Count - 1 do
    with TColorBox.Create(sbLexerColors) do
    begin
      Anchors := [akTop, akRight];
      AnchorSideRight.Control := sbLexerColors;
      AnchorSideRight.Side := asrBottom;
      AnchorSideTop.Control := sbLexerColors.Controls[i];
      AnchorSideTop.Side := asrCenter;
      BorderSpacing.Around := 24;
      Parent := sbLexerColors;
      OnChange := @lexerColorBoxChange;
      Style := [cbPrettyNames, cbStandardColors, cbExtendedColors, cbCustomColor, cbCustomColors, cbIncludeNone];

      with TIniFile.Create(getColorThemeFilename()) do
        try
          Selected :=
            StringToColor(ReadString(COLOR_SCHEME_CONFIG_SECTION_LEXER + syntAnalyzer.LexerName,
            syntAnalyzer.Formats.Items[i].DisplayName, 'clNone'));
        finally
          Free;
        end;

      Width := 200;
    end;
end;

procedure TfrmColorThemeEditor.initThemeEditControls;
var
  i: integer;
  sections: TStringList;

begin
  if sbMainColors.ControlCount > 0 then
    for i := sbMainColors.ControlCount - 1 downto 0 do
      sbMainColors.Controls[i].Free;

  sections := TStringList.Create;

  with TIniFile.Create(getColorThemeFilename()) do
    try
      ReadSectionRaw(COLOR_SCHEME_CONFIG_SECTION_MAIN, sections)
    finally
      Free;
    end;

  for i := 0 to sections.Count - 1 do
    with TLabel.Create(sbMainColors) do
    begin
      AutoSize := True;
      Parent := sbMainColors;
      Caption := sections.Names[i];

      if i <> 0 then
      begin
        AnchorSideTop.Control := sbMainColors.Controls[i - 1];
        AnchorSideTop.Side := asrBottom;
      end
      else
        AnchorSideTop.Control := sbMainColors;

      AnchorSideLeft.Control := sbMainColors;
      BorderSpacing.Around := 24;
    end;

  for i := 0 to sections.Count - 1 do
    with TColorBox.Create(sbMainColors) do
    begin
      Anchors := [akTop, akRight];
      AnchorSideRight.Control := sbMainColors;
      AnchorSideRight.Side := asrBottom;
      AnchorSideTop.Control := sbMainColors.Controls[i];
      AnchorSideTop.Side := asrCenter;
      BorderSpacing.Around := 24;
      Parent := sbMainColors;
      OnChange := @themeColorBoxChange;
      Style := [cbPrettyNames, cbStandardColors, cbExtendedColors, cbCustomColor, cbCustomColors];
      Selected := StringToColor(sections.ValueFromIndex[i]);
      Width := 200;
    end;

  FreeAndNil(sections);
end;

procedure TfrmColorThemeEditor.loadEditorConfiig;
begin
  frmMain.loadEditorConfig(synEdit, editor, False);
end;

procedure TfrmColorThemeEditor.loadLexersList;
var
  selectedIndex: integer;
  lexList: TStringList;
  lexName: string;

begin
  selectedIndex := cmbLexers.ItemIndex;
  cmbLexers.Items.Clear;

  lexList := TStringList.Create;
  editor.editorHighlighter.getLexersList(lexList);

  for lexName in lexList do
    cmbLexers.Items.Add(lexName);

  FreeAndNil(lexList);

  if selectedIndex >= 0 then
    cmbLexers.ItemIndex := selectedIndex
  else
    updateSelectedLexer();
end;

procedure TfrmColorThemeEditor.updateSelectedLexer;
begin
  with cmbLexers do
    if not frmMain.editor.lexerName.IsEmpty then
      ItemIndex := Items.IndexOf(frmMain.editor.lexerName)
    else
      ItemIndex := 0;

  updateEditorLexer();
end;

procedure TfrmColorThemeEditor.updateEditorLexer;
var
  lexer: TecSyntAnalyzer;

begin
  lexer := nil;
  editor.editorHighlighter.lexer := nil;
  synEdit.Text := 'SampleText';

  lexer := editor.editorHighlighter.findAnalyzer(getSelectedLexerName());

  if Assigned(lexer) then
  begin
    editor.editorHighlighter.lexer := lexer;

    if Assigned(lexer.SampleText) then
      synEdit.Text := lexer.SampleText.Text;
  end;
end;

procedure TfrmColorThemeEditor.updateControls;
begin
  loadEditorConfiig();
  initThemeEditControls();
  loadLexersList();
  initColorEditControls();
end;

end.
