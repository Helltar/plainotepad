unit uColorThemeEditorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniFiles,
  StdCtrls, ExtCtrls, ColorBox, FileUtil, LCLIntf, ActnList,
  ATSynEdit, ec_SyntAnal, ec_syntax_collection,
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
    procedure initLexerColorBoxControls();
    procedure initThemeColorBoxControls();
    procedure loadEditorConfiig();
    procedure loadLexersList();
    procedure updateEditorLexer();
    procedure updateSelectedLexer();
    procedure updateThemesList();
  end;

implementation

uses
  uLogger, uConsts, uMainForm, uSettingsForm, uUtils, uColorThemeNewNameDialog;

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

  updateThemesList();
  loadEditorConfiig();
  initThemeColorBoxControls();
  loadLexersList();
  initLexerColorBoxControls();

  cmbLexers.Enabled := cmbLexers.Items.Count > 0;
  splVert.Left := Width div 2;
  splHoriz.Top := Height div 2;
end;

procedure TfrmColorThemeEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(editor);
end;

procedure TfrmColorThemeEditor.cmbEditColorThemeChange(Sender: TObject);
begin
  initThemeColorBoxControls();
  initLexerColorBoxControls();
end;

procedure TfrmColorThemeEditor.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmColorThemeEditor.actCopyExecute(Sender: TObject);
begin
  with TdlgColorThemeNewName.Create(Self) do
    try
      formCaption := getSelectedColorThemeName();
      colorThemeFilename := getColorThemeFilename();
      ShowModal;
      if copyFileResultOk then
      begin
        updateThemesList();
        cmbEditColorTheme.ItemIndex := cmbEditColorTheme.Items.IndexOf(colorThemeNewName);
      end;
    finally
      Free;
    end;
end;

procedure TfrmColorThemeEditor.actDelExecute(Sender: TObject);
begin
  DeleteFile(getColorThemeFilename());
  updateThemesList();
end;

procedure TfrmColorThemeEditor.actDelUpdate(Sender: TObject);
begin
  actDel.Enabled := (getSelectedColorThemeName() <> COLOR_THEME_CREAM) and (getSelectedColorThemeName() <> COLOR_THEME_DARK);
end;

procedure TfrmColorThemeEditor.cmbLexersChange(Sender: TObject);
begin
  updateEditorLexer();
  initLexerColorBoxControls();
  editor.setColorTheme(getSelectedColorThemeName());
end;

procedure TfrmColorThemeEditor.FormShow(Sender: TObject);
begin
  cmbLexersChange(nil);
end;

procedure TfrmColorThemeEditor.lexerColorBoxChange(Sender: TObject);
var
  analyzer: TecSyntAnalyzer;
  syntItem: TSyntCollectionItem;

begin
  analyzer := editor.editorHighlighter.findAnalyzer(getSelectedLexerName());

  if Assigned(analyzer) then
  begin
    syntItem := analyzer.Formats.ItemByName(TWinControl(Sender).AnchorSideTop.Control.Caption);
    if Assigned(syntItem) then
      analyzer.Formats.Items[syntItem.Index].Font.Color := TColorBox(Sender).Selected;
  end;

  synEdit.Update();
  colorBoxChange(Sender, COLOR_SCHEME_CONFIG_SECTION_LEXER + getSelectedLexerName());
end;

procedure TfrmColorThemeEditor.themeColorBoxChange(Sender: TObject);
begin
  colorBoxChange(Sender, COLOR_SCHEME_CONFIG_SECTION_MAIN);
  editor.setColorTheme(getSelectedColorThemeName());
  synEdit.Update();
end;

procedure TfrmColorThemeEditor.colorBoxChange(Sender: TObject; const iniFileSection: string);
begin
  with TIniFile.Create(getColorThemeFilename()) do
    try
      WriteString(iniFileSection, TLabel(TColorBox(Sender).AnchorSideTop.Control).Caption, ColorToString(TColorBox(Sender).Selected));
    finally
      Free;
    end;
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

procedure TfrmColorThemeEditor.initLexerColorBoxControls;
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
      Style := [cbPrettyNames, cbStandardColors, cbExtendedColors, cbCustomColor, cbCustomColors, cbIncludeNone];
      Width := 200;

      with TIniFile.Create(getColorThemeFilename()) do
        try
          Selected :=
            StringToColor(ReadString(COLOR_SCHEME_CONFIG_SECTION_LEXER + syntAnalyzer.LexerName,
            syntAnalyzer.Formats.Items[i].DisplayName, 'clNone'));
        finally
          Free;
        end;

      OnChange := @lexerColorBoxChange;
    end;
end;

procedure TfrmColorThemeEditor.initThemeColorBoxControls;
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
      Style := [cbPrettyNames, cbStandardColors, cbExtendedColors, cbCustomColor, cbCustomColors];
      OnChange := @themeColorBoxChange;
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

procedure TfrmColorThemeEditor.updateThemesList;
begin
  frmSettings.loadColorThemes();
  cmbEditColorTheme.Items := frmSettings.cmbColorTheme.Items;
  cmbEditColorTheme.ItemIndex := frmSettings.cmbColorTheme.ItemIndex;
  editor.setColorTheme(getSelectedColorThemeName());
  synEdit.Update();
end;

end.
