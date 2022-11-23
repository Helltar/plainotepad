unit uColorThemeEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ColorBox, FileUtil, LCLIntf,
  ATSynEdit, ATSynEdit_Globals, ec_SyntAnal,
  uEditor;

type

  { TfrmColorThemeEditor }

  TfrmColorThemeEditor = class(TForm)
    Button1: TButton;
    synEdit: TATSynEdit;
    cmbEditColorTheme: TComboBox;
    cmbLexers: TComboBox;
    lblLexer: TLabel;
    lblTheme: TLabel;
    sbLexerColors: TScrollBox;
    sbMainColors: TScrollBox;
    splHoriz: TSplitter;
    splVert: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure cmbEditColorThemeChange(Sender: TObject);
    procedure cmbLexersChange(Sender: TObject);
    procedure colorBoxChange(Sender: TObject; const filename: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lexerColorBoxChange(Sender: TObject);
    procedure themeColorBoxChange(Sender: TObject);
  private
    function getColorThemeFilename(): string;
    function getLexerFilename(): string;
    function getSelectedColorThemeName(): string;
    function getSelectedLexerName(): string;
  private
    editor: TEditor;
    procedure initColorEditControls();
    procedure initControls(const filename: string; AParent: TWinControl; onChangeEvent: TNotifyEvent);
    procedure initThemeEditControls();
    procedure loadEditorConfiig();
    procedure loadLexersList();
    procedure updateControls();
    procedure updateSelectedLexer();
    procedure updateEditorLexer();
  end;

implementation

uses
  uLogger, uConsts, uMainForm, uSettingsForm;

resourcestring
  ERROR_LOAD_CONFIG_FILE = 'Error when load a color theme config file: %s';
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';

{$R *.lfm}

{ TfrmColorThemeEditor }

procedure TfrmColorThemeEditor.FormCreate(Sender: TObject);
begin
  editor := TEditor.Create(synEdit);

  cmbEditColorTheme.Items := frmSettings.cmbColorTheme.Items;
  cmbEditColorTheme.ItemIndex := frmSettings.cmbColorTheme.ItemIndex;

  updateControls();
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

procedure TfrmColorThemeEditor.Button1Click(Sender: TObject);
begin
  Close;
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

procedure TfrmColorThemeEditor.colorBoxChange(Sender: TObject; const filename: string);
begin
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

  synEdit.Update();
end;

procedure TfrmColorThemeEditor.themeColorBoxChange(Sender: TObject);
begin
  colorBoxChange(Sender, getColorThemeFilename());
end;

procedure TfrmColorThemeEditor.lexerColorBoxChange(Sender: TObject);
begin
  colorBoxChange(Sender, getLexerFilename());
end;

function TfrmColorThemeEditor.getColorThemeFilename: string;
begin
  Result := GetAppConfigDir(False) + DIR_COLOR_SCHEMES + getSelectedColorThemeName() + DirectorySeparator + COLOR_SCHEME_MAIN_FILE;
end;

function TfrmColorThemeEditor.getLexerFilename: string;
begin
  Result := GetAppConfigDir(False) + DIR_COLOR_SCHEMES + getSelectedColorThemeName() +
    DirectorySeparator + DIR_COLOR_SCHEMES_LEXERS + getSelectedLexerName();
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
begin
  initControls(getLexerFilename(), sbLexerColors, @lexerColorBoxChange);
end;

procedure TfrmColorThemeEditor.initControls(const filename: string; AParent: TWinControl; onChangeEvent: TNotifyEvent);
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
          AutoSize := True;
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

procedure TfrmColorThemeEditor.initThemeEditControls;
begin
  initControls(getColorThemeFilename(), sbMainColors, @themeColorBoxChange);
end;

procedure TfrmColorThemeEditor.loadEditorConfiig;
begin
  frmMain.loadEditorConfig(synEdit, editor, False);
end;

procedure TfrmColorThemeEditor.loadLexersList;
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
  synEdit.Text := 'empty';

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
