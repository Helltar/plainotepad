unit uEditorHighlighter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IniFiles,
  ATSynEdit, ATSynEdit_Adapter_EControl,
  ec_SyntAnal, ec_proc_lexer, ec_syntax_collection;

type

  { TEditorHighlighter }

  TEditorHighlighter = class
  private
    adapterEControl: TATAdapterEControl;
    syntaxManager: TecSyntaxManager;
    synEdit: TATSynEdit;
    currentColorTheme: string;
    function GetLexer: TecSyntAnalyzer;
    function getLexerName(): string;
    procedure SetLexer(AValue: TecSyntAnalyzer);
    procedure setLexerColorsFromFile(const filename: string);
    procedure getLexersList();
  public
    constructor Create(AEditor: TATSynEdit);
    destructor Destroy; override;
  public
    function findAnalyzer(const lexerName: string): TecSyntAnalyzer;
  public
    procedure setColorTheme(const colorThemeName: string);
    procedure setHighlighterByFilename(const filename: string);
  public
    property lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    property lexerName: string read getLexerName;
  end;

implementation

uses
  uUtils, uConsts, uLogger;

resourcestring
  ERROR_OPEN_COLOR_THEME = 'Error when opening a color theme: %s';
  ERROR_LOAD_LEXLIB = 'Failed to load lexlib: %s';

{ TEditorHighlighter }

constructor TEditorHighlighter.Create(AEditor: TATSynEdit);
var
  lexlibFilename: string;

begin
  synEdit := AEditor;

  syntaxManager := TecSyntaxManager.Create(nil);

  lexlibFilename := getConfigDir() + LEXLIB_FILE_NAME;

  try
    syntaxManager.LoadFromFile(lexlibFilename);
  except
    addLog(Format(ERROR_LOAD_LEXLIB, [lexlibFilename]));
  end;

  adapterEControl := TATAdapterEControl.Create(nil);
  adapterEControl.AddEditor(synEdit);
  adapterEControl.DynamicHiliteEnabled := True;

  //getLexersList();
end;

destructor TEditorHighlighter.Destroy;
begin
  FreeAndNil(adapterEControl);
  FreeAndNil(syntaxManager);
  inherited Destroy;
end;

function TEditorHighlighter.findAnalyzer(const lexerName: string): TecSyntAnalyzer;
begin
  Result := syntaxManager.FindAnalyzer(lexerName);
end;

procedure TEditorHighlighter.setLexerColorsFromFile(const filename: string);
var
  analyzer: TecSyntAnalyzer;
  i, j: integer;
  iniFile: TIniFile;
  item: TSyntCollectionItem;
  list: TStringList;

begin
  iniFile := TIniFile.Create(filename);
  list := TStringList.Create;

  analyzer := adapterEControl.Lexer;

  if Assigned(analyzer) then
  begin
    iniFile.ReadSectionRaw(COLOR_SCHEME_CONFIG_SECTION_LEXER + analyzer.LexerName, list);

    for i := 0 to analyzer.Formats.Count - 1 do
      for j := 0 to list.Count - 1 do
      begin
        item := analyzer.Formats.ItemByName(list.Names[j]);

        if Assigned(item) then
          analyzer.Formats.Items[item.Index].Font.Color := StringToColor(list.ValueFromIndex[j]);
      end;
  end;

  FreeAndNil(list);
  FreeAndNil(iniFile);
end;

procedure TEditorHighlighter.getLexersList;
var
  iniFile: TIniFile;
  i, j: integer;

begin
  iniFile := TIniFile.Create(getConfigDir() + DIR_COLOR_SCHEMES + 'test' + COLOR_SCHEME_CONFIG_FILE_EXT);

  with synEdit.Colors do
    with iniFile do
    begin
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'CurrentLineBG', ColorToString(CurrentLineBG));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'GutterBG', ColorToString(GutterBG));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'GutterCaretBG', ColorToString(GutterCaretBG));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'GutterFoldBG', ColorToString(GutterFoldBG));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'GutterFont', ColorToString(GutterFont));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'MarginRight', ColorToString(MarginRight));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'MinimapBorder', ColorToString(MinimapBorder));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'MinimapTooltipBG', ColorToString(MinimapTooltipBG));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'MinimapTooltipBorder', ColorToString(MinimapTooltipBorder));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'TextBG', ColorToString(TextBG));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'TextFont', ColorToString(TextFont));
      WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, 'TextSelBG', ColorToString(TextSelBG));
    end;

  for i := 0 to syntaxManager.AnalyzerCount - 1 do
  begin
    for j := 0 to syntaxManager.Analyzers[i].Formats.Count - 1 do
      iniFile.WriteString(
        COLOR_SCHEME_CONFIG_SECTION_LEXER + syntaxManager.Analyzers[i].LexerName,
        syntaxManager.Analyzers[i].Formats.Items[j].DisplayName,
        ColorToString(syntaxManager.Analyzers[i].Formats.Items[j].Font.Color));
  end;

  FreeAndNil(iniFile);
end;

function TEditorHighlighter.getLexerName: string;
begin
  Result := adapterEControl.GetLexerName;
end;

function TEditorHighlighter.GetLexer: TecSyntAnalyzer;
begin
  Result := adapterEControl.Lexer;
end;

procedure TEditorHighlighter.SetLexer(AValue: TecSyntAnalyzer);
begin
  adapterEControl.Lexer := AValue;
end;

procedure TEditorHighlighter.setHighlighterByFilename(const filename: string);
begin
  adapterEControl.Lexer := nil;
  adapterEControl.Lexer := Lexer_FindForFilename(syntaxManager, fileName);

  if Assigned(adapterEControl.Lexer) then
  begin
    setColorTheme(currentColorTheme);
    synEdit.DoEventChange(0);
    synEdit.Update();
  end;
end;

procedure TEditorHighlighter.setColorTheme(const colorThemeName: string);
begin
  currentColorTheme := colorThemeName;

  if getLexerName() <> '-' then
    setLexerColorsFromFile(getConfigDir() + DIR_COLOR_SCHEMES + colorThemeName + COLOR_SCHEME_CONFIG_FILE_EXT);
end;

end.
