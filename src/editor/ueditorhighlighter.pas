unit uEditorHighlighter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IniFiles, LazFileUtils,
  ATSynEdit, ATSynEdit_Adapter_EControl,
  ec_SyntAnal, ec_proc_lexer, ec_syntax_collection;

type

  { TEditorHighlighter }

  TEditorHighlighter = class
  private
    adapterEControl: TATAdapterEControl;
    FSyntaxManager: TecSyntaxManager;
    synEdit: TATSynEdit;
    currentColorTheme: string;
    function GetLexer: TecSyntAnalyzer;
    function getLexerName(): string;
    procedure SetLexer(AValue: TecSyntAnalyzer);
    procedure setLexerColorsFromFile(const filename: string);
    procedure loadLexers();
  public
    constructor Create(AEditor: TATSynEdit);
    destructor Destroy; override;
    function findAnalyzer(const lexerName: string): TecSyntAnalyzer;
    function getLexersList(): TStringList;
    procedure setColorTheme(const colorThemeName: string);
    procedure setHighlighterByFilename(const filename: string);
    property lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    property lexerName: string read getLexerName;
    property syntaxManager: TecSyntaxManager read FSyntaxManager;
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

  FSyntaxManager := TecSyntaxManager.Create(nil);
  loadLexers();

  adapterEControl := TATAdapterEControl.Create(nil);
  adapterEControl.AddEditor(synEdit);
  adapterEControl.DynamicHiliteEnabled := True;
end;

destructor TEditorHighlighter.Destroy;
begin
  FreeAndNil(adapterEControl);
  FreeAndNil(FSyntaxManager);
  inherited Destroy;
end;

function TEditorHighlighter.findAnalyzer(const lexerName: string): TecSyntAnalyzer;
begin
  Result := FSyntaxManager.FindAnalyzer(lexerName);
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

function TEditorHighlighter.getLexersList: TStringList;
var
  searchRec: TSearchRec;
  dirLexlib: string;

begin
  Result := TStringList.Create;
  dirLexlib := getConfigDir() + DIR_LEXLIB;

  if FindFirst(dirLexlib + '*' + FILE_EXT_SYNT_ANALYZER, faAnyFile, searchRec) = 0 then
  begin
    repeat
      with searchRec do
        if (Attr and faDirectory) = 0 then
          Result.Add(ExtractFileNameWithoutExt(Name));
    until FindNext(searchRec) <> 0;

    FindClose(searchRec);
  end;
end;

procedure TEditorHighlighter.loadLexers;
var
  dirLexlib, filename: string;
  lexList: TStringList;
  i: integer;

begin
  lexList := TStringList.Create;
  lexList := getLexersList();

  dirLexlib := getConfigDir() + DIR_LEXLIB;

  for i := 0 to lexList.Count - 1 do
    try
      filename := dirLexlib + lexList[i] + FILE_EXT_SYNT_ANALYZER;
      FSyntaxManager.AddAnalyzer.LoadFromFile(filename);
    except
      addLog(Format(ERROR_LOAD_LEXLIB, [filename]));
    end;

  FreeAndNil(lexList);

  synEdit.Text := FSyntaxManager.AnalyzerCount.ToString;
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
  adapterEControl.Lexer := Lexer_FindForFilename(FSyntaxManager, fileName);

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
    setLexerColorsFromFile(getConfigDir() + DIR_COLOR_SCHEMES + colorThemeName + FILE_EXT_COLOR_SCHEME);
end;

end.
