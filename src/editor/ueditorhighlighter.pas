unit uEditorHighlighter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit, ATSynEdit_Adapter_EControl,
  ec_proc_lexer, ec_syntax_collection, ec_SyntAnal;

type

  { TEditorHighlighter }

  TEditorHighlighter = class
  private
    adapterEControl: TATAdapterEControl;
    currentColorTheme: string;
    synEdit: TATSynEdit;
    syntaxManager: TecSyntaxManager;
    function getLexerName(): string;
    procedure setLexerColorsFromFile(const filename: string);
  public
    constructor Create(AEditor: TATSynEdit);
    destructor Destroy; override;
    procedure setColorTheme(const colorThemeName: string);
    procedure setHighlighterByFilename(const filename: string);
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

  lexlibFilename := getAppPath() + 'lib.lxl';

  try
    syntaxManager.LoadFromFile(lexlibFilename);
  except
    addLog(Format(ERROR_LOAD_LEXLIB, [lexlibFilename]));
  end;

  adapterEControl := TATAdapterEControl.Create(nil);
  adapterEControl.AddEditor(synEdit);
  adapterEControl.DynamicHiliteEnabled := True;

  synEdit.AdapterForHilite := adapterEControl;
end;

destructor TEditorHighlighter.Destroy;
begin
  FreeAndNil(adapterEControl);
  FreeAndNil(syntaxManager);
  FreeAndNil(synEdit);
  inherited Destroy;
end;

procedure TEditorHighlighter.setLexerColorsFromFile(const filename: string);
var
  analyzer: TecSyntAnalyzer;
  item: TSyntCollectionItem;
  i, j: integer;

begin
  with TStringList.Create do
    try
      try
        Duplicates := dupIgnore;
        LoadFromFile(filename);

        analyzer := adapterEControl.Lexer;

        if Assigned(analyzer) then
          for i := 0 to analyzer.Formats.Count - 1 do
            for j := 0 to Count - 1 do
            begin
              item := analyzer.Formats.ItemByName(Names[j]);

              if Assigned(item) then
                analyzer.Formats.Items[item.Index].Font.Color := StringToColor(ValueFromIndex[j]);
            end;
      except
        addLog(Format(ERROR_OPEN_COLOR_THEME, [filename]));
      end;
    finally
      Free;
    end;
end;

function TEditorHighlighter.getLexerName: string;
begin
  Result := adapterEControl.GetLexerName;
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
    setLexerColorsFromFile(
      GetAppConfigDir(False) + DIR_COLOR_SCHEMES + colorThemeName + DirectorySeparator + DIR_COLOR_SCHEMES_LEXERS + getLexerName());
end;

end.
