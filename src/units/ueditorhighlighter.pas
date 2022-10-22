unit uEditorHighlighter;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Graphics,
  SynHighlighterJScript, SynHighlighterHTML, SynHighlighterCss,
  SynHighlighterPHP, SynHighlighterPython, SynHighlighterBat,
  synhighlighterunixshellscript, SynHighlighterJava, SynHighlighterSQL,
  uEditor;

type

  { TEditorHighlighter }

  TEditorHighlighter = class
  private
    commentColor: TColor;
    cssMeasurementUnitColor: TColor;
    htmlValueColor: TColor;
    keyColor: TColor;
    numberColor: TColor;
    pythonFloatColor: TColor;
    pythonNonKeyColor: TColor;
    shellVarColor: TColor;
    stringColor: TColor;
    symbolColor: TColor;
  private
    FSynBat: TSynBatSyn;
    FSynCss: TSynCssSyn;
    FSynHtml: TSynHTMLSyn;
    FSynJava: TSynJavaSyn;
    FSynJs: TSynJScriptSyn;
    FSynPhp: TSynPHPSyn;
    FSynPython: TSynPythonSyn;
    FSynShell: TSynUNIXShellScriptSyn;
    FSynSql: TSynSQLSyn;
  public
    constructor Create(colorTheme: TColorTheme);
    destructor Destroy; override;
  public
    procedure enableDarkTheme();
    procedure enableLightTheme();
  public
    property batHighlighter: TSynBatSyn read FSynBat;
    property cssHighlighter: TSynCssSyn read FSynCss;
    property htmlHighlighter: TSynHTMLSyn read FSynHtml;
    property javaHighlighter: TSynJavaSyn read FSynJava;
    property jsHighlighter: TSynJScriptSyn read FSynJs;
    property phpHighlighter: TSynPHPSyn read FSynPhp;
    property pythonHighlighter: TSynPythonSyn read FSynPython;
    property shellScriptHighlighter: TSynUNIXShellScriptSyn read FSynShell;
    property sqlHighlighter: TSynSQLSyn read FSynSql;
  end;

implementation

{ TEditorHighlighter }

constructor TEditorHighlighter.Create(colorTheme: TColorTheme);
begin
  case colorTheme of
    default: enableLightTheme();
    dark: enableDarkTheme();
    white: enableLightTheme();
  end;

  FSynBat := TSynBatSyn.Create(nil);

  with FSynBat do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
  end;

  FSynCss := TSynCssSyn.Create(nil);

  with FSynCss do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    SelectorAttri.Style := [fsBold];
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
    MeasurementUnitAttri.Foreground := cssMeasurementUnitColor;
  end;

  FSynHtml := TSynHTMLSyn.Create(nil);

  with FSynHtml do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    SymbolAttri.Foreground := symbolColor;
    ValueAttri.Foreground := htmlValueColor;
  end;

  FSynJava := TSynJavaSyn.Create(nil);

  with FSynJava do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  FSynJs := TSynJScriptSyn.Create(nil);

  with FSynJs do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  FSynPhp := TSynPHPSyn.Create(nil);

  with FSynPhp do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  FSynPython := TSynPythonSyn.Create(nil);

  with FSynPython do
  begin
    CommentAttri.Foreground := commentColor;
    FloatAttri.Foreground := pythonFloatColor;
    KeyAttri.Foreground := keyColor;
    NonKeyAttri.Foreground := pythonNonKeyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  FSynShell := TSynUNIXShellScriptSyn.Create(nil);

  with FSynShell do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
    VarAttri.Foreground := shellVarColor;
  end;

  FSynSql := TSynSQLSyn.Create(nil);

  with FSynSql do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;
end;

destructor TEditorHighlighter.Destroy;
begin
  FreeAndNil(FSynBat);
  FreeAndNil(FSynCss);
  FreeAndNil(FSynHtml);
  FreeAndNil(FSynJava);
  FreeAndNil(FSynJs);
  FreeAndNil(FSynPhp);
  FreeAndNil(FSynPython);
  FreeAndNil(FSynShell);
  FreeAndNil(FSynSql);
  inherited Destroy;
end;

procedure TEditorHighlighter.enableLightTheme;
begin
  commentColor := clGray;
  cssMeasurementUnitColor := clMaroon;
  htmlValueColor := clGreen;
  keyColor := clTeal;
  numberColor := clRed;
  stringColor := clGreen;
  shellVarColor := clMaroon;
  symbolColor := clBlack;
  pythonFloatColor := clMaroon;
end;

procedure TEditorHighlighter.enableDarkTheme;
begin
  commentColor := clSilver;
  cssMeasurementUnitColor := clRed;
  htmlValueColor := clGreen;
  keyColor := $00D59B55;
  numberColor := clYellow;
  stringColor := clGreen;
  symbolColor := clWhite;
  shellVarColor := clSkyBlue;
  pythonNonKeyColor := clSkyBlue;
  pythonFloatColor := clSkyBlue;
end;

end.
