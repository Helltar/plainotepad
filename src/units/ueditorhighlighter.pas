unit uEditorHighlighter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  SynHighlighterJava, SynHighlighterJScript, SynHighlighterHTML,
  synhighlighterunixshellscript, SynHighlighterCss, SynHighlighterPHP,
  SynHighlighterSQL, SynHighlighterPython, SynHighlighterBat;

type

  { TdmHighlighter }

  TdmHighlighter = class(TDataModule)
    synBatSyn: TSynBatSyn;
    synCssSyn: TSynCssSyn;
    synHTMLSyn: TSynHTMLSyn;
    synJavaSyn: TSynJavaSyn;
    synJScriptSyn: TSynJScriptSyn;
    synPHPSyn: TSynPHPSyn;
    synPythonSyn: TSynPythonSyn;
    synSQLSyn: TSynSQLSyn;
    synUNIXShellScriptSyn: TSynUNIXShellScriptSyn;
    procedure DataModuleCreate(Sender: TObject);
  private
    commentColor, cssMeasurementUnitColor, htmlValueColor, keyColor, numberColor,
    pythonFloatColor, pythonNonKeyColor, shellVarColor, stringColor, symbolColor: TColor;
    procedure updateColors();
  public
    procedure enableDarkTheme();
    procedure enableLightTheme();
  end;

implementation

{$R *.lfm}

{ TdmHighlighter }

procedure TdmHighlighter.DataModuleCreate(Sender: TObject);
begin
  enableLightTheme();
end;

procedure TdmHighlighter.updateColors;
begin
  with synBatSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
  end;

  with synCssSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    MeasurementUnitAttri.Foreground := cssMeasurementUnitColor;
    NumberAttri.Foreground := numberColor;
    SelectorAttri.Style := [fsBold];
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  with synHTMLSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    SymbolAttri.Foreground := symbolColor;
    ValueAttri.Foreground := htmlValueColor;
  end;

  with synJavaSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  with synJScriptSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  with synPHPSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;

  with synPythonSyn do
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

  with synUNIXShellScriptSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
    VarAttri.Foreground := shellVarColor;
  end;

  with synSQLSyn do
  begin
    CommentAttri.Foreground := commentColor;
    KeyAttri.Foreground := keyColor;
    NumberAttri.Foreground := numberColor;
    StringAttri.Foreground := stringColor;
    StringAttri.Style := [fsBold];
    SymbolAttri.Foreground := symbolColor;
  end;
end;

procedure TdmHighlighter.enableDarkTheme;
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

  updateColors();
end;

procedure TdmHighlighter.enableLightTheme;
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

  updateColors();
end;

end.
