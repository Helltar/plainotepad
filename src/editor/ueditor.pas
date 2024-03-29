unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, IniFiles, Forms,
  ATSynEdit, ATStrings,
  uEditorHighlighter;

type

  { TEditor }

  TEditor = class
  private
    synEdit: TATSynEdit;
    FLexerName: string;
    FHighlighter: boolean;
    function getColorFromConfigSectionMain(iniFile: TIniFile; const AIdent: string; const ADefault: string): TColor;
    procedure addTextToParentCaption(AText: string);
    function GetFileModified: boolean;
    procedure SetFileModified(AValue: boolean);
    procedure SetHighlighter(AValue: boolean);
    procedure setTextToParentCaption(const AText: string);
    procedure synEditChange(Sender: TObject);
    procedure updateParentCaption();
  public
    editorHighlighter: TEditorHighlighter;
    constructor Create(AOwner: TATSynEdit);
    destructor Destroy; override;
    function getCurrentFilename(): string;
    function isNotNewFile(): boolean;
    function openFile(const filename: string): boolean;
    function saveCurrentFile(): boolean;
    function saveFile(const filename: string): boolean;
    procedure closeFile();
    procedure setColorTheme(const colorThemeName: string);
    property fileModified: boolean read GetFileModified write SetFileModified;
    property lexerName: string read FLexerName;
    property highlighter: boolean read FHighlighter write setHighlighter;
  end;

implementation

uses
  uLogger, uConsts, uUtils;

resourcestring
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';
  TITLE_READONLY = '%s - ReadOnly';

{ TEditor }

constructor TEditor.Create(AOwner: TATSynEdit);
begin
  synEdit := AOwner;
  synEdit.OnChange := @synEditChange;
  synEdit.Colors.TextSelFont := clBlack;
  synEdit.Modified := False;

  editorHighlighter := TEditorHighlighter.Create(synEdit);
end;

destructor TEditor.Destroy;
begin
  FreeAndNil(editorHighlighter);
  inherited Destroy;
end;

procedure TEditor.synEditChange(Sender: TObject);
begin
  if isNotNewFile() then
    addTextToParentCaption('*');
end;

function TEditor.getColorFromConfigSectionMain(iniFile: TIniFile; const AIdent: string; const ADefault: string): TColor;
begin
  if not iniFile.ValueExists(COLOR_SCHEME_CONFIG_SECTION_MAIN, AIdent) then
    iniFile.WriteString(COLOR_SCHEME_CONFIG_SECTION_MAIN, AIdent, ADefault);

  Result := StringToColor(iniFile.ReadString(COLOR_SCHEME_CONFIG_SECTION_MAIN, AIdent, ADefault));
end;

procedure TEditor.addTextToParentCaption(AText: string);
begin
  if not AText.IsEmpty then
    AText := ' ' + AText;

  // todo: Parent.Parent - synedit on panel, qt5 mainmenu color
  synEdit.Parent.Parent.Caption := ExtractFileName(getCurrentFilename()) + AText;

  {$IfDef MSWINDOWS}
  Application.Title := synEdit.Parent.Parent.Caption;
  {$EndIf}
end;

function TEditor.GetFileModified: boolean;
begin
  Result := synEdit.Modified;
end;

procedure TEditor.SetFileModified(AValue: boolean);
begin
  synEdit.Modified := AValue;
  addTextToParentCaption('*');
end;

procedure TEditor.SetHighlighter(AValue: boolean);
begin
  FHighlighter := AValue;

  if not AValue then
    editorHighlighter.lexer := nil;
end;

procedure TEditor.setTextToParentCaption(const AText: string);
begin
  // todo: Parent.Parent - synedit on panel, qt5 mainmenu color
  synEdit.Parent.Parent.Caption := AText;
end;

procedure TEditor.updateParentCaption;
begin
  addTextToParentCaption('');
end;

function TEditor.isNotNewFile: boolean;
begin
  Result := not synEdit.FileName.IsEmpty;
end;

function TEditor.openFile(const filename: string): boolean;
begin
  Result := False;

  if not FileExists(filename) then
    Exit;

  try
    synEdit.LoadFromFile(filename, []);
  except
    closeFile();
    addLog(Format(ERROR_OPEN_FILE, [filename]));
    Exit;
  end;

  if FHighlighter then
    editorHighlighter.setHighlighterByFilename(filename);

  FLexerName := editorHighlighter.lexerName;

  if not FileIsReadOnly(filename) then
    updateParentCaption()
  else
    setTextToParentCaption(Format(TITLE_READONLY, [ExtractFileName(filename)]));

  Result := True;
end;

function TEditor.saveFile(const filename: string): boolean;
begin
  Result := False;

  try
    synEdit.SaveToFile(filename);

    synEdit.Update();
    updateParentCaption();

    Result := True;
  except
    addLog(Format(ERROR_SAVE_FILE, [filename]));
  end;
end;

procedure TEditor.closeFile;
begin
  editorHighlighter.lexer := nil;
  setTextToParentCaption(APP_NAME);
  synEdit.FileName := '';
  synEdit.Text := '';
  FLexerName := '';
  synEdit.Modified := False;
end;

function TEditor.saveCurrentFile: boolean;
begin
  Result := saveFile(getCurrentFilename());
end;

function TEditor.getCurrentFilename: string;
begin
  Result := synEdit.FileName;
end;

procedure TEditor.setColorTheme(const colorThemeName: string);
var
  iniFile: TIniFile;

begin
  iniFile := TIniFile.Create(getConfigDir() + DIR_COLOR_SCHEMES + colorThemeName + FILE_EXT_COLOR_SCHEME);

  with synEdit.Colors do
  begin
    CurrentLineBG := getColorFromConfigSectionMain(iniFile, 'CurrentLineBG', '$002E2E2E');
    GutterBG := getColorFromConfigSectionMain(iniFile, 'GutterBG', '$001E1E1E');
    GutterCaretBG := getColorFromConfigSectionMain(iniFile, 'GutterCaretBG', '$002E2E2E');
    GutterFoldBG := getColorFromConfigSectionMain(iniFile, 'GutterFoldBG', '$001E1E1E');
    GutterFont := getColorFromConfigSectionMain(iniFile, 'GutterFont', '$00606060');
    MarginRight := getColorFromConfigSectionMain(iniFile, 'MarginRight', '$00303030');
    MinimapBorder := getColorFromConfigSectionMain(iniFile, 'MinimapBorder', '$00303030');
    MinimapTooltipBG := getColorFromConfigSectionMain(iniFile, 'MinimapTooltipBG', '$00333333');
    MinimapTooltipBorder := getColorFromConfigSectionMain(iniFile, 'MinimapTooltipBorder', '$00555555');
    TextBG := getColorFromConfigSectionMain(iniFile, 'TextBG', '$001E1E1E');
    TextFont := getColorFromConfigSectionMain(iniFile, 'TextFont', 'clWhite');
    TextSelBG := getColorFromConfigSectionMain(iniFile, 'TextSelBG', 'clSilver');
    UnprintedBG := getColorFromConfigSectionMain(iniFile, 'UnprintedBG', 'clNone');
    UnprintedFont := getColorFromConfigSectionMain(iniFile, 'UnprintedFont', 'clGray');
    UnprintedHexFont := getColorFromConfigSectionMain(iniFile, 'UnprintedHexFont', 'clGray');
  end;

  FreeAndNil(iniFile);

  if highlighter then
    editorHighlighter.setColorTheme(colorThemeName);
end;

end.
