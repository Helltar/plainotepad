unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  ATSynEdit,
  uEditorHighlighter;

type

  { TEditor }

  TEditor = class
  private
    synEdit: TATSynEdit;
    FFileModified: boolean;
    FLexerName: string;
    procedure addTextToParentCaption(AText: string);
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
    procedure setColorTheme(const colorThemeName: string);
    property fileModified: boolean read FFileModified;
    property lexerName: string read FLexerName;
  end;

implementation

uses
  uLogger, uConsts;

resourcestring
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';
  ERROR_OPEN_COLOR_THEME = 'Error when opening a color theme: %s';
  TITLE_READONLY = '%s - ReadOnly';

{ TEditor }

constructor TEditor.Create(AOwner: TATSynEdit);
begin
  synEdit := AOwner;
  synEdit.OnChange := @synEditChange;
  synEdit.Colors.TextSelFont := clBlack;

  editorHighlighter := TEditorHighlighter.Create(synEdit);
end;

destructor TEditor.Destroy;
begin
  FreeAndNil(editorHighlighter);
  inherited Destroy;
end;

procedure TEditor.synEditChange(Sender: TObject);
begin
  FFileModified := True;

  if isNotNewFile() then
    addTextToParentCaption('*');
end;

procedure TEditor.addTextToParentCaption(AText: string);
begin
  if not AText.IsEmpty then
    AText := ' ' + AText;

  synEdit.Parent.Caption := ExtractFileName(getCurrentFilename()) + AText;
end;

procedure TEditor.setTextToParentCaption(const AText: string);
begin
  synEdit.Parent.Caption := AText;
end;

procedure TEditor.updateParentCaption;
begin
  addTextToParentCaption('');
end;

function TEditor.isNotNewFile: boolean;
begin
  Result := not getCurrentFilename().IsEmpty;
end;

function TEditor.openFile(const filename: string): boolean;
var
  errMsg: string;

begin
  Result := False;

  if not FileExists(filename) then
    Exit;

  try
    synEdit.LoadFromFile(filename);

    editorHighlighter.setHighlighterByFilename(filename);
    FLexerName := editorHighlighter.lexerName;
    FFileModified := False;

    if not FileIsReadOnly(filename) then
      updateParentCaption()
    else
      setTextToParentCaption(Format(TITLE_READONLY, [ExtractFileName(filename)]));

    Result := True;
  except
    setTextToParentCaption(APP_NAME);
    errMsg := Format(ERROR_OPEN_FILE, [filename]);
    synEdit.Text := errMsg;
    addLog(errMsg, False);
  end;
end;

function TEditor.saveFile(const filename: string): boolean;
begin
  Result := False;

  try
    synEdit.SaveToFile(filename);

    FFileModified := False;
    synEdit.Update();
    updateParentCaption();

    Result := True;
  except
    addLog(Format(ERROR_SAVE_FILE, [filename]));
  end;
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
  filename: string;

begin
  filename :=
    GetAppConfigDir(False) + DIR_COLOR_SCHEMES + colorThemeName + DirectorySeparator + COLOR_SCHEME_MAIN_FILE;

  with TStringList.Create do
    try
      try
        LoadFromFile(filename);

        with synEdit.Colors do
        begin
          GutterBG := StringToColor(Values['GutterBG']);
          GutterCaretBG := StringToColor(Values['GutterCaretBG']);
          GutterFoldBG := StringToColor(Values['GutterFoldBG']);
          GutterFont := StringToColor(Values['GutterFont']);
          MarginRight := StringToColor(Values['MarginRight']);
          MinimapBorder := StringToColor(Values['MinimapBorder']);
          MinimapTooltipBG := StringToColor(Values['MinimapTooltipBG']);
          MinimapTooltipBorder := StringToColor(Values['MinimapTooltipBorder']);
          TextBG := StringToColor(Values['TextBG']);
          TextFont := StringToColor(Values['TextFont']);
          TextSelBG := StringToColor(Values['TextSelBG']);
        end;

        editorHighlighter.setColorTheme(colorThemeName);
      except
        addLog(Format(ERROR_OPEN_COLOR_THEME, [filename]));
      end;
    finally
      Free;
    end;
end;

end.
