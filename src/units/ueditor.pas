unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Graphics, Dialogs,
  SynEdit, SynExportHTML,
  uEditorHighlighter;

type

  { TColorTheme }

  TColorTheme = (cream, dark, white);

  { TEditor }

  TEditor = class
  private
    currentColorTheme: TColorTheme;
    currentFileName: string;
    editorHighlighter: TdmHighlighter;
    fFileModified: boolean;
    isHighlighterEnabled: boolean;
    synEdit: TSynEdit;
    procedure setHighlighterByFileExt(fileName: string);
    procedure synEditChange(Sender: TObject);
    procedure updateParentCaption();
    procedure updateParentCaption(addedText: string);
  public
    constructor Create(AOwner: TSynEdit);
    function isHighlighterUsed(): boolean;
    function isNotNewFile(): boolean;
    function openFile(fileName: string): boolean;
    function saveCurrentFile(): boolean;
    function saveFile(fileName: string): boolean;
    procedure enableHighlighter(AEnabled: boolean);
    procedure exportToHtml();
    procedure setColorTheme(colorTheme: TColorTheme);
    property fileModified: boolean read fFileModified;
  end;

implementation

uses
  uLogger, uUtils;

resourcestring
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';

{ TEditor }

constructor TEditor.Create(AOwner: TSynEdit);
begin
  synEdit := AOwner;
  synEdit.OnChange := @synEditChange;
  editorHighlighter := TdmHighlighter.Create(synEdit);
end;

procedure TEditor.synEditChange(Sender: TObject);
begin
  fFileModified := True;

  if isNotNewFile() then
    updateParentCaption(' *');
end;

procedure TEditor.updateParentCaption(addedText: string);
begin
  synEdit.Parent.Caption := ExtractFileName(currentFileName) + addedText;
end;

procedure TEditor.updateParentCaption;
begin
  updateParentCaption('');
end;

function TEditor.isNotNewFile: boolean;
begin
  Result := not currentFileName.IsEmpty;
end;

function TEditor.isHighlighterUsed: boolean;
begin
  Result := synEdit.Highlighter <> nil;
end;

function TEditor.openFile(fileName: string): boolean;
begin
  Result := False;

  if not FileExists(fileName) then
    Exit;

  try
    synEdit.Lines.LoadFromFile(fileName);

    currentFileName := fileName;
    fFileModified := False;
    updateParentCaption();

    if isHighlighterEnabled then
      setHighlighterByFileExt(fileName);

    Result := True;
  except
    addLog(Format(ERROR_OPEN_FILE, [fileName]));
  end;
end;

function TEditor.saveFile(fileName: string): boolean;
begin
  Result := False;

  try
    synEdit.Lines.SaveToFile(fileName);
    fFileModified := False;
    updateParentCaption();
    Result := True;
  except
    addLog(Format(ERROR_SAVE_FILE, [fileName]));
  end;
end;

procedure TEditor.exportToHtml;
var
  saveDialog: TSaveDialog;
  filename: string;

begin
  if not isHighlighterUsed() then
    Exit;

  saveDialog := TSaveDialog.Create(nil);
  saveDialog.InitialDir := GetUserDir;

  if saveDialog.Execute then
  begin
    filename := saveDialog.FileName + '.html';

    with TSynExporterHTML.Create(nil) do
      try
        try
          Title := getAppOriginalFilename() + ' ' + getAppFileVersion() + ': ' + ExtractFileName(saveDialog.FileName);
          Highlighter := synEdit.Highlighter;
          ExportAll(synEdit.Lines);
          SaveToFile(filename);
        except
          addLog(Format(ERROR_SAVE_FILE, [filename]));
        end;
      finally
        Free;
      end;
  end;

  FreeAndNil(saveDialog);
end;

procedure TEditor.enableHighlighter(AEnabled: boolean);
begin
  isHighlighterEnabled := AEnabled;

  if AEnabled then
  begin
    with editorHighlighter do
      case currentColorTheme of
        cream: enableLightTheme();
        dark: enableDarkTheme();
        white: enableLightTheme();
      end;

    setHighlighterByFileExt(currentFileName);
  end
  else
    synEdit.Highlighter := nil;
end;

function TEditor.saveCurrentFile: boolean;
begin
  Result := saveFile(currentFileName);
end;

procedure TEditor.setHighlighterByFileExt(fileName: string);
begin
  if ExtractFileExt(fileName) = '' then
    fileName := ExtractFileName(fileName)
  else
    fileName := ExtractFileExt(fileName);

  with synEdit do
    with editorHighlighter do
      case fileName of
        '.bat', '.cmd': Highlighter := synBatSyn;
        '.css': Highlighter := synCssSyn;
        '.html', '.htm', '.xml': Highlighter := synHTMLSyn;
        '.java', '.kt', '.gradle': Highlighter := synJavaSyn;
        '.js', '.ts', '.json': Highlighter := synJScriptSyn;
        '.php', '.php3', '.phtml', '.inc': Highlighter := synPHPSyn;
        '.py': Highlighter := synPythonSyn;
        '.sh', '.bash', '.bashrc': Highlighter := synUNIXShellScriptSyn;
        '.sql': Highlighter := synSQLSyn;
        else
          Highlighter := nil;
      end;
end;

procedure TEditor.setColorTheme(colorTheme: TColorTheme);
begin
  currentColorTheme := colorTheme;

  case colorTheme of
    cream:
      with synEdit do
      begin
        Color := clCream;
        Font.Color := $00222222;
        Gutter.Color := clCream;
        RightEdgeColor := $00DDDDDD;
        Gutter.Parts[1].MarkupInfo.Foreground := $00DDDDDD; // SynGutterLineNumber
        Gutter.Parts[3].MarkupInfo.Foreground := $00EEEEEE; // SynGutterSeparator
        SelectedColor.Background := $00EEEEEE;
      end;

    white:
      with synEdit do
      begin
        Color := clWhite;
        Font.Color := $00222222;
        Gutter.Color := clWhite;
        RightEdgeColor := clSilver;
        Gutter.Parts[1].MarkupInfo.Foreground := clSilver;
        Gutter.Parts[3].MarkupInfo.Foreground := clSilver;
        SelectedColor.Background := $00EEEEEE;
      end;

    dark:
      with synEdit do
      begin
        Color := $001e1e1e;
        Font.Color := clWhite;
        Gutter.Color := $001e1e1e;
        Gutter.Parts[1].MarkupInfo.Foreground := $00606060;
        Gutter.Parts[3].MarkupInfo.Foreground := $00303030;
        RightEdgeColor := $00303030;
        SelectedColor.Background := clSilver;
      end;
  end;
end;

end.
