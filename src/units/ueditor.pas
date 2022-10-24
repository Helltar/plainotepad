unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  SynEdit, SynExportHTML,
  uEditorHighlighter;

type

  TColorTheme = (default, dark, white);

  { TEditor }

  TEditor = class
  private
    editorHighlighter: TdmHighlighter;
    synEdit: TSynEdit;
    currentFileName: string;
    fFileModified: boolean;
    procedure setHighlighterByFileExt(fileExt: string);
    procedure synEditChange(Sender: TObject);
    procedure updateParentCaption();
    procedure updateParentCaption(addedText: string);
  public
    constructor Create(AOwner: TSynEdit);
  public
    function isNotNewFile(): boolean;
    function isHighlighterUsed(): boolean;
    function openFile(fileName: string): boolean;
    function saveCurrentFile(): boolean;
    function saveFile(fileName: string): boolean;
    procedure enableHighlighter(darkTheme: boolean);
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
end;

procedure TEditor.synEditChange(Sender: TObject);
begin
  fFileModified := True;

  if isNotNewFile() then
    updateParentCaption('*');
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
  Result := Assigned(synEdit.Highlighter);
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

    if Assigned(editorHighlighter) then
      if ExtractFileExt(fileName) = '' then
        setHighlighterByFileExt(ExtractFileName(fileName))
      else
        setHighlighterByFileExt(ExtractFileExt(fileName));

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

function TEditor.saveCurrentFile: boolean;
begin
  Result := saveFile(currentFileName);
end;

procedure TEditor.setHighlighterByFileExt(fileExt: string);
begin
  with synEdit do
    with editorHighlighter do
      case fileExt of
        '.bat', '.cmd': Highlighter := synBatSyn;
        '.css': Highlighter := synCssSyn;
        '.html', '.htm', '.xml': Highlighter := synHTMLSyn;
        '.java', '.kt': Highlighter := synJavaSyn;
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
  case colorTheme of
    white:
    begin
      synEdit.Color := clWhite;
      synEdit.Gutter.Color := clWhite;
      synEdit.RightEdgeColor := clSilver;
      synEdit.Gutter.Parts[3].MarkupInfo.Foreground := clSilver; // SynGutterSeparator
    end;

    dark:
    begin
      synEdit.Color := $001e1e1e;
      synEdit.Font.Color := clWhite;
      synEdit.Gutter.Color := $001e1e1e;
      synEdit.Gutter.Parts[3].MarkupInfo.Foreground := clHighlight;
    end;
  end;
end;

procedure TEditor.enableHighlighter(darkTheme: boolean);
begin
  editorHighlighter := TdmHighlighter.Create(synEdit);

  if darkTheme then
    editorHighlighter.enableDarkTheme();
end;

end.
