unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit;

type

  TColorTheme = (default, dark, white);

  { TEditor }

  TEditor = class
  private
    synEdit: TSynEdit;
    currentFileName: string;
    fFileModified: boolean;
    procedure synEditChange(Sender: TObject);
    procedure updateParentCaption();
    procedure updateParentCaption(addedText: string);
    procedure setHighlighterByFileExt(fileExt: string);
  public
    constructor Create(AOwner: TSynEdit);
    destructor Destroy; override;
  public
    function isNotNewFile(): boolean;
    function openFile(fileName: string): boolean;
    function saveCurrentFile(): boolean;
    function saveFile(fileName: string): boolean;
    procedure setColorTheme(colorTheme: TColorTheme);
    procedure setHighlighterColorTheme(colorTheme: TColorTheme);
    property fileModified: boolean read fFileModified;
  end;

implementation

uses
  uEditorHighlighter, uLogger;

resourcestring
  ERROR_OPEN_FILE = 'Error when opening a file: %s';
  ERROR_SAVE_FILE = 'Error when saving the file: %s';

var
  editorHighlighter: TEditorHighlighter;

{ TEditor }

constructor TEditor.Create(AOwner: TSynEdit);
begin
  synEdit := AOwner;
  synEdit.OnChange := @synEditChange;
end;

destructor TEditor.Destroy;
begin
  FreeAndNil(synEdit);

  if Assigned(editorHighlighter) then
    FreeAndNil(editorHighlighter);

  inherited Destroy;
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

function TEditor.saveCurrentFile: boolean;
begin
  Result := False;

  if isNotNewFile() then
    Result := saveFile(currentFileName);
end;

procedure TEditor.setHighlighterByFileExt(fileExt: string);
begin
  if Assigned(editorHighlighter) then
    with synEdit do
      with editorHighlighter do
        case fileExt of
          '.bat', '.cmd': Highlighter := batHighlighter;
          '.css': Highlighter := cssHighlighter;
          '.html', '.htm', '.xml': Highlighter := htmlHighlighter;
          '.java', '.kt': Highlighter := javaHighlighter;
          '.js', '.ts', '.json': Highlighter := jsHighlighter;
          '.php', '.php3', '.phtml', '.inc': Highlighter := phpHighlighter;
          '.py': Highlighter := pythonHighlighter;
          '.sh', '.bash', '.bashrc': Highlighter := shellScriptHighlighter;
          '.sql': Highlighter := sqlHighlighter;
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

procedure TEditor.setHighlighterColorTheme(colorTheme: TColorTheme);
begin
  editorHighlighter := TEditorHighlighter.Create(colorTheme);
end;

end.
