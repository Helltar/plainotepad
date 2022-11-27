unit uUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, fileinfo, LCLType;

function createDesktopEntry(): boolean;
function getAppFileVersion(): string;
function getAppInfo(const AType: string): string;
function getAppOriginalFilename(): string;
function getAppPath: string;
function getConfigDir: string;
procedure copyResToDir(const resName: string; const destDir: string);

implementation

uses
  uConsts, uLogger;

resourcestring
  FILE_CREATION_ERROR = 'File creation error: %s';
  DIR_CREATION_ERROR = 'Error when create a directory: %s';

function createDesktopEntry: boolean;
const
  iconName = APP_FILE_NAME + '.svg';

var
  iconPath: string;
  dotDesktopFile: string;

begin
  Result := False;

  iconPath := getConfigDir() + 'icons' + DirectorySeparator;

  with TStringList.Create do
    try
      Add('[Desktop Entry]');
      Add('Type=Application');
      Add('Name=' + APP_NAME);
      Add('Comment=Text Editor');
      Add('Comment[uk]=Текстовий редактор');
      Add('Comment[ru]=Текстовый редактор');
      Add('Icon=' + iconPath + iconName);
      Add('MimeType=text/plain;');
      Add('Exec=' + getAppPath() + APP_FILE_NAME);
      Add('Terminal=false');
      Add('Categories=Qt;Utility;TextEditor;');
      Add('StartupWMClass=' + APP_FILE_NAME);

      try
        copyResToDir(iconName, iconPath);
        dotDesktopFile := GetUserDir + '.local/share/applications/' + APP_FILE_NAME + '.desktop';
        SaveToFile(dotDesktopFile);
        Result := True;
      except
        addLog(Format(FILE_CREATION_ERROR, [dotDesktopFile]));
      end;
    finally
      Free;
    end;
end;

function getAppFileVersion: string;
begin
  Result := getAppInfo('FileVersion');
end;

function getAppInfo(const AType: string): string;
begin
  Result := APP_NAME;

  with TFileVersionInfo.Create(nil) do
    try
      ReadFileInfo;
      Result := VersionStrings.Values[AType];
    finally
      Free;
    end;
end;

function getAppOriginalFilename: string;
begin
  Result := getAppInfo('OriginalFilename');
end;

function getAppPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function getConfigDir: string;
begin
  Result := getAppConfigDir(False);
end;

procedure copyResToDir(const resName: string; const destDir: string);
var
  resStream: TResourceStream;
  fileStream: TFileStream;

begin
  if not DirectoryExists(destDir) then
    if not CreateDir(destDir) then
    begin
      addLog(Format(DIR_CREATION_ERROR, [destDir]));
      Exit;
    end;

  try
    resStream := TResourceStream.Create(HInstance, UpperCase(resName), RT_RCDATA);
    fileStream := TFileStream.Create(destDir + resName.ToLower, fmCreate);
    try
      fileStream.CopyFrom(resStream, resStream.Size);
    finally
      FreeAndNil(fileStream);
    end;
  finally
    FreeAndNil(resStream);
  end;
end;

end.
