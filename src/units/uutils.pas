unit uUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Process, Zipper;

function createDesktopEntry(): boolean;
function getAppPath: string;
function getConfigDir: string;
procedure copyResToDir(const resName: string; const destDir: string);
procedure runProcess(const AExecutable: string; const AParameters: string = '');
procedure unzipArchive(const AFilename: string; const AOutputPath: string);

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
      Add('StartupNotify=true');

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

function getAppPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function getConfigDir: string;
begin
  // todo: getAppConfigDir()
  {$IfDef MSWINDOWS}
  Result := GetUserDir + 'AppData\Local\' + APP_NAME + '\';
  {$Else}
  Result := GetUserDir + '.config/' + APP_NAME.ToLower + '/';
  {$EndIf}
end;

procedure runProcess(const AExecutable: string; const AParameters: string);
begin
  with TProcess.Create(nil) do
    try
      Executable := AExecutable;
      if not AParameters.IsEmpty then
        Parameters.Text := AParameters;
      Execute;
    finally
      Free;
    end;
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

procedure unzipArchive(const AFilename: string; const AOutputPath: string);
begin
  with TUnZipper.Create do
    try
      FileName := AFilename;
      OutputPath := AOutputPath;
      Examine;
      UnZipAllFiles;
    finally
      Free;
    end;
end;

end.
