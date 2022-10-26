unit uUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, fileinfo;

function createDesktopEntry(): boolean;
function getAppFileVersion(): string;
function getAppInfo(const AType: string): string;
function getAppOriginalFilename(): string;
function getAppPath: string;

implementation

uses
  uConsts, uLogger;

resourcestring
  FILE_CREATION_ERROR = 'File creation error: %s';

function createDesktopEntry: boolean;

  procedure saveIcon(const dir, filename: string);
  var
    S: TResourceStream;
    F: TFileStream;

  begin
    S := TResourceStream.Create(HInstance, 'APP_ICON', RT_RCDATA);

    try
      CreateDir(dir);
      F := TFileStream.Create(dir + filename, fmCreate);
      try
        F.CopyFrom(S, S.Size);
      finally
        FreeAndNil(F);
      end;
    finally
      FreeAndNil(S);
    end;
  end;

const
  iconName = APP_FILE_NAME + '.svg';

var
  iconPath: string;
  dotDesktopFile: string;

begin
  Result := False;

  iconPath := GetAppConfigDir(False) + 'icons' + DirectorySeparator;

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
        saveIcon(iconPath, iconName.ToLower);
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

end.
