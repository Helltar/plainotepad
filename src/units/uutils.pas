unit uUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, fileinfo;

function getAppFileVersion(): string;
function getAppInfo(const AType: string): string;
function getAppOriginalFilename(): string;

implementation

uses
  uConsts;

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

end.
