unit uLogger;

{$mode ObjFPC}{$H+}

interface

uses
  uLoggerForm;

procedure addLog(const msg: string; const showForm: boolean = True; const isErr: boolean = True);

implementation

procedure addLog(const msg: string; const showForm: boolean; const isErr: boolean);
begin
  if showForm then
    with TfrmLogger.Create(nil, msg) do
      try
        if isErr then
          fontColor := $006C60DF;
        ShowModal;
      finally
        Free;
      end;

  {$IfDef UNIX}
  WriteLn(msg);
  {$EndIf}
end;

end.
