unit uLogger;

{$mode ObjFPC}{$H+}

interface

uses
  uLoggerForm;

procedure addLog(const msg: string; const showForm: boolean = True);

implementation

procedure addLog(const msg: string; const showForm: boolean = True);
begin
  if showForm then
    with TfrmLogger.Create(nil, msg) do
      try
        ShowModal;
      finally
        Free;
      end;

  {$IfDef UNIX}
  WriteLn(msg);
  {$EndIf}
end;

end.
