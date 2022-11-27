unit uLogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

procedure addLog(const msg: string; const showForm: boolean = True);

implementation

procedure addLog(const msg: string; const showForm: boolean = True);
begin
  if showForm then
    ShowMessage(msg);

  //WriteLn(msg);
end;

end.
