unit uLogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

procedure addLog(msg: string);

implementation

procedure addLog(msg: string);
begin
  ShowMessage(msg);
  WriteLn(msg);
end;

end.
