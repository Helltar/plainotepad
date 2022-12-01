unit uLoggerForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ATButtons;

type

  { TfrmLogger }

  TfrmLogger = class(TForm)
    btnOk: TButton;
    lblLogMessage: TLabel;
    procedure FormShow(Sender: TObject);
  private
    FFontColor: TColor;
  public
    constructor Create(TheOwner: TComponent; const logMessage: string);
    property fontColor: TColor write FFontColor;
  end;

implementation

uses
  uConsts;

{$R *.lfm}

{ TfrmLogger }

procedure TfrmLogger.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  lblLogMessage.Font.Color := FFontColor;
end;

constructor TfrmLogger.Create(TheOwner: TComponent; const logMessage: string);
begin
  inherited Create(TheOwner);
  Caption := APP_NAME;
  lblLogMessage.Caption := logMessage;
end;

end.
