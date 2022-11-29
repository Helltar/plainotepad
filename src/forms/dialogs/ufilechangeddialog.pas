unit uFileChangedDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls;

type

  { TDlgResult }

  TDlgResult = (dlgResOk, dlgResNo, dlgResCancel);

  { TdlgFileChanged }

  TdlgFileChanged = class(TForm)
    btnYes: TButton;
    btnNo: TButton;
    btnCancel: TButton;
    lblTitle: TLabel;
    procedure ButtonClick(Sender: TObject);
  private
    FDlgResult: TDlgResult;
  public
    constructor Create(TheOwner: TComponent; const AFileName: string);
    property dlgResult: TDlgResult read FDlgResult;
  end;

implementation

{$R *.lfm}

{ TdlgFileChanged }

procedure TdlgFileChanged.ButtonClick(Sender: TObject);
begin
  case TButton(Sender).Tag of
    0: FDlgResult := dlgResOk;
    1: FDlgResult := dlgResNo;
    2: FDlgResult := dlgResCancel;
  end;

  Close;
end;

constructor TdlgFileChanged.Create(TheOwner: TComponent; const AFileName: string);
begin
  inherited Create(TheOwner);

  FDlgResult := dlgResCancel;

  if not AFileName.IsEmpty then
    lblTitle.Caption := lblTitle.Caption + LineEnding + LineEnding + AFileName;
end;

end.
