unit uFileChangedOnDiskDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TDlgResult }

  TDlgResult = (dlgResReload, dlgResIgnore, dlgResClose);

  { TdlgFileChangedOnDisk }

  TdlgFileChangedOnDisk = class(TForm)
    btnReload: TButton;
    btnIgnore: TButton;
    lblTitle: TLabel;
    procedure btnClick(Sender: TObject);
  private
    FDlgResult: TDlgResult;
  public
    constructor Create(TheOwner: TComponent; const AFileName: string);
    property dlgResult: TDlgResult read FDlgResult;
  end;

implementation

{$R *.lfm}

{ TdlgFileChangedOnDisk }

procedure TdlgFileChangedOnDisk.btnClick(Sender: TObject);
begin
  case TButton(Sender).Tag of
    0: FDlgResult := dlgResReload;
    1: FDlgResult := dlgResIgnore;
  end;

  Close;
end;

constructor TdlgFileChangedOnDisk.Create(TheOwner: TComponent; const AFileName: string);
begin
  inherited Create(TheOwner);

  FDlgResult := dlgResClose;

  if not AFileName.IsEmpty then
    lblTitle.Caption := lblTitle.Caption + LineEnding + LineEnding + AFileName;
end;

end.
