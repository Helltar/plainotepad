unit uAboutForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Dialogs, ExtCtrls, StdCtrls, LCLIntf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    imgAbout: TImage;
    lblHomepage: TLabel;
    lblGithub: TLabel;
    lblLicense: TLabel;
    procedure FormShow(Sender: TObject);
    procedure lblClick(Sender: TObject);
    procedure lblLicenseClick(Sender: TObject);
  end;

implementation

uses
  uConsts, uLicenseForm;

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.lblClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TfrmAbout.lblLicenseClick(Sender: TObject);
begin
  with TfrmLicense.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

end.
