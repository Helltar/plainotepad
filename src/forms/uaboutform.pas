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
    procedure FormCreate(Sender: TObject);
    procedure lblClick(Sender: TObject);
    procedure lblLicenseClick(Sender: TObject);
  end;

implementation

uses
  uConsts;

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.lblClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblLicense.Hint := URL_LICENSE;
end;

procedure TfrmAbout.lblLicenseClick(Sender: TObject);
begin
  OpenURL(URL_LICENSE);
end;

end.
