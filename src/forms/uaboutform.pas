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
    procedure lblClick(Sender: TObject);
    procedure lblLicenseClick(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.lblClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TfrmAbout.lblLicenseClick(Sender: TObject);
begin
  OpenURL('https://github.com/Helltar/plainotepad/blob/master/LICENSE');
end;

end.
