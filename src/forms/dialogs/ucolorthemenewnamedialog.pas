unit uColorThemeNewNameDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, FileUtil, LCLType;

type

  { TdlgColorThemeNewName }

  TdlgColorThemeNewName = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edtNewName: TEdit;
    lblNewName: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure edtNewNameChange(Sender: TObject);
    procedure edtNewNameKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FColorThemeFilename: string;
    FColorThemeNewName: string;
    FCopyFileResultOk: boolean;
    FFormCaption: string;
  public
    property colorThemeFilename: string write FColorThemeFilename;
    property copyFileResultOk: boolean read FCopyFileResultOk write FCopyFileResultOk;
    property colorThemeNewName: string read FColorThemeNewName write FColorThemeNewName;
    property formCaption: string write FFormCaption;
  end;

implementation

uses
  uConsts, uLogger, uUtils;

resourcestring
  COLOR_THEME_ALREADY_EXISTS = 'A color theme with this name already exists';
  ERROR_COPY_COLOR_THEME = 'Error when copy a color theme: %s';
  CAPTION_COPY = 'Copy: %s';

{$R *.lfm}

{ TdlgColorThemeNewName }

procedure TdlgColorThemeNewName.btnOkClick(Sender: TObject);
var
  filename: string;

begin
  filename := getConfigDir() + DIR_COLOR_SCHEMES + edtNewName.Text + FILE_EXT_COLOR_SCHEME;

  if not FileExists(filename) then
  begin
    if CopyFile(FColorThemeFilename, filename) then
    begin
      FColorThemeNewName := edtNewName.Text;
      FCopyFileResultOk := True;
      Close;
    end
    else
      addLog(Format(ERROR_COPY_COLOR_THEME, [filename]));
  end
  else
    addLog(COLOR_THEME_ALREADY_EXISTS);
end;

procedure TdlgColorThemeNewName.edtNewNameChange(Sender: TObject);
begin
  btnOk.Enabled := edtNewName.Text <> '';
end;

procedure TdlgColorThemeNewName.edtNewNameKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if btnOk.Enabled then
      btnOkClick(nil);
end;

procedure TdlgColorThemeNewName.FormShow(Sender: TObject);
begin
  Caption := Format(CAPTION_COPY, [FFormCaption]);
end;

end.
