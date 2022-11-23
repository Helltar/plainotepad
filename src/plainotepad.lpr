program plainotepad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, lazcontrols,
  uMainForm, uAboutForm, uSettingsForm, uLicenseForm,
  uConsts, uUtils, uConfig, uLogger,
  uEditor, uEditorHighlighter, uColorThemeEditor;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
