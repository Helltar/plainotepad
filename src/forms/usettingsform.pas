unit uSettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, StdCtrls, Spin;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnSelectFont: TButton;
    btnSave: TButton;
    cmbColorTheme: TComboBox;
    cbHighlighter: TCheckBox;
    cbLineNumbers: TCheckBox;
    cbScrollBars: TCheckBox;
    edtFontName: TEdit;
    fontDialog: TFontDialog;
    gbBorderSpace: TGroupBox;
    lblRightMargin: TLabel;
    lblBottomSpace: TLabel;
    lblLeftSpace: TLabel;
    lblRightSpace: TLabel;
    lblTopSpace: TLabel;
    lblColorTheme: TLabel;
    lblFont: TLabel;
    seFontSize: TSpinEdit;
    seRightMargin: TSpinEdit;
    seBottomSpace: TSpinEdit;
    seLeftSpace: TSpinEdit;
    seRightSpace: TSpinEdit;
    seTopSpace: TSpinEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

implementation

uses
  uMainForm, uConsts;

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.btnSelectFontClick(Sender: TObject);
begin
  if fontDialog.Execute then
  begin
    edtFontName.Text := fontDialog.Font.Name;
    seFontSize.Value := fontDialog.Font.Size;
  end;
end;

procedure TfrmSettings.btnSaveClick(Sender: TObject);
begin
  with frmMain.config do
  begin
    borderSpaceBottom := seBottomSpace.Value;
    borderSpaceLeft := seLeftSpace.Value;
    borderSpaceRight := seRightSpace.Value;
    borderSpaceTop := seTopSpace.Value;
    colorTheme := cmbColorTheme.Items[cmbColorTheme.ItemIndex];
    fontName := edtFontName.Text;
    fontSize := seFontSize.Value;
    highlighter := cbHighlighter.Checked;
    lineNumbers := cbLineNumbers.Checked;
    rightEdge := seRightMargin.Value;
    scrollBars := cbScrollBars.Checked;
  end;

  frmMain.updateConfig();
  Close;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  cmbColorTheme.Items.Add(COLOR_THEME_CREAM);
  cmbColorTheme.Items.Add(COLOR_THEME_DARK);
  cmbColorTheme.Items.Add(COLOR_THEME_WHITE);

  with frmMain.config do
  begin
    cbHighlighter.Checked := highlighter;
    cbLineNumbers.Checked := lineNumbers;
    cbScrollBars.Checked := scrollBars;
    edtFontName.Text := fontName;
    seBottomSpace.Value := borderSpaceBottom;
    seFontSize.Value := fontSize;
    seLeftSpace.Value := borderSpaceLeft;
    seRightMargin.Value := rightEdge;
    seRightSpace.Value := borderSpaceRight;
    seTopSpace.Value := borderSpaceTop;

    case colorTheme of
      COLOR_THEME_CREAM: cmbColorTheme.ItemIndex := 0;
      COLOR_THEME_DARK: cmbColorTheme.ItemIndex := 1;
      COLOR_THEME_WHITE: cmbColorTheme.ItemIndex := 2;
    end;
  end;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

end.
