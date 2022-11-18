unit uSettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, StdCtrls, Spin, ComCtrls,
  ComboEx, ColorBox;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnSelectFont: TButton;
    btnSave: TButton;
    btnCreateDesktopEntry: TButton;
    cbHighlighter: TCheckBox;
    cbLineNumbers: TCheckBox;
    cbScrollBars: TCheckBox;
    cbWordWrap: TCheckBox;
    clbColorTheme: TColorBox;
    edtFontName: TEdit;
    fontDialog: TFontDialog;
    lblRightMargin: TLabel;
    lblBottomSpace: TLabel;
    lblLeftSpace: TLabel;
    lblRightSpace: TLabel;
    lblTopSpace: TLabel;
    lblColorTheme: TLabel;
    lblFont: TLabel;
    pcSettings: TPageControl;
    seFontSize: TSpinEdit;
    seRightMargin: TSpinEdit;
    seBottomSpace: TSpinEdit;
    seLeftSpace: TSpinEdit;
    seRightSpace: TSpinEdit;
    seTopSpace: TSpinEdit;
    tsEditor: TTabSheet;
    tsBorderSpace: TTabSheet;
    procedure btnCreateDesktopEntryClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

implementation

uses
  uMainForm, uConsts, uUtils, uLogger;

resourcestring
  DESKTOP_ENTRY_CREATED = 'Desktop entry created';

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
    colorTheme := clbColorTheme.Items[clbColorTheme.ItemIndex];
    fontName := edtFontName.Text;
    fontSize := seFontSize.Value;
    highlighter := cbHighlighter.Checked;
    lineNumbers := cbLineNumbers.Checked;
    rightEdge := seRightMargin.Value;
    scrollBars := cbScrollBars.Checked;
    wordWrap := cbWordWrap.Checked;
  end;

  frmMain.updateConfig();
  Close;
end;

procedure TfrmSettings.btnCreateDesktopEntryClick(Sender: TObject);
begin
  if createDesktopEntry() then
    addLog(DESKTOP_ENTRY_CREATED);
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  btnCreateDesktopEntry.Visible := False;
  {$ENDIF}

  clbColorTheme.AddItem(COLOR_THEME_CREAM, TObject(clCream));
  clbColorTheme.AddItem(COLOR_THEME_DARK, TObject(clBlack));
  clbColorTheme.AddItem(COLOR_THEME_WHITE, TObject(clWhite));

  with frmMain.config do
  begin
    cbHighlighter.Checked := highlighter;
    cbLineNumbers.Checked := lineNumbers;
    cbScrollBars.Checked := scrollBars;
    cbWordWrap.Checked := wordWrap;
    edtFontName.Text := fontName;
    seBottomSpace.Value := borderSpaceBottom;
    seFontSize.Value := fontSize;
    seLeftSpace.Value := borderSpaceLeft;
    seRightMargin.Value := rightEdge;
    seRightSpace.Value := borderSpaceRight;
    seTopSpace.Value := borderSpaceTop;

    case colorTheme of
      COLOR_THEME_CREAM: clbColorTheme.Selected := clCream;
      COLOR_THEME_DARK: clbColorTheme.Selected := clBlack;
      COLOR_THEME_WHITE: clbColorTheme.Selected := clWhite;
    end;
  end;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

end.
