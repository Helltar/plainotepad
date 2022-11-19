unit uSettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, StdCtrls, Spin, ComCtrls, ComboEx, ColorBox;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnSelectFont: TButton;
    btnSave: TButton;
    btnCreateDesktopEntry: TButton;
    cbHighlighter: TCheckBox;
    cbLineNumbers: TCheckBox;
    cbScrollBars: TCheckBox;
    cbNonSystemScrollBars: TCheckBox;
    cbMiniMap: TCheckBox;
    cbWordWrap: TCheckBox;
    clbColorTheme: TColorBox;
    cmbMouseMiddleClickAction: TComboBox;
    edtFontName: TEdit;
    fontDialog: TFontDialog;
    gbBorderSpace: TGroupBox;
    lblMouseMiddleClickAction: TLabel;
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
    tsGeneral: TTabSheet;
    tsBorders: TTabSheet;
    procedure btnCreateDesktopEntryClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure cbScrollBarsChange(Sender: TObject);
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

procedure TfrmSettings.cbScrollBarsChange(Sender: TObject);
begin
  cbNonSystemScrollBars.Enabled := cbScrollBars.Checked;
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
    miniMap := cbMiniMap.Checked;
    nonSystemScrollBars := cbNonSystemScrollBars.Checked;
    rightEdge := seRightMargin.Value;
    scrollBars := cbScrollBars.Checked;
    wordWrap := cbWordWrap.Checked;
    mouseMiddleClickAction := cmbMouseMiddleClickAction.ItemIndex;
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
  pcSettings.ActivePageIndex := 0;

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
    cbMiniMap.Checked := miniMap;
    cbNonSystemScrollBars.Checked := nonSystemScrollBars;
    cbNonSystemScrollBars.Enabled := scrollBars;
    cbScrollBars.Checked := scrollBars;
    cbWordWrap.Checked := wordWrap;
    edtFontName.Text := fontName;
    seBottomSpace.Value := borderSpaceBottom;
    seFontSize.Value := fontSize;
    seLeftSpace.Value := borderSpaceLeft;
    seRightMargin.Value := rightEdge;
    seRightSpace.Value := borderSpaceRight;
    seTopSpace.Value := borderSpaceTop;

    if mouseMiddleClickAction <= cmbMouseMiddleClickAction.Items.Count then
      cmbMouseMiddleClickAction.ItemIndex := mouseMiddleClickAction
    else
      cmbMouseMiddleClickAction.ItemIndex := 0;

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
