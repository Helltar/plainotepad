unit uSettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Forms, Graphics, Dialogs,
  StdCtrls, Spin, ComCtrls, LazFileUtils, IniFiles;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnSelectFont: TButton;
    btnOk: TButton;
    btnCreateDesktopEntry: TButton;
    btnEditColorTheme: TButton;
    cbHighlighter: TCheckBox;
    cbLineNumbers: TCheckBox;
    cbScrollBars: TCheckBox;
    cbNonSystemScrollBars: TCheckBox;
    cbMiniMap: TCheckBox;
    cbWordWrap: TCheckBox;
    cmbMouseMiddleClickAction: TComboBox;
    cmbColorTheme: TComboBox;
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
    scGeneral: TScrollBox;
    seFontSize: TSpinEdit;
    seRightMargin: TSpinEdit;
    seBottomSpace: TSpinEdit;
    seLeftSpace: TSpinEdit;
    seRightSpace: TSpinEdit;
    seTopSpace: TSpinEdit;
    tsGeneral: TTabSheet;
    tsBorders: TTabSheet;
    procedure btnCreateDesktopEntryClick(Sender: TObject);
    procedure btnEditColorThemeClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure cbHighlighterChange(Sender: TObject);
    procedure cbLineNumbersChange(Sender: TObject);
    procedure cbMiniMapChange(Sender: TObject);
    procedure cbNonSystemScrollBarsChange(Sender: TObject);
    procedure cbScrollBarsChange(Sender: TObject);
    procedure cbWordWrapChange(Sender: TObject);
    procedure cmbColorThemeChange(Sender: TObject);
    procedure cmbMouseMiddleClickActionChange(Sender: TObject);
    procedure edtFontNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure seBottomSpaceChange(Sender: TObject);
    procedure seFontSizeChange(Sender: TObject);
    procedure seLeftSpaceChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
    procedure seRightSpaceChange(Sender: TObject);
    procedure seTopSpaceChange(Sender: TObject);
    procedure tsThemeEditorShow(Sender: TObject);
  private
    function getSelectedColorThemeName(): string;
  public
    procedure loadColorThemes();
  end;

var
  frmSettings: TfrmSettings;

implementation

uses
  uMainForm, uConsts, uUtils, uLogger, uColorThemeEditorForm;

resourcestring
  DESKTOP_ENTRY_CREATED = 'Desktop entry created';

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  pcSettings.ActivePageIndex := 0;

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
  end;

  with cmbMouseMiddleClickAction do
    if frmMain.config.mouseMiddleClickAction <= Items.Count then
      ItemIndex := frmMain.config.mouseMiddleClickAction
    else
      ItemIndex := 0;

  {$IFDEF MSWINDOWS}
  btnCreateDesktopEntry.Visible := False;
  {$ENDIF}

  loadColorThemes();

  with cmbColorTheme do
    if Items.Count > 0 then
    begin
      if Items.IndexOf(frmMain.config.colorTheme) >= 0 then
        ItemIndex := Items.IndexOf(frmMain.config.colorTheme)
      else
        ItemIndex := 0;
    end;
end;

procedure TfrmSettings.btnSelectFontClick(Sender: TObject);
begin
  if fontDialog.Execute then
  begin
    edtFontName.Text := fontDialog.Font.Name;
    seFontSize.Value := fontDialog.Font.Size;
  end;
end;

procedure TfrmSettings.cbHighlighterChange(Sender: TObject);
begin
  frmMain.config.highlighter := cbHighlighter.Checked;
end;

procedure TfrmSettings.cbLineNumbersChange(Sender: TObject);
begin
  frmMain.config.lineNumbers := cbLineNumbers.Checked;
end;

procedure TfrmSettings.cbMiniMapChange(Sender: TObject);
begin
  frmMain.config.miniMap := cbMiniMap.Checked;
end;

procedure TfrmSettings.cbNonSystemScrollBarsChange(Sender: TObject);
begin
  frmMain.config.nonSystemScrollBars := cbNonSystemScrollBars.Checked;
end;

procedure TfrmSettings.cbScrollBarsChange(Sender: TObject);
begin
  frmMain.config.scrollBars := cbScrollBars.Checked;
  cbNonSystemScrollBars.Enabled := cbScrollBars.Checked;
end;

procedure TfrmSettings.cbWordWrapChange(Sender: TObject);
begin
  frmMain.config.wordWrap := cbWordWrap.Checked;
end;

procedure TfrmSettings.cmbColorThemeChange(Sender: TObject);
begin
  frmMain.config.colorTheme := getSelectedColorThemeName();
end;

procedure TfrmSettings.cmbMouseMiddleClickActionChange(Sender: TObject);
begin
  frmMain.config.mouseMiddleClickAction := cmbMouseMiddleClickAction.ItemIndex;
end;

procedure TfrmSettings.edtFontNameChange(Sender: TObject);
begin
  frmMain.config.fontName := edtFontName.Text;
end;

procedure TfrmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  frmMain.updateConfig();
end;

procedure TfrmSettings.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettings.btnCreateDesktopEntryClick(Sender: TObject);
begin
  if createDesktopEntry() then
    addLog(DESKTOP_ENTRY_CREATED);
end;

procedure TfrmSettings.btnEditColorThemeClick(Sender: TObject);
begin
  with TfrmColorThemeEditor.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmSettings.loadColorThemes;
var
  searchRec: TSearchRec;

begin
  cmbColorTheme.Items.Clear;

  if FindFirst(getConfigDir() + DIR_COLOR_SCHEMES + '*' + FILE_EXT_COLOR_SCHEME, faAnyFile, searchRec) = 0 then
  begin
    repeat
      with searchRec do
        if (Attr and faDirectory) = 0 then
          with TIniFile.Create(getConfigDir() + DIR_COLOR_SCHEMES + Name) do
            try
              if SectionExists(COLOR_SCHEME_CONFIG_SECTION_MAIN) then
                cmbColorTheme.Items.Add(ExtractFileNameWithoutExt(Name));
            finally
              Free;
            end;
    until FindNext(searchRec) <> 0;

    FindClose(searchRec);
  end;

  cmbColorTheme.ItemIndex := cmbColorTheme.Items.IndexOf(frmMain.config.colorTheme);
end;

function TfrmSettings.getSelectedColorThemeName: string;
begin
  Result := 'null';

  with cmbColorTheme do
    if Items.Count > 0 then
      if ItemIndex >= 0 then
        Result := Items[ItemIndex];
end;

procedure TfrmSettings.seBottomSpaceChange(Sender: TObject);
begin
  frmMain.config.borderSpaceBottom := seBottomSpace.Value;
end;

procedure TfrmSettings.seFontSizeChange(Sender: TObject);
begin
  frmMain.config.fontSize := seFontSize.Value;
end;

procedure TfrmSettings.seLeftSpaceChange(Sender: TObject);
begin
  frmMain.config.borderSpaceLeft := seLeftSpace.Value;
end;

procedure TfrmSettings.seRightMarginChange(Sender: TObject);
begin
  frmMain.config.rightEdge := seRightMargin.Value;
end;

procedure TfrmSettings.seRightSpaceChange(Sender: TObject);
begin
  frmMain.config.borderSpaceRight := seRightSpace.Value;
end;

procedure TfrmSettings.seTopSpaceChange(Sender: TObject);
begin
  frmMain.config.borderSpaceTop := seTopSpace.Value;
end;

procedure TfrmSettings.tsThemeEditorShow(Sender: TObject);
begin
  with TfrmColorThemeEditor.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

end.
