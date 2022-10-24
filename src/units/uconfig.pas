unit uConfig;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, IniFiles;

type
  { TConfig }

  TConfig = class(TIniFile)
  private
    function GetBorderSpaceBottom: integer;
    function GetBorderSpaceLeft: integer;
    function GetBorderSpaceRight: integer;
    function GetBorderSpaceTop: integer;
    function GetColorTheme: string;
    function GetFontName: string;
    function GetFontSize: integer;
    function GetFormHeight: integer;
    function GetFormLeft: integer;
    function GetFormTop: integer;
    function GetFormWidth: integer;
    function GetHighlighter: boolean;
    function GetLineNumbers: boolean;
    function GetRightEdge: integer;
    function GetScrollBars: boolean;
    procedure SetBorderSpaceBottom(AValue: integer);
    procedure SetBorderSpaceLeft(AValue: integer);
    procedure SetBorderSpaceRight(AValue: integer);
    procedure SetBorderSpaceTop(AValue: integer);
    procedure SetColorTheme(AValue: string);
    procedure SetFontName(AValue: string);
    procedure SetFontSize(AValue: integer);
    procedure SetFormHeight(AValue: integer);
    procedure SetFormLeft(AValue: integer);
    procedure SetFormTop(AValue: integer);
    procedure SetFormWidth(AValue: integer);
    procedure SetHighlighter(AValue: boolean);
    procedure SetLineNumbers(AValue: boolean);
    procedure SetRightEdge(AValue: integer);
    procedure SetScrollBars(AValue: boolean);
  public
    procedure createDefaultConfigFile();

    property formHeight: integer read GetFormHeight write SetFormHeight;
    property formLeft: integer read GetFormLeft write SetFormLeft;
    property formTop: integer read GetFormTop write SetFormTop;
    property formWidth: integer read GetFormWidth write SetFormWidth;

    property colorTheme: string read GetColorTheme write SetColorTheme;
    property fontName: string read GetFontName write SetFontName;
    property fontSize: integer read GetFontSize write SetFontSize;
    property highlighter: boolean read GetHighlighter write SetHighlighter;
    property lineNumbers: boolean read GetLineNumbers write SetLineNumbers;
    property rightEdge: integer read GetRightEdge write SetRightEdge;
    property scrollBars: boolean read GetScrollBars write SetScrollBars;

    property borderSpaceBottom: integer read GetBorderSpaceBottom write SetBorderSpaceBottom;
    property borderSpaceLeft: integer read GetBorderSpaceLeft write SetBorderSpaceLeft;
    property borderSpaceRight: integer read GetBorderSpaceRight write SetBorderSpaceRight;
    property borderSpaceTop: integer read GetBorderSpaceTop write SetBorderSpaceTop;
  end;

implementation

const
  SECTION_MAIN = 'MAIN';
  SECTION_FORM = 'FORM';

{ TConfig }

function TConfig.GetBorderSpaceBottom: integer;
begin
  Result := ReadInteger(SECTION_MAIN, 'borderSpaceBottom', 0);
end;

function TConfig.GetBorderSpaceLeft: integer;
begin
  Result := ReadInteger(SECTION_MAIN, 'borderSpaceLeft', 24);
end;

function TConfig.GetBorderSpaceRight: integer;
begin
  Result := ReadInteger(SECTION_MAIN, 'borderSpaceRight', 0);
end;

function TConfig.GetBorderSpaceTop: integer;
begin
  Result := ReadInteger(SECTION_MAIN, 'borderSpaceTop', 24);
end;

function TConfig.GetColorTheme: string;
begin
  Result := ReadString(SECTION_MAIN, 'colorTheme', 'cream');
end;

function TConfig.GetFontName: string;
begin
  Result := ReadString(SECTION_MAIN, 'fontName', 'Monospace');
end;

function TConfig.GetFontSize: integer;
begin
  Result := ReadInteger(SECTION_MAIN, 'fontSize', 12);
end;

function TConfig.GetFormHeight: integer;
begin
  Result := ReadInteger(SECTION_FORM, 'formHeight', 400);
end;

function TConfig.GetFormLeft: integer;
begin
  Result := ReadInteger(SECTION_FORM, 'formLeft', 0);
end;

function TConfig.GetFormTop: integer;
begin
  Result := ReadInteger(SECTION_FORM, 'formTop', 0);
end;

function TConfig.GetFormWidth: integer;
begin
  Result := ReadInteger(SECTION_FORM, 'formWidth', 600);
end;

function TConfig.GetHighlighter: boolean;
begin
  Result := ReadBool(SECTION_MAIN, 'highlighter', False);
end;

function TConfig.GetLineNumbers: boolean;
begin
  Result := ReadBool(SECTION_MAIN, 'lineNumbers', False);
end;

function TConfig.GetRightEdge: integer;
begin
  Result := ReadInteger(SECTION_MAIN, 'rightEdge', -1);
end;

function TConfig.GetScrollBars: boolean;
begin
  Result := ReadBool(SECTION_MAIN, 'scrollBars', True);
end;

procedure TConfig.SetBorderSpaceBottom(AValue: integer);
begin
  WriteInteger(SECTION_MAIN, 'borderSpaceBottom', AValue);
end;

procedure TConfig.SetBorderSpaceLeft(AValue: integer);
begin
  WriteInteger(SECTION_MAIN, 'borderSpaceLeft', AValue);
end;

procedure TConfig.SetBorderSpaceRight(AValue: integer);
begin
  WriteInteger(SECTION_MAIN, 'borderSpaceRight', AValue);
end;

procedure TConfig.SetBorderSpaceTop(AValue: integer);
begin
  WriteInteger(SECTION_MAIN, 'borderSpaceTop', AValue);
end;

procedure TConfig.SetColorTheme(AValue: string);
begin
  WriteString(SECTION_MAIN, 'colorTheme', AValue);
end;

procedure TConfig.SetFontName(AValue: string);
begin
  WriteString(SECTION_MAIN, 'fontName', AValue);
end;

procedure TConfig.SetFontSize(AValue: integer);
begin
  WriteInteger(SECTION_MAIN, 'fontSize', AValue);
end;

procedure TConfig.SetFormHeight(AValue: integer);
begin
  WriteInteger(SECTION_FORM, 'formHeight', AValue);
end;

procedure TConfig.SetFormLeft(AValue: integer);
begin
  WriteInteger(SECTION_FORM, 'formLeft', AValue);
end;

procedure TConfig.SetFormTop(AValue: integer);
begin
  WriteInteger(SECTION_FORM, 'formTop', AValue);
end;

procedure TConfig.SetFormWidth(AValue: integer);
begin
  WriteInteger(SECTION_FORM, 'formWidth', AValue);
end;

procedure TConfig.SetHighlighter(AValue: boolean);
begin
  WriteBool(SECTION_MAIN, 'highlighter', AValue);
end;

procedure TConfig.SetLineNumbers(AValue: boolean);
begin
  WriteBool(SECTION_MAIN, 'lineNumbers', AValue);
end;

procedure TConfig.SetRightEdge(AValue: integer);
begin
  WriteInteger(SECTION_MAIN, 'rightEdge', AValue);
end;

procedure TConfig.SetScrollBars(AValue: boolean);
begin
  WriteBool(SECTION_MAIN, 'scrollBars', AValue);
end;

procedure TConfig.createDefaultConfigFile;
begin
  SetColorTheme(GetColorTheme);
  SetFontName(GetFontName);
  SetFontSize(GetFontSize);
  SetHighlighter(GetHighlighter);
  SetLineNumbers(GetLineNumbers);
  SetRightEdge(GetRightEdge);
  SetScrollBars(GetScrollBars);
  SetBorderSpaceBottom(GetBorderSpaceBottom);
  SetBorderSpaceLeft(GetBorderSpaceLeft);
  SetBorderSpaceRight(GetBorderSpaceRight);
  SetBorderSpaceTop(GetBorderSpaceTop);
end;

end.
