unit uConsts;

{$mode ObjFPC}{$H+}

interface

const
  APP_NAME = 'Plainotepad';
  APP_FILE_NAME = 'plainotepad';

  APP_CONFIG_FILE_NAME = 'config.ini';
  APP_RECENT_FILES_FILENAME = 'recent_files';

  DIR_COLOR_SCHEMES = 'color-schemes' + DirectorySeparator;
  DIR_LEXLIB = 'synt-analyzers' + DirectorySeparator;

  FILE_EXT_SYNT_ANALYZER = '.lcf';
  FILE_EXT_COLOR_SCHEME = '.colors';

  COLOR_THEME_DARK = 'dark';
  COLOR_THEME_CREAM = 'cream';
  RES_SYNT_ANALYZERS = 'SYNT-ANALYZERS';

  COLOR_SCHEME_CONFIG_SECTION_MAIN = 'MAIN';
  COLOR_SCHEME_CONFIG_SECTION_LEXER = 'LEXER-';

  URL_GITHUB = 'https://github.com/Helltar/plainotepad';

implementation

end.
