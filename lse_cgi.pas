{==============================================================================}
{        UNIT: lse_cgi                                                         }
{ DESCRIPTION: CGI utilities in lysee kernel                                   }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2009/01/03                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_cgi;

{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lseu, lse_kernel, lse_funcs
  {$IFDEF WINDOWS},Windows{$ENDIF};

type
  KLiResponse   = class; {forward}
  KLiCookie     = class;
  KLiCookieList = class;

  { KLiCGI }

  KLiCGI = class(TLseObject)
  private
    FEngine: KLiEngine;
    FMode: string;
    FResponse: KLiResponse;
    FResponseStream: PLseStream;
    FTempFiles: TStrings;
    FShowResponseHeader: boolean;
    FHasError: boolean;
    FActionParsed: boolean;
    function Read(const Buf: pointer; Count: integer): integer;
    function ReadBuffer(const Buf: pointer; Count: integer): integer;
    procedure Write(Buf: pointer; Count: integer);
    procedure WriteText(const Text: string);
    procedure WriteStream(AStream: TStream);
    procedure WriteFile(const FileName: string);
    procedure AddTempFile(const FileName: string);
    procedure RemoveTempFiles;
    procedure LoadCookies;
    procedure AddRequest(const ID, VALUE: string);overload;
    procedure AddRequest(const ID: string; Value: int64);overload;
    procedure ParseQueryString(const QUERY_STRING: string);
    procedure InitMPFD(const Boundary: string); {<--multipart/form-data}
  public
    constructor Create(AEngine: KLiEngine);
    destructor Destroy;override;
    property Mode: string read FMode;
    property ShowResponseHeader: boolean read FShowResponseHeader;
    property HasError: boolean read FHasError;
  end;

  KLiCookie = class(TLseObject)
  private
    FList: KLiCookieList;
    FName: string;
    FValue: string;
    FPath: string;
    FDomain: string;
    FExpires: TDateTime;
    FSecure: Boolean;
  public
    constructor Create(AList: KLiCookieList);
    destructor Destroy;override;
    procedure Delete;
    function HeaderValue: string;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Domain: string read FDomain write FDomain;
    property Path: string read FPath write FPath;
    property Expires: TDateTime read FExpires write FExpires;
    property Secure: Boolean read FSecure write FSecure;
  end;

  KLiCookieList = class(TLseObject)
  private
    FCookies: TList;
    function GetCount: integer;
    function GetCookie(index: integer): KLiCookie;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(index: integer);
    procedure Remove(const Name: string);
    function IndexOf(const Name: string): integer;
    function Find(const Name: string): KLiCookie;
    function Add(const Name: string): KLiCookie;
    property Count: integer read GetCount;
    property Cookies[index: integer]: KLiCookie read GetCookie;default;
  end;

  { KLiResponse }

  KLiResponse = class(TLseObject)
  private
    FCGI: KLiCGI;
    FStatusCode: integer;
    FReasonString: string;
    FContentType: string;
    FContentFile: string;
    FCookies: KLiCookieList;
    FStream: TMemoryStream;
    FHeadValues: TStrings;
    FExecPack: boolean;
    procedure SetContentType(const Value: string);
    function GetContent: string;
    procedure SetContent(const Value: string);
    function GetContentLength: integer;
  public
    constructor Create(ACGI: KLiCGI);
    destructor Destroy;override;
    procedure Reset;
    procedure Redirect(const URI: string);
    procedure SetHV(const ID, value: string);
    function GetHV(const ID: string): string;
    function HeaderString: string;
    procedure PackTextHTML;
    function IsTextHTML: boolean;
    procedure NoContent;
    function IsNoContent: boolean;
    function ServeFile(const FileName: string): string;
    property ReasonString: string read FReasonString write FReasonString;
    property ContentType: string read FContentType write SetContentType;
    property ContentLength: integer read GetContentLength;
    property Content: string read GetContent write SetContent;
    property StatusCode: integer read FStatusCode write FStatusCode;
    property Cookies: KLiCookieList read FCookies;
    property ExecPack: boolean read FExecPack write FExecPack;
  end;

{ cgi }

procedure cgi_mode(const Param: PLseParam);cdecl;
procedure cgi_encode(const Param: PLseParam);cdecl;
procedure cgi_decode(const Param: PLseParam);cdecl;
procedure cgi_parse(const Param: PLseParam);cdecl;
procedure cgi_arrstr(const Param: PLseParam);cdecl;
procedure cgi_notags(const Param: PLseParam);cdecl;
procedure cgi_encodeHTML(const Param: PLseParam);cdecl;
procedure cgi_decodeHTML(const Param: PLseParam);cdecl;
procedure cgi_pack(const Param: PLseParam);cdecl;
procedure cgi_mimes(const Param: PLseParam);cdecl;
procedure cgi_mime(const Param: PLseParam);cdecl;
procedure cgi_get_status(const Param: PLseParam);cdecl;
procedure cgi_set_status(const Param: PLseParam);cdecl;
procedure cgi_get_reason(const Param: PLseParam);cdecl;
procedure cgi_set_reason(const Param: PLseParam);cdecl;
procedure cgi_gethv(const Param: PLseParam);cdecl;
procedure cgi_sethv(const Param: PLseParam);cdecl;
procedure cgi_reset(const Param: PLseParam);cdecl;
procedure cgi_headstr(const Param: PLseParam);cdecl;
procedure cgi_get_ContentType(const Param: PLseParam);cdecl;
procedure cgi_set_ContentType(const Param: PLseParam);cdecl;
procedure cgi_get_ContentLength(const Param: PLseParam);cdecl;
procedure cgi_get_Content(const Param: PLseParam);cdecl;
procedure cgi_set_Content(const Param: PLseParam);cdecl;
procedure cgi_serveFile(const Param: PLseParam);cdecl;
procedure cgi_cookieExists(const Param: PLseParam);cdecl;
procedure cgi_getCookie(const Param: PLseParam);cdecl;
procedure cgi_getCookieStr(const Param: PLseParam);cdecl;
procedure cgi_setCookie(const Param: PLseParam);cdecl;
procedure cgi_setCookieDomain(const Param: PLseParam);cdecl;
procedure cgi_setCookiePath(const Param: PLseParam);cdecl;
procedure cgi_setCookieExpires(const Param: PLseParam);cdecl;
procedure cgi_setCookieSecure(const Param: PLseParam);cdecl;
procedure cgi_deleteCookie(const Param: PLseParam);cdecl;
procedure cgi_clearCookies(const Param: PLseParam);cdecl;
procedure cgi_redirect(const Param: PLseParam);cdecl;
procedure cgi_noContent(const Param: PLseParam);cdecl;

const
  cgi_execute_count = 37;
  cgi_execute_array: array[0..cgi_execute_count - 1] of RLseFunc = (
    (fr_prot:'requestMode:string ||';
     fr_addr:@cgi_mode;
     fr_desc:'request mode'
    ),
    (fr_prot:'encode:string |text:string, keepMBC|';
     fr_addr:@cgi_encode;
     fr_desc:'encode text to CGI request'
    ),
    (fr_prot:'decode:string |request:string|';
     fr_addr:@cgi_decode;
     fr_desc:'decode CGI request to original text'
    ),
    (fr_prot:'parse:varlist |queryString:string|';
     fr_addr:@cgi_parse;
     fr_desc:'parse CGI request to string list'
    ),
    (fr_prot:'arrstr:string |str:string, count:int|';
     fr_addr:@cgi_arrstr;
     fr_desc:'arrange string to required length'
    ),
    (fr_prot:'notags:string |HTML:string|';
     fr_addr:@cgi_notags;
     fr_desc:'clear HTML tags'
    ),
    (fr_prot:'encodeHTML:string |S:string|';
     fr_addr:@cgi_encodeHTML;
     fr_desc:'encode string to HTML'
    ),
    (fr_prot:'decodeHTML:string |HTML:string|';
     fr_addr:@cgi_decodeHTML;
     fr_desc:'decode HTML to string'
    ),
    (fr_prot:'pack:void |immediately|';
     fr_addr:@cgi_pack;
     fr_desc:'pack HTML content'
    ),
    (fr_prot:'mimeList:varlist ||';
     fr_addr:@cgi_mimes;
     fr_desc:'get MIME list'
    ),
    (fr_prot:'mimeFile:string |fileName:string|';
     fr_addr:@cgi_mime;
     fr_desc:'get MIME type of specified file'
    ),
    (fr_prot:'get_statusCode:int ||';
     fr_addr:@cgi_get_status;
     fr_desc:'get response status code'
    ),
    (fr_prot:'set_satusCode:void |statusCode:int|';
     fr_addr:@cgi_set_status;
     fr_desc:'set response status code'
    ),
    (fr_prot:'get_statusReason:string ||';
     fr_addr:@cgi_get_reason;
     fr_desc:'get response status reason phrase'
    ),
    (fr_prot:'set_statusReason:void |reason:string|';
     fr_addr:@cgi_set_reason;
     fr_desc:'set response status reason phrase'
    ),
    (fr_prot:'get_contentType:string ||';
     fr_addr:@cgi_get_ContentType;
     fr_desc:'get response content type'
    ),
    (fr_prot:'set_contentType:void |contentType:string|';
     fr_addr:@cgi_set_ContentType;
     fr_desc:'set response content type'
    ),
    (fr_prot:'get_contentLength:int ||';
     fr_addr:@cgi_get_ContentLength;
     fr_desc:'get response content Length'
    ),
    (fr_prot:'get_contentString:string ||';
     fr_addr:@cgi_get_content;
     fr_desc:'get response content'
    ),
    (fr_prot:'set_contentString:void |content:string|';
     fr_addr:@cgi_set_content;
     fr_desc:'set response content'
    ),
    (fr_prot:'serveFile:string |fileName:string|';
     fr_addr:@cgi_serveFile;
     fr_desc:'set reponse file'
    ),
    (fr_prot:'getHV:string |name:string|';
     fr_addr:@cgi_gethv;
     fr_desc:'get customed response header value'
    ),
    (fr_prot:'setHV:void |name:string, value:string|';
     fr_addr:@cgi_sethv;
     fr_desc:'set customed response header value'
    ),
    (fr_prot:'get_headerString:string ||';
     fr_addr:@cgi_headstr;
     fr_desc:'get response header string'
    ),
    (fr_prot:'cookieExists:int |name:string|';
     fr_addr:@cgi_cookieExists;
     fr_desc:'check if the named cookie exists'
    ),
    (fr_prot:'getCookie:string |name:string|';
     fr_addr:@cgi_getCookie;
     fr_desc:'get cookie value'
    ),
    (fr_prot:'getCookieStr:string |name:string|';
     fr_addr:@cgi_getCookieStr;
     fr_desc:'get cookie head value string'
    ),
    (fr_prot:'setCookie:void |name:string, value:string|';
     fr_addr:@cgi_setCookie;
     fr_desc:'set cookie value'
    ),
    (fr_prot:'setCookieDomain:void |name:string, domain:string|';
     fr_addr:@cgi_setCookieDomain;
     fr_desc:'set cookie domain'
    ),
    (fr_prot:'setCookiePath:void |name:string, path:string|';
     fr_addr:@cgi_setCookiePath;
     fr_desc:'set cookie path'
    ),
    (fr_prot:'setCookieExpires:void |name:string, expires:string|';
     fr_addr:@cgi_setCookieExpires;
     fr_desc:'set cookie expires'
    ),
    (fr_prot:'setCookieSecure:void |name:string, secure:string|';
     fr_addr:@cgi_setCookieSecure;
     fr_desc:'set cookie secure attribute'
    ),
    (fr_prot:'deleteCookie:void |name:string|';
     fr_addr:@cgi_deleteCookie;
     fr_desc:'remove cookie by name'
    ),
    (fr_prot:'clearCookies:void ||';
     fr_addr:@cgi_clearCookies;
     fr_desc:'clear cookie list'
    ),
    (fr_prot:'reset:void ||';
     fr_addr:@cgi_reset;
     fr_desc:'reset response'
    ),
    (fr_prot:'redirect:void |URI:string|';
     fr_addr:@cgi_redirect;
     fr_desc:'document moved and redirect'
    ),
    (fr_prot:'noContent:void ||';
     fr_addr:@cgi_noContent;
     fr_desc:'set status code to 204: No Content'
    )
  );

  cgi_module_funcs: RLseFuncListRec = (
    fl_count: cgi_execute_count;
    fl_entry:@cgi_execute_array;
  );

function __parseQuery(const QS: string; fields: TStrings): integer;
function __clearTags(const HTML: string): string;
function __htmlEncode(const S: string; translateMBC: boolean): string;
function __htmlDecode(const HTML: string): string;
function __httpEncode(const data: string; keepmbc: boolean): string;
function __httpDecode(const data: string): string;
function __statusString(StatusCode: Integer): string;
function __getMimeList: TStringList;
function __getMimeType(const FileName: string): string;
function __asCGI(const Param: PLseParam): KLiCGI;
function __getRSP(const Param: PLseParam; var RSP: KLiResponse): boolean;

implementation

uses
  math;

function __parseQuery(const QS: string; fields: TStrings): integer;
var
  next, base: pchar;
  line: string;
begin
  fields.Clear;
  next := __skipch(pchar(QS), ['&'] + SpaceChar);
  while (next <> nil) and (next^ <> #0) do
  begin
    base := next;
    next := __seekch(base, [#0, '&']);
    if base <> next then
    begin
      SetString(line, base, next - base);
      line := Trim(line);
      if line <> '' then
        fields.Add(line);
    end;
    next := __skipch(next, ['&'] + SpaceChar);
  end;
  Result := fields.Count;
end;

function __clearTags(const HTML: string): string;
var
  temp: string;
  strp, next: pchar;

  procedure skip_gt;
  var
    ch: char;
  begin
    while not (next^ in ['>', #0]) do
      if next^ in ['''', '"'] then
      begin
        ch := next^;
        Inc(next);
        while not (next^ in [#0, ch]) do Inc(next);
        if next <> #0 then Inc(next);
      end
      else Inc(next);
    if next <> #0 then Inc(next);
  end;

  procedure skip_tag;
  var
    base: pchar;
    ID: string;
  begin
    Inc(next);
    if next^ = '!' then // <!-- ... -->
    begin
      next := StrPos('-->', next);
      if next = nil then Exit;
    end
    else
    if next^ in IDHeadChar then
    begin
      base := next;
      while next^ in IDChar do Inc(next);
      SetString(ID, base, next - base);
      if (ID = 'script') or (ID = 'style') or (ID = 'form') or (ID = 'input') then
      begin
        ID := '</' + ID;
        next := StrPos(pchar(ID), next);
        if next = nil then Exit;
      end;
    end;
    skip_gt;
  end;

begin
  Result := '';
  temp := LowerCase(HTML);
  strp := pchar(temp);
  next := strp;
  while (next <> nil) and (next^ <> #0) do
    if next^ = '<' then skip_tag else
    begin
      Result := Result + HTML[(next - strp) + 1];
      Inc(next);
    end;
end;

function __htmlEncode(const S: string; translateMBC: boolean): string;
var
  index, count: integer;
  temp: string;
  ustr: WideString;
  ch: char;
  buf: pchar;
begin
  Result := '';
  buf := pchar(S);
  count := Length(S);
  if (buf = nil) or (count < 1) then Exit;
  index := 0;
  while (index < count) and (buf^ <> #0) do
  begin
    ch := buf[index];
    case ch of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      ' ': Result := Result + '&nbsp;';
      '"': Result := Result + '&quot;';
      #9 : Result := Result + '&nbsp;&nbsp;&nbsp;&nbsp;';
      #13: begin
             Result := Result + '<br>' + LB;
             if index < (count - 1) then
               if buf[index + 1] = #10 then Inc(index);
           end;
      #10: Result := Result + '<br>' + LB;
      else
        if translateMBC and (ch in LeadBytes) and (index < count - 1) and (buf[index + 1] in LeadBytes) then
        begin
          SetString(temp, buf + index, 2);
          ustr := temp;
          Result := Result + '&#' + IntToStr(Ord(ustr[1])) + ';';
          Inc(index);
        end
        else
        if buf[index] in [#92, #160 .. #255] then
          Result := Result + '&#' + IntToStr(Ord(ch)) +';' else
          Result := Result + ch;
    end;
    Inc(index);
  end;
end;

function __htmlDecode(const HTML: string): string;
var
  index, cv, count: integer;
  buf: pchar;
  temp: string;
  uch: WideChar;
begin
  Result := '';
  buf := pchar(HTML);
  count := Length(HTML);
  if (buf = nil) or (count < 1) then Exit;
  index := 0;
  while (index < count) and (buf^ <> #0) do
  begin
    if buf[index] = '&' then
    begin
      temp := '';
      Inc(index);
      while (index < count) and not (buf[index] in [#0, ';']) do
      begin
        temp := temp + buf[index];
        Inc(index);
      end;
      temp := LowerCase(Trim(temp));
      if temp <> '' then
      begin
        if temp = 'lt' then Result := Result + '<' else
        if temp = 'gt' then Result := Result + '>' else
        if temp = 'amp' then Result := Result + '&' else
        if temp = 'nbsp' then Result := Result + ' ' else
        if temp = 'quot' then Result := Result + '"' else
        if temp[1] = '#' then
        begin
          cv := __parseInt(pchar(Copy(temp, 2, Length(temp) - 1)));
          if cv > 255 then
          begin
            uch := WideChar(cv);
            Result := Result + uch;
          end
          else
          if cv > 0 then
            Result := Result + Chr(cv)
        end;
      end;
    end
    else Result := Result + buf[index];
    Inc(index);
  end;
  Result := __replaceAll(Result, '<br>' + sLineBreak, sLineBreak);
  Result := __replaceAll(Result, '<br>', sLineBreak);
end;

function __httpEncode(const data: string; keepmbc: boolean): string;
var
  cset: set of char;
  base, next: PChar;
begin
  cset := IDChar + ['*', '@', '.', '-', '$', '!', '''', '(', ')'];
  if keepmbc then
    cset := cset + SysUtils.LeadBytes;
  SetLength(Result, Length(data) * 3);
  base := PChar(data);
  next := PChar(Result);
  while base^ <> #0 do
  begin
    if base^ in cset then
      next^ := base^
    else
      if base^ = ' ' then
        next^ := '+'
      else
      begin
        FormatBuf(next^, 3, '%%%.2x', 6, [Ord(base^)]);
        Inc(next,2);
      end;
    Inc(next);
    Inc(base);
  end;
  SetLength(Result, next - PChar(Result));
end;

function __httpDecode(const data: string): string;
var
  base, next: PChar;

  function translate: boolean;
  var
    temp: pchar;
  begin
    Result := true;
    Inc(base);
    if base^ <> '%' then
    begin
      temp := base;
      Inc(base);
      Result := (temp^ in HexChar) and (base^ in HexChar);
      if Result then
        next^ := Chr(StrToInt('$' + temp^ + base^));
    end
    else next^ := '%'
  end;

begin
  SetLength(Result, Length(data));
  base := PChar(data);
  next := PChar(Result);
  while base^ <> #0 do
  begin
    case base^ of
      '+': next^ := ' ';
      '%': if not translate then Break;
      else next^ := base^;
    end;
    Inc(next);
    Inc(base);
  end;
  SetLength(Result, next - PChar(Result));
end;

function __statusString(StatusCode: Integer): string;
begin
  case StatusCode of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Moved Temporarily';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'None Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Unless True';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    else Result := '';
  end
end;

function __getMimeList: TStringList;
var
  line, ext, mime: string;
  list: TStrings;
  index: integer;
begin
  if sys_mimes = nil then
  begin
    lock_kernel;
    try
      if sys_mimes = nil then
      begin
        sys_mimes := TStringList
        .Create;

        sys_mimes.Add('ai=application/postscript');
        sys_mimes.Add('aif=audio/x-aiff');
        sys_mimes.Add('aifc=audio/x-aiff');
        sys_mimes.Add('aiff=audio/x-aiff');
        sys_mimes.Add('asc=text/plain');
        sys_mimes.Add('asf=application/vnd.ms-asf');
        sys_mimes.Add('au=audio/basic');
        sys_mimes.Add('avi=video/x-msvideo');
        sys_mimes.Add('bin=application/octet-stream');
        sys_mimes.Add('bmp=image/bmp');
        sys_mimes.Add('class=application/octet-stream');
        sys_mimes.Add('cpio=application/x-cpio');
        sys_mimes.Add('csh=application/x-csh');
        sys_mimes.Add('css=text/css');
        sys_mimes.Add('dcr=application/x-director');
        sys_mimes.Add('dir=application/x-director');
        sys_mimes.Add('dms=application/octet-stream');
        sys_mimes.Add('doc=application/msword');
        sys_mimes.Add('dvi=application/x-dvi');
        sys_mimes.Add('dxr=application/x-director');
        sys_mimes.Add('eps=application/postscript');
        sys_mimes.Add('exe=application/octet-stream');
        sys_mimes.Add('gif=image/gif');
        sys_mimes.Add('gtar=application/x-gtar');
        sys_mimes.Add('gz=application/x-gzip');
        sys_mimes.Add('hqx=application/mac-binhex40');
        sys_mimes.Add('htm=text/html');
        sys_mimes.Add('html=text/html');
        sys_mimes.Add('ico=image/x-icon');
        sys_mimes.Add('ief=image/ief');
        sys_mimes.Add('iges=model/iges');
        sys_mimes.Add('igs=model/iges');
        sys_mimes.Add('jpe=image/jpeg');
        sys_mimes.Add('jpeg=image/jpeg');
        sys_mimes.Add('jpg=image/jpeg');
        sys_mimes.Add('js=application/x-javascript');
        sys_mimes.Add('kar=audio/midi');
        sys_mimes.Add('latex=application/x-latex');
        sys_mimes.Add('lha=application/octet-stream');
        sys_mimes.Add('lzh=application/octet-stream');
        sys_mimes.Add('man=application/x-troff-man');
        sys_mimes.Add('me=application/x-troff-me');
        sys_mimes.Add('mesh=model/mesh');
        sys_mimes.Add('mid=audio/midi');
        sys_mimes.Add('midi=audio/midi');
        sys_mimes.Add('mif=application/vnd.mif');
        sys_mimes.Add('mov=video/quicktime');
        sys_mimes.Add('mp2=audio/mpeg');
        sys_mimes.Add('mp3=audio/mpeg');
        sys_mimes.Add('mpe=video/mpeg');
        sys_mimes.Add('mpeg=video/mpeg');
        sys_mimes.Add('mpg=video/mpeg');
        sys_mimes.Add('mpga=audio/mpeg');
        sys_mimes.Add('ms=application/x-troff-ms');
        sys_mimes.Add('msh=model/mesh');
        sys_mimes.Add('pbm=image/x-portable-bitmap');
        sys_mimes.Add('pdf=application/pdf');
        sys_mimes.Add('pgm=image/x-portable-graymap');
        sys_mimes.Add('png=image/png');
        sys_mimes.Add('pnm=image/x-portable-anymap');
        sys_mimes.Add('ppm=image/x-portable-pixmap');
        sys_mimes.Add('ppt=application/vnd.ms-powerpoint');
        sys_mimes.Add('ps=application/postscript');
        sys_mimes.Add('qt=video/quicktime');
        sys_mimes.Add('ra=audio/x-realaudio');
        sys_mimes.Add('ram=audio/x-pn-realaudio');
        sys_mimes.Add('ras=image/x-cmu-raster');
        sys_mimes.Add('rgb=image/x-rgb');
        sys_mimes.Add('rm=audio/x-pn-realaudio');
        sys_mimes.Add('roff=application/x-troff');
        sys_mimes.Add('rtf=text/rtf');
        sys_mimes.Add('rtx=text/richtext');
        sys_mimes.Add('sgm=text/sgml');
        sys_mimes.Add('sgml=text/sgml');
        sys_mimes.Add('sh=application/x-sh');
        sys_mimes.Add('shar=application/x-shar');
        sys_mimes.Add('silo=model/mesh');
        sys_mimes.Add('sit=application/x-stuffit');
        sys_mimes.Add('smi=application/smil');
        sys_mimes.Add('smil=application/smil');
        sys_mimes.Add('snd=audio/basic');
        sys_mimes.Add('swf=application/x-shockwave-flash');
        sys_mimes.Add('t=application/x-troff');
        sys_mimes.Add('tar=application/x-tar');
        sys_mimes.Add('tcl=application/x-tcl');
        sys_mimes.Add('tex=application/x-tex');
        sys_mimes.Add('texi=application/x-texinfo');
        sys_mimes.Add('texinfo=application/x-texinfo');
        sys_mimes.Add('tif=image/tiff');
        sys_mimes.Add('tiff=image/tiff');
        sys_mimes.Add('tr=application/x-troff');
        sys_mimes.Add('tsv=text/tab-separated-values');
        sys_mimes.Add('txt=text/plain');
        sys_mimes.Add('vcd=application/x-cdlink');
        sys_mimes.Add('vrml=model/vrml');
        sys_mimes.Add('wav=audio/x-wav');
        sys_mimes.Add('wbmp=image/vnd.wap.wbmp');
        sys_mimes.Add('wml=text/vnd.wap.wml');
        sys_mimes.Add('wmlc=application/vnd.wap.wmlc');
        sys_mimes.Add('wmls=text/vnd.wap.wmlscript');
        sys_mimes.Add('wrl=model/vrml');
        sys_mimes.Add('xbm=image/x-xbitmap');
        sys_mimes.Add('xls=application/vnd.ms-excel');
        sys_mimes.Add('xml=text/xml');
        sys_mimes.Add('xpm=image/x-xpixmap');
        sys_mimes.Add('xwd=image/x-xwindowdump');
        sys_mimes.Add('Z=application/x-compress');
        sys_mimes.Add('zip=application/zip');
        { default }
        sys_mimes.Add('*=text/html');

        line := sys_knpath + 'sys_mimes.txt';
        if FileExists(line) then
        begin
          list := TStringList.Create;
          try
            list.LoadFromFile(line);
            for index := 0 to list.Count - 1 do
            begin
              ext := Trim(__extractNameValue(list[index], mime));
              if ext <> '' then
              begin
                mime := Trim(mime);
                if mime <> '' then
                  sys_mimes.Values[LowerCase(ext)] := LowerCase(mime);
              end;
            end;
          finally
            list.Free;
          end;
        end;

        sys_mimes.Sorted := true;
      end;
    finally
      unlock_kernel;
    end;
  end;
  Result := sys_mimes;
end;

function __getMimeType(const FileName: string): string;
var
  ext: string;
begin
  ext := Trim(Copy(ExtractFileExt(FileName), 2, MaxInt));
  if ext <> '' then
  begin
    Result := __getMimeList.Values[ext];
    if Result <> '' then Exit;
  end;
  Result := __getMimeList.Values['*'];
end;

function __asCGI(const Param: PLseParam): KLiCGI;
begin
  Result := KLiCGI(__AsEngine(Param).CGI);
end;

function __getRSP(const Param: PLseParam; var RSP: KLiResponse): boolean;
var
  CGI: KLiCGI;
begin
  CGI := __asCGI(Param);
  if CGI <> nil then
    RSP := CGI.FResponse else
    RSP := nil;
  Result := (RSP <> nil);
end;

{ CGI }

procedure cgi_mode(const Param: PLseParam);
var
  CGI: KLiCGI;
begin
  CGI := __asCGI(Param);
  if CGI <> nil then
    lse_set_string(Param^.p_result, CGI.FMode) else
    lse_set_string(Param^.p_result, '');
end;

procedure cgi_encode(const Param: PLseParam);
begin
  lse_set_string(Param^.p_result, __httpEncode(__AsString(Param^.p_param[0]), false));
end;

procedure cgi_decode(const Param: PLseParam);
begin
  lse_set_string(Param^.p_result, __httpDecode(__AsString(Param^.p_param[0])));
end;

procedure cgi_parse(const Param: PLseParam);
var
  list: TStrings;
  L: KLiVarList;
begin
  L := KLiVarList.Create(__AsEngine(Param));
  L.SaveTo(Param^.p_result);
  list := TStringList.Create;
  try
    __parseQuery(__AsString(Param^.p_param[0]), list);
    L.AddStrings(list);
  finally
    list.Free;
  end;
end;

procedure cgi_arrstr(const Param: PLseParam);
var
  str: string;
  len, index, count: integer;
  MB: boolean;
begin
  str := __AsString(Param^.p_param[0]);
  len := Length(str);
  if (len > 0) and (Param^.p_count > 1) then
  begin
    count := __AsInt64(Param^.p_param[1]);
    if (count > 0) and (count < len) then
    begin
      MB := false;
      Dec(count, 3);
      index := 1;
      while index <= count do
      begin
        if str[index] in LeadBytes then
          MB := not MB else
          MB := false;
        Inc(index);
      end;
      if MB then Dec(index);
      if index > count then index := count;
      str := Copy(str, 1, index) + '...';
    end;
  end;
  lse_set_string(Param^.p_result, str);
end;

procedure cgi_notags(const Param: PLseParam);
var
  HTML: string;
begin
  HTML := __clearTags(__AsString(Param^.p_param[0]));
  lse_set_string(Param^.p_result, HTML);
end;

procedure cgi_encodeHTML(const Param: PLseParam);
begin
  lse_set_string(Param^.p_result, __htmlEncode(__AsString(Param^.p_param[0]), false));
end;

procedure cgi_decodeHTML(const Param: PLseParam);
begin
  lse_set_string(Param^.p_result, __htmlDecode(__AsString(Param^.p_param[0])));
end;

procedure cgi_pack(const Param: PLseParam);cdecl;
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    if __AsBool(Param^.p_param[0]) then
      RSP.PackTextHTML else
      RSP.ExecPack := true;
end;

procedure cgi_mimes(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  L := KLiVarList.Create(__AsEngine(Param));
  L.SaveTo(Param^.p_result);
  L.AddStrings(__getMimeList);
end;

procedure cgi_mime(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, __getMimeType(__AsString(Param^.p_param[0])));
end;

procedure cgi_get_status(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_integer(Param^.p_result, RSP.StatusCode);
end;

procedure cgi_set_status(const Param: PLseParam);
var
  RSP: KLiResponse;
  code: integer;
  text: string;
begin
  if __getRSP(Param, RSP) then
  begin
    code := __AsInt64(Param^.p_param[0]);
    if code <> RSP.StatusCode then
    begin
      text := __statusString(code);
      if text <> '' then
        if code <> 204 then
        begin
          RSP.StatusCode := __AsInt64(Param^.p_param[1]);
          RSP.ReasonString := text;
        end
        else RSP.NoContent;
    end;
  end;
end;

procedure cgi_get_reason(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_string(Param^.p_result, RSP.ReasonString);
end;

procedure cgi_set_reason(const Param: PLseParam);
var
  RSP: KLiResponse;
  text: string;
begin
  if __getRSP(Param, RSP) then
  begin
    text := Trim(__AsString(Param^.p_param[0]));
    if text = '' then
      text := __statusString(RSP.StatusCode);
    RSP.ReasonString := text;
  end;
end;

procedure cgi_gethv(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_string(Param^.p_result, RSP.GetHV(__AsString(Param^.p_param[0])));
end;

procedure cgi_sethv(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.SetHV(__AsString(Param^.p_param[0]), __AsString(Param^.p_param[1]));
end;

procedure cgi_get_ContentType(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_string(Param^.p_result, RSP.ContentType);
end;

procedure cgi_set_ContentType(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.ContentType := Trim(__AsString(Param^.p_param[0]));
end;

procedure cgi_reset(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then RSP.Reset;
end;

procedure cgi_headstr(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_string(Param^.p_result, RSP.HeaderString);
end;

procedure cgi_get_ContentLength(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_integer(Param^.p_result, RSP.ContentLength);
end;

procedure cgi_get_Content(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_string(Param^.p_result, RSP.Content);
end;

procedure cgi_set_Content(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.Content := __AsString(Param^.p_param[0]);
end;

procedure cgi_serveFile(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    lse_set_string(Param^.p_result,
      RSP.ServeFile(__AsString(Param^.p_param[0])));
end;

procedure cgi_cookieExists(const Param: PLseParam);cdecl;
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    lse_set_bool(Param^.p_result, cookie <> nil);
  end;
end;

procedure cgi_getCookie(const Param: PLseParam);cdecl;
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    if cookie <> nil then
      lse_set_string(Param^.p_result, cookie.FValue);
  end;
end;

procedure cgi_getCookieStr(const Param: PLseParam);cdecl;
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    if cookie <> nil then
      lse_set_string(Param^.p_result, cookie.HeaderValue);
  end;
end;

procedure cgi_setCookie(const Param: PLseParam);
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Add(__AsString(Param^.p_param[0]));
    cookie.Value := Trim(__AsString(Param^.p_param[1]));
  end;
end;

procedure cgi_setCookieDomain(const Param: PLseParam);
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    if cookie <> nil then
      cookie.Domain := Trim(__AsString(Param^.p_param[1]));
  end;
end;

procedure cgi_setCookiePath(const Param: PLseParam);
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    if cookie <> nil then
      cookie.Path := Trim(__AsString(Param^.p_param[1]));
  end;
end;

procedure cgi_setCookieExpires(const Param: PLseParam);
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    if cookie <> nil then
      cookie.Expires := __AsTime(Param^.p_param[1]);
  end;
end;

procedure cgi_setCookieSecure(const Param: PLseParam);
var
  RSP: KLiResponse;
  cookie: KLiCookie;
begin
  if __getRSP(Param, RSP) then
  begin
    cookie := RSP.FCookies.Find(__AsString(Param^.p_param[0]));
    if cookie <> nil then
      cookie.Secure := __AsBool(Param^.p_param[1]);
  end;
end;

procedure cgi_deleteCookie(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.FCookies.Remove(Trim(__AsString(Param^.p_param[0])));
end;

procedure cgi_clearCookies(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.FCookies.Clear;
end;

procedure cgi_redirect(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.Redirect(Trim(__AsString(Param^.p_param[0])));
end;

procedure cgi_noContent(const Param: PLseParam);
var
  RSP: KLiResponse;
begin
  if __getRSP(Param, RSP) then
    RSP.NoContent;
end;

{ KLiCGI }

procedure KLiCGI.ParseQueryString(const QUERY_STRING: string);
var
  next, base: pchar;
  line, data: string;
begin
  next := pchar(QUERY_STRING);
  while (next <> nil) and (next^ <> #0) do
  begin
    base := next;
    while not (next^ in [#0, '&']) do Inc(next);
    SetString(line, base, next - base);
    line := __extractNameValue(line, data);
    if line <> '' then
      AddRequest(line, __httpDecode(data));
    if next^ = '&' then Inc(next);
  end;
end;

procedure KLiCGI.LoadCookies;
var
  item, ID, value: string;
  list: TStrings;
  index: integer;
begin
  list := TStringList.Create;
  try
    value := Trim(lse_getenv('COOKIE'));
    if value = '' then
      value := Trim(lse_getenv('HTTP_COOKIE'));
    list.Text := __replaceAll(value, ';', sLineBreak);
    for index := 0 to list.Count - 1 do
    begin
      item := Trim(list[index]);
      ID := __extractNameValue(item, value);
      if ID = '' then
      begin
        ID := Trim(list[index]);
        value := '';
      end;
      if ID <> '' then
        FEngine.MainValues.SetStr('cookie' + ID, value);
    end;
  finally
    list.Free;
  end;
end;

procedure KLiCGI.InitMPFD(const Boundary: string);
const
  CRLEN = 2;
var
  size, blen, mlen, file_size: integer;
  line, iname, fname: string;
  base, next: pchar;
  cache: TMemoryStream;

  function get_line(var S: pchar; Count: integer): string;
  var
    P: pchar;
  begin
    P := S;
    while (Count > 0) and not (P^ in [#13, #10]) do
    begin
      Inc(P);
      Dec(Count);
    end;
    SetString(Result, S, P - S);
    S := P + CRLEN;
  end;

  function get_value(const S, ID: string; var Value: string): boolean;
  var
    endx, begx: integer;
  begin
    endx := Pos('; ' + ID + '="', LowerCase(S));
    if endx > 0 then
    begin
      Inc(endx, Length(ID) + 4);
      begx := endx;
      while S[endx] <> '"' do Inc(endx);
      Value := Copy(S, begx, endx - begx);
      Result := true;
    end
    else Result := false;
  end;

  function save_file(buf: pointer; count: integer): string;
  var
    outs: TFileStream;
  begin
    outs := TFileStream.Create(lse_veryPD(sys_tmpath + __genid), fmCreate);
    try
      Result := outs.FileName;
      AddTempFile(Result);
      outs.Write(buf^, count);
    finally
      outs.Free;
    end;
  end;

begin
  blen := Length(Boundary);
  mlen := (blen + CRLEN) * 2 + 8;
  size := __parseInt(pchar(lse_getenv('CONTENT_LENGTH')));
  if size <= mlen then Exit;

  cache := TMemoryStream.Create;
  try
    cache.Size := size;
    if size = ReadBuffer(cache.Memory, size) then
    begin
      base := cache.Memory;
      while (size > mlen) and (base <> nil) do
      begin
        Dec(size, blen + CRLEN);
        Inc(base, blen + CRLEN);
        next := __pos(base, size, pchar(Boundary), blen, false);
        if next <> nil then
        begin
          line := get_line(base, next - base);
          if get_value(line, 'name', iname) then
          begin
            while get_line(base, next - base) <> '' do
            begin
              {do nothing}
            end;
            if get_value(line, 'filename', fname) then
            begin
              fname := ExtractFileName(lse_veryPD(fname));
              if fname <> '' then
              begin
                file_size := next - base - CRLEN;
                AddRequest(iname + '->file', save_file(base, file_size));
                AddRequest(iname + '->size', file_size);
                AddRequest(iname, fname);
              end;
            end
            else
            begin
              SetString(line, base, next - base - CRLEN);
              if line <> '' then
                AddRequest(iname, line);
            end;
          end;
        end;
        base := next;
      end;
    end;
  finally
    cache.Free;
  end;
end;

procedure KLiCGI.AddRequest(const ID, VALUE: string);
begin
  if ID <> '' then
    FEngine.MainValues.SetStr('request.' + ID, VALUE);
end;

constructor KLiCGI.Create(AEngine: KLiEngine);
const
  MFD = 'multipart/form-data';
  BDR = 'boundary=';
var
  value, boundary: string;
  index: integer;
begin
  try
    FActionParsed := false;
    FHasError := false;
    FEngine := AEngine;
    FEngine.CGI := Self;

    FResponse := KLiResponse.Create(Self);
    FResponseStream := lse_wrap_stream(FResponse.FStream, false);
    lse_stream_addref(FResponseStream);
    FEngine.Output := FResponseStream;

    FTempFiles := TStringList.Create;
    LoadCookies;

    FMode := UpperCase(lse_getenv('REQUEST_METHOD'));
    FShowResponseHeader := (FMode <> '') or
      (FEngine.Arguments.IndexOf('--show-response-header') >= 0);

    ParseQueryString(lse_getenv('QUERY_STRING'));

    if FMode = 'POST' then
    begin
      value := lse_getenv('CONTENT_TYPE');
      if AnsiSameText(MFD, Copy(value, 1, Length(MFD))) then
      begin
        index := Pos(BDR, LowerCase(value));
        if index > 0 then
        begin
          value := Copy(value, index + Length(BDR), MaxInt);
          index := Pos(';', value);
          if index > 0 then
            value := Copy(value, 1, index - 1);
          boundary := Trim(value);
          if boundary <> '' then
            InitMPFD('--' + boundary);
        end;
      end
      else
      begin
        index := __parseInt(pchar(lse_getenv('CONTENT_LENGTH')));
        if index > 0 then
        begin
          SetLength(value, index);
          if index = ReadBuffer(pchar(value), index) then
            ParseQueryString(value);
        end;
      end;
    end;
  except
    WriteText(__httpEncode(lse_exception_str, true));
  end;
end;

destructor KLiCGI.Destroy;
var
  S: string;
begin
  try
    FEngine.CGI := nil;
    FEngine.Output := nil;
    if FShowResponseHeader then
    begin
      S := FResponse.HeaderString;
      WriteText(S);
    end;
    if not FResponse.IsNoContent then
      if FResponse.FContentFile = '' then
      begin
        if FResponse.ExecPack then
          FResponse.PackTextHTML;
        FResponse.FStream.Position := 0;
        WriteStream(FResponse.FStream);
      end
      else WriteFile(FResponse.FContentFile);
  finally
    RemoveTempFiles;
    FreeAndNil(FTempFiles);
    lse_stream_close(FResponseStream);
    lse_stream_release(FResponseStream);
    FResponseStream := nil;
    FreeAndNil(FResponse);
  end;
  inherited;
end;

procedure KLiCGI.RemoveTempFiles;
var
  index: integer;
begin
  for index := FTempFiles.Count - 1 downto 0 do
  begin
    SysUtils.DeleteFile(FTempFiles[index]);
    FTempFiles.Delete(index);
  end;
end;

procedure KLiCGI.AddRequest(const ID: string; Value: int64);
begin
  if ID <> '' then
    FEngine.MainValues.SetInt64('request.' + ID, VALUE);
end;

procedure KLiCGI.AddTempFile(const FileName: string);
begin
  FTempFiles.Add(FileName);
end;

function KLiCGI.Read(const Buf: pointer; Count: integer): integer;
begin
  Result := lse_stream_read(FEngine.Input, Buf, Count);
end;

function KLiCGI.ReadBuffer(const Buf: pointer; Count: integer): integer;
var
  size: integer;
  base: pchar;
begin
  Result := 0;
  if Count > 0 then
  begin
    base := Buf;
    repeat
      size := Read(base, Count);
      if size > 0 then
      begin
        Inc(base, size);
        Dec(Count, size);
        Inc(Result, size);
      end
      else Break;
    until Count = 0;
  end;
end;

procedure KLiCGI.Write(Buf: pointer; Count: integer);
begin
  lse_stream_write(FEngine.Output, Buf, Count);
end;

procedure KLiCGI.WriteText(const Text: string);
begin
  Write(pchar(Text), Length(Text));
end;

procedure KLiCGI.WriteStream(AStream: TStream);
var
  buffer: array[0..1023] of char;
  bytes: integer;
begin
  bytes := AStream.Read(buffer, sizeof(buffer));
  while bytes > 0 do
  begin
    Write(@buffer[0], bytes);
    bytes := AStream.Read(buffer, sizeof(buffer));
  end;
end;

procedure KLiCGI.WriteFile(const FileName: string);
var
  fin: TFileStream;
begin
  fin := TFileStream.Create(lse_veryPD(FileName), fmShareDenyWrite);
  try
    WriteStream(fin);
  finally
    fin.Free;
  end;
end;

{ KLiCookie }

constructor KLiCookie.Create(AList: KLiCookieList);
begin
  FList := AList;
  FList.FCookies.Add(Self);
  IncRefcount;
  FExpires := -1;
end;

procedure KLiCookie.Delete;
begin
  if FList <> nil then
  begin
    FList.FCookies.Remove(Self);
    FList := nil;
  end;
  DecRefcount;
end;

destructor KLiCookie.Destroy;
begin
  if FList <> nil then
    FList.FCookies.Remove(Self);
  inherited;
end;

function KLiCookie.HeaderValue: string;
begin
  Result := Format('%s=%s', [__httpEncode(FName, false), __httpEncode(FValue, false)]);
  if FDomain <> '' then
    Result := Format('%s; domain=%s', [Result, FDomain]);
  if FPath <> '' then
    Result := Format('%s; path=%s', [Result, FPath]);
  if FExpires > -1 then
    Result := Format('%s; expires=%s', [Result, lse_encode_GMT(FExpires)]);
  if FSecure then
    Result := Result + '; secure';
end;

{ KLiCookieList }

function KLiCookieList.Add(const Name: string): KLiCookie;
var
  ID: string;
begin
  ID := Trim(Name);
  if ID <> '' then
  begin
    Result := Find(ID);
    if Result = nil then
    begin
      Result := KLiCookie.Create(Self);
      Result.Name := ID;
    end;
  end
  else Result := nil;
end;

procedure KLiCookieList.Clear;
var
  index: integer;
begin
  for index := FCookies.Count - 1 downto 0 do
    Delete(index);
end;

constructor KLiCookieList.Create;
begin
  FCookies := TList.Create;
end;

procedure KLiCookieList.Delete(index: integer);
var
  cookie: KLiCookie;
begin
  cookie := FCookies[index];
  FCookies.Delete(index);
  cookie.FList := nil;
  cookie.DecRefcount;
end;

destructor KLiCookieList.Destroy;
begin
  FreeAndNil(FCookies);
  inherited;
end;

function KLiCookieList.Find(const Name: string): KLiCookie;
var
  index: integer;
begin
  index := IndexOf(Name);
  if index >= 0 then
    Result := GetCookie(index) else
    Result := nil;
end;

function KLiCookieList.GetCookie(index: integer): KLiCookie;
begin
  Result := FCookies[index];
end;

function KLiCookieList.GetCount: integer;
begin
  Result := FCookies.Count;
end;

function KLiCookieList.IndexOf(const Name: string): integer;
var
  index: integer;
begin
  for index := 0 to GetCount - 1 do
    if AnsiSameText(Name, GetCookie(index).Name) then
    begin
      Result := index;
      Exit;
    end;
  Result := -1;
end;

procedure KLiCookieList.Remove(const Name: string);
var
  index: integer;
begin
  index := IndexOf(Name);
  if index >= 0 then Delete(index);
end;

{ KLiResponse }

procedure KLiResponse.SetHV(const ID, value: string);
var
  name: string;
begin
  name := Trim(ID);

  if (name = '') or
     AnsiSameText(name, 'status') or
     AnsiSameText(name, 'content-length') or
     AnsiSameText(name, 'set-cookie') or
     AnsiSameText(name, 'content')
     then Exit;

  if AnsiSameText(name, 'content-type') then
  begin
    SetContentType(value);
    Exit;
  end;

  FHeadValues.Values[name] := Trim(value);
end;

constructor KLiResponse.Create(ACGI: KLiCGI);
begin
  FCGI := ACGI;
  FStream := TMemoryStream.Create;
  FHeadValues := TStringList.Create;
  FCookies := KLiCookieList.Create;
  Reset;
end;

destructor KLiResponse.Destroy;
begin
  Reset;
  FreeAndNil(FStream);
  FreeAndNil(FHeadValues);
  FreeAndNil(FCookies);
  inherited;
end;

function KLiResponse.GetHV(const ID: string): string;
var
  name: string;
begin
  name := Trim(ID);
  if name <> '' then
    Result := FHeadValues.Values[name] else
    Result := '';
end;

function KLiResponse.GetContent: string;
var
  base: pchar;
begin
  if not IsNoContent and (FStream.Size > 0) then
  begin
    base := FStream.Memory;
    SetString(Result, base, FStream.Size);
  end
  else Result := '';
end;

function KLiResponse.GetContentLength: integer;
var
  sr: TSearchRec;
begin
  if FStatusCode = 204 then Result := 0 else
  if FContentFile <> '' then
  begin
    Result := 0;
    if SysUtils.FindFirst(FContentFile, faAnyFile, sr) = 0 then
    try
      if (sr.Attr and faDirectory) = 0 then
        Result := sr.Size;
    finally
      SysUtils.FindClose(sr);
    end;
  end
  else Result := FStream.Size;
end;

function KLiResponse.HeaderString: string;
const
  RHV: array[0..9] of string = (
    'Location', 'Allow', 'Derived-From', 'Date', 'Expires', 'Last-Modified',
    'Title', 'WWW-Authenticate', 'Content-Version', 'Content-Encoding'
  );
var
  headers, name, value: string;
  index: Integer;

  function is_RHV(const ID: string): boolean;
  var
    X: integer;
  begin
    for X := 0 to High(RHV) do
      if AnsiSameText(ID, RHV[X]) then
      begin
        Result := true;
        Exit;
      end;
    Result := false;
  end;

  procedure put_value(const ID: string);
  var
    S: string;
  begin
    S := FHeadValues.Values[ID];
    if S <> '' then
      headers := headers + Format('%s: %s'#13#10, [ID, S]);
  end;

  procedure set_value(const Item, FormatStr: string);
  begin
    if Item <> '' then
      headers := headers + Format(FormatStr, [Item]);
  end;

begin
  headers := Format('Status: %d %s'#13#10, [FStatusCode, FReasonString]);
  put_value('Location');
  put_value('Allow');
  for index := 0 to FCookies.Count - 1 do
    set_value(FCookies[index].HeaderValue, 'Set-Cookie: %s'#13#10);
  put_value('Derived-From');
  put_value('Date');
  put_value('Expires');
  put_value('Last-Modified');
  put_value('Title');
  put_value('WWW-Authenticate');
  for index := 0 to FHeadValues.Count - 1 do
  begin
    name := Trim(__extractNameValue(FHeadValues[index], value));
    if (name <> '') and not is_RHV(name) then
    begin
      value := Trim(value);
      if value <> '' then
        headers := headers + name + ': ' + value + #13#10;
    end;
  end;
  put_value('Content-Version');
  put_value('Content-Encoding');
  set_value(FContentType, 'Content-Type: %s'#13#10);
  set_value(IntToStr(GetContentLength), 'Content-Length: %s'#13#10);
  Result := headers + 'Content:'#13#10#13#10;
end;

function KLiResponse.IsNoContent: boolean;
begin
  Result := (FStatusCode = 204);
end;

function KLiResponse.IsTextHTML: boolean;
begin
  Result := AnsiSameText('text/html', FContentType);
end;

procedure KLiResponse.PackTextHTML;
begin
  if (FStream <> nil) and IsTextHTML then
    FStream.Size := __packHTML(pchar(FStream.Memory), FStream.size);
end;

procedure KLiResponse.Redirect(const URI: string);
const
  DOCMOVED = '<html><head><title>Document Moved 302</title></head>' +
             '<body><h1>Object Moved</h1><hr>' +
             'This Object may be found <a HREF="%s">here.</a><br><br>' +
             '</body></html>';
begin
  SetHV('Location', URI);
  FStatusCode := 302;
  FReasonString := __statusString(FStatusCode);
  FContentType := 'text/html';  { do not localize }
  SetContent(Format(DOCMOVED, [URI]));
end;

procedure KLiResponse.NoContent;
begin
  Reset;
  FStatusCode := 204;
  FReasonString := __statusString(FStatusCode);
end;

procedure KLiResponse.Reset;
begin
  FStatusCode := 200;
  FReasonString := __statusString(FStatusCode);
  FHeadValues.Clear;
  FStream.Size := 0;
  FCookies.Clear;
  FContentFile := '';
  FContentType := 'text/html';
end;

function KLiResponse.ServeFile(const FileName: string): string;
begin
  Result := FContentFile;
  FStream.Size := 0;
  FContentFile := lse_expand_fname(lse_veryPD(FileName));
end;

procedure KLiResponse.SetContent(const Value: string);
begin
  FStream.Size := 0;
  if not IsNoContent then
  begin
    FStream.Write(pointer(Value)^, Length(Value));
    FContentFile := '';
  end;
end;

procedure KLiResponse.SetContentType(const Value: string);
begin
  FContentType := Trim(Value);
end;

end.
