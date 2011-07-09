{==============================================================================}
{        UNIT: lse_funcs                                                       }
{ DESCRIPTION: classes and functions used by kernel                            }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/01/21                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_funcs;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lseu
  {$IFDEF WINDOWS},Windows{$ENDIF};

const
  InWindows      = {$IFDEF WINDOWS}true{$ELSE}false{$ENDIF};
  AlphaChar      = ['A'..'Z', 'a'..'z'];
  DigitChar      = ['0'..'9'];
  AlnumChar      = AlphaChar + DigitChar;
  IDChar         = AlnumChar + ['_'];
  IDHeadChar     = AlphaChar + ['_'];
  ConfigHeadChar = IDChar + ['-', '.', '[', ']', '(', ')', '<', '>'];
  ConfigChar     = ConfigHeadChar + [' ', ':', ','];
  LetterChar     = IDChar;
  PunctChar      = ['!'..'~'] - AlnumChar;
  CntrlChar      = [#$00..#$1F, #$7F];
  SpaceChar      = [#$09, #$0A, #$0C, #$0D, #$20];
  QuoteChar      = ['"', ''''];
  HexChar        = ['A'..'F', 'a'..'f'] + DigitChar;
  AllChar        = [#0..#255];
  NoneZeroChar   = AllChar - [#0];
  LiBufferSize   = 4096;
  LB             = sLineBreak;
  CR             = #13#10;
  BoolText: array[boolean] of pchar = ('false', 'true');
  AaDistance     = Ord('a') - Ord('A');

type
  KLiByteList = array[0..MaxInt - 1] of byte;
  PLiByteList = ^KLiByteList;

  KLiCharList = array[0..MaxInt - 1] of char;
  PLiCharList = ^KLiCharList;

  KLiCharSet = set of Char;
  PLiCharSet = ^KLiCharSet;

  KLiExtInt = (eiNone, eiInt, eiExt);

  { KLiList }

  KLiList = class(TList)
  private
    FRefcount: integer;
    FOnDestroy: TNotifyEvent;
  public
    destructor Destroy;override;
    procedure TailRemove(Item: pointer);
    function IncRefcount: integer;virtual;
    function DecRefcount: integer;virtual;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  { KLiObject }

  KLiObject = TLseObject;

  { KLiNameObject }

  KLiNameObject = class(KLiObject)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  { KLiNameHashed }

  PLiNameItem = ^RLiNameItem;
  RLiNameItem = packed record
    ni_next: PLiNameItem;
    ni_nobj: KLiNameObject;
  end;

  KLiNameHashed = class(KLiObject)
  private
    FBuckets: array of PLiNameItem;
    FSize: cardinal;
    FCount: cardinal;
  protected
    function HashOf(const Key: string): cardinal;
    function NewItem: PLiNameItem;
    procedure FreeItem(Item: PLiNameItem);
    function FindItem(const Key: string): PLiNameItem;
  public
    constructor Create(Size: cardinal);
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(const Key: string);
    procedure Put(AObj: KLiNameObject);
    function Get(const Key: string): KLiNameObject;
  end;

  { KLiHashTable }

  PLiHashItem = ^RLiHashItem;
  RLiHashItem = packed record
    hi_next: PLiHashItem;
    hi_key : pointer;
    hi_data: pointer;
  end;

  RLiHashPair = packed record
    hp_head: PLiHashItem;
    hp_tail: PLiHashItem;
  end;
  PLiHashPair = ^RLiHashPair;
  
  KLiEnumKeyData = procedure(const Key: string; Value, Param: pointer) of object;
  
  KLiHashTable = class(KLiObject)
  private
    FBucketList: array of RLiHashPair;
    FBucketSize: integer;
    FIgnoreCase: boolean;
    FCount: integer;
  protected
    function HashOf(const Key: string): cardinal;
    function NewItem: PLiHashItem;virtual;
    procedure FreeItem(Item: PLiHashItem);virtual;
    function MatchKey(const Key, ID: string): boolean;
    procedure DoPut(const Key: string; Value: pointer);
    function DoGet(const Key: string): pointer;
  public
    constructor Create(Size: integer);
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(const Key: string);
    function Find(const Key: string): PLiHashItem;
    function IsSet(const Key: string): boolean;
    function ListKey(List: TStrings): integer;
    function ListData(List: TList): integer;
    function EnumKeyData(Proc: KLiEnumKeyData; Param: pointer): integer;
    property ItemCount: integer read FCount;
    property IgnoreCase: boolean read FIgnoreCase write FIgnoreCase;
  end;

  { KLiMD5 }
  
  pardinal = ^cardinal;

  KLiMD5 = class(KLiObject)
  private
    FBuffer: array[0..15] of cardinal;
    FA, FB, FC, FD: cardinal;
    PA, PB, PC, PD: pardinal;
    procedure Init;
    procedure Transform;
    procedure FF(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    procedure GG(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    procedure HH(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    procedure II(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    function ROL(A: cardinal; Amount: byte): cardinal;
    function GetDigest: string;
  public
    function sumBuf(Abuf: pchar; count: integer): string;
    function sumStr(const S: string): string;
    function sumStream(Stream: TStream): string;
    function sumFile(const fname: string): string;
  end;

{-----------------------------------------------------------------------
( F_NAME: __hexcv
(
( F_DESC: get value of HEX charactor
( 
( F_ARGS: ch: char - HEX charactor
(
( F_TYPE: integer - 0..15
(
( EXCEPT:
(----------------------------------------------------------------------}
function __hexcv(ch: char): integer;

{-----------------------------------------------------------------------
( F_NAME: __parseExtInt
(
( F_DESC: extract number from source code
(
( F_ARGS: var S: pchar - source code
(         var iv: int64 - integer value
(         var ei: KLiExtInt - value type
( 
( F_TYPE: extended - float value
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __parseExtInt(var S: pchar; var iv: int64; var ei: KLiExtInt): extended;
function __parseExt(S: pchar): extended;
function __parseInt(S: pchar): int64;

{-----------------------------------------------------------------------
( F_NAME: __parseChar
( 
( F_DESC: parse charactor from source code
( 
( F_ARGS: var S: pchar - source code
(         var ch: char - result char
( 
( F_TYPE: boolean - succeeded?
(
( EXCEPT:
(----------------------------------------------------------------------}
function __parseChar(var S: pchar; var ch: char): boolean;

{-----------------------------------------------------------------------
( F_NAME: __parseStr
(
( F_DESC: parse string from source code
(
( F_ARGS: var S: pchar - source code
(         desti: TStream - desti stream
(         allow_esc_char: boolean - allow escape?
(
( F_TYPE: boolean - succeeded?
(
( EXCEPT:
(----------------------------------------------------------------------}
function __parseStr(var S: pchar; desti: TStream; allow_esc_char: boolean): boolean;

{-----------------------------------------------------------------------
( F_NAME: __parseConfig
(
( F_DESC: get effective part of configure string
(
( F_ARGS: const S: string - source string
(         var ID: string - configure ID
(         var value: string - configure value
(
( F_TYPE: boolean - return true if get configure
(
( EXCEPT:
(----------------------------------------------------------------------}
function __parseConfig(const S: string; var ID, value: string): boolean;
function __deleteConfigComment(const S: string): string;

{-----------------------------------------------------------------------
( F_NAME: __extractNameValue
( 
( F_DESC: extract name and value
( 
( F_ARGS: const S: string - source string
(         var V: string - value
(         const separator: string - separate name and value
(
( F_TYPE: string - name
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __extractNameValue(const S: string; var V: string; const separator: string = '='): string;
function __extractName(const S: string; const separator: string = '='): string;
function __extractValue(const S: string; const separator: string = '='): string;

{-----------------------------------------------------------------------
( F_NAME: __IsIDCStr
( 
( F_DESC: is valid identity string?
( 
( F_ARGS: S: pchar - source string
(         Head: KLiCharSet - head char set
( 
( F_TYPE: boolean - true if is valid identity string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __IsIDStr(S: pchar; Head: KLiCharSet = IDHeadChar): boolean;

{-----------------------------------------------------------------------
( F_NAME: __IsIDHead
(
( F_DESC: is valid identity head char?
( 
( F_ARGS: C: char
( 
( F_TYPE: boolean - true if C is valid identity head char
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __IsIDHead(C: char): boolean;

{-----------------------------------------------------------------------
( F_NAME: __newString
( 
( F_DESC: setup pascal string
( 
( F_ARGS: Source: pchar - string buffer
(         Count: integer - string length
( 
( F_TYPE: string - new pascal string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __newString(Source: pchar; Count: integer): string;

{-----------------------------------------------------------------------
( F_NAME: __check
( 
( F_DESC: throw exception if condition is false
(
( F_ARGS:           ok: boolean   - condition
(            const msg: string    - exception message
( 
( F_TYPE:
( 
( EXCEPT: call lse_error to throw the exception
(----------------------------------------------------------------------}
procedure __check(ok: boolean; const msg: string);

{-----------------------------------------------------------------------
( F_NAME: __freeAndNil
( 
( F_DESC: free object and set pointer to nil
( 
( F_ARGS: var obj - object address
( 
( F_TYPE:
( 
( EXCEPT:
(----------------------------------------------------------------------}
procedure __freeAndNil(var obj);

{-----------------------------------------------------------------------
( F_NAME: __nilWhenSame
( 
( F_DESC: set nil when the object is at the specified address
( 
( F_ARGS: var obj - an object
(         const addr: pointer - object address
(
( F_TYPE:
(
( EXCEPT:
(----------------------------------------------------------------------}
procedure __nilWhenSame(var obj; const addr: pointer);

{-----------------------------------------------------------------------
( F_NAME: __zero
( 
( F_DESC: fill buffer with #0
( 
( F_ARGS:   buf: pointer - buffer address
(         count: integer - buffer size
( 
( F_TYPE: pointer - buffer address
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __zero(buf: pointer; count: integer): pointer;

{-----------------------------------------------------------------------
( F_NAME: __fullPath
( 
( F_DESC: fully expand relative file path
(
( F_ARGS: const path: string - relative file path
(
( F_TYPE: string - full file path
(
( EXCEPT:
(----------------------------------------------------------------------}
function __fullPath(const path: string): string;

{-----------------------------------------------------------------------
( F_NAME: __sameFileName
( 
( F_DESC: compare file name
( 
( F_ARGS: const F1: string - file name 1
(         const F2: string - file name 2
( 
( F_TYPE: boolean - true if is the same file
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __sameFileName(const F1, F2: string): boolean;

{-----------------------------------------------------------------------
( F_NAME: __coinit
( 
( F_DESC: init window's COM library
( 
( F_ARGS: 
( 
( F_TYPE:
(
( EXCEPT:
(----------------------------------------------------------------------}
procedure __coinit;

{-----------------------------------------------------------------------
( F_NAME: __bufComp
(
( F_DESC: compare buffer
(
( F_ARGS: B1: pchar - buffer 1
(         L1: integer - length of buffer 1
(         B2: pchar - buffer 2
(         L2: integer - length of buffer 2
(         IgnoreCase: boolean
(
( F_TYPE: integer - 0:equal  <0:less  >0:more
(
( EXCEPT:
(----------------------------------------------------------------------}
function __bufComp(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): integer;
function __strComp(const S1, S2: string; IgnoreCase: boolean): integer;

{-----------------------------------------------------------------------
( F_NAME: __bufSame
(
( F_DESC: is buffer same?
(
( F_ARGS: B1: pchar - buffer 1
(         L1: integer - length of buffer 1
(         B2: pchar - buffer 2
(         L2: integer - length of buffer 2
(         IgnoreCase: boolean
(
( F_TYPE: boolean - true if same
(
( EXCEPT:
(----------------------------------------------------------------------}
function __bufSame(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): boolean;
function __strSame(const S1, S2: string; IgnoreCase: boolean): boolean;

{-----------------------------------------------------------------------
( F_NAME: __upper | __lower
(
( F_DESC: convert buffer to lower or upper case
(
( F_ARGS: buf: pchar - buffer address
(         count: integer - buffer size
(
( F_TYPE:
(
( EXCEPT:
(----------------------------------------------------------------------}
procedure __upper(buf: pchar; count: integer);
procedure __lower(buf: pchar; count: integer);

{-----------------------------------------------------------------------
( F_NAME: __locase | __upcase
(
( F_DESC: convert ch to lower or upper case
(
( F_ARGS: ch: char - source char
(
( F_TYPE: char
(
( EXCEPT:
(----------------------------------------------------------------------}
function __locase(ch: char): char;
function __upcase(ch: char): char;

{-----------------------------------------------------------------------
( F_NAME: __countTab
( 
( F_DESC: count Tab char in buffer
( 
( F_ARGS:  buf: pchar - buffer address
(          count: integer - buffer size
(          var LCount: integer - left count
(          var MCount: integer - middle count
(          var RCount: integer - right count
( 
( F_TYPE: integer - Tab char count
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __countTab(buf: pchar; count: integer; var LCount, MCount, RCount: integer): integer;

{-----------------------------------------------------------------------
( F_NAME: __pos
( 
( F_DESC: get postion of sub buffer
( 
( F_ARGS: this: pchar - source buffer
(         thisLen: integer - source buffer length
(         patten: pchar - sub buffer
(         pattenLen: pchar - sub buffer length
(         IgnoreCase: boolean
(
( F_TYPE: pchar - sub buffer address
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __pos(this: pchar; thisLen: integer;
  patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;

{-----------------------------------------------------------------------
( F_NAME: __lastPos
( 
( F_DESC: get last postion of sub buffer
( 
( F_ARGS: this: pchar - source buffer
(         thisLen: integer - source buffer length
(         patten: pchar - sub buffer
(         pattenLen: pchar - sub buffer length
(         IgnoreCase: boolean
( 
( F_TYPE: pchar - sub buffer address
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __lastPos(this: pchar; thisLen: integer;
  patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;

{-----------------------------------------------------------------------
( F_NAME: __fileText
( 
( F_DESC: load file text
( 
( F_ARGS: const FileName: string - file name
( 
( F_TYPE: string - file text
( 
( EXCEPT: 
(----------------------------------------------------------------------}
function __fileText(const FileName: string): string;

{-----------------------------------------------------------------------
( F_NAME: __encodeUTF8 | __decodeUTF8
(
( F_DESC: encode UNICODE string to UTF8, or on the contrary
( 
( F_ARGS: S: string - source string
(
( F_TYPE: string - result
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __encodeUTF8(const S: string): string;
function __decodeUTF8(const S: string): string;

{-----------------------------------------------------------------------
( F_NAME: __encodeS | __decodeS
(
( F_DESC: encode/decode string value
(
( F_ARGS: AStr: string - source string
(
( F_TYPE: string - result
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __encodeS(const AStr: string; IncLocalBytes: boolean): string;
function __decodeS(const S: string): string;

{-----------------------------------------------------------------------
( F_NAME: __strToComma
( 
( F_DESC: convert normal string to comma format
( 
( F_ARGS: const AStr: string - normal string
( 
( F_TYPE: string - comma string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __strToComma(const AStr: string): string;

{-----------------------------------------------------------------------
( F_NAME: __hashof
(
( F_DESC: calculate hash value of a string
( 
( F_ARGS: const Key: string - source string
(
( F_TYPE: cardinal - hash value
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __hashof(const Key: string): cardinal;

{-----------------------------------------------------------------------
( F_NAME: __fullFileName
( 
( F_DESC: fully expand relative file name
( 
( F_ARGS: const FileName: string
(         const BasePath: string
( 
( F_TYPE: string - full file name
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __fullFileName(const FileName, BasePath: string): string;

{-----------------------------------------------------------------------
( F_NAME: __IsRelativeFileName
( 
( F_DESC: test if is a relative file name
(
( F_ARGS: const FileName: string
(
( F_TYPE: boolean - return true if is relative file name
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __IsRelativeFileName(const FileName: string): boolean;

{-----------------------------------------------------------------------
( F_NAME: __strToFileMode
( 
( F_DESC: decode open file mode string:
(           c  --> create
(           r  --> read share, deny write
(           rw --> read write
(           w  --> write
( 
( F_ARGS: const openMode: string - mode string
(           var fileMode: word - result open mode
(           var R: boolean - read?
(           var W: boolean - write?
( 
( F_TYPE: boolean - succeeded?
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __strToFileMode(const openMode: string; var fileMode: word;
                         var R, W: boolean): boolean;

{-----------------------------------------------------------------------
( F_NAME: __getText
( 
( F_DESC: get TStrings text
( 
( F_ARGS: list: TStrings - string list
(         const eol: string - line break
( 
( F_TYPE: string - result text
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __getText(list: TStrings; const eol: string): string;

{-----------------------------------------------------------------------
( F_NAME: __setText
( 
( F_DESC: set TStrings text
( 
( F_ARGS: list: TStrings - string list
(         const text: string - source string
(         const eol: string - line break
(         IgnoreCase: boolean - 
( 
( F_TYPE: 
( 
( EXCEPT:
(----------------------------------------------------------------------}
procedure __setText(list: TStrings; const text, eol: string; IgnoreCase: boolean);

{-----------------------------------------------------------------------
( F_NAME: __inCharSet
( 
( F_DESC: is buffer chars in specified char set
( 
( F_ARGS: S: pchar - buffer address
(         Len: integer - buffer length
(         Chars: KLiCharSet - char set
( 
( F_TYPE: boolean - true if all the buffer char in range
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __inCharSet(S: pchar; Len: integer; Chars: KLiCharSet): boolean;overload;
function __inCharSet(S: pchar; Chars: KLiCharSet): boolean;overload;

{-----------------------------------------------------------------------
( F_NAME: __IsYMD
( 
( F_DESC: is valid YMD?
( 
( F_ARGS: day: integer - yyyymmdd format
( 
( F_TYPE: boolean - true if the day exists
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __IsYMD(day: integer): boolean;

function __decodeYMD(ymd: integer; var y, m, d: integer): boolean;
function __nextYMD(ymd, offset: integer): integer;
function __prevYMD(ymd, offset: integer): integer;
function __betweenYMD(ymd1, ymd2: integer): integer;
function __weeksBetweenYMD(ymd1, ymd2: integer): integer;

function __decodeYM(ym: integer; var y, m: integer): boolean;
function __nextYM(ym, offset: integer): integer;
function __prevYM(ym, offset: integer): integer;

{-----------------------------------------------------------------------
( F_NAME: __skipch
(
( F_DESC: skip all starting charactors in CharSet
(
( F_ARGS: const S: pchar - C string
(         CharSet: KLiCharSet - charactors to be skiped
(
( F_TYPE: pchar - new position
(
( EXCEPT:
(----------------------------------------------------------------------}
function __skipch(const S: pchar; CharSet: KLiCharSet): pchar;

{-----------------------------------------------------------------------
( F_NAME: __seekch
(
( F_DESC: skip to charactor in CharSet
(
( F_ARGS: const S: pchar - C string
(         CharSet: KLiCharSet - charactors to be seeked
(
( F_TYPE: pchar - new position
(
( EXCEPT:
(----------------------------------------------------------------------}
function __seekch(const S: pchar; CharSet: KLiCharSet): pchar;

{-----------------------------------------------------------------------
( F_NAME: __replaceAll
(
( F_DESC: replace al OldStr with NewStr ignore case
(
( F_ARGS: const S: string - source string
(         const OldStr: string - sub string to be replaced
(         const NewStr: string - new sub string
(
( F_TYPE: string
(
( EXCEPT:
(----------------------------------------------------------------------}
function __replaceAll(const S, OldStr, NewStr: string): string;

{-----------------------------------------------------------------------
( F_NAME: __encodeHTML
( 
( F_DESC: encode text to HTML code
( 
( F_ARGS: Buf: pchar - source buffer
(         Count: integer - buffer length
(         TranslateMBC: boolean - translate multibyte chinese?
( 
( F_TYPE: string - HTML code
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __encodeHTML(Buf: pchar; Count: integer; TranslateMBC: boolean): string;
function __decodeHTML(Buf: pchar; Count: integer): string;

{-----------------------------------------------------------------------
( F_NAME: __packHTML
( 
( F_DESC: pack HTML source
( 
( F_ARGS: const HTML: pchar - HTML source code
(         Count: integer - source length
(
( F_TYPE: integer - result length
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __packHTML(const HTML: pchar; Count: integer): integer;

{-----------------------------------------------------------------------
( F_NAME: __fullDir
( 
( F_DESC: expand relative directory
( 
( F_ARGS: const dir: string - source directory
( 
( F_TYPE: string - full directory name
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __fullDir(const dir: string): string;

{-----------------------------------------------------------------------
( F_NAME: __copyFile
(
( F_DESC: copy file
(
( F_ARGS: const Source: string - source file name
(         const Desti: string - desti file name
(
( F_TYPE: boolean - succeeded?
(
( EXCEPT:
(----------------------------------------------------------------------}
function __copyFile(const Source, Desti: string): boolean;

{-----------------------------------------------------------------------
( F_NAME: __IsFile | __IsDir
(
( F_DESC: check if file/directory exists
(
( F_ARGS: const Source: string - file/directory name
(
( F_TYPE: boolean - true if exits
(
( EXCEPT:
(----------------------------------------------------------------------}
function __IsFile(const Source: string): boolean;
function __IsDir(const Source: string): boolean;

{----------------------------------------------------------------------)
( F_NAME: __releaseSOList
( 
( F_DESC: free TStringList and its objects
( 
( F_ARGS: var list: TStringList - string list
( 
( F_TYPE:
( 
( EXCEPT:
(----------------------------------------------------------------------}
procedure __releaseSOList(var list: TStringList);

{-----------------------------------------------------------------------
( F_NAME: __removeFrom
( 
( F_DESC: delete string-item from string list
( 
( F_ARGS: list: TStrings - string list
(         item: pointer - item object
( 
( F_TYPE: boolean - taken place?
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __removeFrom(list: TStrings; item: pointer): boolean;
function __removeAllFrom(list: TStrings; item: pointer): integer;

{-----------------------------------------------------------------------
( F_NAME: __namedIndex
( 
( F_DESC: get index of object name in string list
( 
( F_ARGS: list: TStrings - object name list
(         const name: string - object name
( 
( F_TYPE: integer - object name index
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __namedIndex(list: TStrings; const name: string): integer;

{-----------------------------------------------------------------------
( F_NAME: __namedExists
( 
( F_DESC: is object name in string list?
( 
( F_ARGS: list: TStrings - object name list
(         const name: string - object name
( 
( F_TYPE: boolean - true if exists
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __namedExists(list: TStrings; const name: string): boolean;

{-----------------------------------------------------------------------
( F_NAME: __findNamed
( 
( F_DESC: get object by name
( 
( F_ARGS: list: TStrings - string-object list
(         const name: string - object name
( 
( F_TYPE: pointer - object
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __findNamed(list: TStrings; const name: string): pointer;

{-----------------------------------------------------------------------
( F_NAME: __newNamedList
( 
( F_DESC: create name-object list
( 
( F_ARGS: Sorted: boolean - sort by name?
( 
( F_TYPE: TStringList - name-object list
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __newNamedList(Sorted: boolean): TStringList;

{-----------------------------------------------------------------------
( F_NAME: __genid
( 
( F_DESC: generate 32-char-length global unique string
( 
( F_ARGS:
( 
( F_TYPE: string - id string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __genid: string;

{-----------------------------------------------------------------------
( F_NAME: md5sumBuf
( 
( F_DESC: calculate MD5 digest of the buffer
( 
( F_ARGS: buf: pchar - buffer address
(         count: integer - buffer size
( 
( F_TYPE: string - MD5 digest
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __md5sumBuf(buf: pchar; count: integer): string;
function __md5sumStr(const S: string): string;
function __md5sumFile(const fname: string): string;

{-----------------------------------------------------------------------
( F_NAME: __programFile
( 
( F_DESC: current program file name
( 
( F_ARGS:
(
( F_TYPE: string - program file name
( 
( EXCEPT: 
(----------------------------------------------------------------------}
function __programFile: string;

{-----------------------------------------------------------------------
( F_NAME: __libraryFile
( 
( F_DESC: get current library file name
( 
( F_ARGS:
(
( F_TYPE: string - library file name
( 
( EXCEPT: 
(----------------------------------------------------------------------}
function __libraryFile: string;

{----------------------------------------------------------------------)
(                                                                      )
(                         string manage                                )
(                                                                      )
(----------------------------------------------------------------------)
( F_NAME: __smrComp
( 
( F_DESC: compare PLseString
( 
( F_ARGS: S1, S2: PLseString
(         IgnoreCase: boolean
( 
( F_TYPE: integer - 0:same <0:less >0:great
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __smrComp(S1, S2: PLseString; IgnoreCase: boolean): integer;

{-----------------------------------------------------------------------
( F_NAME: __smrSame
( 
( F_DESC: compare PLseString
( 
( F_ARGS: S1, S2: PLseString
(         IgnoreCase: boolean
( 
( F_TYPE: true - true if same?
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __smrSame(S1, S2: PLseString; IgnoreCase: boolean): boolean;

{-----------------------------------------------------------------------
( F_NAME: __smrUpper
( 
( F_DESC: convert 'a'..'z' to 'A'..'Z'
( 
( F_ARGS: smr: PLseString - source string
( 
( F_TYPE: PLseString - 
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __smrUpper(smr: PLseString): PLseString;

{-----------------------------------------------------------------------
( F_NAME: __smrLower
( 
( F_DESC: convert 'A'..'Z' to 'a'..'z'
( 
( F_ARGS: smr: PLseString - source string
( 
( F_TYPE: PLseString - 
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __smrLower(smr: PLseString): PLseString;

{-----------------------------------------------------------------------
( F_NAME: __smrCountTab
( 
( F_DESC: count TAB chars in string
( 
( F_ARGS:  this: PLseString - source string
(          var LCount: integer - left count
(          var MCount: integer - middle count
(          var RCount: integer - right count
( 
( F_TYPE: integer - Tab char count
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __smrCountTab(this: PLseString;
  var LCount, MCount, RCount: integer): integer;
  
implementation

uses
  Math, DateUtils
  {$IFDEF WINDOWS},ActiveX{$ENDIF};

function __hexcv(ch: char): integer;
begin
  case ch of
    '0'..'9': Result := Ord(ch) - Ord('0');
    'A'..'F': Result := Ord(ch) - Ord('A') + 10;
    'a'..'f': Result := Ord(ch) - Ord('a') + 10;
         else Result := -1;
  end;
end;

function __parseExtInt(var S: pchar; var iv: int64; var ei: KLiExtInt): extended;
label
  LEAVE;
var
  signf, divf: boolean;
  times: int64;
begin
  Result := 0;
  iv := 0;
  ei := eiNone;

  // 1. skip heading spaces

  if S = nil then Exit;
  S := __skipch(S, SpaceChar);

  // 2. read value flag

  signf := (S^ = '-');
  if signf then Inc(S);
  if not (S^ in DigitChar) then goto LEAVE;

  // 3. read hex

  ei := eiInt;

  if S^ = '0' then
  begin
    Inc(S);
    if S^ in ['x', 'X'] then
    begin
      Inc(S);
      if S^ in HexChar then
      begin
        repeat
          iv := (iv * 16) + __hexcv(S^);
          Inc(S);
        until not (S^ in HexChar);
        Result := iv;
      end
      else ei := eiNone;
      goto LEAVE;
    end;
    
    if S^ in ['o', 'O'] then
    begin
      Inc(S);
      if S^ in ['0'..'7'] then
      begin
        repeat
          iv := (iv * 8) + Ord(S^) - Ord('0');
          Inc(S);
        until not (S^ in ['0'..'7']);
        Result := iv;
      end
      else ei := eiNone;
      goto LEAVE;
    end;
    
    if S^ in ['b', 'B'] then
    begin
      Inc(S);
      if S^ in ['0', '1'] then
      begin
        repeat
          iv := (iv * 2) + Ord(S^) - Ord('0');
          Inc(S);
        until not (S^ in ['0', '1']);
        Result := iv;
      end
      else ei := eiNone;
      goto LEAVE;
    end;

    while S^ = '0' do Inc(S);
  end;

  // 3. read integer part

  while S^ in DigitChar do
  begin
    iv := (iv * 10) + Ord(S^) - Ord('0');
    Inc(S);
  end;
  Result := iv;

  // 4. read frac part

  if S^ = '.' then
  begin
    Inc(S);
    if S^ in DigitChar then
    begin
      ei := eiExt;
      times := 1;
      while (S^ in DigitChar) and (times < 1000000000000000000) do
      begin
        times := times * 10;
        Result := Result + ((Ord(S^) - Ord('0')) / times);
        Inc(S);
      end;
      while S^ in DigitChar do Inc(S);
    end
    else
    begin
      Dec(S);
      goto LEAVE;
    end;
  end;

  // 5. check science fomrat

  if S^ in ['e', 'E'] then
  begin
    ei := eiExt;
    Inc(S);
    divf := (S^ = '-');
    if divf then Inc(S);
    if S^ in DigitChar then
    begin
      times := 0;
      repeat
        times := (times * 10) + Ord(S^) - Ord('0');
        Inc(S);
      until not (S^ in DigitChar);
      while times > 0 do
      begin
        if divf then
          Result := Result / 10 else
          Result := Result * 10;
        Dec(times);
      end;
      iv := Round(int(Result));
    end
    else ei := eiNone;
  end;

  LEAVE:

  if ei = eiNone then
  begin
    Inc(S);
    iv := 0;
    Result := 0;
  end
  else
  if signf then
  begin
    iv := - iv;
    Result := - Result;
  end;
end;

function __parseExt(S: pchar): extended;
var
  iv: int64;
  ei: KLiExtInt;
begin
  Result := __parseExtInt(S, iv, ei);
end;

function __parseInt(S: pchar): int64;
var
  ei: KLiExtInt;
begin
  Result := 0;
  __parseExtInt(S, Result, ei);
end;

function __parseChar(var S: pchar; var ch: char): boolean;
begin
  Result := false;
  if S = nil then Exit;
  
  S := __skipch(S, SpaceChar);
  if S^ <> '''' then Exit;
  
  Inc(S); ch := S^;
  Inc(S);
  if ch = '\' then
  begin
    ch := S^;
    case ch of
      '0': ch := #0;   {<--NULL}
      'a': ch := #7;   {<--BELL}
      'b': ch := #8;   {<--BACKSPACE}
      'e': ch := #27;  {<--ESCAPE}
      'f': ch := #12;  {<--FORMFEED}
      'n': ch := #10;  {<--NEWLINE}
      'r': ch := #13;  {<--CARRIGE RETURN}
      't': ch := #9;   {<--TAB}
      'v': ch := #11;  {<--VERTICAL TAB}
      'x': begin       {<--HEX}
             Inc(S);
             if not (S^ in HexChar) then
             begin
               Inc(S);
               Exit;
             end;
             Inc(S);
             if not (S^ in HexChar) then
             begin
               Inc(S);
               Exit;
             end;
             ch := char(StrToInt('$' + (S-1)^ + S^));
           end;
      else if not (ch in ['\', '''', '"']) then Exit;
    end;
    Inc(S);
  end;
  
  Result := (S^ = '''');
  Inc(S);
end;

function __parseStr(var S: pchar; desti: TStream; allow_esc_char: boolean): boolean;
var
  times: integer;
  quotc, ch: char;

  function str_end: boolean;
  var
    count: integer;
    next: pchar;
  begin
    Result := (S^ = quotc);
    if Result and (times > 2) then
    begin
      count := times;
      next := S;
      repeat
        Dec(count);
        Inc(next);
      until (count = 0) or (next^ <> quotc);
      Result := (count = 0) and (next^ <> quotc);
    end;
  end;
  
begin
  quotc := #0;
  times := 0;
  if S <> nil then
  begin
    S := __skipch(S, SpaceChar);
    if S^ in ['"', ''''] then
    begin
      quotc := S^;
      while (S^ = quotc) and (times < 3) do
      begin
        Inc(S);
        Inc(times);
      end;
    end;
  end;
  
  Result := (times = 2);
  if Result or not (times in [1, 3]) then Exit;
  if (times = 3) or (quotc = '''') then
    allow_esc_char := false;
    
  while not str_end do
  begin
    if S^ = #0 then Exit; {<--not terminated}
    ch := S^;
    if (ch = '\') and allow_esc_char then
    begin
      Inc(S);
      if S^ = #0 then Exit; {<--not terminated}
      case S^ of
        '0': ch := #0;   {<--NULL}
        'a': ch := #7;   {<--BELL}
        'b': ch := #8;   {<--BACKSPACE}
        'e': ch := #27;  {<--ESCAPE}
        'f': ch := #12;  {<--FORMFEED}
        'n': ch := #10;  {<--NEWLINE}
        'r': ch := #13;  {<--CARRIGE RETURN}
        't': ch := #9;   {<--TAB}
        'v': ch := #11;  {<--VERTICAL TAB}
        'x': begin      {<--HEX}
               Inc(S);
               if not (S^ in HexChar) then
               begin
                 Inc(S);
                 Exit;
               end;
               Inc(S);
               if not (S^ in HexChar) then
               begin
                 Inc(S);
                 Exit;
               end;
               ch := char(StrToInt('$' + (S - 1)^ + S^));
             end;
        else if S^ in ['\', '''', '"'] then
               ch := S^ else
               Exit;
      end;
    end;
    desti.WriteBuffer(ch, sizeof(ch));
    Inc(S);
  end;

  Inc(S, times);
  Result := true;
end;

function __parseConfig(const S: string; var ID, value: string): boolean;
var
  line: string;
begin
  line := __deleteConfigComment(S);
  Result := (line <> '') and (line[1] in ConfigHeadChar);
  if Result then
  begin
    ID := Trim(__extractNameValue(line, value));
    Result := __inCharSet(pchar(ID), Length(ID), ConfigChar);
    if Result then
      value := Trim(value);
  end;
end;

function __deleteConfigComment(const S: string): string;
var
  index: integer;
begin
  index := Pos('#', S);
  if index > 0 then
    Result := Trim(Copy(S, 1, index - 1)) else
    Result := Trim(S);
end;

function __extractNameValue(const S: string; var V: string; const separator: string): string;
var
  X: Integer;
begin
  X := AnsiPos(separator, S);
  if X > 0 then
  begin
    V := Copy(S, X + Length(separator), MaxInt);
    Result := Copy(S, 1, X - 1);
  end
  else
  begin
    V := '';
    Result := '';
  end;
end;

function __extractName(const S, separator: string): string;
var
  X: Integer;
begin
  X := AnsiPos(separator, S);
  if X > 1 then
    Result := Copy(S, 1, X - 1) else
    Result := '';
end;

function __extractValue(const S, separator: string): string;
var
  X: Integer;
begin
  X := AnsiPos(separator, S);
  if X > 0 then
    Result := Copy(S, X + Length(separator), MaxInt) else
    Result := '';
end;

function __IsIDStr(S: pchar; Head: KLiCharSet): boolean;
begin
  Result := false;
  if (S <> nil) and (S^ in Head) then
  begin
    Inc(S);
    Result := (S^ = #0) or __inCharSet(S, IDChar);
  end;
end;

function __IsIDHead(C: char): boolean;
begin
  Result := (C in IDHeadChar);
end;

function __newString(Source: pchar; Count: integer): string;
begin
  if (Source <> nil) and (Count > 0) then
    SetString(Result, Source, Count) else
    Result := '';
end;

procedure __check(ok: boolean; const msg: string);
begin
  if not ok then lse_error(msg);
end;

procedure __freeAndNil(var obj);
var
  temp: TObject;
begin
  temp := TObject(obj);
  if temp <> nil then
  begin
    temp.Free;
    pointer(obj) := nil;
  end;
end;

procedure __nilWhenSame(var obj; const addr: pointer);
begin
  if pointer(obj) = addr then
    pointer(obj) := nil;
end;

function __zero(buf: pointer; count: integer): pointer;
begin
  FillChar(buf^, count, 0);
  Result := buf;
end;

function __fullPath(const path: string): string;
begin
  Result := lse_expand_fname(path);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function __sameFileName(const F1, F2: string): boolean;
begin
  {$IFDEF WINDOWS}
  Result := CompareText(F1, F2) = 0;
  {$ELSE}
  Result := (F1 = F2);
  {$ENDIF}
end;

{$IFDEF WINDOWS}
var
  com_need_uninitialize: boolean;
  com_initialized: boolean;
{$ENDIF}

procedure __coinit;
begin
  {$IFDEF WINDOWS}
  if not com_initialized then
  begin
    com_initialized := true;
    com_need_uninitialize := Succeeded(CoInitialize(nil));
  end;
  {$ENDIF}
end;

function __bufComp(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): integer;
var
  len: integer;
begin
  Result := 0;
  if B1 <> B2 then
  begin
    if (B1 = nil) or (L1 < 1) then
    begin
      if (B2 <> nil) and (L2 > 0) then Result := -1;
      Exit;
    end;

    if (B2 = nil) or (L2 < 1) then
    begin
      Result := 1;
      Exit;
    end;

    for len := min(L1, L2) downto 1 do
    begin
      if IgnoreCase and (B1^ in AlphaChar) and (B2^ in AlphaChar) then
        Result := (Ord(B1^) or $20) - (Ord(B2^) or $20) else
        Result := Ord(B1^) - Ord(B2^);
      if Result <> 0 then Exit;
      Inc(B1);
      Inc(B2);
    end;

    Result := L1 - L2;
  end;
end;

function __strComp(const S1, S2: string; IgnoreCase: boolean): integer;
begin
  Result := __bufComp(pchar(S1), Length(S1), pchar(S2), Length(S2), IgnoreCase);
end;

function __bufSame(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): boolean;
begin
  Result := (__bufComp(B1, L1, B2, L2, IgnoreCase) = 0);
end;

function __strSame(const S1, S2: string; IgnoreCase: boolean): boolean;
begin
  Result := (__strComp(S1, S2, IgnoreCase) = 0);
end;

procedure __upper(buf: pchar; count: integer);
begin
  if (buf <> nil) and (count > 0) then
  repeat
    if buf^ in ['a'..'z'] then
      Dec(buf^, AaDistance);
    Inc(buf);
    Dec(count);
  until count < 1;
end;

procedure __lower(buf: pchar; count: integer);
begin
  if (buf <> nil) and (count > 0) then
  repeat
    if buf^ in ['A'..'Z'] then
      Inc(buf^, AaDistance);
    Inc(buf);
    Dec(count);
  until count < 1;
end;

function __locase(ch: char): char;
begin
  Result := ch;
  if Result in ['A'..'Z'] then
    Inc(Result, AaDistance);
end;

function __upcase(ch: char): char;
begin
  Result := ch;
  if Result in ['a'..'z'] then
    Dec(Result, AaDistance);
end;

function __countTab(buf: pchar; count: integer; var LCount, MCount, RCount: integer): integer;
var
  head, last: pchar;
begin
  LCount := 0;
  MCount := 0;
  RCount := 0;
  if (buf <> nil) and (count > 0) then
  begin
    head := buf;
    last := buf + (count - 1);
    while (head <= last) and (head^ in SpaceChar) do Inc(head);
    LCount := head - buf;     // buf .. head .. last
    buf := last;
    while (head <= buf) and (buf^ in SpaceChar) do Dec(buf);
    RCount := last - buf;     // head .. buf .. last
    Inc(head);
    while head < buf do
    begin
      if head^ in SpaceChar then Inc(MCount);
      Inc(head);
    end;
  end;
  Result := LCount + MCount + RCount;
end;

function __pos(this: pchar; thisLen: integer;
  patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;
var
  endpos: pchar;
begin
  if (this <> nil) and (patten <> nil) and (pattenLen > 0) and (pattenLen <= thisLen) then
  begin
    endpos := this + (thisLen - pattenLen);
    Result := this;
    while Result <= endpos do
    begin
      if __bufSame(Result, pattenLen, patten, pattenLen, IgnoreCase) then Exit;
      Inc(Result);
    end;
  end;
  Result := nil;
end;

function __lastPos(this: pchar; thisLen: integer;
  patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;
var
  endpos: pchar;
begin
  if (this <> nil) and (patten <> nil) and (pattenLen > 0) and (pattenLen <= thisLen) then
  begin
    Result := this + (thisLen - pattenLen);
    endpos := this;
    while Result >= endpos do
    begin
      if __bufSame(Result, pattenLen, patten, pattenLen, IgnoreCase) then Exit;
      Dec(Result);
    end;
  end;
  Result := nil;
end;

function __fileText(const FileName: string): string;
var
  N: integer;
begin
  with TFileStream.Create(lse_veryPD(FileName), fmShareDenyWrite) do
  try
    N := size;
    if N > 0 then
    begin
      SetString(Result, nil, N);
      SetLength(Result, Read(pchar(Result)^, N));
    end
    else Result := '';
  finally
    Free;
  end;
end;

function __encodeUTF8(const S: string): string;
begin
  Result := AnsiToUtf8(S);
end;

function __decodeUTF8(const S: string): string;
begin
  Result := Utf8ToAnsi(S);
end;

function __encodeS(const AStr: string; IncLocalBytes: boolean): string;
const
  ESCH = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  TARC = [#0..#31, '''', '"', '\', '%', '^', #126..#128];
  LBCS = [#128..#255];

  function get_esc(chr: char): char;
  begin
    case chr of
      #0..#31: Result := ESCH[Ord(chr) + 1]; // 1..32
      ''''   : Result := ESCH[33];
      '"'    : Result := ESCH[34];
      '\'    : Result := ESCH[35];
      '%'    : Result := ESCH[36];
      #126   : Result := ESCH[37];
      #127   : Result := ESCH[38];
      #128   : Result := ESCH[39];
      '^'    : Result := '^';
          else Result := #0;
    end;
  end;

  function encode_str(S: pchar; count: integer): string;
  var
    index, total, dummy, X: integer;
    ch: char;
    bufc: pchar;
    bufb: PLiByteList absolute bufc;

    procedure add_chr(chr: char);
    begin
      Inc(total);
      if total > Length(Result) then
        SetLength(Result, Length(Result) + 256);
      Result[total] := chr;
    end;

    procedure add_esc(esc: char);
    begin
      add_chr('^');
      add_chr(esc);
    end;

    procedure add_str(const str: string);
    var
      index: integer;
    begin
      for index := 1 to Length(str) do
        add_chr(str[index]);
    end;

    procedure count_dummy_char(CSet: KLiCharSet);
    begin
      dummy := index + 1;
      while (dummy < count) and (S[dummy] in CSet) do Inc(dummy);
      Dec(dummy, index);
    end;
    
  begin
    Result := '';
    SetLength(Result, count * 2);
    total := 0;
    index := 0;
    while index < count do
    begin
      ch := S[index];
      if ch in TARC then
      begin
        count_dummy_char(TARC); // target char count
        if dummy > 2 then
        begin
          add_esc('[');
          while dummy > 0 do
          begin
            add_chr(get_esc(S[index]));
            Dec(dummy);
            Inc(index);
          end;
          add_chr(']');
        end
        else
        while dummy > 0 do
        begin
          add_esc(get_esc(S[index]));
          Dec(dummy);
          Inc(index);
        end;
      end
      else
      if IncLocalBytes and (ch in LBCS) then
      begin
        count_dummy_char(LBCS); // local byte count
        bufc := lse_mem_alloc(dummy);
        try
          Move(S[index], bufc^, dummy);
          Inc(index, dummy);
          for X := 0 to dummy - 1 do
            bufb^[X] := (bufb^[X] and $7F);
          add_esc('_');
          IncLocalBytes := false;
          add_str(encode_str(bufc, dummy));
          IncLocalBytes := true;
          add_esc('_');
        finally
          lse_mem_free(bufc, dummy);
        end;
      end
      else
      begin
        add_chr(ch);
        Inc(index);
      end;
    end;
    SetLength(Result, total);
  end;

begin
  Result := encode_str(pchar(AStr), Length(AStr));
end;

function __decodeS(const S: string): string;
var
  index, count, total: integer;
  ch: char;
  hibit: boolean;

  procedure add_chr(chr: char);
  var
    chr_byte: byte absolute chr;
  begin
    Inc(total);
    if total > Length(Result) then
      SetLength(Result, Length(Result) + 256);
    if hibit then
      chr_byte := chr_byte or $80;
    Result[total] := chr;
  end;

  function get_esc(esc: char): char;
  var
    index: integer;
  begin
    if esc in ['0'..'9', 'A'..'Z', 'a'..'z'] then
    begin
      if esc in ['A'..'Z'] then index := Ord(esc) - Ord('A') + 10 else
      if esc in ['0'..'9'] then index := Ord(esc) - Ord('0')      else
                                index := Ord(esc) - Ord('a') + 36;
      case index + 1 of
        1..32: Result := char(index);
        33   : Result := '''';
        34   : Result := '"';
        35   : Result := '\';
        36   : Result := '%';
        37   : Result := #126;
        38   : Result := #127;
        39   : Result := #128;
          else Result := #0;
      end;
    end
    else
    if esc = '^' then
      Result := '^' else
      Result := #0;
  end;
  
begin
  Result := S;
  count := Length(S);
  if count > 1 then
  begin
    hibit := false;
    total := 0;
    index := 1;
    while index <= count do
    begin
      ch := S[index];
      if ch <> '^' then add_chr(ch) else
      if index < count then
      begin
        Inc(index);
        ch := S[index];
        if ch = '_' then hibit := not hibit else
        if ch = '[' then
        begin
          Inc(index);
          while (index <= count) and (S[index] <> ']') do
          begin
            add_chr(get_esc(S[index]));
            Inc(index);
          end;
        end
        else add_chr(get_esc(ch));
      end;
      Inc(index);
    end;
    SetLength(Result, total);
  end;
end;

function __strToComma(const AStr: string): string;
var
  list: TStrings;
begin
  list := TStringList.Create;
  try
    list.Text := AStr;
    Result := list.CommaText;
  finally
    list.Free;
  end;
end;

function __hashof(const Key: string): cardinal;
var
  index, count: integer;
begin
  Result := 0;
  count := Length(Key);
  if count > 64 then count := 64;
  for index := 1 to count do
    Result := ((Result shl 2) or (Result shr (sizeof(Result) * 8 - 2)))
      xor Ord(Key[index]);
end;

function __fullFileName(const FileName, BasePath: string): string;
begin
  Result := Trim(FileName);
  if __IsRelativeFileName(Result) then
    Result := IncludeTrailingPathDelimiter(Trim(BasePath)) + Result;
  Result := lse_expand_fname(Result);
end;

function __IsRelativeFileName(const FileName: string): boolean;
begin
  Result := (FileName <> '') and (FileName[1] <> LSE_PATH_DELIMITER);
  {$IFDEF WINDOWS}
  if Result then
    if (FileName[1] in AlphaChar) and (Length(FileName) > 1) then
      Result := (FileName[2] <> ':');
  {$ENDIF}
end;

function __strToFileMode(const openMode: string; var fileMode: word;
                         var R, W: boolean): boolean;
var
  index: integer;
  C, E: boolean;
begin
  C := false;
  E := false;
  R := false;
  W := false;
  for index := 1 to Length(openMode) do
    case openMode[index] of
      'c', 'C': C := true; // create
      'e', 'E': E := true; // exclusive
      'r', 'R': R := true; // read
      'w', 'W': W := true; // write
    end;
  Result := (C or E or R or W);
  if Result then
  begin
    if C then
    begin
      fileMode := fmCreate;
      R := true;
      W := true;
    end
    else
    if R then
    begin
      if W then fileMode := fmOpenReadWrite or fmShareExclusive else
      if E then fileMode := fmOpenRead or fmShareExclusive else
                fileMode := fmShareDenyWrite;
    end
    else
    if W then
      fileMode := fmOpenWrite or fmShareExclusive else
//  if E then
      fileMode := fmOpenRead or fmShareExclusive;
  end
end;

function __getText(list: TStrings; const eol: string): string;
var
  index, slen, size: Integer;
  base: pchar;
  temp: string;
begin
  size := 0;
  for index := 0 to list.Count - 1 do
    Inc(size, Length(list[index]) + Length(eol));
  SetString(Result, nil, size);
  base := pointer(Result);
  for index := 0 to list.Count - 1 do
  begin
    temp := list[index];
    slen := Length(temp);
    if slen <> 0 then
    begin
      System.Move(pointer(temp)^, base^, slen);
      Inc(base, slen);
    end;
    slen := Length(eol);
    if slen <> 0 then
    begin
      System.Move(pointer(eol)^, base^, slen);
      Inc(base, slen);
    end;
  end;
end;

procedure __setText(list: TStrings; const text, eol: string; IgnoreCase: boolean);
var
  base, next: pchar;
  eolen, slen: integer;
  temp: string;
begin
  eolen := Length(eol);

  if (eol = #13#10) or (eol = #10) or (eolen = 0) then
  begin
    list.Text := text;
    Exit;
  end;

  list.BeginUpdate;
  try
    list.Clear;
    base := pointer(text);
    if base <> nil then
    begin
      slen := StrLen(base);
      while base^ <> #0 do
      begin
        next := __pos(base, slen, pchar(eol), eolen, IgnoreCase);
        if next = nil then
        begin
          SetString(temp, base, slen);
          list.Add(temp);
          break;
        end;
        SetString(temp, base, next - base);
        list.Add(temp);
        Inc(next, eolen);
        Dec(slen, next - base);
        base := next;
      end;
    end;
  finally
    list.EndUpdate;
  end;
end;

function __inCharSet(S: pchar; Len: integer; Chars: KLiCharSet): boolean;
begin
  Result := (Len > 0) and (S <> nil) and (S^ in Chars);
  while Result and (Len > 1) do
  begin
    Dec(Len);
    Inc(S);
    Result := S^ in Chars;
  end;
end;

function __inCharSet(S: pchar; Chars: KLiCharSet): boolean;
begin
  Result := false;
  if (S <> nil) and (S^ <> #0) and (S^ in Chars) then
  begin
    Chars := Chars - [#0];
    repeat Inc(S) until not (S^ in Chars);
    Result := (S^ = #0);
  end;
end;

function __IsYMD(day: integer): boolean;
var
  Y, M, D: word;
begin
  D := day mod 100;  // day
  day := day div 100;
  M := day mod 100;  // month
  Y := day div 100;  // year
  Result := IsValidDate(Y, M, D);
end;

function __decodeYMD(ymd: integer; var y, m, d: integer): boolean;
var
  T: TDateTime;
begin
  d := ymd mod 100;
  if d in [1..31] then
    if __decodeYM((ymd - d) div 100, y, m) then
    begin
      Result := TryEncodeDate(y, m, d, T);
      Exit;
    end;
  Result := false;
end;

function __nextYMD(ymd, offset: integer): integer;
var
  iy, im, id: integer;
  wy, wm, wd: word;
  T: TDateTime;
begin
  id := ymd mod 100;
  if id in [1..31] then
    if __decodeYM((ymd - id) div 100, iy, im) then
      if TryEncodeDate(iy, im, id, T) then
      begin
        DecodeDate(IncDay(T, offset), wy, wm, wd);
        Result := (wy * 10000) + (wm * 100) + wd;
        Exit;
      end;
  Result := 0;
end;

function __prevYMD(ymd, offset: integer): integer;
begin
  Result := __nextYMD(ymd, - offset);
end;

function __betweenYMD(ymd1, ymd2: integer): integer;
var
  y, m, d: integer;
  T1, T2: TDateTime;
begin
  Result := -1;
  if __decodeYMD(ymd1, y, m, d) then
  begin
    T1 := EncodeDate(y, m, d);
    if __decodeYMD(ymd2, y, m, d) then
    begin
      T2 := EncodeDate(y, m, d);
      Result := DaysBetween(T1, T2);
    end;
  end;
end;

function __weeksBetweenYMD(ymd1, ymd2: integer): integer;
begin
  Result := (__betweenYMD(ymd1, ymd2) + 6) div 7;
end;

function __decodeYM(ym: integer; var y, m: integer): boolean;
begin
  m := ym mod 100;
  if m in [1..12] then
  begin
    y := (ym - m) div 100;
    Result := (y >= 1) and (y <= 9999);
    Exit;
  end;
  Result := false;
end;

function __nextYM(ym, offset: integer): integer;
var
  y, m: integer;
begin
  if not __decodeYM(ym, y, m) then
    Result := 0 else
  if offset <> 0 then
  begin
    Inc(y, offset div 12);
    Inc(m, offset mod 12);
    if m > 12 then
    begin
      m := m - 12;
      Inc(y);
    end
    else
    if m < 1 then
    begin
      m := m + 12;
      Dec(y);
    end;
    Result := (y * 100) + m;
  end
  else Result := ym;
end;

function __prevYM(ym, offset: integer): integer;
begin
  Result := __nextYM(ym, - offset);
end;

function __skipch(const S: pchar; CharSet: KLiCharSet): pchar;
begin
  Result := S;
  if Result <> nil then
    while Result^ in CharSet do
      Inc(Result);
end;

function __seekch(const S: pchar; CharSet: KLiCharSet): pchar;
begin
  Result := S;
  if Result <> nil then
    while not (Result^ in CharSet) do
      Inc(Result);
end;

function __replaceAll(const S, OldStr, NewStr: string): string;
begin
  Result := StringReplace(S, OldStr, NewStr, [rfReplaceAll, rfIgnoreCase]);
end;

function __encodeHTML(Buf: pchar; Count: integer; TranslateMBC: boolean): string;
var
  index: integer;
  ch: char;
//temp: string;
//ustr: WideString;
begin
  Result := '';
  if (Buf <> nil) and (Buf^ <> #0) and (Count > 0) then
  begin
    index := 0;
    while (index < Count) and (Buf^ <> #0) do
    begin
      ch := Buf[index];
      case ch of
        '<': Result := Result + '&lt;';
        '>': Result := Result + '&gt;';
        '&': Result := Result + '&amp;';
        ' ': Result := Result + '&nbsp;';
        '"': Result := Result + '&quot;';
        #9 : Result := Result + '&nbsp;&nbsp;&nbsp;&nbsp;';
        #13: begin
               Result := Result + '<br>' + LB;
               if index < (Count - 1) then
                 if Buf[index + 1] = #10 then Inc(index);
             end;
        #10: Result := Result + '<br>' + LB;
        else {
          if TranslateMBC and (ch in LeadBytes) and (index < Count - 1) and (Buf[index + 1] in LeadBytes) then
          begin
            SetString(temp, Buf + index, 2);
            ustr := temp;
            Result := Result + '&#' + IntToStr(Ord(ustr[1])) + ';';
            Inc(index);
          end
          else
          if Buf[index] in [#92, #160 .. #255] then
            Result := Result + '&#' + IntToStr(Ord(ch)) +';' else
        }   Result := Result + ch;
      end;
      Inc(index);
    end;
  end;
end;

function __decodeHTML(Buf: pchar; Count: integer): string;
var
  index, cv: integer;
  temp: string;
  uch: WideChar;
begin
  Result := '';
  if (Buf <> nil) and (Buf^ <> #0) and (Count > 0) then
  begin
    index := 0;
    while (index < Count) and (Buf^ <> #0) do
    begin
      if Buf[index] = '&' then
      begin
        temp := '';
        Inc(index);
        while (index < Count) and not (Buf[index] in [#0, ';']) do
        begin
          temp := temp + Buf[index];
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
      else
      if not (Buf[index] in [#13, #10]) then
        Result := Result + Buf[index];
      Inc(index);
    end;
    Result := __replaceAll(Result, '<br>', LB);
  end;
end;

function __packHTML(const HTML: pchar; Count: integer): integer;
var
  base, begs, ends, head, tail: pchar;
  divn: integer;
begin
  if (HTML = nil) or (Count < 1) then
  begin
    Result := 0;
    Exit;
  end;

  base := HTML;
  begs := base;
  ends := base + Count;

  // 1. seek first '<'
  while (begs < ends) and (begs^ <> '<') do Inc(begs);

  // 2. trim heading & tailing spaces
  while begs < ends do
  begin
    // 2.1. seek line break or EOF
    divn := 0;
    head := begs;
    while (begs < ends) and not (begs^ in [#10, #13]) do
    begin
      if begs^ = '/' then Inc(divn) else
      if divn < 2 then divn := 0;
      Inc(begs);
    end;

    // 2.2. seek last none space char
    tail := begs - 1;
    while tail^ in SpaceChar do Dec(tail);

    // 2.3. move none space char
    repeat
      base^ := head^;
      Inc(base);
      Inc(head);
    until head > tail;

    // 2.4. add LB if encounter '//'
    if (divn > 1) and (begs^ in [#10, #13]) then
    begin
      base^ := begs^;
      Inc(begs);
      if (begs^ = #10) and (base^ = #13) then
      begin
        Inc(base);
        base^ := begs^;
        Inc(begs);
      end;
      Inc(base);
    end;
    
    // 2.5. skip spaces
    if begs < ends then
    begin
      while (begs < ends) and (begs^ in SpaceChar) do Inc(begs);
      if (begs < ends) and (begs^ <> '<') then
      begin
        base^ := ' ';
        Inc(base);
      end;
    end;
  end;

  // 3. return result length
  Result := base - HTML;
end;

function __fullDir(const dir: string): string;
begin
  Result := lse_expand_fname(dir);
  if Result <> '' then
    Result := ExcludeTrailingPathDelimiter(Result);
end;

function __copyFile(const Source, Desti: string): boolean;
{$IFNDEF WINDOWS}
var
  fi, fo: TFileStream;
{$ENDIF}
begin
  Result := false;
  try
    {$IFDEF WINDOWS}
    Result := Windows.CopyFile(pchar(Source), pchar(Desti), false);
    {$ELSE}
    fi := TFileStream.Create(lse_veryPD(Source), fmShareDenyWrite);
    try
      fo := TFileStream.Create(lse_veryPD(Desti), fmCreate);
      try
        fo.CopyFrom(fi, 0);
      finally
        fo.Free;
      end;
    finally
      fi.Free;
    end;
    Result := true;
    {$ENDIF}
  except
    { do nothing }
  end;
end;

function __IsFile(const Source: string): boolean;
begin
  {$IFNDEF WINDOWS}
  Result := FileExists(Source) and not DirectoryExists(Source);
  {$ELSE}
  Result := FileExists(Source);
  {$ENDIF}
end;

function __IsDir(const Source: string): boolean;
begin
  Result := DirectoryExists(Source);
end;

procedure __releaseSOList(var list: TStringList);
var
  A: integer;
  O: TObject;
begin
  if list <> nil then
  begin
    for A := list.Count - 1 downto 0 do
    begin
      O := list.Objects[A];
      list.Delete(A);
      if O <> nil then O.Free;
    end;
    FreeAndNil(list);
  end;
end;

function __removeFrom(list: TStrings; item: pointer): boolean;
var
  X: integer;
begin
  Result := list <> nil;
  if Result then
  begin
    X := list.IndexOfObject(TObject(item));
    Result := X >= 0;
    if Result then list.Delete(X);
  end;
end;

function __removeAllFrom(list: TStrings; item: pointer): integer;
var
  X: integer;
begin
  Result := 0;
  if list <> nil then
    for X := list.Count - 1 downto 0 do
      if list.Objects[X] = TObject(item) then
      begin
        Inc(Result);
        list.Delete(X);
      end;
end;

function __namedIndex(list: TStrings; const name: string): integer;
begin
  if list <> nil then
    Result := list.IndexOf(name) else
    Result := -1;
end;

function __namedExists(list: TStrings; const name: string): boolean;
begin
  Result := __namedIndex(list, name) >= 0;
end;

function __findNamed(list: TStrings; const name: string): pointer;
var
  X: integer;
begin
  X := __namedIndex(list, name);
  if X >= 0 then
    Result := list.Objects[X] else
    Result := nil;
end;

function __newNamedList(Sorted: boolean): TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := true;
  Result.Sorted := Sorted;
end;

function __genid: string;
var
  guid: TGuid;
  index: integer;
begin
  CreateGuid(guid);
  Result := UpperCase(GuidToString(guid));
  for index := Length(Result) downto 1 do
    if not (Result[index] in HexChar) then
      System.Delete(Result, index, 1);
end;

function __md5sumBuf(buf: pchar; count: integer): string;
begin
  with KLiMD5.Create do
  try
    Result := sumBuf(buf, count);
  finally
    Free;
  end;
end;

function __md5sumStr(const S: string): string;
begin
  with KLiMD5.Create do
  try
    Result := sumStr(S);
  finally
    Free;
  end;
end;

function __md5sumFile(const fname: string): string;
begin
  with KLiMD5.Create do
  try
    Result := sumFile(fname);
  finally
    Free;
  end;
end;

function __programFile: string;
{$IFDEF WINDOWS}
var
  buffer: array[0..MAX_PATH] of char;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if IsLibrary then
  begin
    GetModuleFileName(MainInstance, buffer, sizeof(buffer));
    Result := lse_expand_fname(buffer);
    Exit;
  end;
  {$ENDIF}
  Result := lse_expand_fname(ParamStr(0));
end;

function __libraryFile: string;
{$IFDEF WINDOWS}
var
  buffer: array[0..MAX_PATH] of char;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if IsLibrary then
  begin
    GetModuleFileName(HInstance, buffer, sizeof(buffer));
    Result := lse_expand_fname(buffer);
    Exit;
  end;
  {$ENDIF}
  Result := __programFile;
end;

{----------------------------------------------------------------------)
(                                                                      )
(                         string manage                                )
(                                                                      )
(----------------------------------------------------------------------}

function __smrComp(S1, S2: PLseString; IgnoreCase: boolean): integer;
begin
  Result := __bufComp(lse_strec_data(S1), lse_strec_length(S1),
    lse_strec_data(S2), lse_strec_length(S2), IgnoreCase);
end;

function __smrSame(S1, S2: PLseString; IgnoreCase: boolean): boolean;
begin
  Result := (S1 = S2) or (__smrComp(S1, S2, IgnoreCase) = 0);
end;

function __smrUpper(smr: PLseString): PLseString;
begin
  __upper(lse_strec_data(smr), lse_strec_length(smr));
  Result := smr;
end;

function __smrLower(smr: PLseString): PLseString;
begin
  __lower(lse_strec_data(smr), lse_strec_length(smr));
  Result := smr;
end;

function __smrCountTab(this: PLseString;
  var LCount, MCount, RCount: integer): integer;
begin
  Result := __countTab(lse_strec_data(this), lse_strec_length(this), LCount, MCount, RCount);
end;

{ KLiList }

function KLiList.DecRefcount: integer;
begin
  Dec(FRefcount);
  Result := FRefcount;
  if Result = 0 then Free;
end;

destructor KLiList.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited;
end;

function KLiList.IncRefcount: integer;
begin
  Inc(FRefcount);
  Result := FRefcount;
  if Result = 0 then Free;
end;

procedure KLiList.TailRemove(Item: pointer);
var
  index: integer;
begin
  for index := Count - 1 downto 0 do
    if Item = Get(Index) then
    begin
      Delete(index);
      Exit;
    end;
end;

{ KLiNameObject }

constructor KLiNameObject.Create(const AName: string);
begin
  FName := AName;
end;

{ KLiNameHashed }

procedure KLiNameHashed.Clear;
var
  X: integer;
  B, N: PLiNameItem;
begin
  for X := 0 to FSize - 1 do
  begin
    B := FBuckets[X];
    FBuckets[X] := nil;
    while B <> nil do
    begin
      N := B^.ni_next;
      FreeItem(B);
      B := N;
    end;
  end;
end;

constructor KLiNameHashed.Create(Size: cardinal);
begin
  inherited Create;
  FSize := Max(2, Size);
  SetLength(FBuckets, FSize);
  FillChar(FBuckets[0], FSize * sizeof(PLiNameItem), 0);
end;

destructor KLiNameHashed.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited;
end;

function KLiNameHashed.FindItem(const Key: string): PLiNameItem;
begin
  Result := FBuckets[HashOf(Key)];
  while Result <> nil do
  begin
    if Key = Result^.ni_nobj.FName then Exit;
    Result := Result^.ni_next;
  end;
end;

procedure KLiNameHashed.FreeItem(Item: PLiNameItem);
begin
  Dec(FCount);
  lse_mem_free(Item, sizeof(RLiNameItem));
end;

function KLiNameHashed.Get(const Key: string): KLiNameObject;
var
  M: PLiNameItem;
begin
  M := FindItem(Key);
  if M <> nil then
    Result := M^.ni_nobj else
    Result := nil;
end;

function KLiNameHashed.HashOf(const Key: string): cardinal;
begin
  Result := __hashof(Key) mod FSize;
end;

function KLiNameHashed.NewItem: PLiNameItem;
var
  Z: cardinal;
  L: TList;
  M: PLiNameItem;
  X, H: integer;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiNameItem));
  Inc(FCount);
  Z := FCount div 16;
  if Z > FSize then
  begin
    L := TList.Create;
    try
      for X := 0 to FSize - 1 do
      begin
        M := FBuckets[X];
        while M <> nil do
        begin
          L.Add(M);
          M := M^.ni_next;
        end;
      end;
      SetLength(FBuckets, Z);
      FillChar(FBuckets[0], Z * sizeof(PLiNameItem), 0);
      FSize := Z;
      for X := 0 to L.Count - 1 do
      begin
        M := PLiNameItem(L[X]);
        H := HashOf(M^.ni_nobj.FName);
        M^.ni_next := FBuckets[H];
        FBuckets[H] := M;
      end;
    finally
      L.Free;
    end;
  end; 
end;

procedure KLiNameHashed.Put(AObj: KLiNameObject);
var
  X: integer;
  M: PLiNameItem;
begin
  if AObj <> nil then
    if FindItem(AObj.FName) = nil then
    begin
      X := HashOf(AObj.FName);
      M := NewItem;
      M^.ni_nobj := AObj;
      M^.ni_next := FBuckets[X];
      FBuckets[X] := M;
    end;
end;

procedure KLiNameHashed.Remove(const Key: string);
var
  X: integer;
  M, P: PLiNameItem;
begin
  X := HashOf(Key);
  P := FBuckets[X];
  if P <> nil then
    if Key = P^.ni_nobj.FName then
    begin
      FBuckets[X] := P^.ni_next;
      FreeItem(P);
    end
    else
    while P^.ni_next <> nil do
    begin
      M := P^.ni_next;
      if Key = M^.ni_nobj.FName then
      begin
        P^.ni_next := M^.ni_next;
        FreeItem(M);
        Exit;
      end;
      P := M;
    end;
end;

{ KLiHashTable }

procedure KLiHashTable.Clear;
var
  index: integer;
  base, next: PLiHashItem;
begin
  for index := 0 to FBucketSize - 1 do
  begin
    base := FBucketList[index].hp_head;
    while base <> nil do
    begin
      next := base^.hi_next;
      FreeItem(base);
      base := next;
      Dec(FCount);
    end;
    FBucketList[index].hp_head := nil;
    FBucketList[index].hp_tail := nil;
  end;
  FCount := 0;
end;

constructor KLiHashTable.Create(Size: integer);
begin
  inherited Create;
  FCount := 0;
  FBucketSize := Max(1, Size);
  SetLength(FBucketList, FBucketSize);
  FillChar(FBucketList[0], FBucketSize * sizeof(RLiHashPair), 0);
end;

destructor KLiHashTable.Destroy;
begin
  Clear;
  SetLength(FBucketList, 0);
  inherited;
end;

function KLiHashTable.DoGet(const Key: string): pointer;
var
  hash: PLiHashItem;
begin
  hash := Find(Key);
  if hash <> nil then
    Result := hash^.hi_data else
    Result := nil;
end;

procedure KLiHashTable.DoPut(const Key: string; Value: pointer);
var
  item: PLiHashItem;
begin
  item := NewItem;
  string(item^.hi_key) := Key;
  item^.hi_data := Value;
  with FBucketList[HashOf(Key)] do
  begin
    if hp_tail <> nil then
      hp_tail^.hi_next := item else
      hp_head := item;
    hp_tail := item;
  end;
  Inc(FCount);
end;

function KLiHashTable.EnumKeyData(Proc: KLiEnumKeyData; Param: pointer): integer;
var
  next: integer;
  item: PLiHashItem;
begin
  Result := 0;
  for next := 0 to FBucketSize - 1 do
  begin
    item := FBucketList[next].hp_head;
    while item <> nil do
    begin
      Proc(string(item^.hi_key), item^.hi_data, Param);
      item := item^.hi_next;
      Inc(Result);
    end;
  end;
end;

function KLiHashTable.Find(const Key: string): PLiHashItem;
begin
  Result := FBucketList[HashOf(Key)].hp_head;
  while Result <> nil do
  begin
    if MatchKey(Key, string(Result^.hi_key)) then Exit;
    Result := Result^.hi_next;
  end;
end;

procedure KLiHashTable.FreeItem(Item: PLiHashItem);
begin
  string(Item^.hi_key) := '';
  lse_mem_free(Item, sizeof(RLiHashItem));
end;

function KLiHashTable.HashOf(const Key: string): cardinal;
begin
  if FBucketSize = 1 then Result := 0 else
  if FIgnoreCase then
    Result := __hashof(LowerCase(Key)) mod cardinal(FBucketSize) else
    Result := __hashof(Key) mod cardinal(FBucketSize);
end;

function KLiHashTable.IsSet(const Key: string): boolean;
begin
  Result := (Find(Key) <> nil);
end;

function KLiHashTable.ListKey(List: TStrings): integer;
var
  next: integer;
  item: PLiHashItem;
begin
  List.Clear;
  for next := 0 to FBucketSize - 1 do
  begin
    item := FBucketList[next].hp_head;
    while item <> nil do
    begin
      List.Add(string(item^.hi_key));
      item := item^.hi_next;
    end;
  end;
  Result := List.Count;
end;

function KLiHashTable.ListData(List: TList): integer;
var
  next: integer;
  item: PLiHashItem;
begin
  List.Clear;
  for next := 0 to FBucketSize - 1 do
  begin
    item := FBucketList[next].hp_head;
    while item <> nil do
    begin
      List.Add(item^.hi_data);
      item := item^.hi_next;
    end;
  end;
  Result := List.Count;
end;

function KLiHashTable.MatchKey(const Key, ID: string): boolean;
begin
  if FIgnoreCase then
    Result := AnsiSameText(Key, ID) else
    Result := AnsiSameStr(Key, ID);
end;

function KLiHashTable.NewItem: PLiHashItem;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiHashItem)); 
end;

procedure KLiHashTable.Remove(const Key: string);
var
  item, root: PLiHashItem;
begin
  root := nil;
  with FBucketList[HashOf(Key)] do
  begin
    item := hp_head;
    while item <> nil do
    begin
      if MatchKey(Key, string(item^.hi_key)) then
      begin
        if root <> nil then
          root^.hi_next := item^.hi_next else
          hp_head := item^.hi_next;
        if hp_tail = item then
          hp_tail := root;
        lse_mem_free(item, sizeof(RLiHashItem));
        Dec(FCount);
        Exit;
      end;
      root := item;
      item := item^.hi_next;
    end;
  end;
end;

{ KLiMD5 }

procedure KLiMD5.FF(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + ((b^ and c^) or ((not b^) and d^)) + x^ + ac, s) + b^;
end;

function KLiMD5.GetDigest: string;

  function VS(V: cardinal): string;
  var
    B: array[0..3] of byte;
  begin
    Move(V, B, 4);
    Result := Format('%.2x%.2x%.2x%.2x', [B[0], B[1], B[2], B[3]]);
  end;

begin
  Result := Format('%s%s%s%s', [VS(PA^), VS(PB^), VS(PC^), VS(PD^)]);
end;

procedure KLiMD5.GG(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + ((b^ and d^) or (c^ and (not d^))) + x^ + ac, s) + b^;
end;

procedure KLiMD5.HH(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + (b^ xor c^ xor d^) + x^ + ac, s) + b^;
end;

procedure KLiMD5.II(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + (c^ xor (b^ or (not d^))) + x^ + ac, s) + b^;
end;

procedure KLiMD5.Init;
begin
  FA := cardinal($67452301); PA := @FA;
  FB := cardinal($efcdab89); PB := @FB;
  FC := cardinal($98badcfe); PC := @FC;
  FD := cardinal($10325476); PD := @FD;
end;

function KLiMD5.ROL(A: cardinal; Amount: byte): cardinal;
const
  CARMASK = $80000000;
var
  X: byte;
begin
  for X := 1 to Amount do
    if (A and CARMASK) = CARMASK then
      A := (A shl 1) or $01 else  
      A := (A shl 1);  
   Result := A;  
end;

function KLiMD5.sumBuf(Abuf: pchar; count: integer): string;
var
  buf: array[0..4159] of byte;
  len: int64;
  eob: boolean;
  bytes, index: integer;
begin
  Init;
  eob := False;
  len := 0;
  repeat
    bytes := Min(4096, count);
    Move(Abuf^, buf, bytes);
    Inc(Abuf, bytes);
    Dec(count, bytes);
    len := len + bytes;
    if bytes <> 4096 then
    begin
      buf[bytes] := $80;
      Inc(bytes);
      while (bytes mod 64) <> 56 do
      begin
        buf[bytes] := 0;
        Inc(bytes);
      end;
      len := len * 8;
      Move(len, buf[bytes], 8);
      Inc(bytes, 8);
      eob := True;
    end;
    index := 0;
    repeat
      Move(buf[index], FBuffer, 64);
      Transform;
      Inc(index, 64);
    until index = bytes;
  until eob;
  Result := GetDigest;
end;

function KLiMD5.sumFile(const fname: string): string;
var
  F: TFileStream;
begin
  F := TFileStream.Create(lse_veryPD(fname), fmShareDenyWrite);
  try
    Result := sumStream(F);
  finally
    F.Free;
  end;
end;

function KLiMD5.sumStr(const S: string): string;
begin
  Result := sumBuf(pchar(S), Length(S));
end;

function KLiMD5.sumStream(Stream: TStream): string;
var
  buf: array[0..4159] of byte;
  len: int64;
  eof: Boolean;
  bytes, index: integer;
begin
  Init;
  eof := False;
  len := 0;
  repeat
    bytes := Stream.Read(buf, 4096);
    len := len + bytes;
    if bytes <> 4096 then
    begin
      buf[bytes] := $80;
      Inc(bytes);
      while (bytes mod 64) <> 56 do
      begin
        buf[bytes] := 0;
        Inc(bytes);
      end;
      len := len * 8;
      Move(len, buf[bytes], 8);
      Inc(bytes, 8);
      eof := True;
    end;
    index := 0;
    repeat
      Move(buf[index], FBuffer, 64);
      Transform;
      Inc(index, 64);
    until index = bytes;
  until eof;
  Result := GetDigest;
end;

procedure KLiMD5.Transform;
const
  S11 = 7;  S12 = 12;  S13 = 17;  S14 = 22;
  S21 = 5;  S22 = 9;   S23 = 14;  S24 = 20;
  S31 = 4;  S32 = 11;  S33 = 16;  S34 = 23;
  S41 = 6;  S42 = 10;  S43 = 15;  S44 = 21;
var
  FAA, FBB, FCC, FDD: cardinal;
begin
  FAA := FA;
  FBB := FB;
  FCC := FC;
  FDD := FD;

  { Round 1 }
  
  FF (PA, PB, PC, PD, @FBuffer[ 0], S11, cardinal($d76aa478)); {  1 }
  FF (PD, PA, PB, PC, @FBuffer[ 1], S12, cardinal($e8c7b756)); {  2 }
  FF (PC, PD, PA, PB, @FBuffer[ 2], S13, cardinal($242070db)); {  3 }
  FF (PB, PC, PD, PA, @FBuffer[ 3], S14, cardinal($c1bdceee)); {  4 }
  FF (PA, PB, PC, PD, @FBuffer[ 4], S11, cardinal($f57c0faf)); {  5 }
  FF (PD, PA, PB, PC, @FBuffer[ 5], S12, cardinal($4787c62a)); {  6 }
  FF (PC, PD, PA, PB, @FBuffer[ 6], S13, cardinal($a8304613)); {  7 }
  FF (PB, PC, PD, PA, @FBuffer[ 7], S14, cardinal($fd469501)); {  8 }
  FF (PA, PB, PC, PD, @FBuffer[ 8], S11, cardinal($698098d8)); {  9 }
  FF (PD, PA, PB, PC, @FBuffer[ 9], S12, cardinal($8b44f7af)); { 10 }
  FF (PC, PD, PA, PB, @FBuffer[10], S13, cardinal($ffff5bb1)); { 11 }
  FF (PB, PC, PD, PA, @FBuffer[11], S14, cardinal($895cd7be)); { 12 }
  FF (PA, PB, PC, PD, @FBuffer[12], S11, cardinal($6b901122)); { 13 }
  FF (PD, PA, PB, PC, @FBuffer[13], S12, cardinal($fd987193)); { 14 }
  FF (PC, PD, PA, PB, @FBuffer[14], S13, cardinal($a679438e)); { 15 }
  FF (PB, PC, PD, PA, @FBuffer[15], S14, cardinal($49b40821)); { 16 }

  { Round 2 }

  GG (PA, PB, PC, PD, @FBuffer[ 1], S21, cardinal($f61e2562)); { 17 }
  GG (PD, PA, PB, PC, @FBuffer[ 6], S22, cardinal($c040b340)); { 18 }
  GG (PC, PD, PA, PB, @FBuffer[11], S23, cardinal($265e5a51)); { 19 }
  GG (PB, PC, PD, PA, @FBuffer[ 0], S24, cardinal($e9b6c7aa)); { 20 }
  GG (PA, PB, PC, PD, @FBuffer[ 5], S21, cardinal($d62f105d)); { 21 }
  GG (PD, PA, PB, PC, @FBuffer[10], S22,  cardinal($2441453)); { 22 }
  GG (PC, PD, PA, PB, @FBuffer[15], S23, cardinal($d8a1e681)); { 23 }
  GG (PB, PC, PD, PA, @FBuffer[ 4], S24, cardinal($e7d3fbc8)); { 24 }
  GG (PA, PB, PC, PD, @FBuffer[ 9], S21, cardinal($21e1cde6)); { 25 }
  GG (PD, PA, PB, PC, @FBuffer[14], S22, cardinal($c33707d6)); { 26 }
  GG (PC, PD, PA, PB, @FBuffer[ 3], S23, cardinal($f4d50d87)); { 27 }
  GG (PB, PC, PD, PA, @FBuffer[ 8], S24, cardinal($455a14ed)); { 28 }
  GG (PA, PB, PC, PD, @FBuffer[13], S21, cardinal($a9e3e905)); { 29 }
  GG (PD, PA, PB, PC, @FBuffer[ 2], S22, cardinal($fcefa3f8)); { 30 }
  GG (PC, PD, PA, PB, @FBuffer[ 7], S23, cardinal($676f02d9)); { 31 }
  GG (PB, PC, PD, PA, @FBuffer[12], S24, cardinal($8d2a4c8a)); { 32 }

  { Round 3 }
  
  HH (PA, PB, PC, PD, @FBuffer[ 5], S31, cardinal($fffa3942)); { 33 }
  HH (PD, PA, PB, PC, @FBuffer[ 8], S32, cardinal($8771f681)); { 34 }
  HH (PC, PD, PA, PB, @FBuffer[11], S33, cardinal($6d9d6122)); { 35 }
  HH (PB, PC, PD, PA, @FBuffer[14], S34, cardinal($fde5380c)); { 36 }
  HH (PA, PB, PC, PD, @FBuffer[ 1], S31, cardinal($a4beea44)); { 37 }
  HH (PD, PA, PB, PC, @FBuffer[ 4], S32, cardinal($4bdecfa9)); { 38 }
  HH (PC, PD, PA, PB, @FBuffer[ 7], S33, cardinal($f6bb4b60)); { 39 }
  HH (PB, PC, PD, PA, @FBuffer[10], S34, cardinal($bebfbc70)); { 40 }
  HH (PA, PB, PC, PD, @FBuffer[13], S31, cardinal($289b7ec6)); { 41 }
  HH (PD, PA, PB, PC, @FBuffer[ 0], S32, cardinal($eaa127fa)); { 42 }
  HH (PC, PD, PA, PB, @FBuffer[ 3], S33, cardinal($d4ef3085)); { 43 }
  HH (PB, PC, PD, PA, @FBuffer[ 6], S34,  cardinal($4881d05)); { 44 }
  HH (PA, PB, PC, PD, @FBuffer[ 9], S31, cardinal($d9d4d039)); { 45 }
  HH (PD, PA, PB, PC, @FBuffer[12], S32, cardinal($e6db99e5)); { 46 }
  HH (PC, PD, PA, PB, @FBuffer[15], S33, cardinal($1fa27cf8)); { 47 }
  HH (PB, PC, PD, PA, @FBuffer[ 2], S34, cardinal($c4ac5665)); { 48 }

  { Round 4 }
  
  II (PA, PB, PC, PD, @FBuffer[ 0], S41, cardinal($f4292244)); { 49 }
  II (PD, PA, PB, PC, @FBuffer[ 7], S42, cardinal($432aff97)); { 50 }
  II (PC, PD, PA, PB, @FBuffer[14], S43, cardinal($ab9423a7)); { 51 }
  II (PB, PC, PD, PA, @FBuffer[ 5], S44, cardinal($fc93a039)); { 52 }
  II (PA, PB, PC, PD, @FBuffer[12], S41, cardinal($655b59c3)); { 53 }
  II (PD, PA, PB, PC, @FBuffer[ 3], S42, cardinal($8f0ccc92)); { 54 }
  II (PC, PD, PA, PB, @FBuffer[10], S43, cardinal($ffeff47d)); { 55 }
  II (PB, PC, PD, PA, @FBuffer[ 1], S44, cardinal($85845dd1)); { 56 }
  II (PA, PB, PC, PD, @FBuffer[ 8], S41, cardinal($6fa87e4f)); { 57 }
  II (PD, PA, PB, PC, @FBuffer[15], S42, cardinal($fe2ce6e0)); { 58 }
  II (PC, PD, PA, PB, @FBuffer[ 6], S43, cardinal($a3014314)); { 59 }
  II (PB, PC, PD, PA, @FBuffer[13], S44, cardinal($4e0811a1)); { 60 }
  II (PA, PB, PC, PD, @FBuffer[ 4], S41, cardinal($f7537e82)); { 61 }
  II (PD, PA, PB, PC, @FBuffer[11], S42, cardinal($bd3af235)); { 62 }
  II (PC, PD, PA, PB, @FBuffer[ 2], S43, cardinal($2ad7d2bb)); { 63 }
  II (PB, PC, PD, PA, @FBuffer[ 9], S44, cardinal($eb86d391)); { 64 }

  FA := FA + FAA;
  FB := FB + FBB;
  FC := FC + FCC;
  FD := FD + FDD;

  FillChar(FBuffer, SizeOf(FBuffer), #0);
end;

initialization
begin
  { do nothing }
end;

{$IFDEF WINDOWS}
finalization
begin
  if com_need_uninitialize then CoUninitialize;
end;
{$ENDIF}

end.
