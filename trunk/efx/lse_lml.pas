unit lse_lml;

interface

{$ifndef USE_LSEU}
{$ifdef IS_LYSEE}
{$define USE_LSEU}
{$endif}
{$endif}

uses
  SysUtils, Classes, lseu, lse_funcs;

const
  LmlMargin     = 2;
  LmlHeadChar   = IDChar;
  LmlChar       = ['.', ':'] + LmlHeadChar;
  LmlBufferSize = 1024;
  
type
  TLmlStreamType = (lstUnknown, lstMemory, lstFile);

  TLmlStream = class(TLseObject)
  private
    FStreamType: TLmlStreamType;               {<--流类型}
    FStream: TStream;                         {<--outside stream}
    FBuf: array[0..LmlBufferSize - 1] of char; {<--buffer in memory}
    FLen: longint;                            {<--data length}
    FPtr: longint;                            {<--position of the buffer}
    FFreeOnClose: boolean;                    {<--free FStream when close}
    FTag: integer;
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    function GetSize: Int64;
    procedure SetSize64(const NewSize: Int64);
    procedure ResetPosition;
    function GetCrPos(var X: integer): boolean;
    function PrepareBuf: boolean;
    function GetEof: boolean;
  protected
    procedure ErrorOpenStream;
    procedure ErrorOpenFileStream;
    procedure ErrorOpenMemoryStream;
  public
    constructor Create;
    destructor Destroy;override;
    procedure CloseStream;virtual;
    procedure OpenStream(AStream: TStream; FreeStream: boolean);virtual;
    procedure OpenFileStream(const FileName: string; Mode: word);virtual;
    procedure OpenMemoryStream;virtual;

    {write}

    function Write(const Buffer; Count: Longint): Longint;
    procedure WriteBuffer(const Buffer; Count: Longint);
    function WriteText(const Text: string): Longint;
    function WriteLine(const Text: string = ''): Longint; // Text + CR
    function LineWrite(const Text: string = ''): Longint; // CR + Text
    function WriteFormat(const Fmt: string; const Args: array of const;
      LineBreak: boolean = false): Longint;
    function CopyFromStream(Source: TLmlStream; Count: Int64): Int64;
    function CopyFrom(Source: TStream; Count: Int64): Int64;

    {read}

    function Read(var Buffer; Count: Longint): Longint;
    procedure ReadBuffer(var Buffer; Count: Longint);
    function ReadText(Count: integer): string;
    function ReadLine(var S: string): boolean;
    function ReadNotEmptyLine(var S: string): boolean;

    {seek}

    function Seek(Offset: Longint; Origin: Word): Longint;overload;
    function Seek(Offset: Int64; Origin: TSeekOrigin): Int64;overload;

    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
    property Eof: boolean read GetEof;
    property Stream: TStream read FStream;
    property FreeOnClose: boolean read FFreeOnClose write FFreeOnClose;
    property StreamType: TLmlStreamType read FStreamType;
    property Tag: integer read FTag write FTag;
  end;

  TLmlMemoryStream = class(TLmlStream)
  public
    constructor Create;
    procedure OpenStream(AStream: TStream; FreeStream: boolean);override;
    procedure OpenFileStream(const FileName: string; Mode: word);override;
  end;

  TLmlFileStream = class(TLmlStream)
  private
    FFileName: string;
  public
    constructor Create(const FileName: string; Mode: Word);
    procedure CloseStream;override;
    procedure OpenStream(AStream: TStream; FreeStream: boolean);override;
    procedure OpenFileStream(const FileName: string; Mode: word);override;
    procedure OpenMemoryStream;override;
    property FileName: string read FFileName;
  end;

  TLmlException = class(Exception);

  TLmlDocumentReader = class;
  TLmlDocument       = class;
  TLmlNode           = class;
  TRecRecord         = class;
  TRecField          = class;
  TRecFieldList      = class;

  TLmlNodeType = (
    lntUnknown,
    lntElement,
    lntAttribute,
    lntText,
//  lntCData, lntEntityRef, lntEntity, lntProcessingInstr, lntComment,
    lntDocument
//  lntDocType, lntDocFragment, lntNotation
  );

  TLmlReaderEvent = procedure(Sender: TObject; var Terminate: boolean) of object;

  TLmlReader = class(TLseObject)
  private
    FInStream: TLmlStream;        {<--input source stream}
    FNodeType: TLmlNodeType;     {<--current element}
    FNodeName: string;           {<--element name}
    FNodeText: string;           {<--element value}
    FLevel: integer;             {<--element level}
    FIsNode: boolean;            {<--true node false attribute}
    FIsTextNode: boolean;        {<--it is a text node}
    FTerminated: boolean;        {<--termination flag}
    FReleaseStream: boolean;     {<--relase FInStream when destory}
    FOnNotify: TLmlReaderEvent;  {<--notification event}
    procedure Notify;
    procedure Reset;
    function GetIsAttr: boolean;
  public
    constructor Create(InStream: TLmlStream);
    destructor Destroy;override;
    procedure Execute;
    procedure Terminate;
    property NodeType: TLmlNodeType read FNodeType;
    property NodeName: string read FNodeName;
    property NodeText: string read FNodeText;
    property Level: integer read FLevel;
    property IsNode: boolean read FIsNode;
    property IsTextNode: boolean read FIsTextNode;
    property IsAttr: boolean read GetIsAttr;
    property Terminated: boolean read FTerminated write FTerminated;
    property ReleaseStream: boolean read FReleaseStream write FReleaseStream;
    property OnNotify: TLmlReaderEvent read FOnNotify write FOnNotify;
  end;

  TLmlWriter = class(TLseObject)
  private
    FOutStream: TLmlStream;
  public
    constructor Create(OutStream: TLmlStream);
  end;

  TLmlAttrList = class(TStringList)
  private
    function GetValue(Index: integer): string;
    procedure SetValue(Index: integer; const Value: string);
  public
    constructor Create;
    function IndexOf(const Name: string): integer;override;
    function Exist(const Name: string): boolean;
    function Valued(const Name: string; var Value: string): boolean;
    procedure Remove(const Name: string);
    procedure AssignText(const Text: string);
    procedure AssignStrings(Strings: TStrings);
    { read }
    function Read(const Name, DefaultValue: string): string;
    function ReadBool(const Name: string; DefaultValue: boolean): boolean;
    function ReadDate(const Name: string; DefaultValue: TDateTime): TDateTime;
    function ReadTime(const Name: string; DefaultValue: TDateTime): TDateTime;
    function ReadDateTime(const Name: string; DefaultValue: TDateTime): TDateTime;
    function ReadFloat(const Name: string; DefaultValue: double): double;
    function ReadMoney(const Name: string; DefaultValue: currency): currency;
    function ReadInteger(const Name: string; DefaultValue: int64): int64;
    function ReadHex(const Name: string; DefaultValue: int64): int64;
    { write }
    procedure Write(const Name, Value: string; DoWrite: boolean = true);
    procedure WriteBool(const Name: string; Value: boolean; DoWrite: boolean = true);
    procedure WriteDate(const Name: string; Value: TDateTime; DoWrite: boolean = true);
    procedure WriteTime(const Name: string; Value: TDateTime; DoWrite: boolean = true);
    procedure WriteDateTime(const Name: string; Value: TDateTime; DoWrite: boolean = true);
    procedure WriteFloat(const Name: string; Value: double; DoWrite: boolean = true);
    procedure WriteMoney(const Name: string; Value: currency; DoWrite: boolean = true);
    procedure WriteInteger(const Name: string; Value: int64; DoWrite: boolean = true);
    procedure WriteHex(const Name: string; Value: int64; DoWrite: boolean = true);
    { property }
    property Value[Index: integer]: string read GetValue write SetValue;
  end;

  TLmlNode = class(TLseObject)
  private
    FDocument: TLmlDocument;
    FParent: TLmlNode;
    FName: string;
    FText: string;
    FTextNodeCount: integer;
    FAttrList: TLmlAttrList;
    FItemList: TList;
    FNodeType: TLmlNodeType;
    FData: pointer;
    function GetItemCount: integer;
    function GetItem(Index: integer): TLmlNode;
    function GetLevel: integer;
    procedure SetName(const Value: string);
    function GetNamePath: string;
    function GetRoot: TLmlNode;
    function GetItemNameList: string;
    function GetID: cardinal;
    function GetIndex: integer;
    procedure SetIndex(const Value: integer);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetIsTextNode: boolean;
    function GetPPCode: string;
    procedure SetPPCode(const Value: string);
  public
    constructor Create(Document: TLmlDocument; const Name, Text: string);
    destructor Destroy;override;
    procedure ParsePPC(stream: TLmlStream); // parentheses-pair code
    procedure WritePPC(stream: TLmlStream);
    procedure SaveToString(var S: string);
    function TraceNode(const APath: string; ForceExists: boolean): TLmlNode;
    function TraceAttr(const AAttr: string; ForceExists: boolean): TLmlNode;
    function IsChildOf(node: TLmlNode): boolean;
    function IsFrom(node: TLmlNode): boolean;
    function Add(const Name, Text: string): TLmlNode;
    function AddText(const Text: string): TLmlNode;
    function Insert(Index: integer; const Name, Text: string): TLmlNode;
    function InsertText(Index: integer; const Text: string): TLmlNode;
    function IndexOf(const Name: string; ID: cardinal): integer;
    function Find(const Name: string; ID: cardinal): TLmlNode;
    function FindByName(const Name: string; matchCase: boolean): TLmlNode;
    function Exist(const Name: string; ID: cardinal): boolean;
    procedure Remove(const Name: string; ID: cardinal);overload;
    procedure Remove(item: TLmlNode);overload;
    procedure Delete(Index: integer);
    procedure Clear;
    function Dir(showID: boolean): string;
    procedure Assign(Node: TLmlNode);
    function IncRefcount: integer;override;
    function DecRefcount: integer;override;
    procedure NilDocument;
    procedure Sort(Recurse, IgnoreCase, DescOrder: boolean);
    property Count: integer read GetItemCount;
    property Item[Index: integer]: TLmlNode read GetItem;default;
    property ItemNameList: string read GetItemNameList;
    property Name: string read FName write SetName;
    property ID: cardinal read GetID;
    property Text: string read GetText write SetText;
    property NodeText: string read FText write FText;
    property Level: integer read GetLevel;
    property Parent: TLmlNode read FParent;
    property Root: TLmlNode read GetRoot;
    property NamePath: string read GetNamePath;
    property Document: TLmlDocument read FDocument;
    property attr: TLmlAttrList read FAttrList;
    property Index: integer read GetIndex write SetIndex;
    property NodeType: TLmlNodeType read FNodeType;
    property IsTextNode: boolean read GetIsTextNode;
    property TextNodeCount: integer read FTextNodeCount;
    property Data: pointer read FData write FData;
    property PPCode: string read GetPPCode write SetPPCode;
  end;

  TLmlNodeEvent = procedure(Sender: TObject; Node: TLmlNode) of object;

  TLmlDocumentState = (ldsLoading, ldsSaving, ldsDestroying);
  TLmlDocumentStates = set of TLmlDocumentState;

  TLmlDocument = class(TLseObject)
  private
    FFileName: string;
    FRoot: TLmlNode;
    FState: TLmlDocumentStates;
    FLoadCount: integer;
    FSaveCount: integer;
    FXmlDocStandalone: boolean;
    FXmlDocVersion: string;
    FXmlDocEncoding: string;
    FOnLoadingNode: TLmlNodeEvent;
    FOnLoadNode: TLmlNodeEvent;
    procedure SetFileName(const Value: string);
    function CreateDocumentNode(const Name, Text: string): TLmlNode;
    function CreateNode(Parent: TLmlNode; const Name, Text: string): TLmlNode;
    function CreateTextNode(Parent: TLmlNode; const Text: string): TLmlNode;
    procedure NotifyRemove(node: TLmlNode);
    function GetState(Index: TLmlDocumentState): boolean;
    procedure SetState(Index: TLmlDocumentState; const Value: boolean);
    function GetRoot: TLmlNode;
  public
    constructor Create(const RootName: string);virtual;
    constructor CreateFromFile(const FileName: string);
    destructor Destroy;override;
    procedure Assign(doc: TLmlDocument);
    procedure Clear;
    procedure BeginLoad;
    procedure EndLoad;
    procedure BeginSave;
    procedure EndSave;
    procedure LoadFromPPCFile(const FileName: string);
    procedure SaveToPPCFile(const FileName: string);
    procedure LoadFromPPCStream(stream: TLmlStream);
    procedure SaveToPPCStream(stream: TLmlStream);
    procedure LoadFromPPCString(const PPC: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure EventLoadingNode(Node: TLmlNode);
    procedure EventLoadNode(Node: TLmlNode);
    property Root: TLmlNode read GetRoot;
    property FileName: string read FFileName write SetFileName;
    property Loading: boolean index ldsLoading read GetState write SetState;
    property Saving: boolean index ldsSaving read GetState write SetState;
    property Destroying: boolean index ldsDestroying read GetState write SetState;
    property OnLoadingNode: TLmlNodeEvent read FOnLoadingNode write FOnLoadingNode;
    property OnLoadNode: TLmlNodeEvent read FOnLoadNode write FOnLoadNode;
    property XmlDocVersion: string read FXmlDocVersion write FXmlDocVersion;
    property XmlDocEncoding: string read FXmlDocEncoding write FXmlDocEncoding;
    property XmlDocStandalone: boolean read FXmlDocStandalone write FXmlDocStandalone;
  end;

  TLmlDocumentReader = class(TLseObject)
  private
    FDocument: TLmlDocument;
    FReader: TLmlReader;
    FStack: TList;
  protected
    procedure ReaderNotify(Sender: TObject; var Terminate: boolean);
    procedure PushNode(node: TLmlNode);
    procedure PopNode;
    procedure PopToCount(count: integer);
  public
    constructor Create(ADocument: TLmlDocument);
    destructor Destroy;override;
    procedure ReadFromStream(Stream: TLmlStream);
  end;

  // RECORD --------------------------------------------------------------------
  
  TRecFieldType = (rftInteger, rftString, rftBoolean, rftFloat, rftMoney);

  RRecValue = packed record
  case TRecFieldType of
    rftInteger: (VInteger: integer);
    rftString : (VString: pointer);
    rftBoolean: (VBoolean: boolean);
    rftFloat  : (VFloat: double);
    rftMoney  : (VMoney: currency);
  end;
  PRecValue = ^RRecValue;
  
  TRecRange = set of 0..255;

  TRecRecord = class(TLseObject)
  private
    FFieldList: TRecFieldList;
    FValues: array of RRecValue;
    FData: pointer;
    function GetCount: integer;
    function GetField(index: integer): TRecField;
    function ValueAt(index: integer): PRecValue;
    function GetFieldName(Index: integer): string;
    function GetFieldTitle(Index: integer): string;
    function GetFieldType(Index: integer): TRecFieldType;
    function GetPPCode: string;
    procedure SetPPCode(const Value: string);
  public
    constructor Create(FieldList: TRecFieldList);
    destructor Destroy;override;
    function IndexOf(const FieldName: string): integer;
    function FindField(const FieldName: string): TRecField;
    function FieldByName(const FieldName: string): TRecField;
    function IsSame(ARecord: TRecRecord): boolean;
    function CheckSame(ARecord: TRecRecord): boolean;
    function Clear(Range: TRecRange = []): TRecRecord;
    function ClearAll: TRecRecord;
    procedure LoadFromPPCFile(const FileName: string);
    procedure SaveToPPCFile(const FileName: string);
    function Assign(ARecord: TRecRecord; Range: TRecRange = []): boolean;virtual;
    function ReadMatched(ARecord: TRecRecord; Range: TRecRange = []): TRecRange;virtual;
    procedure Reload;virtual;
    function LoadFromNode(Node: TLmlNode): TLmlNode;virtual;
    function SaveToNode(Node: TLmlNode): TLmlNode;virtual;
    function Explain(Index: integer): string;virtual;
    function Compare(Other: TRecRecord): integer;virtual;
    function Filter: string;virtual;
    function VaryRange(Range: TRecRange): TRecRange;
    function FullRange: TRecRange;
    function Changed(ARecord: TRecRecord): boolean;
    function Diff(ARecord: TRecRecord): TRecRange;
    {$ifdef USE_LSEU}
    function ReadDs(DS: TLseDataset; Range: TRecRange = []): TRecRange;
    {$endif}
    function SQLValue(Index: integer): string;
    function InsertSQL(const Table: string; Range: TRecRange = []): string;
    function UpdateSQL(const Table, Filter: string; Range: TRecRange = []): string;
    { GET }
    function GetIV(Index: integer): integer;
    function GetSV(Index: integer): string;
    function GetBV(Index: integer): boolean;
    function GetFV(Index: integer): double;
    function GetMV(Index: integer): currency;
    { SET }
    procedure SetIV(Index: integer; Value: integer);
    procedure SetSV(Index: integer; const Value: string);
    procedure SetBV(Index: integer; Value: boolean);
    procedure SetFV(Index: integer; Value: double);
    procedure SetMV(Index: integer; Value: currency);
    { PROP }
    property FieldList: TRecFieldList read FFieldList;
    property FieldCount: integer read GetCount;
    property Fields[index: integer]: TRecField read GetField;default;
    property FieldType[Index:integer]: TRecFieldType read GetFieldType;
    property FieldName[Index:integer]: string read GetFieldName;
    property FieldTitle[Index:integer]: string read GetFieldTitle;
    property PPCode: string read GetPPCode write SetPPCode;
    property Data: pointer read FData write FData;
  end;

  TRecRecordClass = class of TRecRecord;
  TRecCreate = function: TRecRecord;

  TRecField = class(TLseObject)
  private
    FFieldList: TRecFieldList;
    FFieldType: TRecFieldType;
    FFieldSize: integer;
    FFieldName: string;
    FFieldTitle: string;
    FFieldIndex: integer;
    FPickList: TStringList;
    FUnique: boolean;
    function InitFV(FV: PRecValue): PRecValue;
    function ClearFV(FV: PRecValue): PRecValue;
    function CopyFV(FV, Desti: PRecValue): PRecValue;
    function EqualFV(V1, V2: PRecValue): boolean;
    { GET }
    function GetInteger(FV: PRecValue): integer;
    function GetString(FV: PRecValue): string;
    function GetBoolean(FV: PRecValue): boolean;
    function GetFloat(FV: PRecValue): double;
    function GetMoney(FV: PRecValue): currency;
    { SET }
    function SetInteger(FV: PRecValue; Value: integer): PRecValue;
    function SetString(FV: PRecValue; const Value: string): PRecValue;
    function SetBoolean(FV: PRecValue; Value: boolean): PRecValue;
    function SetFloat(FV: PRecValue; Value: double): PRecValue;
    function SetMoney(FV: PRecValue; Value: currency): PRecValue;
    { MSC }
    function Explain(FV: PRecValue): string;
    function SQLValue(FV: PRecValue): string;
    function GetUnique: boolean;
    procedure SetPrimary(const Value: boolean);
    function GetPrimary: boolean;
  public
    constructor Create(FieldList: TRecFieldList);
    destructor Destroy;override;
    property FieldList: TRecFieldList read FFieldList;
    property FieldType: TRecFieldType read FFieldType;
    property FieldSize: integer read FFieldSize;
    property FieldName: string read FFieldName;
    property FieldTitle: string read FFieldTitle;
    property FieldIndex: integer read FFieldIndex;
    property PickList: TStringList read FPickList write FPickList;
    property Unique: boolean read GetUnique write FUnique;
    property Primary: boolean read GetPrimary write SetPrimary;
  end;

  TRecFieldList = class(TLseObject)
  private
    FName: string;
    FFields: TList;
    FRecordClass: TRecRecordClass;
    FPrimaryField: TRecField;
    function GetCount: integer;
    function GetField(index: integer): TRecField;
  public
    constructor Create(const Name: string);
    destructor Destroy;override;
    function AddIntegerField(const FieldName, Title: string): TRecField;
    function AddStringField(const FieldName, Title: string; FieldSize: integer = 0): TRecField;
    function AddBooleanField(const FieldName, Title: string): TRecField;
    function AddFloatField(const FieldName, Title: string): TRecField;
    function AddMoneyField(const FieldName, Title: string): TRecField;
    function IndexOf(const FieldName: string): integer;
    function FindField(const FieldName: string): TRecField;
    function FieldByName(const FieldName: string): TRecField;
    function CreateRecord: TRecRecord;
    property Name: string read FName;
    property FieldCount: integer read GetCount;
    property Fields[index: integer]: TRecField read GetField;default;
    property RecordClass: TRecRecordClass read FRecordClass write FRecordClass;
    property PrimaryField: TRecField read FPrimaryField write FPrimaryField;
  end;

(*----------------------------------------------------------------------
(*函数名: SaveStreamToFile
(*
(*功  能: 将流保存到指定的文件
(*
(*参  数: AStream: TLmlStream - 数据流
(*        const FileName: string - 文件名
(*
(*返回值:
(*
(*异  常:
(*--------------------------------------------------------------------*)
procedure SaveStreamToFile(AStream: TLmlStream; const FileName: string);

{{----------------------------------------------------------------------
{{函数名: lml_is_tag_name                                     [20050406]
{{
{{功  能: 检查节点/属性名称是否符合要求
{{
{{参  数: const S: string - 节点/属性名称
{{
{{返回值: boolean - 符合要求时返回真值
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_is_tag_name(const S: string): boolean;

{{----------------------------------------------------------------------
{{函数名: lml_prefix                                          [20070105]
{{
{{功  能: 返回以':'分隔的节点/属性名称前缀
{{
{{参  数: const tag_name: string - 合法的节点/属性名称
{{
{{返回值: string - 名称前缀
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_prefix(const tag_name: string): string;

{{----------------------------------------------------------------------
{{函数名: lml_has_prefix                                      [20070105]
{{
{{功  能: 检查节点/属性名称是否含有指定的前缀
{{
{{参  数: const tag_name: string - 合法的节点/属性名称
{{        const prefix: string - 前缀
{{
{{返回值: boolean - 匹配时返回真
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_has_prefix(const tag_name, prefix: string): boolean;

{{----------------------------------------------------------------------
{{函数名: lml_check_tag_name                                  [20050406]
{{
{{功  能: 检查节点/属性名称是否符合要求
{{
{{参  数: const S: string - 节点/属性名称
{{
{{返回值:
{{
{{异  常: 不符合要求时抛出异常
-----------------------------------------------------------------------}
procedure lml_check_tag_name(const S: string);

{{----------------------------------------------------------------------
{{函数名: lml_check_list_index                                [20050406]
{{
{{功  能: 检查列表索引是否在允许的范围内
{{
{{参  数: index: integer - 列表索引
{{        count: integer - 列表成员数
{{
{{返回值:
{{
{{异  常: 索引超出允许的范围时抛出异常
-----------------------------------------------------------------------}
procedure lml_check_list_index(index, count: integer);

{{----------------------------------------------------------------------
{{函数名: lml_extract_path_attr                               [20050406]
{{
{{功  能: 解码节点属性路径，分出节点路径和属性名称，路径分隔符为'/'
{{
{{参  数: const S: string - 节点属性路径
{{        var path: string - 节点路径
{{        var attr: string - 属性名称
{{
{{返回值: integer - 最后一个节点路径分隔符的位置(从1开始)
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_extract_path_attr(const S: string; var path, attr: string): integer;

{{----------------------------------------------------------------------
{{函数名: lml_encode_line                                     [20050406]
{{
{{功  能: 写LML文件时对节点和属性的文本进行编码
{{
{{参  数: const line: string - 等待编码的文本
{{
{{返回值: string - 编码后的文本
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_encode_line(const line: string): string;

{{----------------------------------------------------------------------
{{函数名: lml_decode_line                                     [20050406]
{{
{{功  能: 读LML文件时解码节点和属性的文本
{{
{{参  数: const line: string - 等待解码的文本
{{
{{返回值: string - 解码后的文本
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_decode_line(const line: string): string;

{{----------------------------------------------------------------------
{{函数名: lml_write_str                            [20050406 - 20050406]
{{
{{功  能: 将字符串写入流中
{{
{{参  数: stream: TStream - 流
{{        const S: string - 字符串
{{
{{返回值: integer - 写入长度
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_write_str(stream: TStream; const S: string): integer;

{{----------------------------------------------------------------------
{{函数名: lml_match_at                             [20050406 - 20050406]
{{
{{功  能: 检查指定的子串是否在字符串中指定的位置出现，不区分大小写
{{
{{参  数: const S: string - 子串
{{        const Text: string - 字符串
{{        Start: integer - 起始位置
{{
{{返回值: boolean - 匹配时返回真值
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_match_at(const S: string; const Text: string; Start: integer): boolean;

{{----------------------------------------------------------------------
{{函数名: lml_find_match_at                        [20050406 - 20050406]
{{
{{功  能: 检查指定的子串是否在字符串中指定的位置出现，不区分大小写
{{
{{参  数: const S: string - 子串
{{        const Text: string - 字符串
{{        Start: integer - 起始位置
{{        Close: integer - 结束位置
{{        var APos: integer - 出现的位置
{{
{{返回值: boolean - 找到时返回真值
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_find_match_at(const S: string; const Text: string;
  Start, Close: integer; var APos: integer): boolean;

{{----------------------------------------------------------------------
{{函数名: lml_match_char                           [20050406 - 20050406]
{{
{{功  能: 检查两个字符是否相同，不区分大小写
{{
{{参  数: c1, c2: char - 两个字符
{{
{{返回值: boolean - 匹配时返回真值
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_match_char(c1, c2: char): boolean;

{{----------------------------------------------------------------------
{{函数名: lml_encode_ppc | lml_decode_ppc          [20071121 - 20071121]
{{
{{功  能: 进行PPC(parentheses-pair code)编码和解码
{{
{{参  数: const source: string - 原字符串
{{
{{返回值: string - 编码或解码后的字符串
{{
{{异  常:
-----------------------------------------------------------------------}
function lml_encode_ppc(const source: string): string;
function lml_decode_ppc(const source: string): string;

{{----------------------------------------------------------------------
{{函数名: xml_encode_str                           [20050406 - 20050406]
{{
{{功  能: 对字符串进行XML编码，将"<>\'\"&"等字符替换为编码格式
{{
{{参  数: const S: string - 编码前文本
{{
{{返回值: string - 编码结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_encode_buf(buf: pchar; count: integer): string;
function xml_encode_str(const S: string): string;

{{----------------------------------------------------------------------
{{函数名: xml_unescape_ANSI                        [20050406 - 20050406]
{{
{{功  能: lml_encode_str的逆过程，同时翻译&#XXXXX编码(UNICODE)
{{
{{参  数: const S: string - 解码前文本
{{
{{返回值: string - 解码结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_unescape_ANSI(const S: string): string;

{{----------------------------------------------------------------------
{{函数名: xml_unescape_ANSI                        [20050406 - 20050406]
{{
{{功  能: lml_encode_str的逆过程，同时翻译&#XXXXX编码(UNICODE)，UTF8编码
{{
{{参  数: const S: string - 解码前文本
{{
{{返回值: string - 解码结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_unescape_UTF8(const S: string): string;

{{----------------------------------------------------------------------
{{函数名: lml_unescape                             [20050406 - 20050406]
{{
{{功  能: 将类似&amp;&quot;...;的编码还原成原始的样子
{{
{{参  数: const S: string - 解码前文本
{{
{{返回值: string - 解码结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_unescape(const S: string): string;

{{----------------------------------------------------------------------
{{函数名: xml_trim_pos                             [20050406 - 20050406]
{{
{{功  能: 定位字符串两边的空格
{{
{{参  数: const S: string - 字符串
{{        var Start: integer - 第一个非制表符的位置
{{        var Close: integer - 后续制表符的起始位置
{{
{{返回值: boolean - 含有非制表符时返回真
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_trim_pos(const S: string; var Start, Close: integer): boolean;

{{----------------------------------------------------------------------
{{函数名: xml_wrap_text                            [20050406 - 20050406]
{{
{{功  能: 折断文本
{{
{{参  数: const S: string - 字符串
{{        const SepStr: string - 用于折断的文本
{{        Interval: integer - 行长度
{{
{{返回值: string - 折断后的文本
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_wrap_text(const S, SepStr: string; Interval: integer): string;

{{----------------------------------------------------------------------
{{函数名: xml_skip_space                           [20050406 - 20050406]
{{
{{功  能: 跳过流中的空格
{{
{{参  数: stream: TStream - 流
{{        var ch: char - 读取的字符
{{
{{返回值: boolean - 遇到非空格符时返回真
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_skip_space(stream: TStream; var ch: char): boolean;

{{----------------------------------------------------------------------
{{函数名: xml_skip_space                           [20050407 - 20050407]
{{
{{功  能: 采用W3C标准格式化时间: http://www.w3.org/TR/NOTE-datetime
{{
{{参  数: AData: TDateTime - 时间
{{
{{返回值: string - 格式化后的字符串
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_datetime_str(ADate: TDateTime): string;
function xml_str_datetime(const ADate: string; DefValue: TDateTime): TDateTime;

{{----------------------------------------------------------------------
{{函数名: xml_unicode_utf8 | xml_utf8_unicode      [20050408 - 20050408]
{{
{{功  能: 在UNICODE和UTF8格式字符串转换
{{
{{参  数: const S: - 需要转化的字符串
{{
{{返回值: 转化后的结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_unicode_utf8(const S: widestring): string;
function xml_utf8_unicode(const S: string): WideString;

{{----------------------------------------------------------------------
{{函数名: xml_ansi_utf8 | xml_utf8_ansi            [20050408 - 20050408]
{{
{{功  能: 在ANSI和UTF8格式字符串转换
{{
{{参  数: const S: - 需要转化的字符串
{{
{{返回值: 转化后的结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_ansi_utf8(const S: string): string;
function xml_utf8_ansi(const S: string): string;

{{----------------------------------------------------------------------
{{函数名: xml_quote | xml_unquote                  [20050408 - 20050408]
{{
{{功  能: 给字符串加引号或去引号
{{
{{参  数: const S: string - 需要转化的字符串
{{
{{返回值: 转化后的结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_quote(const S: string): string;
function xml_unquote(const S: string): string;

{{----------------------------------------------------------------------
{{函数名: xml_parse_attr                           [20050408 - 20050408]
{{
{{功  能: 解析XML字符串中的属性和属性值
{{
{{参  数: const S: string - XML字符串
{{
{{返回值: 转化后的结果
{{
{{异  常:
-----------------------------------------------------------------------}
function xml_parse_attr(const S: string; Start, Close: integer; Attrs: TStrings): TStrings;

{{----------------------------------------------------------------------
{{函数名: list_compare_record                                 [20071129]
{{
{{功  能: 比较两个TRecRecord类实体的关系
{{
{{参  数: Item1, Item2: pointer - 两个TRecRecord类实体的地址
{{
{{返回值: integer - 负值表示小 0表示相同 正值表示大
{{
{{异  常:
-----------------------------------------------------------------------}
function list_compare_record(Item1, Item2: pointer): integer;

{{----------------------------------------------------------------------
{{函数名: list_setup                                          [20071130]
{{
{{功  能: 查询并建立TRecRecord类实体列表
{{
{{参  数: const SQL: string - 查询语句
{{        Creator: TRecCreate - 创建TRecRecord类实体的函数
{{
{{返回值: TList - 结果列表
{{
{{异  常:
-----------------------------------------------------------------------}
function list_setup(const SQL: string; Creator: TRecCreate): TList;overload;
function list_setup(const SQL: string;
  const Args: array of const; Creator: TRecCreate): TList;overload;
{$ifdef USE_LSEU}
function list_setup(ds: TLseDataset;Creator: TRecCreate): TList;
overload;
{$endif}

{{----------------------------------------------------------------------
{{函数名: list_release                                        [20071130]
{{
{{功  能: 释放列表对象及其存储的对象
{{
{{参  数: var List: TList - 对象列表
{{
{{返回值: integer - 列表长度
{{
{{异  常:
-----------------------------------------------------------------------}
function list_release(var List: TList): integer;

function lml_read_value(Strings: TStrings; const Name, DefValue: string): string;
function lml_readv(Strings: TStrings; const Name: string; var Value: string): boolean;

implementation

uses
  RTLConsts, windows, math, DateUtils, lse_efx;

function list_compare_record(Item1, Item2: pointer): integer;
var
  R1, R2: TRecRecord;
begin
  R1 := Item1;
  R2 := Item2;
  Result := R1.Compare(R2);
end;

function list_setup(const SQL: string; Creator: TRecCreate): TList;
{$ifdef USE_LSEU}
var
  db: TLseDatabase;
  ds: TLseDataset;
{$endif}
begin
  {$ifdef USE_LSEU}
  db := lse_sysdb;
  try
    ds := lse_sysdb.OpenSQL(SQL);
    try
      Result := list_setup(ds, Creator);
    finally
      ds.Free;
    end;
  finally
    db.Free;
  end;
  {$else}
  Result := nil;
  {$endif}
end;

function list_setup(const SQL: string;
  const Args: array of const; Creator: TRecCreate): TList;
begin
  Result := list_setup(Format(SQL, Args), Creator);
end;

{$ifdef USE_LSEU}
function list_setup(ds: TLseDataset;Creator: TRecCreate): TList;
var
  rr: TRecRecord;
begin
  Result := TList.Create;
  try
    while not ds.Eof do
    begin
      rr := Creator();
      rr.ReadDs(ds);
      Result.Add(rr);
      ds.Next;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{$endif}

function list_release(var List: TList): integer;
var
  index: integer;
begin
  if List <> nil then
  try
    Result := List.Count;
    for index := List.Count - 1 downto 0 do
      TObject(List[index]).Free;
  finally
    FreeAndNil(List);
  end
  else Result := 0;
end;

function lml_read_value(Strings: TStrings; const Name, DefValue: string): string;
begin
  Result := DefValue;
  lml_readv(Strings, Name, Result);
end;

function lml_readv(Strings: TStrings; const Name: string; var Value: string): boolean;
var
  index: integer;
begin
  index := Strings.IndexOfName(Name);
  Result := (index >= 0);
  if Result then
    Value := System.Copy(Strings[index], Length(Name) + 2, MaxInt);
end;

function lml_is_tag_name(const S: string): boolean;
var
  base: pchar;
begin
  base := pchar(S);
  Result := (base <> nil) and (base^ in LmlHeadChar) and
    __inCharSet(base, LmlChar);
end;

function lml_prefix(const tag_name: string): string;
var
  index: integer;
begin
  index := Pos(':', tag_name);
  if index > 0 then
    Result := Copy(tag_name, 1, index - 1) else
    Result := '';
end;

function lml_has_prefix(const tag_name, prefix: string): boolean;
begin
  Result := (prefix <> '') and
            (prefix = lml_prefix(tag_name));
end;

procedure lml_check_tag_name(const S: string);
begin
  if not lml_is_tag_name(S) then
    raise TLmlException.CreateFmt('Invalid tag name (%s)', [S]);
end;

procedure lml_check_list_index(index, count: integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise TLmlException.CreateFmt('List index out of bounds (%d)', [Index]);
end;

function lml_extract_path_attr(const S: string; var path, attr: string): integer;
begin
  Result := Length(S);
  while (Result > 0) and (S[Result] <> '/') do
    Dec(Result); {<--Result Result}
  path := Trim(Copy(S, 1, Result));
  attr := Trim(Copy(S, Result + 1, MaxInt));
end;

function lml_encode_line(const line: string): string;
var
  base, next: PChar;
  slen: integer;

  function CalcBufSize(buf: pchar; len: integer): integer;
  begin
    Result := 0;
    if buf <> nil then
    begin
      while (len > 0) and (buf^ <> #0) do
      begin
        Inc(Result, 1 + integer(buf^ in ['\', #10, #13]));
        Dec(len);
        Inc(buf);
      end;
      if (buf - 1)^ in [#1..' ', ';'] then
        Inc(Result); {<--plus ';'}
    end;
  end;

begin
  base := PChar(line);
  slen := CalcBufSize(base, Length(line));
  SetLength(Result, slen);
  if slen = 0 then Exit;
  next := PChar(Result);
  while base^ <> #0 do
  begin
    if base^ in ['\', #10, #13] then
    begin
      next^ := '\';
      Inc(next);
      case base^ of
        #10: next^ := 'n';
        #13: next^ := 'r';
        '\': next^ := '\';
      end;
    end
    else next^ := base^;
    Inc(next);
    Inc(base);
  end;
  base := PChar(Result);
  if (next - base) < slen then
  begin
    next^ := ';';
    Inc(next);
  end;
  SetLength(Result, next - base);
end;

function lml_decode_line(const line: string): string;
var
  base, next: PChar;
begin
  if line <> '' then
  begin
    SetLength(Result, Length(line));
    base := PChar(line);
    next := PChar(Result);
    while base^ <> #0 do
    begin
      if base^ = '\' then
      begin
        Inc(base);
        if base^ = '\' then next^ := '\' else
        if base^ = 'n' then next^ := #10 else
        if base^ = 'r' then next^ := #13 else Break;
      end
      else next^ := base^;
      Inc(next);
      Inc(base);
    end;
    base := PChar(Result);
    if (next > base) and ((next - 1)^ = ';') then Dec(next);
    SetLength(Result, next - base);
  end
  else Result := '';
end;

function lml_write_str(stream: TStream; const S: string): integer;
begin
  Result := stream.Write(pointer(S)^, length(S));
end;

function lml_match_at(const S: string; const Text: string; Start: integer): boolean;
var
  index, size: integer;
begin
  Result := False;
  size := Length(S);
  if (Start > 0) and ((Length(Text) - Start + 1) >= size) then
  begin
    index := 1;
    while (index <= size) and lml_match_char(S[index], Text[Start]) do
    begin
      Inc(index);
      Inc(Start);
    end;
    Result := index > size;
  end;
end;

function lml_find_match_at(const S: string; const Text: string;
  Start, Close: integer; var APos: integer): boolean;
var
  index, size: integer;
begin
  Result := False;
  size := Length(S);
  if (Start > 0) and ((Close - Start + 1) >= size) then
    for index := Start to Close - size do
      if lml_match_char(S[1], Text[index]) then
        if lml_match_at(S, Text, index) then
        begin
          APos := index;
          Result := True;
          exit;
        end;
end;

function lml_match_char(c1, c2: char): boolean;
begin
  Result := UpCase(c1) = UpCase(c2);
end;

function lml_encode_ppc(const source: string): string;
const
  ESCHAR = ['^', #0, #10, #13, '''', '"', '\', '(', ')', '%'];
var
  slen, base, next: integer;
  text: string;

  procedure write(ch: char; escape: boolean = false);
  begin
    if escape then
    begin
      Inc(base);
      text[base] := '^';
    end;
    Inc(base);
    text[base] := ch;
  end;
  
begin
  slen := Length(source);
  base := slen;
  next := 1;
  while next <= slen do
  begin
    if source[next] in ESCHAR then Inc(base);
    Inc(next);
  end;
  SetLength(text, base);
  base := 0;
  next := 1;
  while next <= slen do
  begin
    case source[next] of
      '^': write('^', true);
      #0 : write('0', true);
      #10: write('1', true);
      #13: write('2', true);
     '''': write('3', true);
      '"': write('4', true);
      '\': write('5', true);
      '(': write('6', true);
      ')': write('7', true);
      '%': write('8', true);
      else write(source[next]);
    end;
    Inc(next);
  end;
  SetLength(text, base);
  Result := text;
end;

function lml_decode_ppc(const source: string): string;
var
  slen, base, next: integer;
  text: string;

  procedure write(ch: char);
  begin
    Inc(base);
    text[base] := ch;
  end;
  
begin
  slen := Length(source);
  SetLength(text, slen);
  base := 0;
  next := 1;
  while next <= slen do
  begin
    if source[next] = '^' then
    begin
      Inc(next);
      if next <= slen then
        case source[next] of
          '^': write('^');
          '0': write(#0);
          '1': write(#10);
          '2': write(#13);
          '3': write('''');
          '4': write('"');
          '5': write('\');
          '6': write('(');
          '7': write(')');
          '8': write('%');
        end;
    end
    else write(source[next]);
    Inc(next);
  end;
  SetLength(text, base);
  Result := text;
end;

function xml_encode_str(const S: string): string;
begin
  Result := xml_encode_buf(pchar(S), Length(S));
end;

function xml_encode_buf(buf: pchar; count: integer): string;
var
  index, bytes: integer;
  next: pchar;

  procedure putc(ch: char);
  begin
    next^ := ch;
    Inc(next);
  end;

  procedure puts(S: pchar);
  begin
    while S^ <> #0 do
    begin
      putc(S^);
      Inc(S);
    end;
  end;

begin
  if (buf <> nil) and (buf^ <> #0) and (count > 0) then
  begin
    bytes := 0;
    index := 0;
    while (buf^ <> #0) and (index < count) do
    begin
      case buf[index] of
        '<': Inc(bytes, Length('&lt;'));
        '>': Inc(bytes, Length('&gt;'));
        '&': Inc(bytes, Length('&amp;'));
       '''': Inc(bytes, Length('&apos;'));
        '"': Inc(bytes, Length('&quot;'));
        else Inc(bytes);
      end;
      Inc(index);
    end;
    SetLength(Result, bytes);
    next := pchar(Result);
    while (buf^ <> #0) and (index < count) do
    begin
      case buf[index] of
        '<': puts('&lt;');
        '>': puts('&gt;');
        '&': puts('&amp;');
       '''': puts('&apos;');
        '"': puts('&quot;');
        else putc(buf[index]);
      end;
      Inc(index);
    end
  end;
end;

function xml_unescape_ANSI(const S: string): string;
var
  refs, temp: string;
  code: Integer;
  base, next, sptr: pchar;

  procedure putc(ch: char);
  begin
    next^ := ch;
    Inc(next);
  end;

  procedure puts(S: pchar);
  begin
    while S^ <> #0 do
    begin
      putc(S^);
      Inc(S);
    end;
  end;

begin
  base := pchar(S);
  if (base = nil) or (base^ = #0) then Exit;

  SetLength(Result, Length(S));
  next := pchar(Result);

  while base^ <> #0 do
  begin
    while not (base^ in [#0, '&']) do
    begin
      putc(base^);
      Inc(base);
    end;

    if base^ = #0 then Break;
    sptr := base;
    while not (base^ in [#0, ';']) do Inc(base);
    if base^ = #0 then Break;

    SetString(refs, sptr, base - sptr);
    temp := refs;

    if refs[2] = '#' then
    begin
      refs := copy(refs, 3, length(refs) - 3);
      if length(refs) > 0 then
      begin
        if refs[1] in ['x', 'X'] then
          refs[1] := '$';
        code := StrToIntDef(refs, -1);
        if (code >= 0) and (code < $FF) then
          temp := char(code);
      end;
    end
    else temp := xml_unescape(refs);

    puts(pchar(temp));
  end;

  SetLength(Result, next - pchar(Result));
end;

function xml_unescape_UTF8(const S: string): string;
var
  refs, temp: string;
  code: Integer;
  base, next, sptr: pchar;

  procedure putc(ch: char);
  begin
    next^ := ch;
    Inc(next);
  end;

  procedure puts(S: pchar);
  begin
    while S^ <> #0 do
    begin
      putc(S^);
      Inc(S);
    end;
  end;

begin
  base := pchar(S);
  if (base = nil) or (base^ = #0) then Exit;

  SetLength(Result, Length(S));
  next := pchar(Result);

  while base^ <> #0 do
  begin
    while not (base^ in [#0, '&']) do
    begin
      putc(base^);
      Inc(base);
    end;

    if base^ = #0 then Break;
    sptr := base;
    while not (base^ in [#0, ';']) do Inc(base);
    if base^ = #0 then Break;

    SetString(refs, sptr, base - sptr);
    temp := refs;

    if refs[2] = '#' then
    begin
      refs := copy(refs, 3, length(refs) - 3);
      if length(refs) > 0 then
      begin
        if refs[1] in ['x', 'X'] then
          refs[1] := '$';
        code := StrToIntDef(refs, -1);
        if (Code >= 0) and (Code < $FFFF) then
          temp := xml_unicode_utf8(WideChar(word(Code)));
      end;
    end
    else temp := xml_unescape(refs);

    puts(pchar(temp));
  end;

  SetLength(Result, next - pchar(Result));
end;

function xml_unescape(const S: string): string;
begin
  if AnsiSameText(S, '&amp;' ) then Result := '&' else
  if AnsiSameText(S, '&lt;'  ) then Result := '<' else
  if AnsiSameText(S, '&gt;'  ) then Result := '>' else
  if AnsiSameText(S, '&apos;') then Result := '''' else
  if AnsiSameText(S, '&quot;') then Result := '"';
end;

function xml_trim_pos(const S: string; var Start, Close: integer): boolean;
begin
  Start := Max(1, Start);
  Close := Min(Length(S) + 1, Close);
  if Close > Start then
  begin
    while (Start < Close) and (S[Start] in SpaceChar) do inc(Start);
    while (Start < Close) and (S[Close - 1] in SpaceChar) do dec(Close);
  end;
  Result := Close > Start;
end;

function xml_wrap_text(const S, SepStr: string; Interval: integer): string;
var
  index, size, slen, total: integer;
  next: pchar;

  procedure putc(ch: char);
  begin
    next^ := ch;
    Inc(next);
  end;

  procedure puts(S: pchar);
  begin
    while S^ <> #0 do
    begin
      putc(S^);
      Inc(S);
    end;
  end;

begin
  size := Length(S);
  slen := Length(SepStr);
  if (size > 0) and (slen > 0) and (Interval > 1) then
  begin
    total := size + ((size + slen - 1) div slen) * slen;
    SetLength(Result, total);
    next := pchar(Result);
    for index := 1 to size do
    begin
      if (index mod Interval) = 1 then
        puts(pchar(SepStr));
      putc(S[index]);
    end;
    SetLength(Result, next - pchar(Result));
  end
  else Result := S;
end;

function xml_skip_space(stream: TStream; var ch: char): boolean;
begin
  Result := stream.Read(ch, sizeof(ch)) = sizeof(ch);
  while Result and (ch in SpaceChar) do
    Result := stream.Read(ch, sizeof(ch)) = sizeof(ch);
end;

function xml_datetime_str(ADate: TDateTime): string;
const
  F1 = '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ';
  F2 = '%.4d-%.2d-%.2d';
var
  y, m, d, h, n, s, z: word;
begin
  DecodeDateTime(ADate, y, m, d, h, n, s, z);
  if (h + n + s) > 0 then
    Result := Format(F1, [y, m, d, h, n, s, z]) else
    Result := Format(F2, [y, m, d]);
end;

function xml_str_datetime(const ADate: string; DefValue: TDateTime): TDateTime;
var
  y, m, d, h, n, s, z: word;
begin
  y  := StrToInt(copy(ADate, 1, 4));
  m := StrToInt(copy(ADate, 6, 2));
  d   := StrToInt(copy(ADate, 9, 2));
  if Length(ADate) > 16 then
  begin // Suggestion JH
    h := StrToInt(copy(ADate, 12, 2));
    n  := StrToInt(copy(ADate, 15, 2));
    s  := StrToIntDef(copy(ADate, 18, 2), 0); // They might be omitted, so default to 0
    z := StrToIntDef(copy(ADate, 21, 3), 0); // They might be omitted, so default to 0
  end
  else
  begin
    h := 0;
    n  := 0;
    s  := 0;
    z := 0;
  end;
  if not TryEncodeDateTime(y, m, d, h, n, s, z, Result) then
    Result := DefValue;
end;

function xml_unicode_utf8(const S: widestring): string;
var
  slen, size, index, count, ch: integer;
  next: pchar;
begin
  slen := Length(S);
  if slen > 0 then
  begin
    size := slen * 3 + 1;
    SetLength(Result, size);
    next := pchar(Result);
    count := 0;
    index := 1;
    while (index <= slen) and (count < size) do
    begin
      ch := integer(S[index]);
      Inc(index);
      if ch <= $7F then
      begin
        next[count] := Char(ch);
        Inc(count);
      end
      else
      if ch > $7FF then
      begin
        if count + 3 > size then
          break;
        next[count] := Char($E0 or (ch shr 12));
        next[count+1] := Char($80 or ((ch shr 6) and $3F));
        next[count+2] := Char($80 or (ch and $3F));
        Inc(count,3);
      end
      else
      begin //  $7F < Source[index] <= $7FF
        if count + 2 > size then
          break;
        next[count] := Char($C0 or (ch shr 6));
        next[count+1] := Char($80 or (ch and $3F));
        Inc(count,2);
      end;
    end;
    SetLength(Result, count);
  end;
end;

function xml_utf8_unicode(const S: string): WideString;
var
  slen, size, index, count, ch: Integer;
  next: PWideChar;
  c: Byte;
begin
  slen := Length(S);
  if slen > 0 then
  begin
    size := slen + 1;
    SetLength(Result, size);
    next := PWideChar(Result);
    count := 0;
    index := 1;
    while (index <= slen) and (count < size) do
    begin
      ch := integer(S[index]);
      Inc(index);
      if (ch and $80) <> 0 then
      begin
        if index > slen then Break;
        ch := ch and $3F;
        if (ch and $20) <> 0 then
        begin
          c := Byte(S[index]);
          Inc(index);
          if (c and $C0) <> $80 then Break;
          if index > slen then Break;
          ch := (ch shl 6) or (c and $3F);
        end;
        c := Byte(S[index]);
        Inc(index);
        if (c and $C0) <> $80 then Break;
        next[count] := WideChar((ch shl 6) or (c and $3F));
      end
      else next[count] := WideChar(ch);
      Inc(count);
    end;
    SetLength(Result, count);
  end;
end;

function xml_ansi_utf8(const S: string): string;
begin
  Result := xml_unicode_utf8(S);
end;

function xml_utf8_ansi(const S: string): string;
begin
  Result := xml_utf8_unicode(S);
end;

function xml_quote(const S: string): string;
var
  quote: char;
begin
  if System.Pos('"', S) > 0 then
    quote := '''' else
    quote := '"';
  Result := AnsiQuotedStr(S, quote);
end;

function xml_unquote(const S: string): string;
var
  size: integer;
  next: pchar;
begin
  size := Length(S);
  if (size > 1) and (S[1] in QuoteChar) then
  begin
    next := pchar(S);
    Result := AnsiExtractQuotedStr(next, S[1]);
  end
  else Result := S;
end;

function xml_parse_attr(const S: string; Start, Close: integer; Attrs: TStrings): TStrings;
var
  index: integer;
  in_quote: boolean;
  quote: char;
begin
  Result := Attrs;
  if Result = nil then Exit;
  Result.Clear;
  in_quote := False;
  quote := '"';
  if xml_trim_pos(S, Start, Close) then
  begin
    for index := Start to Close - 1 do
    begin
      if in_quote then
      begin
        if S[index] = quote then
          in_quote := False;
      end
      else
      if S[index] in QuoteChar then
      begin
        in_quote := True;
        quote := S[index];
      end;

      if not in_quote then
        if S[index] in SpaceChar then
        begin
          if index > Start then
            Result.Add(copy(S, Start, index - Start));
          Start := index + 1;
        end;
    end;

    if Start < Close then
      Result.Add(copy(S, Start, Close - Start));

    // First-char "=" signs should append to previous
    for index := Result.Count - 1 downto 1 do
      if Result[index][1] = '=' then
      begin
        Result[index - 1] := Result[index - 1] + Result[index];
        Result.Delete(index);
      end;
      
    // First-char quotes should append to previous
    for index := Result.Count - 1 downto 1 do
      if (Result[index][1] in QuoteChar) and (Pos('=', Result[index - 1]) > 0) then
      begin
        Result[index - 1] := Result[index - 1] + Result[index];
        Result.Delete(index);
      end;
  end;
end;

procedure SaveStreamToFile(AStream: TLmlStream; const FileName: string);
var
  fout: TStream;
begin
  fout := TFileStream.Create(FileName, fmCreate);
  try
    if AStream <> nil then
      fout.CopyFrom(AStream.Stream, 0);
  finally
    fout.Free;
  end;
end;

{ TLmlStream }

function TLmlStream.CopyFrom(Source: TStream; Count: Int64): Int64;
begin
  ResetPosition;
  Result := FStream.CopyFrom(Source, Count);
end;

function TLmlStream.GetPosition: Int64;
begin
  Result := FStream.Position - (FLen - FPtr);
end;

function TLmlStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TLmlStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PChar;
  L: integer;
begin
  Result := 0;
  if Count > 0 then
  begin
    P := PChar(@Buffer);
    L := Min(FLen - FPtr, Count);
    if L > 0 then
    begin
      Move(FBuf[FPtr], P^, L);
      Inc(FPtr, L);
      Inc(P, L);
      Inc(Result, L);
      Dec(Count, L);
    end;
    if Count > 0 then
    begin
      Inc(Result, FStream.Read(P^, Count));
      FLen := 0;
      FPtr := 0;
    end;
  end;
end;

procedure TLmlStream.ReadBuffer(var Buffer; Count: Integer);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.CreateRes(@SReadError);
end;

function TLmlStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  ResetPosition;
  Result := FStream.Seek(Offset, Origin);
end;

function TLmlStream.Seek(Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  ResetPosition;
  Result := FStream.Seek(Offset, Origin);
end;

procedure TLmlStream.SetPosition(const Pos: Int64);
begin
  FStream.Position := Pos;
  FLen := 0;
  FPtr := 0;
end;

procedure TLmlStream.SetSize64(const NewSize: Int64);
begin
  ResetPosition;
  FStream.Size := NewSize;
end;

function TLmlStream.Write(const Buffer; Count: Integer): Longint;
begin
  ResetPosition;
  Result := FStream.Write(Buffer, Count);
end;

function TLmlStream.WriteText(const Text: string): Longint;
begin
  Result := Length(Text);
  if Result > 0 then
    Result := Write(Text[1], Result);
end;

procedure TLmlStream.WriteBuffer(const Buffer; Count: Integer);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.CreateRes(@SWriteError);
end;

function TLmlStream.WriteLine(const Text: string): Longint;
begin
  Result := WriteText(Text + sLineBreak);
end;

function TLmlStream.LineWrite(const Text: string): Longint;
begin
  Result := WriteText(sLineBreak + Text);
end;

function TLmlStream.ReadLine(var S: string): boolean;
var
  X: integer;
  T: string;
begin
  Result := PrepareBuf;
  if not Result then Exit;

  if GetCrPos(X) then
  begin
    Dec(X, FPtr);
    SetLength(S, X);
    Move(FBuf[FPtr], S[1], X);
    Inc(FPtr, X + 1);
    if PrepareBuf and (FBuf[FPtr] = #10) then Inc(FPtr);
  end
  else
  begin
    X := FLen - FPtr;
    SetLength(S, X);
    Move(FBuf[FPtr], S[1], X);
    FPtr := 0;
    FLen := 0;
    if ReadLine(T) then S := S + T;
  end;
end;

function TLmlStream.ReadNotEmptyLine(var S: string): boolean;
var
  slen: Integer;
begin
  Result := ReadLine(S);
  while Result do
  begin
    slen := Length(S);
    if slen > 0 then
    begin
      while (slen > 0) and (S[slen] <= ' ') do Dec(slen);
      if slen > 0 then
      begin
        SetLength(S, slen);
        Exit;
      end;
    end;
    Result := ReadLine(S);
  end;
end;

procedure TLmlStream.CloseStream;
begin
  try
    if FFreeOnClose and Assigned(FStream) then
      FStream.Free;
  finally
    FStreamType := lstUnknown;
    FStream := nil;
    FLen := 0;
    FPtr := 0;
    FFreeOnClose := false;
  end;
end;

procedure TLmlStream.OpenStream(AStream: TStream; FreeStream: boolean);
begin
  CloseStream;
  FStream := AStream;
  FFreeOnClose := FreeStream;
end;

destructor TLmlStream.Destroy;
begin
  CloseStream;
  inherited;
end;

procedure TLmlStream.OpenFileStream(const FileName: string; Mode: word);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, Mode);
  try
    CloseStream;
    FStream := S;
    FFreeOnClose := true;
    FStreamType := lstFile;
  except
    S.Free; raise;
  end;
end;

procedure TLmlStream.OpenMemoryStream;
begin
  CloseStream;
  FStream := TMemoryStream.Create;
  FFreeOnClose := true;
  FStreamType := lstMemory;
end;

procedure TLmlStream.ResetPosition;
var
  L: integer;
begin
  L := FLen - FPtr;
  if L > 0 then
  begin
    FStream.Seek(-L, soCurrent);
    FLen := 0;
    FPtr := 0;
  end;
end;

function TLmlStream.GetCrPos(var X: integer): boolean;
begin
  Result := true;
  X := FPtr;
  while X < FLen do
  begin
    if FBuf[X] in [#10, #13] then Exit;
    Inc(X);
  end;
  Result := false;
end;

function TLmlStream.PrepareBuf: boolean;
begin
  if FLen <= FPtr then
  begin
    FLen := FStream.Read(FBuf[0], LmlBufferSize);
    FPtr := 0;
    Result := (FLen > FPtr);
  end
  else Result := true;
end;

constructor TLmlStream.Create;
begin
  CloseStream;
end;

function TLmlStream.WriteFormat(const Fmt: string;
  const Args: array of const; LineBreak: boolean): Longint;
var
  S: string;
begin
  S := Format(Fmt, Args);
  if LineBreak then
    Result := WriteLine(S) else
    Result := WriteText(S);
end;

function TLmlStream.GetEof: boolean;
begin
  Result := GetPosition = GetSize;
end;

function TLmlStream.ReadText(Count: integer): string;
begin
  Count := Min(Max(0, Count), GetSize);
  if Count > 0 then
  begin
    SetLength(Result, Count);
    Count := Max(0, Read(PChar(Result)^, Count));
    SetLength(Result, Count);
  end
  else Result := '';
end;

procedure TLmlStream.ErrorOpenFileStream;
begin
  lse_error('call to %s.OpenFileStream() is invalid', [ClassName]);
end;

procedure TLmlStream.ErrorOpenMemoryStream;
begin
  lse_error('call to %s.OpenMemoryStream() is invalid', [ClassName]);
end;

procedure TLmlStream.ErrorOpenStream;
begin
  lse_error('call to %s.OpenStream() is invalid', [ClassName]);
end;

function TLmlStream.CopyFromStream(Source: TLmlStream; Count: Int64): Int64;
begin
  Source.ResetPosition;
  Result := CopyFrom(Source.FStream, Count);
end;

{ TLmlMemoryStream }

constructor TLmlMemoryStream.Create;
begin
  inherited;
  OpenMemoryStream;
end;

procedure TLmlMemoryStream.OpenFileStream(const FileName: string; Mode: word);
begin
  ErrorOpenFileStream;
end;

procedure TLmlMemoryStream.OpenStream(AStream: TStream; FreeStream: boolean);
begin
  ErrorOpenStream;
end;

{ TLmlFileStream }

procedure TLmlFileStream.CloseStream;
begin
  inherited;
  FFileName := '';
end;

constructor TLmlFileStream.Create(const FileName: string; Mode: Word);
begin
  inherited Create;
  OpenFileStream(FileName, Mode);
end;

procedure TLmlFileStream.OpenFileStream(const FileName: string; Mode: word);
var
  F: string;
begin
  F := ExpandFileName(FileName);
  inherited OpenFileStream(F, Mode);
  FFileName := F;
end;

procedure TLmlFileStream.OpenMemoryStream;
begin
  ErrorOpenMemoryStream;
end;

procedure TLmlFileStream.OpenStream(AStream: TStream; FreeStream: boolean);
begin
  ErrorOpenStream;
end;

{ TLmlReader }

constructor TLmlReader.Create(InStream: TLmlStream);
begin
  FInStream := InStream;
  Reset;
end;

destructor TLmlReader.Destroy;
begin
  if FReleaseStream then
    FreeAndNil(FInStream);
  inherited;
end;

procedure TLmlReader.Execute;
var
  head_line: string;
  last_level: integer;

  function read_next_line(var line: string): boolean;
  begin
    if head_line <> '' then
    begin
      line := head_line;
      head_line := '';
      Result := true;
    end
    else
    begin
      Result := false;
      while not Result and FInStream.ReadLine(line) do
      begin
        line := TrimRight(line);
        Result := line <> '';
      end;
    end;
  end;

  function read_following_text(offset: integer; var text: string): boolean;
  var
    line: string;
    index: integer;
  begin
    Result := false;
    head_line := '';
    if read_next_line(line) then
    begin
      index := 1;
      while (index <= Length(line)) and (line[index] = ' ') do
        Inc(index);
      if (index = offset) and (line[index] = '=') then
      begin
        text := lml_decode_line(Copy(line, index + 1, Length(line)));
        Result := true;
      end
      else head_line := line;
    end;
  end;

  function read_target: boolean;
  var
    line: string;
    index: integer;
    maybe_has_text: boolean;
  begin
    Result := false;

    // 1. get target head line & seek head char

    if not read_next_line(line) then Exit;
    index := 1;
    while (index <= Length(line)) and (line[index] = ' ') do
      Inc(index);
    if ((index mod LmlMargin) <> 1)
      or not (line[index] in (LmlHeadChar + ['.'])) then
        Exit;

    // 2. determin target level, type, name & value

    FLevel := index div LmlMargin;
    FIsNode := line[index] <> '.';
    if not FIsNode then Inc(index);

    line := Copy(line, index, Length(line));
    if line = '' then Exit;
    FNodeName := __extractNameValue(line, FNodeText);
    if FNodeName = '' then
    begin
      maybe_has_text := false;
      FNodeName := line;
      FNodeText := '';
    end
    else maybe_has_text := true;

    FIsTextNode := FIsNode and (FNodeName = ':text');
    if not FIsTextNode and not lml_is_tag_name(FNodeName) then Exit;

    // 3. read following text

    FNodeText := lml_decode_line(FNodeText);
    if maybe_has_text then
    begin
      Inc(index, Length(FNodeName));
      while read_following_text(index, line) do
        FNodeText := FNodeText + sLineBreak + line;
    end;

    Result := true;
  end;

begin
  try
    FTerminated := false;
    head_line := '';
    if read_target and (FLevel = 0) and FIsNode and not FIsTextNode then
    begin
      last_level := 0;
      FNodeType := lntDocument;
      Notify;
      while not FTerminated and read_target and (FLevel > 0) and (FLevel <= last_level + 1) do
      begin
        last_level := FLevel;
        if FIsTextNode then
          FNodeType := lntText else
        if FIsNode then
          FNodeType := lntElement else
          FNodeType := lntAttribute;
        Notify;
      end;
    end;
  finally
    Reset; { status }
  end;
end;

function TLmlReader.GetIsAttr: boolean;
begin
  Result := not FIsNode;
end;

procedure TLmlReader.Notify;
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, FTerminated);
end;

procedure TLmlReader.Reset;
begin
  FNodeType := lntUnknown;
  FNodeName := '';
  FNodeText := '';
  FLevel := 0;
  FIsNode := false;
  FIsTextNode := false;
  FTerminated := true;
end;

procedure TLmlReader.Terminate;
begin
  FTerminated := true;
end;

{ TLmlWriter }

constructor TLmlWriter.Create(OutStream: TLmlStream);
begin
  FOutStream := OutStream;
end;

{ TLmlAttr }

procedure TLmlAttrList.AssignStrings(Strings: TStrings);
var
  index: integer;
  name, value: string;
begin
  BeginUpdate;
  try
    Assign(Strings);
    for index := Count - 1 downto 0 do
    begin
      name := __extractNameValue(Strings[index], value);
      if not lml_is_tag_name(name) or (IndexOf(name) < index) then
        Delete(index);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLmlAttrList.AssignText(const Text: string);
var
  index: integer;
  name, value: string;
begin
  BeginUpdate;
  try
    SetTextStr(Text);
    for index := Count - 1 downto 0 do
    begin
      name := __extractNameValue(Strings[index], value);
      if not lml_is_tag_name(name) or (IndexOf(name) < index) then
        Delete(index);
    end;
  finally
    EndUpdate;
  end;
end;

constructor TLmlAttrList.Create;
begin
  CaseSensitive := true;
  Sorted := true;
end;

function TLmlAttrList.Exist(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function TLmlAttrList.GetValue(Index: integer): string;
begin
  lml_check_list_index(Index, Count);
  Result := __extractValue(Strings[Index]);
end;

function TLmlAttrList.IndexOf(const Name: string): integer;
begin
  Result := IndexOfName(Name);
end;

function TLmlAttrList.Read(const Name, DefaultValue: string): string;
begin
  if not Valued(Name, Result) then
    Result := DefaultValue;
end;

function TLmlAttrList.ReadBool(const Name: string;
  DefaultValue: boolean): boolean;
var
  value: string;
begin
  if Valued(Name, value) then
    Result := stoi(value) <> 0 else
    Result := DefaultValue;
end;

function TLmlAttrList.ReadDate(const Name: string;
  DefaultValue: TDateTime): TDateTime;
var
  value: string;
  year, month, day: integer;
begin
  if Valued(Name, value) then
  begin
    if Length(value) = 8 then
    begin
      year := stoi(Copy(value, 1, 4));
      month := stoi(Copy(value, 5, 2));
      day := stoi(Copy(value, 7, 2));
      if not SysUtils.TryEncodeDate(year, month, day, Result) then
        Result := DefaultValue;
    end
    else Result := DefaultValue;
  end
  else Result := DefaultValue;
end;

function TLmlAttrList.ReadDateTime(const Name: string;
  DefaultValue: TDateTime): TDateTime;
var
  value: string;
  year, month, day, hour, minute, second, mills: integer;
begin
  if Valued(Name, value) then
  begin
    if Length(value) = 17 then
    begin
      year := stoi(Copy(value, 1, 4));
      month := stoi(Copy(value, 5, 2));
      day := stoi(Copy(value, 7, 2));
      hour := stoi(Copy(value, 9, 2));
      minute := stoi(Copy(value, 11, 2));
      second := stoi(Copy(value, 13, 2));
      mills :=  stoi(Copy(value, 15, 3));
      if not dateutils.TryEncodeDateTime(year, month, day,
        hour, minute, second, mills, Result) then
          Result := DefaultValue;
    end
    else Result := DefaultValue;
  end
  else Result := DefaultValue;
end;

function TLmlAttrList.ReadFloat(const Name: string;
  DefaultValue: double): double;
var
  value: string;
begin
  if Valued(Name, value) then
    Result := stod(value) else
    Result := DefaultValue;
end;

function TLmlAttrList.ReadHex(const Name: string;
  DefaultValue: int64): int64;
var
  value: string;
begin
  if Valued(Name, value) then
  begin
    if __inCharSet(pchar(Value), HexChar) then
      Result := StrToInt64('$' + value) else
      Result := DefaultValue;
  end
  else Result := DefaultValue;
end;

function TLmlAttrList.ReadInteger(const Name: string;
  DefaultValue: int64): int64;
var
  value: string;
begin
  if Valued(Name, value) then
    Result := stoi(value) else
    Result := DefaultValue;
end;

function TLmlAttrList.ReadMoney(const Name: string;
  DefaultValue: currency): currency;
var
  value: string;
begin
  if Valued(Name, value) then
    Result := stod(value) else
    Result := DefaultValue;
end;

function TLmlAttrList.ReadTime(const Name: string;
  DefaultValue: TDateTime): TDateTime;
var
  value: string;
  hour, minute, second, mills: integer;
begin
  if Valued(Name, value) then
  begin
    if Length(value) = 9 then
    begin
      hour := stoi(Copy(value, 1, 2));
      minute := stoi(Copy(value, 3, 2));
      second := stoi(Copy(value, 5, 2));
      mills := stoi(Copy(value, 7, 3));
      if not SysUtils.TryEncodeTime(hour, minute, second, mills, Result) then
        Result := DefaultValue;
    end
    else Result := DefaultValue;
  end
  else Result := DefaultValue;
end;

procedure TLmlAttrList.Remove(const Name: string);
var
  index: integer;
begin
  index := IndexOf(Name);
  if index >= 0 then
    Delete(index);
end;

procedure TLmlAttrList.SetValue(Index: integer; const Value: string);
begin
  lml_check_list_index(Index, Count);
  Sorted := false;
  Strings[Index] := Names[Index] + '=' + Value;
  Sorted := true;
end;

function TLmlAttrList.Valued(const Name: string; var Value: string): boolean;
var
  index: integer;
begin
  index := IndexOf(Name);
  Result := Index >= 0;
  if Result then
    Value := __extractValue(Strings[index]) else
    Value := '';
end;

procedure TLmlAttrList.Write(const Name, Value: string; DoWrite: boolean = true);
var
  index: integer;
begin
  lml_check_tag_name(Name);
  if not DoWrite then Exit;
  index := IndexOf(Name);
  if index >= 0 then
  begin
    Sorted := false;
    Strings[Index] := Name + '=' + Value;
    Sorted := true;
  end
  else Add(Name + '=' + Value);
end;

procedure TLmlAttrList.WriteBool(const Name: string; Value, DoWrite: boolean);
begin
  WriteInteger(Name, Ord(Value), DoWrite);
end;

procedure TLmlAttrList.WriteDate(const Name: string; Value: TDateTime;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, FormatDateTime('yyyymmdd', Value));
end;

procedure TLmlAttrList.WriteDateTime(const Name: string; Value: TDateTime;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, FormatDateTime('yyyymmddhhnnsszzz', Value));
end;

procedure TLmlAttrList.WriteFloat(const Name: string; Value: double;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, FloatToStr(Value));
end;

procedure TLmlAttrList.WriteHex(const Name: string; Value: int64;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, IntToHex(Value, 1));
end;

procedure TLmlAttrList.WriteInteger(const Name: string; Value: int64;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, IntToStr(Value));
end;

procedure TLmlAttrList.WriteMoney(const Name: string; Value: currency;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, FloatToStr(Value));
end;

procedure TLmlAttrList.WriteTime(const Name: string; Value: TDateTime;
  DoWrite: boolean);
begin
  if DoWrite then Write(Name, FormatDateTime('hhnnsszzz', Value));
end;

{ TLmlNode }

function TLmlNode.Add(const Name, Text: string): TLmlNode;
begin
  Result := FDocument.CreateNode(Self, Name, Text);
end;

procedure TLmlNode.Assign(Node: TLmlNode);
var
  index: integer;
  child: TLmlNode;
begin
  if (Node <> Self) and (Node <> nil) then
  begin
    Clear;
    Name := Node.Name;
    Text := Node.Text;
    Attr.Assign(Node.Attr);
    for index := 0 to Node.Count - 1 do
    begin
      child := Add('child', '');
      child.Assign(Node.Item[index]);
    end;
  end;
end;

procedure TLmlNode.Clear;
var
  A: integer;
begin
  FAttrList.Clear;
  for A := FItemList.Count - 1 downto 0 do
    Delete(A);
  FTextNodeCount := 0;
end;

constructor TLmlNode.Create(Document: TLmlDocument; const Name, Text: string);
begin
  lml_check_tag_name(Name);
  FDocument := Document;
  FName := Name;
  FText := Text;
  FAttrList := TLmlAttrList.Create;
  FItemList := TList.Create;
  IncRefcount;
  FNodeType := lntElement;
end;

function TLmlNode.DecRefcount: integer;
begin
  Result := inherited DecRefcount;
  if Result >= 1 then
    if (FDocument <> nil) and not FDocument.Destroying then
      FDocument.DecRefcount;
end;

procedure TLmlNode.Delete(Index: integer);
var
  node: TLmlNode;
begin
  node := FItemList[Index];
  FItemList.Delete(Index);
  if node.IsTextNode then
    Dec(FTextNodeCount);
  node.FParent := nil;
  node.Clear;
  node.DecRefcount;
end;

destructor TLmlNode.Destroy;
begin
  if Assigned(FParent) then
  begin
    if IsTextNode then
      Dec(FParent.FTextNodeCount);
    FParent.FItemList.Remove(Self);
    FParent := nil;
  end;
  if Assigned(FDocument) then
  begin
    FDocument.NotifyRemove(Self);
    NilDocument;
  end;
  Clear;
  FreeAndNil(FItemList);
  FreeAndNil(FAttrList);
  inherited;
end;

function TLmlNode.Dir(showID: boolean): string;
var
  L: TStrings;
  N: TLmlNode;
  A: integer;
begin
  L := TStringList.Create;
  try
    for A := 0 to GetItemCount - 1 do
    begin
      N := GetItem(A);
      if showID then
        L.Add(Format('%s/ID=%d', [N.Name, N.ID])) else
        L.Add(N.Name);
    end;
    Result := L.Text;
  finally
    L.Free;
  end;
end;

function TLmlNode.Exist(const Name: string; ID: cardinal): boolean;
begin
  Result := IndexOf(Name, ID) >= 0;
end;

function TLmlNode.Find(const Name: string; ID: cardinal): TLmlNode;
var
  X: integer;
begin
  X := IndexOf(Name, ID);
  if X >= 0 then
    Result := FItemList[X] else
    Result := nil;
end;

function TLmlNode.FindByName(const Name: string; matchCase: boolean): TLmlNode;
var
  A: integer;
begin
  if lml_is_tag_name(Name) then
    for A := 0 to FItemList.Count - 1 do
    begin
      Result := FItemList[A];
      if __strComp(Name, Result.FName, not matchCase) = 0 then Exit;
    end;
  Result := nil;
end;

function TLmlNode.GetID: cardinal;
begin
  Result := cardinal(Self);
end;

function TLmlNode.GetItem(Index: integer): TLmlNode;
begin
  Result := FItemList[Index];
end;

function TLmlNode.GetItemCount: integer;
begin
  if FItemList <> nil then
    Result := FItemList.Count else
    Result := 0;
end;

function TLmlNode.GetItemNameList: string;
begin
  Result := Dir(false);
end;

function TLmlNode.GetLevel: integer;
var
  P: TLmlNode;
begin
  Result := 0;
  P := FParent;
  while P <> nil do
  begin
    Inc(Result);
    P := P.FParent;
  end;
end;

function TLmlNode.GetNamePath: string;
begin
  if FParent <> nil then
  begin
    Result := FParent.NamePath;
    if Result <> '/' then
      Result := Result + '/' + FName else
      Result := Result + FName;
  end
  else Result := '/';
end;

function TLmlNode.GetPPCode: string;
var
  ss: TStringStream;
  ls: TLmlStream;
begin
  ss := TStringStream.Create('');
  try
    ls := TLmlStream.Create;
    try
      ls.OpenStream(ss, false);
      WritePPC(ls);
      Result := ss.DataString;
    finally
      ls.Free;
    end;
  finally
    ss.Free;
  end;
end;

function TLmlNode.GetRoot: TLmlNode;
begin
  Result := Self;
  while Result.FParent <> nil do
    Result := Result.FParent;
end;

function TLmlNode.IncRefcount: integer;
begin
  Result := inherited IncRefcount;
  if Result > 1 then
    if (FDocument <> nil) and not FDocument.Destroying then
      FDocument.IncRefcount;
end;

function TLmlNode.IndexOf(const Name: string; ID: cardinal): integer;
var
  A: integer;
  N: TLmlNode;
begin
  if lml_is_tag_name(Name) or (ID > 0) then
    for A := 0 to FItemList.Count - 1 do
    begin
      N := FItemList[A];
      if (ID = 0) or (ID = N.GetID) then
        if (Name = '') or (N.FName = Name) then
        begin
          Result := A; Exit;
        end;
    end;
  Result := -1;
end;

function TLmlNode.IsChildOf(node: TLmlNode): boolean;
var
  item: TLmlNode;
begin
  Result := (node <> nil) and (Self <> nil);
  if Result then
  begin
    item := Self;
    repeat
      item := item.FParent;
      Result := (item = node);
    until Result or (item = nil);
  end;
end;

function TLmlNode.IsFrom(node: TLmlNode): boolean;
begin
  Result := (node <> nil) and ((node = Self) or node.IsChildOf(Self));
end;

procedure TLmlNode.Remove(const Name: string; ID: cardinal);
var
  index: integer;
begin
  index := IndexOf(Name, ID);
  if index >= 0 then
    Delete(index);
end;

procedure TLmlNode.NilDocument;
var
  index: integer;
begin
  if FDocument <> nil then
  begin
    index := Refcount;
    while index > 1 do
    begin
      FDocument.DecRefcount;
      Dec(index);
    end;
    FDocument := nil;
    for index := 0 to GetItemCount - 1 do
      GetItem(index).NilDocument;
  end;
end;

procedure TLmlNode.ParsePPC(stream: TLmlStream);
var
  buf: array[0..1023] of char;
  buf_count, buf_index: integer;
  cur_char: char;
  s_name, s_text: string;

  function check(OK: boolean): boolean;
  begin
    Result := OK;
    if not Result then Abort;
  end;
  
  function getch: boolean; // get char
  begin
    if (buf_count = 0) or (buf_index >= buf_count) then
    begin
      buf_index := 0;
      buf_count := stream.Read(buf, sizeof(buf));
    end;
    Result := buf_index < buf_count;
    if Result then
    begin
      cur_char := buf[buf_index];
      Inc(buf_index);
    end
    else cur_char := #0;
  end;

  function gethc: boolean; // get head char
  begin
    while cur_char in SpaceChar do
    begin
      Result := getch();
      if not Result then Exit;
    end;
    Result := true;
  end;

  function getnv(var name, text: string): boolean; // get name value
  begin
    name := cur_char;
    text := '';
    Result := getch();
    if Result then
    begin
      while cur_char in LmlChar do
      begin
        name := name + cur_char;
        Result := getch();
        if not Result then Exit;
      end;
      Result := gethc() and (cur_char in ['(', ',', ')']);
      if Result and (cur_char <> ')') then
        if cur_char = '(' then
        begin
          Result := getch();
          if Result then
          begin
            while Result and (cur_char <> ')') do
            begin
              text := text + cur_char;
              Result := getch();
            end;
            if Result then
            begin
              Result := getch();
              if Result then
                text := lml_decode_ppc(text);
            end;
          end;
        end
        else Result := getch();  // ','
    end;
  end;

  procedure parse(node: TLmlNode);
  begin
    check(getnv(s_name, s_text));
    node.Name := s_name;
    node.Text := s_text;
    while check(gethc()) do
      if cur_char in LmlHeadChar then
      begin
        check(getnv(s_name, s_text));
        node.attr.Write(s_name, s_text);
      end
      else
      if cur_char = '(' then
      begin
        check(getch() and gethc() and (cur_char in LmlHeadChar));
        parse(node.Add('x', ''));
      end
      else
      if cur_char = ')' then
      begin
        check((node = Self) or getch());
        Exit;
      end
      else check(false);
  end;
  
begin
  Document.BeginLoad;
  try
    Clear;
    buf_count := 0;
    buf_index := 0;
    check(getch() and gethc() and (cur_char = '('));
    check(getch() and gethc() and (cur_char in LmlHeadChar));
    parse(Self);
  except
    { do nothing }
  end;
  Document.EndLoad;
end;

procedure TLmlNode.Remove(item: TLmlNode);
var
  index: integer;
begin
  index := FItemList.IndexOf(item);
  if index >= 0 then
    Delete(index);
end;

procedure TLmlNode.SaveToString(var S: string);
var
  M: TLmlMemoryStream;
  L: integer;
  P: TLmlNode;
begin
  M := TLmlMemoryStream.Create;
  try
    P := FParent;
    try
      FParent := nil;
      WritePPC(M);
      L := M.Size;
      SetLength(S, L);
      if L > 0 then
      begin
        M.Position := 0;
        M.Read(pointer(S)^, L);
      end;
    finally
      FParent := P;
    end;
  finally
    M.Free;
  end;
end;

procedure TLmlNode.SetName(const Value: string);
begin
  lml_check_tag_name(Value);
  FName := Value;
end;

procedure TLmlNode.SetPPCode(const Value: string);
var
  ss: TStringStream;
  ls: TLmlStream;
begin
  ss := TStringStream.Create(Value);
  try
    ls := TLmlStream.Create;
    try
      ls.OpenStream(ss, false);
      ParsePPC(ls);
    finally
      ls.Free;
    end;
  finally
    ss.Free;
  end;
end;

function TLmlNode.TraceAttr(const AAttr: string;
  ForceExists: boolean): TLmlNode;
var
  attr: string;
  last: integer;
begin
  last := Length(AAttr);
  while (last > 0) and (AAttr[last] <> '/') do Dec(last); {<--last last}
  attr := Trim(Copy(AAttr, last + 1, MaxInt));
  if lml_is_tag_name(attr) then
  begin
    if last > 0 then
      Result := TraceNode(Copy(AAttr, 1, last), ForceExists) else
      Result := Self;
    if (Result <> nil) and not Result.attr.Exist(attr) then
      if ForceExists then
        Result.attr.Add(attr + '=') else
        Result := nil;
  end
  else Result := nil;
end;

function TLmlNode.TraceNode(const APath: string; ForceExists: boolean): TLmlNode;
var
  node: TLmlNode;
  path, temp: string;
  base, next, size: integer;
begin
  Result := Self;
  
  path := Trim(APath);
  if path = '' then Exit;

  if path = '/' then
  begin
    Result := GetRoot;
    Exit;
  end;

  size := Length(path);
  if path[size] <> '/' then
  begin
    path := path + '/';
    Inc(size);
  end;

  if path[1] = '/' then {<--Absolute path}
  begin
    Result := GetRoot;
    base := 2;
  end
  else base := 1;

  next := 2;
  while next <= size do
  begin
    if path[next] = '/' then
    begin
      temp := Copy(path, base, next - base);
      if (temp <> '') and (temp <> '.') then
      begin
        if temp <> '..' then
        begin
          node := Result.Find(temp, 0);
          if (node = nil) and ForceExists and lml_is_tag_name(temp) then
            node := Result.Add(temp, '');
          Result := node;
        end
        else Result := Result.FParent;
        if Result = nil then Exit;
      end;
      base := next + 1;
    end;
    inc(next);
  end;
end;

procedure TLmlNode.WritePPC(stream: TLmlStream);
var
  index, i_count, a_count: integer;
  is_last: boolean;
  N, V: string;

  function pptext(const S: string): string;
  begin
    if S <> '' then
      Result := '(' + lml_encode_ppc(S) + ')' else
    if is_last then
      Result := '' else
      Result := ',';
  end;

begin
  Document.BeginSave;
  try
    a_count := FAttrList.Count;
    i_count := GetItemCount;
    is_last := (i_count = 0) and (a_count = 0);
    stream.WriteText('(' + FName + pptext(FText));
    for index := 0 to a_count - 1 do
    begin
      is_last := (i_count = 0) and (index = (a_count -1));
      N := __extractNameValue(FAttrList[index], V);
      stream.WriteText(N + pptext(V));
    end;
    for index := 0 to i_count - 1 do
      GetItem(index).WritePPC(stream);
    stream.WriteText(')');
  finally
    Document.EndSave;
  end; 
end;

function TLmlNode.Insert(Index: integer; const Name, Text: string): TLmlNode;
begin
  if Index < Count then
  begin
    Result := FDocument.CreateNode(Self, Name, Text);
    if Index < 0 then Index := 0;
    FItemList.Move(Count - 1, Index);
  end
  else Result := Add(Name, Text);
end;

function TLmlNode.GetIndex: integer;
begin
  if Assigned(FParent) then
    Result := FParent.FItemList.IndexOf(Self) else
    Result := -1;
end;

procedure TLmlNode.SetIndex(const Value: integer);
begin
  if Assigned(FParent) then
    FParent.FItemList.Move(Index, Value);
end;

procedure TLmlNode.Sort(Recurse, IgnoreCase, DescOrder: boolean);

  function CompareNode(Node1, Node2: TLmlNode): integer;
  begin
    if IgnoreCase then
      Result := AnsiCompareText(Node1.Name, Node2.Name) else
      Result := AnsiCompareStr(Node1.Name, Node2.Name);
    if Result = 0 then
      if IgnoreCase then
        Result := AnsiCompareText(Node1.Text, Node2.Text) else
        Result := AnsiCompareStr(Node1.Text, Node2.Text);
    if DescOrder then
      Result := - Result;
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: TLmlNode;
  begin
    repeat
      I := L;
      J := R;
      P := FItemList[(L + R) shr 1];
      repeat
        while CompareNode(FItemList[I], P) < 0 do Inc(I);
        while CompareNode(FItemList[J], P) > 0 do Dec(J);
        if I <= J then
        begin
          T := FItemList[I];
          FItemList[I] := FItemList[J];
          FItemList[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  index: integer;
begin
  if Count > 1 then
    QuickSort(0, Count - 1);
  if Recurse then
    for index := 0 to Count - 1 do
      GetItem(index).Sort(Recurse, IgnoreCase, DescOrder);
end;

function TLmlNode.AddText(const Text: string): TLmlNode;
begin
  Result := FDocument.CreateTextNode(Parent, Text);
end;

function TLmlNode.InsertText(Index: integer; const Text: string): TLmlNode;
begin
  if Index < Count then
  begin
    Result := FDocument.CreateTextNode(Self, Text);
    if Index < 0 then Index := 0;
    FItemList.Move(Count - 1, Index);
  end
  else Result := AddText(Text);
end;

function TLmlNode.GetText: string;
{var
  index: integer;
  node: TLmlNode;}
begin
  Result := FText;
{  if FTextNodeCount > 0 then
    for index := Count - 1 downto 0 do
    begin
      node := Item[index];
      if node.IsTextNode then
        Result := Result + node.Text;
    end;}
end;

procedure TLmlNode.SetText(const Value: string);
{var
  index: integer;}
begin
  FText := Value;
{  if FTextNodeCount > 0 then
  begin
    for index := Count - 1 downto 0 do
      if GetItem(index).IsTextNode then
        Delete(index);
    FTextNodeCount := 0;
  end;}
end;

function TLmlNode.GetIsTextNode: boolean;
begin
  Result := FNodeType = lntText;
end;

{ TLmlDocument }

procedure TLmlDocument.Assign(doc: TLmlDocument);
begin
  if doc <> Self then Root.Assign(doc.Root);
end;

procedure TLmlDocument.Clear;
var
  entry: TLmlNode;
begin
  entry := FRoot;

  if Destroying then FRoot := nil else
  if Assigned(entry) then
    FRoot := CreateDocumentNode(entry.FName, '') else
    FRoot := CreateDocumentNode('', '');

  if Assigned(entry) then
  begin
    entry.NilDocument;
    entry.Clear;
    entry.DecRefcount;
  end;
end;

constructor TLmlDocument.Create(const RootName: string);
begin
  FRoot := CreateDocumentNode(RootName, '');
  FXmlDocVersion := '1.0';
  FXmlDocEncoding := 'UTF-8';
  FXmlDocStandalone := false;
end;

constructor TLmlDocument.CreateFromFile(const FileName: string);
begin
  Create('lml');
  LoadFromFile(FileName);
end;

function TLmlDocument.CreateNode(Parent: TLmlNode; const Name, Text: string): TLmlNode;
begin
  Result := TLmlNode.Create(Self, Name, Text);
  if Parent <> nil then
  begin
    Parent.FItemList.Add(Result);
    Result.FParent := Parent;
  end;
end;

destructor TLmlDocument.Destroy;
begin
  Destroying := true;
  Clear;
  inherited;
end;

procedure TLmlDocument.EventLoadingNode(Node: TLmlNode);
begin
  if Assigned(FOnLoadingNode) then
    FOnLoadingNode(Self, Node);
end;

procedure TLmlDocument.EventLoadNode(Node: TLmlNode);
begin
  if Assigned(FOnLoadNode) then
    FOnLoadNode(Self, Node);
end;

procedure TLmlDocument.LoadFromFile(const FileName: string);
var
  fname: string;
begin
  BeginLoad;
  try
    fname := ExpandFileName(Trim(FileName));
    LoadFromPPCFile(fname);
  finally
    EndLoad;
  end;
end;

procedure TLmlDocument.LoadFromPPCFile(const FileName: string);
var
  ls: TLmlFileStream;
begin
  ls := TLmlFileStream.Create(FileName, fmShareDenyWrite);
  try
    LoadFromPPCStream(ls);
  finally
    ls.Free;
  end;
end;

procedure TLmlDocument.LoadFromPPCStream(stream: TLmlStream);
begin
  Root.ParsePPC(stream);
end;

procedure TLmlDocument.LoadFromPPCString(const PPC: string);
var
  S: TLmlMemoryStream;
begin
  S := TLmlMemoryStream.Create;
  try
    S.WriteText(PPC);
    S.Position := 0;
    LoadFromPPCStream(S);
  finally
    S.Free;
  end;
end;

procedure TLmlDocument.NotifyRemove(node: TLmlNode);
begin
  if node = FRoot then
    if not Destroying then
      FRoot := CreateDocumentNode(FRoot.Name, '') else
      FRoot := nil;
end;

procedure TLmlDocument.SaveToFile(const FileName: string);
var
  fname: string;
begin
  BeginSave;
  try
    fname := ExpandFileName(Trim(FileName));
    SaveToPPCFile(fname);
  finally
    EndSave;
  end;
end;

procedure TLmlDocument.SaveToPPCFile(const FileName: string);
var
  ls: TLmlFileStream;
begin
  ls := TLmlFileStream.Create(FileName, fmCreate);
  try
    SaveToPPCStream(ls);
  finally
    ls.Free;
  end;
end;

procedure TLmlDocument.SaveToPPCStream(stream: TLmlStream);
begin
  Root.WritePPC(stream);
end;

procedure TLmlDocument.SetFileName(const Value: string);
var
  fname: string;
begin
  fname := ExpandFileName(Trim(Value));
  if not AnsiSameText(fname, FFileName) then
    LoadFromFile(fname);
end;

function TLmlDocument.GetState(Index: TLmlDocumentState): boolean;
begin
  Result := Index in FState;
end;

procedure TLmlDocument.SetState(Index: TLmlDocumentState; const Value: boolean);
begin
  if Value then
    FState := FState + [Index] else
    FState := FState - [Index];
end;

procedure TLmlDocument.BeginLoad;
begin
  Inc(FLoadCount);
  Loading := FLoadCount <> 0;
end;

procedure TLmlDocument.EndLoad;
begin
  Dec(FLoadCount);
  Loading := FLoadCount <> 0;
end;

procedure TLmlDocument.BeginSave;
begin
  Inc(FSaveCount);
  Saving := FSaveCount <> 0;
end;

procedure TLmlDocument.EndSave;
begin
  Dec(FSaveCount);
  Saving := FSaveCount <> 0;
end;

function TLmlDocument.CreateTextNode(Parent: TLmlNode; const Text: string): TLmlNode;
begin
  Inc(Parent.FTextNodeCount);
  Result := CreateNode(Parent, '@text', Text);
  Result.FNodeType := lntText;
end;

function TLmlDocument.GetRoot: TLmlNode;
begin
  if (FRoot = nil) and not Destroying then
  begin
    FRoot := CreateDocumentNode('', '');
    FRoot.FNodeType := lntDocument;
  end;
  Result := FRoot;
end;

function TLmlDocument.CreateDocumentNode(const Name,
  Text: string): TLmlNode;
var
  RootName: string;
begin
  RootName := Name;
  if RootName = '' then
    RootName := 'lml';
  Result := CreateNode(nil, RootName, Text);
  Result.FNodeType := lntDocument;
end;

{ TLmlDocumentReader }

constructor TLmlDocumentReader.Create(ADocument: TLmlDocument);
begin
  FStack := TList.Create;
  FDocument := ADocument;
end;

destructor TLmlDocumentReader.Destroy;
begin
  FreeAndNil(FReader);
  FreeAndNil(FStack);
  inherited;
end;

procedure TLmlDocumentReader.PopNode;
var
  index: integer;
  node: TLmlNode;
begin
  index := FStack.Count - 1;
  node := FStack[index];
  FStack.Delete(index);
  FDocument.EventLoadNode(node);
end;

procedure TLmlDocumentReader.PopToCount(count: integer);
begin
  if count < 0 then count := 0;
  while FStack.Count > count do PopNode;
end;

procedure TLmlDocumentReader.PushNode(node: TLmlNode);
begin
  FStack.Add(node);
  FDocument.EventLoadingNode(node);
end;

procedure TLmlDocumentReader.ReaderNotify(Sender: TObject; var Terminate: boolean);
var
  node: TLmlNode;
begin
  if FReader.NodeType = lntDocument then
  begin
    FDocument.Root.Name := FReader.NodeName;
    FDocument.Root.Text := FReader.NodeText;
    PushNode(FDocument.Root);
  end
  else
  begin
    PopToCount(FReader.Level);
    node := FStack.Last;
    case FReader.NodeType of
      lntElement  : PushNode(node.Add(FReader.NodeName, FReader.NodeText));
      lntAttribute: node.attr.Write(FReader.NodeName, FReader.NodeText);
      lntText     : PushNode(node.AddText(FReader.NodeText));
      else FReader.Terminate;
    end;
  end;
end;

procedure TLmlDocumentReader.ReadFromStream(Stream: TLmlStream);
begin
  FReader := TLmlReader.Create(Stream);
  try
    FReader.OnNotify := ReaderNotify;
    FDocument.BeginLoad;
    try
      FDocument.Clear;
      FStack.Clear;
      FReader.Execute;
      PopToCount(0);
    finally
      FDocument.EndLoad;
    end;
  finally
    FreeAndNil(FReader);
  end;
end;

{ TRecRecord }

function TRecRecord.Assign(ARecord: TRecRecord; Range: TRecRange): boolean;
var
  index: integer;
begin
  Result := CheckSame(ARecord);
  if Result and (ARecord <> Self) then
  begin
    Range := VaryRange(Range);
    for index := GetCount - 1 downto 0 do
      if index in Range then
        GetField(index).CopyFV(ARecord.ValueAt(index), ValueAt(index));
  end;
end;

function TRecRecord.Changed(ARecord: TRecRecord): boolean;
var
  index: integer;
begin
  if (ARecord <> Self) and CheckSame(ARecord) then
    for index := GetCount - 1 downto 0 do
      if not GetField(index).EqualFV(ValueAt(index), ARecord.ValueAt(index)) then
      begin
        Result := true;
        Exit;
      end;
  Result := false;
end;

function TRecRecord.CheckSame(ARecord: TRecRecord): boolean;
begin
  Result := IsSame(ARecord);
  if not Result then
    raise Exception.CreateFmt('%s can''not operate with %s',
      [ClassName, ARecord.ClassName]);
end;

function TRecRecord.Clear(Range: TRecRange): TRecRecord;
var
  index: integer;
begin
  Result := Self;
  Range := VaryRange(Range);
  for index := GetCount - 1 downto 0 do
    if index in Range then
      GetField(index).ClearFV(ValueAt(index));
end;

function TRecRecord.ClearAll: TRecRecord;
begin
  Result := Clear([]);
end;

function TRecRecord.Compare(Other: TRecRecord): integer;
begin
  if Self = Other then
    Result := 0 else
    Result := AnsiCompareText(GetSV(0), Other.GetSV(0));
end;

constructor TRecRecord.Create(FieldList: TRecFieldList);
var
  index: integer;
begin
  FFieldList := FieldList;
  SetLength(FValues, GetCount);
  for index := GetCount - 1 downto 0 do
    GetField(index).InitFV(ValueAt(index));
end;

destructor TRecRecord.Destroy;
begin
  ClearAll;
  SetLength(FValues, 0);
  inherited;
end;

function TRecRecord.Diff(ARecord: TRecRecord): TRecRange;
var
  index: integer;
begin
  Result := [];
  if (ARecord <> Self) and CheckSame(ARecord) then
    for index := GetCount - 1 downto 0 do
      if not GetField(index).EqualFV(ValueAt(index), ARecord.ValueAt(index)) then
        Result := Result + [index];
end;

function TRecRecord.FieldByName(const FieldName: string): TRecField;
begin
  Result := FFieldList.FieldByName(FieldName);
end;

function TRecRecord.Filter: string;
var
  PF: TRecField;
begin
  PF := FFieldList.PrimaryField;
  if PF = nil then
    PF := GetField(0);
  if PF <> nil then
    Result := Format('%s=%s', [PF.FieldName, SQLValue(PF.FieldIndex)]) else
    Result := '';
end;

function TRecRecord.FindField(const FieldName: string): TRecField;
begin
  Result := FFieldList.FindField(FieldName);
end;

function TRecRecord.FullRange: TRecRange;
begin
  Result := [0..GetCount - 1];
end;

function TRecRecord.GetBV(Index: integer): boolean;
begin
  Result := GetField(Index).GetBoolean(ValueAt(Index));
end;

function TRecRecord.GetCount: integer;
begin
  Result := FFieldList.GetCount;
end;

function TRecRecord.GetField(index: integer): TRecField;
begin
  Result := FFieldList.GetField(index);
end;

function TRecRecord.GetFieldName(Index: integer): string;
begin
  Result := GetField(Index).FieldName;
end;

function TRecRecord.GetFieldTitle(Index: integer): string;
begin
  Result := GetField(Index).FieldTitle;
end;

function TRecRecord.GetFieldType(Index: integer): TRecFieldType;
begin
  Result := GetField(Index).FieldType;
end;

function TRecRecord.GetFV(Index: integer): double;
begin
  Result := GetField(Index).GetFloat(ValueAt(Index));
end;

function TRecRecord.GetIV(Index: integer): integer;
begin
  Result := GetField(Index).GetInteger(ValueAt(Index));
end;

function TRecRecord.GetMV(Index: integer): currency;
begin
  Result := GetField(Index).GetMoney(ValueAt(Index));
end;

function TRecRecord.GetPPCode: string;
var
  doc: TLmlDocument;
begin
  doc := TLmlDocument.Create(FFieldList.FName);
  try
    SaveToNode(doc.Root);
    Result := doc.Root.PPCode;
  finally
    doc.Free;
  end;
end;

function TRecRecord.GetSV(Index: integer): string;
begin
  Result := GetField(Index).GetString(ValueAt(Index));
end;

function TRecRecord.IndexOf(const FieldName: string): integer;
begin
  Result := FFieldList.IndexOf(FieldName);
end;

function TRecRecord.InsertSQL(const Table: string; Range: TRecRange): string;
const
  INSFMT = 'INSERT INTO %s (%s) VALUES (%s);';
var
  field, value, flist, vlist: string;
  index: integer;
begin
  flist := '';
  vlist := '';
  Range := VaryRange(Range);
  for index := 0 to GetCount - 1 do
    if index in Range then
    begin
      field := GetField(index).FieldName;
      value := SQLValue(index);
      if flist = '' then
      begin
        flist := field;
        vlist := value;
      end
      else
      begin
        flist := flist + ',' + field;
        vlist := vlist + ',' + value;
      end;
    end;
  if flist <> '' then
    Result := Format(INSFMT, [Table, flist, vlist]) else
    Result := '';
end;

function TRecRecord.IsSame(ARecord: TRecRecord): boolean;
begin
  Result := (ARecord <> nil) and
            (ARecord.FFieldList = FFieldList);
end;

function TRecRecord.LoadFromNode(Node: TLmlNode): TLmlNode;
var
  index: integer;
  field: TRecField;
  value: PRecValue;
begin
  Result := Node;
  for index := GetCount - 1 downto 0 do
  begin
    field := GetField(index);
    value := ValueAt(index);
    field.SetString(value, Node.attr.Read(field.FieldName, ''));
  end;
end;

procedure TRecRecord.LoadFromPPCFile(const FileName: string);
var
  doc: TLmlDocument;
begin
  doc := TLmlDocument.Create('');
  try
    doc.LoadFromPPCFile(FileName);
    LoadFromNode(doc.Root);
  finally
    doc.Free;
  end;
end;

function TRecRecord.Explain(Index: integer): string;
begin
  Result := GetField(Index).Explain(ValueAt(Index));
end;

{$ifdef USE_LSEU}
function TRecRecord.ReadDs(DS: TLseDataset; Range: TRecRange): TRecRange;
var
  field: TLseField;
  desti: TRecField;
  value: PRecValue;
  index: integer;
begin
  Result := VaryRange(Range);
  for index := GetCount - 1 downto 0 do
    if index in Result then
    begin
      desti := GetField(index);
      field := DS.FindField(desti.FFieldName);
      if field <> nil then
      begin
        value := ValueAt(index);
        case desti.FFieldType of
          rftInteger: desti.SetInteger(value, field.AsInteger);
          rftString : desti.SetString(value, field.AsString);
          rftBoolean: desti.SetBoolean(value, field.AsBoolean);
          rftFloat  : desti.SetFloat(value, field.AsFloat);
          rftMoney  : desti.SetMoney(value, field.AsCurrency);
        end;
      end
      else Result := Result - [index];
    end;
end;
{$endif}

function TRecRecord.ReadMatched(ARecord: TRecRecord; Range: TRecRange): TRecRange;
var
  fsrc, fdst: TRecField;
  vsrc, vdst: PRecValue;
  index: integer;
begin
  Result := VaryRange(Range);

  if IsSame(ARecord) then
  begin
    Assign(ARecord, Result);
    Exit;
  end;

  for index := GetCount - 1 downto 0 do
    if index in Result then
    begin
      fdst := GetField(index);
      fsrc := ARecord.FindField(fdst.FFieldName);
      if fsrc <> nil then
      begin
        vsrc := ARecord.ValueAt(fsrc.FFieldIndex);
        vdst := ValueAt(index);
        case fdst.FFieldType of
          rftInteger: fdst.SetInteger(vdst, fsrc.GetInteger(vsrc));
          rftString : fdst.SetString(vdst, fsrc.GetString(vsrc));
          rftBoolean: fdst.SetBoolean(vdst, fsrc.GetBoolean(vsrc));
          rftFloat  : fdst.SetFloat(vdst, fsrc.GetFloat(vsrc));
          rftMoney  : fdst.SetMoney(vdst, fsrc.GetMoney(vsrc));
        end;
      end
      else Result := Result - [index];
    end;
end;

procedure TRecRecord.Reload;
{$ifdef USE_LSEU}
var
  db: TLseDatabase;
  ds: TLseDataset;
  fs: string;
{$endif}
begin
{$ifdef USE_LSEU}
  fs := Filter;
  if fs <> '' then
    fs := 'WHERE ' + fs;
  db := lse_sysdb;
  try
    ds := db.OpenSQL('SELECT * FROM %s %s', [FFieldList.FName, fs]);
    try
      ReadDs(ds);
    finally
      ds.Free;
    end;
  finally
    db.Free;
  end;
{$endif}
end;

function TRecRecord.SaveToNode(Node: TLmlNode): TLmlNode;
var
  index: integer;
begin
  Result := Node;
  Node.Clear;
  Node.Name := FFieldList.FName;
  for index := GetCount - 1 downto 0 do
    Node.attr.Write(GetFieldName(index), GetSV(index));
end;

procedure TRecRecord.SaveToPPCFile(const FileName: string);
var
  doc: TLmlDocument;
begin
  doc := TLmlDocument.Create('');
  try
    SaveToNode(doc.Root);
    doc.Root.Name := FFieldList.FName;
    doc.SaveToPPCFile(FileName);
  finally
    doc.Free;
  end;
end;

procedure TRecRecord.SetBV(Index: integer; Value: boolean);
begin
  GetField(Index).SetBoolean(ValueAt(Index), Value);
end;

procedure TRecRecord.SetFV(Index: integer; Value: double);
begin
  GetField(Index).SetFloat(ValueAt(Index), Value);
end;

procedure TRecRecord.SetIV(Index, Value: integer);
begin
  GetField(Index).SetInteger(ValueAt(Index), Value);
end;

procedure TRecRecord.SetMV(Index: integer; Value: currency);
begin
  GetField(Index).SetMoney(ValueAt(Index), Value);
end;

procedure TRecRecord.SetPPCode(const Value: string);
var
  doc: TLmlDocument;
begin
  doc := TLmlDocument.Create(FFieldList.FName);
  try
    doc.Root.PPCode := Value;
    LoadFromNode(doc.Root);
  finally
    doc.Free;
  end;
end;

procedure TRecRecord.SetSV(Index: integer; const Value: string);
begin
  GetField(Index).SetString(ValueAt(Index), Value);
end;

function TRecRecord.SQLValue(Index: integer): string;
begin
  Result := GetField(Index).SQLValue(ValueAt(Index));
end;

function TRecRecord.UpdateSQL(const Table, Filter: string; Range: TRecRange): string;
const
  UPDFMT = 'UPDATE %s SET %s %s;';
var
  setfv, flist, where: string;
  index: integer;
begin
  flist := '';
  Range := VaryRange(Range);
  for index := 0 to GetCount - 1 do
    if index in Range then
    begin
      setfv := GetField(index).FieldName + '=' + SQLValue(index);
      if flist = '' then
        flist := setfv else
        flist := flist + ',' + setfv;
    end;
  if flist <> '' then
  begin
    if Filter <> '' then
      where := 'WHERE ' + Filter;
    Result := Format(UPDFMT, [Table, flist, where]);
  end
  else Result := '';
end;

function TRecRecord.ValueAt(index: integer): PRecValue;
begin
  Result := @FValues[index];
end;

function TRecRecord.VaryRange(Range: TRecRange): TRecRange;
begin
  if Range = [] then
    Result := [0..GetCount - 1] else
    Result := Range;
end;

{ TRecField }

function TRecField.ClearFV(FV: PRecValue): PRecValue;
begin
  Result := FV;
  if FFieldType = rftString then
    string(FV^.VString) := '' else
    FillChar(FV^, sizeof(RRecValue), 0);
end;

function TRecField.CopyFV(FV, Desti: PRecValue): PRecValue;
begin
  case FFieldType of
    rftInteger: Desti^.VInteger := FV^.VInteger;
    rftString : string(Desti^.VString) := string(FV^.VString);
    rftBoolean: Desti^.VBoolean := FV^.VBoolean;
    rftFloat  : Desti^.VFloat := FV^.VFloat;
    rftMoney  : Desti^.VMoney := FV^.VMoney;
  end;
  Result := FV;
end;

constructor TRecField.Create(FieldList: TRecFieldList);
begin
  FFieldList := FieldList;
  FFieldIndex := FFieldList.FFields.Add(Self);
end;

destructor TRecField.Destroy;
begin
  SetPrimary(false);
  FFieldList.FFields.Remove(Self);
  inherited;
end;

function TRecField.EqualFV(V1, V2: PRecValue): boolean;
begin
  case FFieldType of
    rftInteger: Result := (V1^.VInteger = V2^.VInteger);
    rftString : Result := (string(V1^.VString) = string(V2^.VString));
    rftBoolean: Result := (V1^.VBoolean = V2^.VBoolean);
    rftFloat  : Result := IsZero(V1^.VFloat - V2^.VFloat);
    rftMoney  : Result := (V1^.VMoney = V2^.VMoney);
        else    Result := false;
  end;
end;

function TRecField.GetBoolean(FV: PRecValue): boolean;
begin
  case FFieldType of
    rftInteger: Result := (FV^.VInteger <> 0);
    rftString : Result := (string(FV^.VString) <> '');
    rftBoolean: Result := FV^.VBoolean;
    rftFloat  : Result := not IsZero(FV^.VFloat);
    rftMoney  : Result := (FV^.VMoney <> 0);
        else    Result := false;
  end;
end;

function TRecField.GetFloat(FV: PRecValue): double;
begin
  case FFieldType of
    rftInteger: Result := FV^.VInteger;
    rftString : Result := stod(string(FV^.VString));
    rftBoolean: Result := Ord(FV^.VBoolean);
    rftFloat  : Result := FV^.VFloat;
    rftMoney  : Result := FV^.VMoney;
        else    Result := 0;
  end;
end;

function TRecField.GetInteger(FV: PRecValue): integer;
begin
  case FFieldType of
    rftInteger: Result := FV^.VInteger;
    rftString : Result := stoi(string(FV^.VString));
    rftBoolean: Result := Ord(FV^.VBoolean);
    rftFloat  : Result := Trunc(FV^.VFloat);
    rftMoney  : Result := Trunc(FV^.VMoney);
        else    Result := 0;
  end;
end;

function TRecField.GetMoney(FV: PRecValue): currency;
begin
  case FFieldType of
    rftInteger: Result := FV^.VInteger;
    rftString : Result := stod(string(FV^.VString));
    rftBoolean: Result := Ord(FV^.VBoolean);
    rftFloat  : Result := FV^.VFloat;
    rftMoney  : Result := FV^.VMoney;
        else    Result := 0;
  end;
end;

function TRecField.GetPrimary: boolean;
begin
  Result := (Self = FFieldList.FPrimaryField);
end;

function TRecField.GetString(FV: PRecValue): string;
begin
  case FFieldType of
    rftInteger: Result := IntToStr(FV^.VInteger);
    rftString : Result := string(FV^.VString);
    rftBoolean: Result := IntToStr(Ord(FV^.VBoolean));
    rftFloat  : if IsZero(FV^.VFloat) then
                  Result := '0' else
                  Result := FloatToStr(FV^.VFloat);
    rftMoney  : Result := FloatToStr(FV^.VMoney);
        else    Result := '';
  end;
end;

function TRecField.GetUnique: boolean;
begin
  Result := FUnique or GetPrimary;
end;

function TRecField.InitFV(FV: PRecValue): PRecValue;
begin
  Result := FV;
  if FFieldType = rftString then
  begin
    FV^.VString := nil;
    string(FV^.VString) := '';
  end
  else FillChar(FV^, sizeof(RRecValue), 0);
end;

function TRecField.Explain(FV: PRecValue): string;
begin
  Result := GetString(FV);
  if FPickList <> nil then
    Result := lml_read_value(FPickList, Result, Result);
end;

function TRecField.SetBoolean(FV: PRecValue; Value: boolean): PRecValue;
begin
  case FFieldType of
    rftInteger: FV^.VInteger := Ord(Value);
    rftString : string(FV^.VString) := IntToStr(Ord(Value));
    rftBoolean: FV^.VBoolean := Value;
    rftFloat  : FV^.VFloat := Ord(Value);
    rftMoney  : FV^.VMoney := Ord(Value);
  end;
  Result := FV;
end;

function TRecField.SetFloat(FV: PRecValue; Value: double): PRecValue;
begin
  case FFieldType of
    rftInteger: FV^.VInteger := Trunc(Value);
    rftString : string(FV^.VString) := FloatToStr(Value);
    rftBoolean: FV^.VBoolean := not IsZero(Value);
    rftFloat  : FV^.VFloat := Value;
    rftMoney  : FV^.VMoney := Value;
  end;
  Result := FV;
end;

function TRecField.SetInteger(FV: PRecValue; Value: integer): PRecValue;
begin
  case FFieldType of
    rftInteger: FV^.VInteger := Value;
    rftString : string(FV^.VString) := IntToStr(Value);
    rftBoolean: FV^.VBoolean := (Value <> 0);
    rftFloat  : FV^.VFloat := Value;
    rftMoney  : FV^.VMoney := Value;
  end;
  Result := FV;
end;

function TRecField.SetMoney(FV: PRecValue; Value: currency): PRecValue;
begin
  case FFieldType of
    rftInteger: FV^.VInteger := Trunc(Value);
    rftString : string(FV^.VString) := FloatToStr(Value);
    rftBoolean: FV^.VBoolean := (Value <> 0);
    rftFloat  : FV^.VFloat := Value;
    rftMoney  : FV^.VMoney := Value;
  end;
  Result := FV;
end;

procedure TRecField.SetPrimary(const Value: boolean);
begin
  if Value then
    FFieldList.FPrimaryField := Self else
  if GetPrimary then
    FFieldList.FPrimaryField := nil;
end;

function TRecField.SetString(FV: PRecValue; const Value: string): PRecValue;
begin
  case FFieldType of
    rftInteger: FV^.VInteger := stoi(Value);
    rftString : string(FV^.VString) := Value;
    rftBoolean: FV^.VBoolean := (Value <> '');
    rftFloat  : FV^.VFloat := stod(Value);
    rftMoney  : FV^.VMoney := stod(Value);
  end;
  Result := FV;
end;

function TRecField.SQLValue(FV: PRecValue): string;
begin
  Result := GetString(FV);
  if FFieldType = rftString then
    Result := '''' + StringReplace(Result,
              '''', '''''', [rfReplaceAll]) + '''';
end;

{ TRecFieldList }

function TRecFieldList.AddBooleanField(const FieldName, Title: string): TRecField;
begin
  Result := TRecField.Create(Self);
  Result.FFieldName := LowerCase(FieldName);
  Result.FFieldTitle := Title;
  Result.FFieldType := rftBoolean;
  Result.FFieldSize := sizeof(boolean);
end;

function TRecFieldList.AddFloatField(const FieldName, Title: string): TRecField;
begin
  Result := TRecField.Create(Self);
  Result.FFieldName := LowerCase(FieldName);
  Result.FFieldTitle := Title;
  Result.FFieldType := rftFloat;
  Result.FFieldSize := sizeof(double);
end;

function TRecFieldList.AddIntegerField(const FieldName, Title: string): TRecField;
begin
  Result := TRecField.Create(Self);
  Result.FFieldName := LowerCase(FieldName);
  Result.FFieldTitle := Title;
  Result.FFieldType := rftInteger;
  Result.FFieldSize := sizeof(integer);
end;

function TRecFieldList.AddMoneyField(const FieldName, Title: string): TRecField;
begin
  Result := TRecField.Create(Self);
  Result.FFieldName := LowerCase(FieldName);
  Result.FFieldTitle := Title;
  Result.FFieldType := rftMoney;
  Result.FFieldSize := sizeof(currency);
end;

function TRecFieldList.AddStringField(const FieldName, Title: string; FieldSize: integer): TRecField;
begin
  Result := TRecField.Create(Self);
  Result.FFieldName := LowerCase(FieldName);
  Result.FFieldTitle := Title;
  Result.FFieldType := rftString;
  Result.FFieldSize := FieldSize;
end;

constructor TRecFieldList.Create(const Name: string);
begin
  FName := LowerCase(Name);
  FRecordClass := TRecRecord;
  FFields := TList.Create;
end;

function TRecFieldList.CreateRecord: TRecRecord;
begin
  Result := FRecordClass.Create(Self);
end;

destructor TRecFieldList.Destroy;
var
  index: integer;
begin
  for index := GetCount - 1 downto 0 do
    GetField(index).Free;
  FFields.Free;
  FRecordClass := nil;
  inherited;
end;

function TRecFieldList.FieldByName(const FieldName: string): TRecField;
begin
  Result := FindField(FieldName);
  if Result = nil then
    raise Exception.CreateFmt('TRecField(''%s'') not found', [FieldName]);
end;

function TRecFieldList.FindField(const FieldName: string): TRecField;
var
  index: integer;
begin
  index := IndexOf(FieldName);
  if index >= 0 then
    Result := GetField(index) else
    Result := nil;
end;

function TRecFieldList.GetCount: integer;
begin
  Result := FFields.Count;
end;

function TRecFieldList.GetField(index: integer): TRecField;
begin
  Result := FFields[index];
end;

function TRecFieldList.IndexOf(const FieldName: string): integer;
var
  index: integer;
begin
  for index := GetCount - 1 downto 0 do
    if AnsiSameText(FieldName, GetField(index).FFieldName) then
    begin
      Result := index;
      Exit;
    end;
  Result := -1;
end;

end.
