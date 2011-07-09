unit lse_efx;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, stdctrls, extctrls, Forms,
  {$IFDEF WINDOWS}
  Windows, Messages,
  {$ELSE}
  LCLType, LMessages,
  {$ENDIF}
  lseu, lse_funcs;

const
  MaxEasyRanges = 128;

type
  TEasyForm   = class; {<--forward}
  TEasyScript = class;
  TEasyView   = class;
  TEasyStyle  = class;
  TEasyStyles = class;

  TEasyTagType = (egtInt8, egtInt16, egtInt32, egtInt64, egtString);

  TEasyTag = (egMagic, egName, egRowCount, egColCount, egFixedRows,
              egFixedCols, egItemCount, egItemBegin, egItemEnd, egItemRow,
              egItemCol, egItemRowSpan, egItemColSpan, egItemEdges,
              egItemText, egItemSname);

  TEasyStream = class(TMemoryStream)
  public
    function skip_value: TEasyTagType;

    function read_easytag: TEasyTag;
    function read_easytag_check(tag: TEasyTag): TEasyTag;
    function read_easytag_type: TEasyTagType;
    function read_easytag_type_check(tagtype: TEasyTagType): TEasyTagType;

    function read_shortint: shortint;
    function read_smallint: smallint;
    function read_longint: longint;
    function read_int64: int64;
    function read_byte: byte;
    function read_word: word;
    function read_longword: longword;
    function read_string: string;

    function read_taged_shortint(tag: TEasyTag): shortint;
    function read_taged_smallint(tag: TEasyTag): smallint;
    function read_taged_longint(tag: TEasyTag): longint;
    function read_taged_int64(tag: TEasyTag): int64;
    function read_taged_byte(tag: TEasyTag): byte;
    function read_taged_word(tag: TEasyTag): word;
    function read_taged_longword(tag: TEasyTag): longword;
    function read_taged_string(tag: TEasyTag): string;

    procedure write_easytag(value: TEasyTag);
    procedure write_easytag_type(value: TEasyTagType);

    procedure write_shortint(value: shortint);
    procedure write_smallint(value: smallint);
    procedure write_longint(value: longint);
    procedure write_int64(value: int64);
    procedure write_byte(value: byte);
    procedure write_word(value: word);
    procedure write_longword(value: longword);
    procedure write_string(const value: string);

    procedure write_taged_shortint(tag: TEasyTag; value: shortint);
    procedure write_taged_smallint(tag: TEasyTag; value: smallint);
    procedure write_taged_longint(tag: TEasyTag; value: longint);
    procedure write_taged_int64(tag: TEasyTag; value: int64);
    procedure write_taged_byte(tag: TEasyTag; value: byte);
    procedure write_taged_word(tag: TEasyTag; value: word);
    procedure write_taged_longword(tag: TEasyTag; value: longword);
    procedure write_taged_string(tag: TEasyTag; const value: string);

    function write_hex(const HS: string): integer;
    function read_hex(Count: integer): string;
  end;

  REasyRanges = packed record
    range: array[0..MaxEasyRanges - 1] of TRect;
    count: integer;
  end;
  PEasyRanges = ^REasyRanges;

  TEasyItem = class(TLseObject)
  private
    FForm: TEasyForm;
    FText: WideString;
    FStyle: string;
    FRow: integer;
    FCol: integer;
    FRowSpan: integer;
    FColSpan: integer;
    procedure SetText(const Value: WideString);
    procedure SetRow(Value: integer);
    procedure SetCol(Value: integer);
    procedure SetRowSpan(Value: integer);
    procedure SetColSpan(Value: integer);
    function GetStyle: TEasyStyle;
    procedure SetStyle(const Value: TEasyStyle);
    procedure LoadFromStream(Stream: TEasyStream);
    procedure SaveToStream(Stream: TEasyStream);
    function Contains(ACol, ARow: integer): boolean;
    function Intersects(ARange: TRect): boolean;
    function IntersectRanges(Ranges: PEasyRanges): boolean;
    procedure Resize(ACol, ARow, AColSpan, ARowSpan: integer);
    procedure Ensure;
    procedure Leave;
  public
    constructor Create(AForm: TEasyForm);
    destructor Destroy;override;
    procedure Validate;
    procedure Assign(AItem: TEasyItem);
    procedure Changed;
    procedure Delete;
    procedure Draw(Canvas: TCanvas; P: TPoint; Printing: boolean);
    function Left: integer;
    function Top: integer;
    function Width: integer;
    function Height: integer;
    function Active: boolean;
    function Selected: boolean;
    function ItemID: string;
    function ItemRange: TRect;
    function ItemRangeID: string;
    property Form: TEasyForm read FForm;
    property Text: WideString read FText write SetText;
    property Row: integer read FRow write SetRow;
    property Col: integer read FCol write SetCol;
    property RowSpan: integer read FRowSpan write SetRowSpan;
    property ColSpan: integer read FColSpan write SetColSpan;
    property Style: TEasyStyle read GetStyle write SetStyle;
  end;

  TEasyFormState = (efsModified, efsChanged, efsResized, efsDestroying);
  TEasyFormStates = set of TEasyFormState;

  TEasyForm = class(TLseObject)
  private
    FView: TEasyView;
    FName: string;
    FItems: TList;
    FWidths: TList;
    FHeights: TList;
    FFixedColCount: integer;
    FFixedRowCount: integer;
    FState: TEasyFormStates;
    FScript: TEasyScript;
    FStyles: TEasyStyles;
    FUpdateCount: integer;
    FOnChange: TNotifyEvent;
    FOnDestroying: TNotifyEvent;
    FOnDelete: TNotifyEvent;
    procedure SetName(const Value: string);
    function GetModified: boolean;
    procedure SetModified(Value: boolean);
    function GetColCount: integer;
    procedure SetColCount(Value: integer);
    function GetColWidth(ACol: integer): integer;
    procedure SetColWidth(ACol, Value: integer);
    function GetRowCount: integer;
    procedure SetRowCount(Value: integer);
    function GetRowHeight(ARow: integer): integer;
    procedure SetRowHeight(ARow, Value: integer);
    function GetWidth: integer;
    function GetHeight: integer;
    function GetItemCount: integer;
    function GetItem(Index: integer): TEasyItem;
    function GetLefts(ACol: integer): integer;
    function GetTops(ARow: integer): integer;
    procedure SetFixedCols(Value: integer);
    procedure SetFixedRows(Value: integer);
    procedure ResizeView;
    procedure DrawOnPrinter;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Assign(AForm: TEasyForm);
    function Clone: TEasyForm;
    function CopyPart(Range: TRect): TEasyForm;
    function GetItemsInRange(ARange: TRect): TList;
    function Active: boolean;
    { update }
    function TryBeginUpdate(Condition: boolean): boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    { load }
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TEasyStream);
    { save }
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TEasyStream);
    { item }
    function FindByPos(ACol, ARow: integer): TEasyItem;
    function Force(ACol, ARow: integer): TEasyItem;
    function NewBy(ACol, ARow: integer): TEasyItem;
    procedure Delete(Index: integer);
    procedure Sort;
    { range }
    function SetupRangeRec(const Range: string; ER: PEasyRanges): boolean;
    function GetRangeID(ARange: TRect): string;
    function GetRangedItems(const Range: string): TList;
    function SumRanged(const Range: string; var sumv, avev, maxv, minv: double): integer;
    { print }
    procedure Draw(Canvas: TCanvas; P: TPoint);
    procedure Print;
    { column }
    procedure InsertColumn(ACol: integer);
    procedure AppendColumn;
    procedure DeleteColumn(ACol: integer);
    function FixedColWidth: integer;
    function ColFromX(X: integer): integer;
    function ColDistance(Col1, Col2: integer): integer;
    function HasCol(ACol: integer): boolean;
    { row }
    procedure InsertRow(Index: integer);
    procedure AppendRow;
    procedure DeleteRow(Index: integer);
    function FixedRowHeight: integer;
    function RowFromY(Y: integer): integer;
    function RowDistance(Row1, Row2: integer): integer;
    function HasRow(ARow: integer): boolean;
    { property }
    property View: TEasyView read FView;
    property Name: string read FName write SetName;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Modified: boolean read GetModified write SetModified;
    property ItemCount: integer read GetItemCount;
    property Items[Index: integer]: TEasyItem read GetItem;default;
    property ColCount: integer read GetColCount write SetColCount;
    property ColWidths[Index: integer]: integer read GetColWidth write SetColWidth;
    property Lefts[ACol: integer]: integer read GetLefts;
    property FixedColCount: integer read FFixedColCount write SetFixedCols;
    property RowCount: integer read GetRowCount write SetRowCount;
    property RowHeights[Index: integer]: integer read GetRowHeight write SetRowHeight;
    property FixedRowCount: integer read FFixedRowCount write SetFixedRows;
    property Tops[ARow: integer]: integer read GetTops;
    property Styles: TEasyStyles read FStyles;
    property Script: TEasyScript read FScript;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDestroying: TNotifyEvent read FOnDestroying write FOnDestroying;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
  end;

  TEasyScript = class(TLseObject)
  private
    FForm: TEasyForm;
    FList: TStringList;
    function GetCount: integer;
    function GetEntry(Index: integer): string;
    function GetScript(Index: integer): string;
    procedure SetScript(Index: integer; const Value: string);
    procedure SetEntry(Index: integer; const Value: string);
  public
    constructor Create(AForm: TEasyForm);
    destructor Destroy;override;
    function IndexOf(const Name: string): integer;
    procedure Delete(Index: integer);
    procedure Remove(const Name: string);
    procedure Clear;
    function Read(const Name, DefaultScript: string): string;
    function Write(const Name, Script: string): string;
    property Count: integer read GetCount;
    property Entry[Index: integer]: string read GetEntry write SetEntry;
    property Macro[Index: integer]: string read GetScript write SetScript;
    property Form: TEasyForm read FForm;
  end;

  TSizing = (szNone, szHorz, szVert, szSelect);
  TXYHitTest = (xyHead, xyNone, xyItem, xyLine, xyTail);

  TEasyOption = (eoShowRule, eoShowGridLine);
  TEasyOptions = set of TEasyOption;

  TEasyMode = (emDesign, emEnter);

  TInsertRowEvent = procedure(Sender: TObject; Index: integer) of object;

  TUndoOper = (undoChangeTarget, undoChange, undoResize, undoInsertRow,
               undoInsertColumn, undoChangeHeight, undoChangeWidth);

  TUndoRec = record
    ur_oper : TUndoOper; {<--undo operation}
    ur_range: TRect;     {<--undo range}
    ur_index: integer;   {<--row or column index}
    ur_size : integer;   {<--height/width}
    ur_spos : int64;     {<--start position in undo stream}
    ur_epos : int64;     {<--end position in undo stream}
  end;
  PUndoRec = ^TUndoRec;

{ TEasyView                 :TWinControl
    FHorzScrollBar          :TScrollBar
    FVertScrollBar          :TPanel
    FEditArea               :TPanel
      FHorzRulePanel        :TPanel
        FCornerTL           :TPainBox
        FHorzRule           :TPainBox
      FVertRule             :TPainBox
      FBody                 :TPainBox
}
  TEasyView = class(TWinControl)
  private
    FForm: TEasyForm;
    FSizing: TSizing;
    FSizingPoint: TPoint;
    FOption: TEasyOptions;
    FMode: TEasyMode;
    FLineColor: TColor;
    FBackground: TColor;
    FHot: TPoint;
    FEnd: TPoint;
    FStartCol: integer;
    FStartRow: integer;
    FTargeted: boolean;
    FTargetedRange: TRect;
    FChangedRange: TRect;
    FOnSelect: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnAddingRow: TNotifyEvent;
    FOnAddingColumn: TNotifyEvent;
    FOnChangingTarget: TNotifyEvent;
    FOnPasting: TNotifyEvent;
    FOnInsertingRow: TInsertRowEvent;
    FOnInsertingColumn: TInsertRowEvent;
    FOnChangingRowHeight: TInsertRowEvent;
    FOnChangingColumnWidth: TInsertRowEvent;
    FOnPreview: TNotifyEvent;
    FOnPrint: TNotifyEvent;
    FOnUndoStatus: TNotifyEvent;
    FKeyClear: boolean;
    FLeadChar: char;
    FEditArea: TPanel;
    FBody: TPaintBox;
    FCorner: TPaintBox;
    FHorzRulePanel: TPanel;
    FHorzRule: TPaintBox;
    FHorzRuleHeight: integer;
    FVertRule: TPaintBox;
    FVertRuleWidth: integer;
    FHorzScrollBar: TScrollBar;
    FVertScrollBar: TScrollBar;
    FUndoStream: TEasyStream;
    FUndoList: TList;
    FUndoing: boolean;
    procedure SetGridLineColor(Value: TColor);
    function GetOption(AOption: TEasyOption): boolean;
    procedure SetOption(AOption: TEasyOption; Value: boolean);
    function GetSelectedItem: TEasyItem;
    procedure SetMode(Value: TEasyMode);
    procedure SetBackground(Value: TColor);
    function GetModified: boolean;
    procedure SetModified(Value: boolean);
    function GetHotPoint: TPoint;
    function GetEndPoint: TPoint;
    function GetTargetedRangeID: string;
    procedure SetTargetedRangeID(const Value: string);
    procedure ResetDesignView;
    procedure ResetEnterView;
    procedure BodyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BodyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BodyMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BodyPaint(Sender: TObject);
    procedure BodyDrawGridLine;
    function BodyHitOnCell(X, Y: integer; var Col, Row: integer): boolean;
    procedure CornerPaint(Sender: TObject);
    procedure CornerLTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HorzRuleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HorzRuleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HorzRuleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HorzRulePaint(Sender: TObject);
    procedure HorzRuleDrawRule;
    procedure HorzRuleDrawFixedRows;
    function HorzRuleHitTest(X: integer; var Col: integer): TXYHitTest;
    procedure VertRuleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VertRuleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VertRuleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VertRulePaint(Sender: TObject);
    procedure VertRuleDrawRule;
    procedure VertRuleDrawFixedCols;
    function VertRuleHitTest(Y: integer; var Row: integer): TXYHitTest;
    procedure HorzScrollBarChange(Sender: TObject);
    function HorzScrollBarMax(ViewWidth: integer): integer;
    procedure VertScrollBarChange(Sender: TObject);
    function VertScrollBarMax(ViewHeight: integer): integer;
  protected
    {$IFDEF WINDOWS}
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    {$ENDIF}
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF FPC}
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char);override;
    {$ENDIF}
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ExecChar(Ch: WideChar);
    procedure ExecUp(Shift: TShiftState);
    procedure ExecDown(Shift: TShiftState);
    procedure ExecLeft(Shift: TShiftState);
    procedure ExecRight(Shift: TShiftState);
    procedure ExecDelete(Shift: TShiftState);

    procedure SeekNext(Horz: boolean);
    procedure SeekPrev(Horz: boolean);
    function FindDataItem(Col, Row: integer; Next, Horz: boolean): TEasyItem;

    procedure EventSelect(RepaintRules: boolean);
    procedure EventChange(Sender: TObject);
    procedure EventChangingTarget;
    procedure EventPasting;
    procedure EventInsertingRow(Index: integer);
    procedure EventInsertingColumn(Index: integer);
    procedure EventChangingRowHeight(Index: integer);
    procedure EventChangingColumnWidth(Index: integer);
    procedure EventUndoStatus;

    function RowToY(Row: integer): integer;
    function ColToX(Col: integer): integer;
    function SelectedRect: TRect;
    procedure DrawRanged(ABox: TPaintBox; Range: TRect);
    procedure DrawChangedRange;

    function UndoNew: PUndoRec;
    function UndoGet(Index: integer): PUndoRec;
    function UndoGetLast: PUndoRec;
    function UndoDeleteLast: boolean;
    function UndoSaveInsertRow(Index: integer): PUndoRec;
    function UndoSaveInsertColumn(Index: integer): PUndoRec;
    function UndoSaveRowHeight(Row: integer): PUndoRec;
    function UndoSaveColumnWidth(Col: integer): PUndoRec;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Repaint; override;
    procedure LoadFromFile(const fname: string);
    procedure SaveToFile(const fname: string);
    procedure SelectRange(P, E: TPoint);
    procedure Clear;
    procedure Preview;
    procedure Print;
    procedure ResetView;
    procedure MakeCellVisible(Col, Row: integer);
    procedure MakeFocusedCellVisible;
    procedure RefreshSelected;
    procedure ListTarget(List: TList);
    procedure SaveTargetToStream(Stream: TEasyStream);
    procedure LoadTargetFromStream(Stream: TEasyStream);
    procedure Sort;
    procedure AddChangedRange(ARange: TRect);
    { update }
    function TryBeginUpdate(Condition: boolean): boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    { target }
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SetTargetText(const NewText: string; IgnoreReadonly: boolean);
    procedure SetTargetStyle(AStyle: TEasyStyle);
    procedure DeleteTarget(IgnoreReadonly: boolean);
    procedure MergeTarget;
    procedure UnmergeTarget;
    procedure InsertColumnBeforeTarget;
    procedure InsertRowBeforeTarget;
    procedure DeleteTargetColumns;
    procedure DeleteTargetRows;
    function CalcTargetRange(ARect: TRect): TRect;
    { undo }
    function UndoSaveChangeTarget: PUndoRec;
    function UndoSaveChange: PUndoRec;
    function UndoSaveResize: PUndoRec;
    function UndoListClear: integer;
    function UndoCount: integer;
    function Undo: boolean;
    function VisibleRowCount: integer;
    function VisibleColCount: integer;
    function VisibleRange: TRect;
    function VisibleItems: TList;
    property Form: TEasyForm read FForm;
    property Modified: boolean read GetModified write SetModified;
    property Targeted: boolean read FTargeted write FTargeted;
    property TargetedRange: TRect read FTargetedRange;
    property TargetedRangeID: string read GetTargetedRangeID write SetTargetedRangeID;
    property HotPoint: TPoint read GetHotPoint;
    property Selected: TEasyItem read GetSelectedItem;
    property EndPoint: TPoint read GetEndPoint;
    property StartRow: integer read FStartRow;
    property StartCol: integer read FStartCol;
    property Mode: TEasyMode read FMode write SetMode;
    property ShowRule: boolean index eoShowRule read GetOption write SetOption;
    property ShowGridLine: boolean index eoShowGridLine read GetOption write SetOption;
    property GridLineColor: TColor read FLineColor write SetGridLineColor;
    property Background: TColor read FBackground write SetBackground;
  published
    property Name;
    property Align;
    property Left;
    property Top;
    property Width;
    property Height;
    property Visible;
    property Enabled;
    property Hint;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Tag;
    property PopupMenu;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAddingRow: TNotifyEvent read FOnAddingRow write FOnAddingRow;
    property OnAddingColumn: TNotifyEvent read FOnAddingColumn write FOnAddingColumn;
    property OnChangingTarget: TNotifyEvent read FOnChangingTarget write FOnChangingTarget;
    property OnPasting: TNotifyEvent read FOnPasting write FOnPasting;
    property OnInsertingRow: TInsertRowEvent read FOnInsertingRow write FOnInsertingRow;
    property OnInsertingColumn: TInsertRowEvent read FOnInsertingColumn write FOnInsertingColumn;
    property OnChangingRowHeight: TInsertRowEvent read FOnChangingRowHeight write FOnChangingRowHeight;
    property OnChangingColumnWidth: TInsertRowEvent read FOnChangingColumnWidth write FOnChangingColumnWidth;
    property OnPreview: TNotifyEvent read FOnPreview write FOnPreview;
    property OnPrint: TNotifyEvent read FOnPrint write FOnPrint;
    property OnUndoStatus: TNotifyEvent read FOnUndoStatus write FOnUndoStatus;
  end;

  TEasyEdge = (eeTop, eeBottom, eeLeft, eeRight);
  TEasyEdges = set of TEasyEdge;

  TEasyHorzAlign = (ehaLeft, ehaCenter, ehaRight);
  TEasyVertAlign = (evaTop, evaCenter, evaBottom);

  TEasyStyleType = (estNormal, estReadonly, estTitle);
  
  TEasyStyle = class(TLseObject)
  private
    FStyleList: TList;
    FName: string;
    FBuiltin: boolean;
    FUpdateCount: integer;
    FModified: boolean;
    FFontName: string;
    FFontSize: integer;
    FFontStyle: TFontStyles;
    FTextColor: TColor;
    FBackground: TColor;
    FHAlign: TEasyHorzAlign;
    FVAlign: TEasyVertAlign;
    FLineDistance: integer;
    FWordWrap: boolean;
    FItemType: TEasyStyleType;
    FEdges: TEasyEdges;
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetFontColor(const Value: TColor);
    procedure SetName(const Value: string);
    function GetFontStyleText: string;
    function GetStyle(const Index: TFontStyle): boolean;
    procedure SetStyle(const Index: TFontStyle; const Value: boolean);
    procedure SetBackColor(const Value: TColor);
    procedure SetHAlign(const Value: TEasyHorzAlign);
    procedure SetVAlign(const Value: TEasyVertAlign);
    procedure SetLineDistance(const Value: integer);
    procedure SetModified(const Value: boolean);
    procedure SetWordWrap(const Value: boolean);
    procedure SetItemType(const Value: TEasyStyleType);
    procedure SetEdges(const Value: TEasyEdges);
  public
    constructor Create(const Name: string; Builtin: boolean);
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Assign(AStyle: TEasyStyle);
    function Match(const AFontName: string; AFontSize: integer;
      AFontStyle: TFontStyles; ATextColor, ABackColor: TColor;
      AHAlign: TEasyHorzAlign; AVAlign: TEasyVertAlign;
      ALineDistance: integer; AWordWrap: boolean;
      AItemType: TEasyStyleType;
      AEdges: TEasyEdges): boolean;
    procedure Read(AFont: TFont);
    procedure Write(AFont: TFont);
    function IsNormal: boolean;
    function IsReadonly: boolean;
    function IsTitle: boolean;
    property Name: string read FName write SetName;
    property FontName: string read FFontName write SetFontName;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property Bold: boolean index fsBold read GetStyle write SetStyle;
    property Italic: boolean index fsItalic read GetStyle write SetStyle;
    property Underline: boolean index fsUnderline read GetStyle write SetStyle;
    property StrikeOut: boolean index fsStrikeOut read GetStyle write SetStyle;
    property FontStyleText: string read GetFontStyleText;
    property TextColor: TColor read FTextColor write SetFontColor;
    property Background: TColor read FBackground write SetBackColor;
    property HAlign: TEasyHorzAlign read FHAlign write SetHAlign;
    property VAlign: TEasyVertAlign read FVAlign write SetVAlign;
    property Builtin: boolean read FBuiltin;
    property LineDistance: integer read FLineDistance write SetLineDistance;
    property WordWrap: boolean read FWordWrap write SetWordWrap;
    property ItemType: TEasyStyleType read FItemType write SetItemType;
    property Modified: boolean read FModified write SetModified;
    property Edges: TEasyEdges read FEdges write SetEdges;
  end;

  TEasyStyles = class(TLseObject)
  private
    FForm: TEasyForm;
    FList: TList;
    function GetCount: integer;
    function GetItem(index: integer): TEasyStyle;
    function NewStyleName: string;
  public
    constructor Create(AForm: TEasyForm);
    destructor Destroy;override;
    procedure Delete(index: integer);
    procedure ClearUserStyles;
    procedure Clear;
    procedure Remove(AStyle: TEasyStyle);
    procedure RemoveBy(const Name: string);
    procedure Change(Sender: TObject);
    function Add(AStyle: TEasyStyle): TEasyStyle;
    function IndexOf(const Name: string): integer;
    function Find(const Name: string): TEasyStyle;
    function Force(const AFontName: string; AFontSize: integer;
      AFontStyle: TFontStyles; ATextColor, ABackground: TColor;
      AHAlign: TEasyHorzAlign; AVAlign: TEasyVertAlign;
      ALineDistance: integer; AWordWrap: boolean;
      AItemType: TEasyStyleType;
      AEdges: TEasyEdges): TEasyStyle;
    function Exists(const Name: string): boolean;
    procedure ListTo(const List: TStrings; Despite: TEasyStyle);
    property Form: TEasyForm read FForm;
    property Count: integer read GetCount;
    property Items[index: integer]: TEasyStyle read GetItem;default;
  end;
  
const
  VerMagic: integer   = $07D935A4;
  DefHAlign            =  ehaCenter;
  DefVAlign            =  evaCenter;
  DefStyleName        = 'ss_normal';
  DefFontItemName     = 'default';
  DefFontName          =  '宋体';
  DefFontSize          =  9;
  DefFontStyles        = [];
  DefEdges            = [];
  BoxEdges            = [eeLeft, eeTop, eeRight, eeBottom];
  DefStyle            = [];
  DefLineDistance      = 0;
  DefWidth            = 80;
  DefHeight            = 20;
  DefBackground       = clWhite;
  DefTextColor        = clBlack;
  DefRuleColor        = clSkyBlue;
  DefHotColor         = clOlive;
  DefEdgeColor        = clBlack;
  DefLineColor        = clSilver;

  RuleBorder: array[boolean] of TColor = (clWindow, clBtnShadow);

  HorzAlignStr: array[TEasyHorzAlign] of string = ('left', 'center', 'right');
  VertAlignStr: array[TEasyVertAlign] of string = ('top', 'center', 'bottom');

  UpperColors: array[0..246] of TColor = (
    $00000000, $00000040, $00000080, $000000FF, $0000130F, $00001918,
    $00002040, $000020C0, $00002520, $00002DAE, $00004000, $00004040,
    $00004060, $000040A0, $000040C0, $000040E0, $00006000, $00006040,
    $00006060, $000060A0, $000060C0, $000060E0, $00006E4F, $00007B00,
    $00008000, $00008080, $0000825E, $00008484, $00009AC0, $0000A000,
    $0000A040, $0000A060, $0000A0A0, $0000A0C0, $0000A0E0, $0000AF70,
    $0000C000, $0000C040, $0000C060, $0000C0A0, $0000C0C0, $0000C0E0,
    $0000CBFA, $0000CCFE, $0000D3FF, $0000E040, $0000E060, $0000E0A0,
    $0000E0C0, $0000FF00, $0000FF80, $0000FFFF, $000186A4, $0001A2CA,
    $00020406, $00028DAC, $00030B1C, $00036478, $0003768C, $00037F98,
    $0003E292, $0004DC8E, $00053A92, $0007F29E, $00086AFF, $000B0A0A,
    $000CCFFF, $000D4BB8, $000E9CBC, $000FB6E0, $00116FFE, $001468EF,
    $0014A2C2, $00152C5E, $0019D1FF, $001A75FE, $001B62D8, $001EC9F4,
    $00227CFE, $00228EA4, $0027D3FE, $002C82FE, $002D70D2, $003589FE,
    $00390906, $003A89EC, $003E90FE, $00400000, $00400040, $00400060,
    $004000A0, $004000C0, $004000E0, $00402020, $00404000, $00404040,
    $00404060, $004040A0, $004040C0, $004040E0, $00406000, $00406040,
    $00406060, $004060A0, $004060C0, $004060E0, $00408000, $00408080,
    $0040A000, $0040A040, $0040A060, $0040A0A0, $0040A0C0, $0040A0E0,
    $0040C000, $0040C040, $0040C060, $0040C0A0, $0040C0C0, $0040C0E0,
    $0040E000, $0040E040, $0040E060, $0040E0A0, $0040E0C0, $0040E0E0,
    $004796FE, $004F9DFE, $005084FF, $0058A3FE, $0062AAFE, $00686868,
    $0070120C, $00783220, $007B0000, $00800000, $00800040, $00800060,
    $00800080, $008000A0, $008000C0, $008000E0, $008000FF, $00804000,
    $00804040, $00804060, $008040A0, $008040C0, $008040E0, $00806000,
    $00806040, $00806060, $008060A0, $008060C0, $008060E0, $00808000,
    $00808040, $00808080, $008080E0, $008080FF, $0080A000, $0080A040,
    $0080A060, $0080A0A0, $0080A0C0, $0080A0E0, $0080C000, $0080C040,
    $0080C060, $0080C0A0, $0080C0C0, $0080C0E0, $0080E000, $0080E040,
    $0080E060, $0080E0A0, $0080E0C0, $0080E0E0, $00840000, $00848484,
    $00A21B14, $00A4A0A0, $00A6A6A6, $00A73F26, $00C00000, $00C00040,
    $00C00060, $00C000A0, $00C000C0, $00C000E0, $00C04000, $00C04040,
    $00C04060, $00C040A0, $00C040C0, $00C040E0, $00C06000, $00C06040,
    $00C06060, $00C060A0, $00C060C0, $00C060E0, $00C0A000, $00C0A040,
    $00C0A060, $00C0A0A0, $00C0A0C0, $00C0A0E0, $00C0C000, $00C0C040,
    $00C0C060, $00C0C0A0, $00C0C0C0, $00C0DCC0, $00C6C6C6, $00C6CED6,
    $00CACACA, $00CC6B4F, $00CD291E, $00D83F2E, $00DF7C64, $00E14E3A,
    $00EA9E8C, $00EB624A, $00EC775A, $00EC8678, $00EE694F, $00F0CAA6,
    $00F0FBFF, $00F1927E, $00F37054, $00F8F8F8, $00F97858, $00FCFCFC,
    $00FD805E, $00FF0000, $00FF00FF, $00FF6D42, $00FF744E, $00FF8000,
    $00FF8766, $00FF8C6C, $00FF9072, $00FF9D82, $00FFA38A, $00FFFF00,
    $00FFFFFF
  );

var
  ss_normal: TEasyStyle;

function EdgeToInt(Value: TEasyEdges): integer;
function IntToEdge(Value: integer): TEasyEdges;

function FsToInt(Value: TFontStyles): integer;
function IntToFs(Value: integer): TFontStyles;

function addInt(List: TList; Value: integer): integer;

{ rect }

function MakeRect(const Left, Top, Right, Bottom: integer): TRect;
function ZoomRect(const R: TRect; const H, V: integer): TRect;
function SameRect(const R1, R2: TRect): boolean;

{ range }

function MakeRange(const Left, Top, Right, Bottom: integer): TRect;overload;
function MakeRange(const P1, P2: TPoint): TRect;overload;
function MakeRange(const R: TRect; ColCount, RowCount: integer): TRect;overload;
function MakeRange(const R1, R2: TRect): TRect;overload;
function IsValidRange(const R: TRect): boolean;
function XyInRange(const R: TRect; X, Y: integer): boolean;
function PtInRange(const R: TRect; P: TPoint): boolean;

procedure WrapLineTo(const Line: string; List: TStrings; MaxCol: integer);
procedure WrapStringsTo(Source, List: TStrings; MaxCol: integer);

{resize font when text is larger than the rect}
procedure Rectout(
  Lines: TStrings;        {lines of text to display}
  Canvas: TCanvas;        {drawing device context}
  Rect: TRect;            {drawing area}
  HA: TEasyHorzAlign;     {horzontal text alignment}
  VA: TEasyVertAlign;     {vertiacl text alignment}
  LineDistance: integer;  {line distance}
  WordWrap: boolean
);

procedure DrawBox(Canvas: TCanvas; R: TRect; Edges: TEasyEdges;
  PointWidth, PointHeight: integer);

function AddEasyForm(AForm: TEasyForm): integer;
function RemoveEasyForm(AForm: TEasyForm): boolean;

function IndexToStr(Index: integer; const List: array of string): string;
function StrToIndex(const S: string; const List: array of string;
  DefValue: integer): integer;

function HAlignToStr(Align: TEasyHorzAlign): string;
function StrToHAlign(const Align: string): TEasyHorzAlign;

function VAlignToStr(Align: TEasyVertAlign): string;
function StrToVAlign(const Align: string): TEasyVertAlign;

function FindEasyItem(Items: TList; Col, Row: integer): TEasyItem;

function stod(const S: string): extended;
function stoi(const S: string): int64;
function stob(const S: string): boolean;
function dtos(const value: double): string;

function EncodeColumnID(const Col: integer): string;
function DecodeColumnID(const ID: string): integer;
function EncodeRangeID(const Row, Col: integer): string;overload;
function EncodeRangeID(const Row, Col, EndRow, EndCol: integer): string;overload;
function DecodeRangeID(const ID: string; var P: TPoint): boolean;overload;
function DecodeRangeID(const ID: string; var R: TRect): boolean;overload;
function ExtractFirstRangeID(const ID: string): string;

function DecodeCommaText(const CommaText: string): string;
function SelectIV(Flag: boolean; TrueValue, FalseValue: integer): integer;
function SwapMinMax(var V1, V2: integer): integer;

procedure CheckRow(ARow, RowRange: integer);
procedure CheckColumn(ACol, ColRange: integer);

function WideCase(Ch: char): WideChar;

type
  TEfxObject = (efxView, efxBook, efxPage, efxCell);

  TEfxBook = class;
  TEfxPage = class;
  TEfxCell = class;

  TEfxView = class(TLseObject)
  private
    FView: TEasyView;
    FBook: TEfxBook;
    function GetPageCount: integer;
    function GetPage(Index: integer): TEfxPage;
  public
    constructor Create(ABook: TEfxBook);
    destructor Destroy;override;
    property PageCount: integer read GetPageCount;
    property Page[Index: integer]: TEfxPage read GetPage;
    property Book: TEfxBook read FBook;
  end;

  TEfxErrorAt = procedure(Sender: TObject;
    Page: TEfxPage; const Cell, Msg: string) of object;

  TEfxDestroyingPage = procedure(Sender: TObject; Page: TEfxPage) of object;

  TEfxBook = class(TLseObject)
  private
    FForm: TEasyForm;
    FView: TEfxView;
    FList: TList;
    FOnErrorAt: TEfxErrorAt;
    FOnDestroyingPage: TEfxDestroyingPage;
    function GetCount: integer;
    function GetPage(Index: integer): TEfxPage;
    function GetView: TEfxView;
    function MakeUp(AForm: TEasyForm): TEfxPage;
    procedure SetModified(Value: boolean);
    function GetModified: boolean;
  protected
    procedure OnDestroying(Sender: TObject);
    procedure ErrorAt(Page: TEfxPage; const Cell, Msg: string);
    procedure DestroyingPage(Page: TEfxPage);
  public
    constructor Create(AForm: TEasyForm);
    destructor Destroy;override;
    procedure LoadFromFile(const fname: string);
    procedure SaveToFile(const fname: string);
    function AddPage: TEfxPage;
    procedure Clear;
    procedure Delete(Index: integer);
    property View: TEfxView read GetView;
    property PageCount: integer read GetCount;
    property Page[Index: integer]: TEfxPage read GetPage;
    property Modified: boolean read GetModified write SetModified;
    property OnErrorAt: TEfxErrorAt read FOnErrorAt write FOnErrorAt;
    property OnDestroyingPage: TEfxDestroyingPage read FOnDestroyingPage write FOnDestroyingPage;
  end;

  TEfxPage = class(TLseObject) {<--Row/Col starts from 1}
  private
    FBook: TEfxBook;
    FPage: TEasyForm;
    FList: TList;
    function GetView: TEfxView;
    procedure SetRowCount(const Value: integer);
    function GetRowCount: integer;
    procedure SetColCount(const Value: integer);
    function GetColCount: integer;
    procedure SetFixedColCount(const Value: integer);
    function GetFixedColCount: integer;
    procedure SetFixedRowCount(const Value: integer);
    function GetFixedRowCount: integer;
    procedure SetRowHeight(Index: Integer; const Value: integer);
    function GetRowHeight(Index: Integer): integer;
    procedure SetColWidth(Index: Integer; const Value: integer);
    function GetColWidth(Index: Integer): integer;
    procedure SetName(const Value: string);
    function GetName: string;
    procedure OnDestroying(Sender: TObject);
    procedure OnDelete(AItem: TObject);
    function MakeUp(AItem: TEasyItem): TEfxCell;
    procedure SetPageIndex(Value: integer);
    function GetPageIndex: integer;
    function Arrange(const Source, Patten, Spacer: string): string;
  public
    constructor Create(ABook: TEfxBook; AForm: TEasyForm);
    destructor Destroy;override;
    procedure ErrorAt(const Cell, Msg: string);
    procedure Clear;
    procedure Assign(AForm: TEfxPage);
    function Clone: TEfxPage;
    function GetCellByPos(const Pos: string; Force: boolean): TEfxCell;
    function ColumnIndex(const ID: string): integer;
    function Sum(const Range: string): double;
    function Average(const Range: string): double;
    function MaxValue(const Range: string): double;
    function MinValue(const Range: string): double;
    procedure InsertCol(Col: integer);
    procedure InsertRow(Row: integer);
    procedure DeleteCol(Col: integer);
    procedure DeleteRow(Row: integer);
    procedure AddCol;
    procedure AddRow;
    procedure Check(const Cell, Oper, Range, Msg, Patten, Spacer: string);
    procedure CheckHiLo(const Cell, Oper, Range, Msg, Patten: string; LoValue, HiValue: integer);
    property Name: string read GetName write SetName;
    property RowCount: integer read GetRowCount write SetRowCount;
    property ColCount: integer read GetColCount write SetColCount;
    property FixedRowCount: integer read GetFixedRowCount write SetFixedRowCount;
    property FixedColCount: integer read GetFixedColCount write SetFixedColCount;
    property RowHeight[Index: Integer]: integer read GetRowHeight write SetRowHeight;
    property ColWidth[Index: Integer]: integer read GetColWidth write SetColWidth;
    property Book: TEfxBook read FBook;
    property View: TEfxView read GetView;
    property PageIndex: integer read GetPageIndex write SetPageIndex;
    property EasyPage: TEasyForm read FPage;
  end;

  TEfxCell = class(TLseObject)
  private
    FPage: TEfxPage;
    FItem: TEasyItem;
    function GetBook: TEfxBook;
    function GetView: TEfxView;
    procedure SetText(const Text: string);
    function GetText: string;
    procedure SetRow(Value: integer);
    function GetRow: integer;
    procedure SetCol(Value: integer);
    function GetCol: integer;
    procedure SetRowSpan(Value: integer);
    function GetRowSpan: integer;
    procedure SetColSpan(Value: integer);
    function GetColSpan: integer;
  public
    constructor Create(AForm: TEfxPage; AItem: TEasyItem);
    destructor Destroy;override;
    property Page: TEfxPage read FPage;
    property Book: TEfxBook read GetBook;
    property View: TEfxView read GetView;
    property Text: string read GetText write SetText;
    property Row: integer read GetRow write SetRow;
    property Col: integer read GetCol write SetCol;
    property RowSpan: integer read GetRowSpan write SetRowSpan;
    property ColSpan: integer read GetColSpan write SetColSpan;
  end;

{ efx module }

procedure efx_BookCount(Param: pointer);cdecl;
procedure efx_Book(Param: pointer);cdecl;
procedure efx_CellID(Param: pointer);cdecl;
procedure efx_RangeID(Param: pointer);cdecl;

const
  efx_error = 'EfxError';

  efx_func_count = 4;
  efx_func_array: array[0..efx_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int get_bookCount()';
     fr_addr:@efx_BookCount;
     fr_desc:'return books available'
    ),
    (fr_prot:'efxbook book(int index)';
     fr_addr:@efx_Book;
     fr_desc:'return specified book'
    ),
    (fr_prot:'string cellID(int col, int row)';
     fr_addr:@efx_CellID;
     fr_desc:'encode to cell range ID'
    ),
    (fr_prot:'string rangeID(int col1, int row1, int col2, int row2)';
     fr_addr:@efx_RangeID;
     fr_desc:'encode to range ID'
    )
  );

{ efxview }

procedure efxview_Book(Param: pointer);cdecl;
procedure efxview_PageCount(Param: pointer);cdecl;
procedure efxview_Page(Param: pointer);cdecl;

const
  efxview_error = 'EfxViewError';

  efxview_func_count = 3;
  efxview_func_array: array[0..efxview_func_count - 1] of RLseFuncRec = (
    (fr_prot:'efxbook get_book()';
     fr_addr:@efxview_Book;
     fr_desc:'return its book'
    ),
    (fr_prot:'int get_pageCount()';
     fr_addr:@efxview_PageCount;
     fr_desc:'return pages available'
    ),
    (fr_prot:'efxpage page(int index)';
     fr_addr:@efxview_Page;
     fr_desc:'return specified page'
    )
  );

{ efxbook }

procedure efxbook_PageCount(Param: pointer);cdecl;
procedure efxbook_Page(Param: pointer);cdecl;
procedure efxbook_LoadFromFile(Param: pointer);cdecl;
procedure efxbook_SaveToFile(Param: pointer);cdecl;
procedure efxbook_Clear(Param: pointer);cdecl;
procedure efxbook_AddPage(Param: pointer);cdecl;
procedure efxbook_DeletePage(Param: pointer);cdecl;
procedure efxbook_View(Param: pointer);cdecl;
procedure efxbook_GetModified(Param: pointer);cdecl;
procedure efxbook_SetModified(Param: pointer);cdecl;

const
  efxbook_error = 'EfxBookError';

  efxbook_func_count = 10;
  efxbook_func_array: array[0..efxbook_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int get_pageCount()';
     fr_addr:@efxbook_PageCount;
     fr_desc:'return pages available'
    ),
    (fr_prot:'efxpage page(int index)';
     fr_addr:@efxbook_Page;
     fr_desc:'return specified page'
    ),
    (fr_prot:'void loadFromFile(string fileName)';
     fr_addr:@efxbook_LoadFromFile;
     fr_desc:'load from specified file'
    ),
    (fr_prot:'void saveToFile(string fileName)';
     fr_addr:@efxbook_SaveToFile;
     fr_desc:'save to specified file'
    ),
    (fr_prot:'void clear()';
     fr_addr:@efxbook_Clear;
     fr_desc:'clear current book'
    ),
    (fr_prot:'efxpage addPage()';
     fr_addr:@efxbook_AddPage;
     fr_desc:'append new page'
    ),
    (fr_prot:'void deletePage(int index)';
     fr_addr:@efxbook_DeletePage;
     fr_desc:'delete specified page'
    ),
    (fr_prot:'efxview get_view()';
     fr_addr:@efxbook_View;
     fr_desc:'return book''s viewer'
    ),
    (fr_prot:'bool get_modified()';
     fr_addr:@efxbook_getmodified;
     fr_desc:'get book modified'
    ),
    (fr_prot:'void set_modified(bool value)';
     fr_addr:@efxbook_setmodified;
     fr_desc:'set book modified'
    )
  );

{ efxpage }

procedure efxpage_Book(Param: pointer);cdecl;
procedure efxpage_View(Param: pointer);cdecl;
procedure efxpage_GetName(Param: pointer);cdecl;
procedure efxpage_SetName(Param: pointer);cdecl;
procedure efxpage_GetRowCount(Param: pointer);cdecl;
procedure efxpage_SetRowCount(Param: pointer);cdecl;
procedure efxpage_GetColCount(Param: pointer);cdecl;
procedure efxpage_SetColCount(Param: pointer);cdecl;
procedure efxpage_GetFixedRowCount(Param: pointer);cdecl;
procedure efxpage_SetFixedRowCount(Param: pointer);cdecl;
procedure efxpage_GetFixedColCount(Param: pointer);cdecl;
procedure efxpage_SetFixedColCount(Param: pointer);cdecl;
procedure efxpage_Clear(Param: pointer);cdecl;
procedure efxpage_Clone(Param: pointer);cdecl;
procedure efxpage_Assign(Param: pointer);cdecl;
procedure efxpage_Sum(Param: pointer);cdecl;
procedure efxpage_Average(Param: pointer);cdecl;
procedure efxpage_Max(Param: pointer);cdecl;
procedure efxpage_Min(Param: pointer);cdecl;
procedure efxpage_InsertRow(Param: pointer);cdecl;
procedure efxpage_InsertCol(Param: pointer);cdecl;
procedure efxpage_DeleteRow(Param: pointer);cdecl;
procedure efxpage_DeleteCol(Param: pointer);cdecl;
procedure efxpage_AddRow(Param: pointer);cdecl;
procedure efxpage_AddCol(Param: pointer);cdecl;
procedure efxpage_GetRowHeight(Param: pointer);cdecl;
procedure efxpage_SetRowHeight(Param: pointer);cdecl;
procedure efxpage_GetColWidth(Param: pointer);cdecl;
procedure efxpage_SetColWidth(Param: pointer);cdecl;
procedure efxpage_ColToIndex(Param: pointer);cdecl;
procedure efxpage_Cell(Param: pointer);cdecl;
procedure efxpage_ErrorAt(Param: pointer);cdecl;
procedure efxpage_Check(Param: pointer);cdecl;
procedure efxpage_CheckHL(Param: pointer);cdecl;

const
  efxpage_error = 'EfxPageError';

  efxpage_func_count = 34;
  efxpage_func_array: array[0..efxpage_func_count - 1] of RLseFuncRec = (
    (fr_prot:'efxbook get_book()';
     fr_addr:@efxpage_Book;
     fr_desc:'return owner book'
    ),
    (fr_prot:'efxview get_view()';
     fr_addr:@efxpage_View;
     fr_desc:'return page''s viewer'
    ),
    (fr_prot:'string get_name()';
     fr_addr:@efxpage_GetName;
     fr_desc:'get page''s name'
    ),
    (fr_prot:'string set_name()';
     fr_addr:@efxpage_SetName;
     fr_desc:'set page''s name'
    ),
    (fr_prot:'int get_rowCount()';
     fr_addr:@efxpage_GetRowCount;
     fr_desc:'return page''s row count'
    ),
    (fr_prot:'int set_rowCount()';
     fr_addr:@efxpage_SetRowCount;
     fr_desc:'set page''s row count'
    ),
    (fr_prot:'int get_fixedRowCount()';
     fr_addr:@efxpage_GetfixedRowCount;
     fr_desc:'return page''s fixed row count'
    ),
    (fr_prot:'int set_fixedRowCount()';
     fr_addr:@efxpage_SetfixedRowCount;
     fr_desc:'set page''s fixed row count'
    ),
    (fr_prot:'int get_colCount()';
     fr_addr:@efxpage_GetcolCount;
     fr_desc:'return page''s col count'
    ),
    (fr_prot:'int set_colCount()';
     fr_addr:@efxpage_SetcolCount;
     fr_desc:'set page''s col count'
    ),
    (fr_prot:'int get_fixedColCount()';
     fr_addr:@efxpage_GetfixedcolCount;
     fr_desc:'return page''s fixed col count'
    ),
    (fr_prot:'int set_fixedColCount()';
     fr_addr:@efxpage_SetfixedcolCount;
     fr_desc:'set page''s fixed col count'
    ),
    (fr_prot:'void clear()';
     fr_addr:@efxpage_Clear;
     fr_desc:'clear current page'
    ),
    (fr_prot:'efxpage clone()';
     fr_addr:@efxpage_Clone;
     fr_desc:'cloner current page'
    ),
    (fr_prot:'void assign(efxpage source)';
     fr_addr:@efxpage_Assign;
     fr_desc:'assign data from specified page'
    ),
    (fr_prot:'float sum(string range)';
     fr_addr:@efxpage_Sum;
     fr_desc:'get sum of ranged cells'
    ),
    (fr_prot:'float average(string range)';
     fr_addr:@efxpage_Average;
     fr_desc:'get average of ranged cells'
    ),
    (fr_prot:'float max(string range)';
     fr_addr:@efxpage_Max;
     fr_desc:'get max value of ranged cells'
    ),
    (fr_prot:'float min(string range)';
     fr_addr:@efxpage_Min;
     fr_desc:'get min value of ranged cells'
    ),
    (fr_prot:'void insertRow(int index)';
     fr_addr:@efxpage_InsertRow;
     fr_desc:'insert row at specified position'
    ),
    (fr_prot:'void insertCol(int index)';
     fr_addr:@efxpage_InsertCol;
     fr_desc:'insert column at specified position'
    ),
    (fr_prot:'void deleteRow(int index)';
     fr_addr:@efxpage_DeleteRow;
     fr_desc:'delete specified row'
    ),
    (fr_prot:'void deleteCol(int index)';
     fr_addr:@efxpage_DeleteCol;
     fr_desc:'delete specified column'
    ),
    (fr_prot:'void addRow()';
     fr_addr:@efxpage_AddRow;
     fr_desc:'Add row'
    ),
    (fr_prot:'void addCol()';
     fr_addr:@efxpage_AddCol;
     fr_desc:'add column'
    ),
    (fr_prot:'int getRowHeight(int row)';
     fr_addr:@efxpage_GetRowHeight;
     fr_desc:'return specified row height'
    ),
    (fr_prot:'void setRowHeight(int row, int height)';
     fr_addr:@efxpage_SetRowHeight;
     fr_desc:'set specified row height'
    ),
    (fr_prot:'int getColWidth(int col)';
     fr_addr:@efxpage_GetColWidth;
     fr_desc:'return specified column width'
    ),
    (fr_prot:'void setColWidth(int col, int width)';
     fr_addr:@efxpage_SetColWidth;
     fr_desc:'set specified column width'
    ),
    (fr_prot:'int columnIndex(string columnID)';
     fr_addr:@efxpage_ColToIndex;
     fr_desc:'translate colument ID into column index'
    ),
    (fr_prot:'efxcell cell(string crpos, bool force)';
     fr_addr:@efxpage_Cell;
     fr_desc:'get specified cell'
    ),
    (fr_prot:'void errorAt(string cell, string msg)';
     fr_addr:@efxpage_ErrorAt;
     fr_desc:'notify data check error'
    ),
    (fr_prot:'void check(string cell, string oper, string range, string msg, string patten, string spacer)';
     fr_addr:@efxpage_Check;
     fr_desc:'check relationships of specified ranges'
    ),
    (fr_prot:'void chkhl(string cell, string oper, string range, string msg, string patten, int loValue, int hiValue)';
     fr_addr:@efxpage_CheckHL;
     fr_desc:'check relationships of specified ranges'
    )
  );

{ efxcell }

procedure efxcell_View(Param: pointer);cdecl;
procedure efxcell_Book(Param: pointer);cdecl;
procedure efxcell_Page(Param: pointer);cdecl;
procedure efxcell_SetText(Param: pointer);cdecl;
procedure efxcell_GetText(Param: pointer);cdecl;
procedure efxcell_SetRow(Param: pointer);cdecl;
procedure efxcell_GetRow(Param: pointer);cdecl;
procedure efxcell_SetCol(Param: pointer);cdecl;
procedure efxcell_GetCol(Param: pointer);cdecl;
procedure efxcell_SetRowSpan(Param: pointer);cdecl;
procedure efxcell_GetRowSpan(Param: pointer);cdecl;
procedure efxcell_SetColSpan(Param: pointer);cdecl;
procedure efxcell_GetColSpan(Param: pointer);cdecl;

const
  efxcell_error = 'EfxCellError';

  efxcell_func_count = 13;
  efxcell_func_array: array[0..efxcell_func_count - 1] of RLseFuncRec = (
    (fr_prot:'efxview get_view()';
     fr_addr:@efxcell_View;
     fr_desc:'return owner view'
    ),
    (fr_prot:'efxbook get_book()';
     fr_addr:@efxcell_Book;
     fr_desc:'return owner book'
    ),
    (fr_prot:'efxpage get_page()';
     fr_addr:@efxcell_Page;
     fr_desc:'return owner page'
    ),
    (fr_prot:'string get_text()';
     fr_addr:@efxcell_GetText;
     fr_desc:'get or set cell text'
    ),
    (fr_prot:'void set_text(string value)';
     fr_addr:@efxcell_SetText;
     fr_desc:'get or set cell text'
    ),
    (fr_prot:'int get_row()';
     fr_addr:@efxcell_getrow;
     fr_desc:'get row'
    ),
    (fr_prot:'void set_row(int value)';
     fr_addr:@efxcell_setrow;
     fr_desc:'set row'
    ),
    (fr_prot:'int get_col()';
     fr_addr:@efxcell_getcol;
     fr_desc:'get col'
    ),
    (fr_prot:'void set_col(int value)';
     fr_addr:@efxcell_setcol;
     fr_desc:'set col'
    ),
    (fr_prot:'int get_rowSpan()';
     fr_addr:@efxcell_getrowSpan;
     fr_desc:'get row span'
    ),
    (fr_prot:'void set_rowSpan(int value)';
     fr_addr:@efxcell_setrowSpan;
     fr_desc:'set row span'
    ),
    (fr_prot:'int get_colSpan()';
     fr_addr:@efxcell_getcolSpan;
     fr_desc:'get col span'
    ),
    (fr_prot:'void set_colSpan(int value)';
     fr_addr:@efxcell_setcolSpan;
     fr_desc:'set col span'
    )
  );

function SetupEfxModule(const fname: string): boolean;
function EfxClass(Index: TEfxObject): pointer;

implementation

uses
  Math, Clipbrd;

var
  EasyFormList: TList;

function AddEasyForm(AForm: TEasyForm): integer;
begin
  Result := EasyFormList.Add(AForm);
end;

function RemoveEasyForm(AForm: TEasyForm): boolean;
var
  X: integer;
begin
  X := EasyFormList.IndexOf(AForm);
  Result := (X >= 0);
  if Result then
    EasyFormList.Delete(X);
end;

function IndexToStr(Index: integer; const List: array of string): string;
begin
  Result := List[Index];
end;

function StrToIndex(const S: string;
  const List: array of string; DefValue: integer): integer;
var
  A: integer;
begin
  for A := 0 to Length(List) - 1 do
    if CompareText(List[A], S) = 0 then
    begin
      Result := A; Exit;
    end;
  Result := DefValue;
end;

function HAlignToStr(Align: TEasyHorzAlign): string;
begin
  Result := HorzAlignStr[Align];
end;

function StrToHAlign(const Align: string): TEasyHorzAlign;
begin
  Result := TEasyHorzAlign(StrToIndex(Align, HorzAlignStr, integer(DefHAlign)));
end;

function VAlignToStr(Align: TEasyVertAlign): string;
begin
  Result := VertAlignStr[Align];
end;

function StrToVAlign(const Align: string): TEasyVertAlign;
begin
  Result := TEasyVertAlign(StrToIndex(Align, VertAlignStr, integer(DefVAlign)));
end;

function FindEasyItem(Items: TList; Col, Row: integer): TEasyItem;
var
  X: integer;
begin
  if Items <> nil then
    for X := 0 to Items.Count - 1 do
    begin
      Result := TEasyItem(Items[X]);
      if Result.Contains(Col, Row) then Exit;
    end;
  Result := nil;
end;

function EdgeToInt(Value: TEasyEdges): integer;
var
  E: TEasyEdge;
begin
  Result := 0;
  for E := Low(TEasyEdge) to High(TEasyEdge) do
    if E in Value then
      Result := Result + (1 shl integer(E));
end;

function IntToEdge(Value: integer): TEasyEdges;
var
  E: TEasyEdge;
begin
  Result := [];
  for E := Low(TEasyEdge) to High(TEasyEdge) do
    if ((Value shr integer(E)) and 1) = 1 then
      Result := Result + [E];
end;

function FsToInt(Value: TFontStyles): integer;
var
  F: TFontStyle;
begin
  Result := 0;
  for F := Low(TFontStyle) to High(TFontStyle) do
    if F in Value then
      Result := Result + (1 shl integer(F));
end;

function IntToFs(Value: integer): TFontStyles;
var
  F: TFontStyle;
begin
  Result := [];
  for F := Low(TFontStyle) to High(TFontStyle) do
    if ((Value shr integer(F)) and 1) = 1 then
      Result := Result + [F];
end;

function addInt(List: TList; Value:integer): integer;
begin
  Result := List.Add(pointer(Value));
end;

function MakeRect(const Left, Top, Right, Bottom: integer): TRect;
begin
  Result.Left   := Left;
  Result.Top    := Top;
  Result.Right  := Right;
  Result.Bottom := Bottom;
end;

function ZoomRect(const R: TRect; const H, V: integer): TRect;
begin
  Result.Left   := R.Left   - H;
  Result.Top    := R.Top    - V;
  Result.Right  := R.Right  + H;
  Result.Bottom := R.Bottom + V;
end;

function SameRect(const R1, R2: TRect): boolean;
begin
  Result := (R1.Left = R2.Left) and
            (R1.Top = R2.Top) and
            (R1.Right = R2.Right) and
            (R1.Bottom = R2.Bottom);
end;

function MakeRange(const Left, Top, Right, Bottom: integer): TRect;overload;
begin
  Result.Left   := Max(Min(Left, Right), 0);
  Result.Top    := Max(Min(Top, Bottom), 0);
  Result.Right  := Max(Left, Right);
  Result.Bottom := Max(Top, Bottom);
end;

function MakeRange(const P1, P2: TPoint): TRect;
begin
  Result.Left   := Max(Min(P1.X, P2.X), 0);
  Result.Top    := Max(Min(P1.Y, P2.Y), 0);
  Result.Right  := Max(P1.X, P2.X);
  Result.Bottom := Max(P1.Y, P2.Y);
end;

function MakeRange(const R: TRect; ColCount, RowCount: integer): TRect;
begin
  Result.Left   := Max(Min(R.Left, R.Right), 0);
  Result.Top    := Max(Min(R.Top, R.Bottom), 0);
  Result.Right  := Min(Max(R.Left, R.Right), ColCount - 1);
  Result.Bottom := Min(Max(R.Top, R.Bottom), RowCount - 1);
end;

function MakeRange(const R1, R2: TRect): TRect;
begin
  Result.Left   := Max(Min(Min(R1.Left, R1.Right), Min(R2.Left, R2.Right)), 0);
  Result.Top    := Max(Min(Min(R1.Top, R1.Bottom), Min(R2.Top, R2.Bottom)), 0);
  Result.Right  := Max(Max(R1.Left, R1.Right), Max(R2.Left, R2.Right));
  Result.Bottom := Max(Max(R1.Top, R1.Bottom), Max(R2.Top, R2.Bottom));
end;

function IsValidRange(const R: TRect): boolean;
begin
  Result := (R.Left >= 0) and
            (R.Right >= R.Left) and
            (R.Top >= 0) and
            (R.Bottom >= R.Top);
end;

function XyInRange(const R: TRect; X, Y: integer): boolean;
begin
  Result := (X >= R.Left) and (X <= R.Right) and
            (Y >= R.Top) and (Y <= R.Bottom);
end;

function PtInRange(const R: TRect; P: TPoint): boolean;
begin
  Result := XyInRange(R, P.X, P.Y);
end;

procedure WrapLineTo(const Line: string; List: TStrings; MaxCol: integer);
var
  next, base: PChar;
  slen: integer;
  stmp: string;
begin
  if MaxCol < 2 then MaxCol := 2;

  if Length(Line) <= MaxCol then
  begin
    List.Add(Line);
    Exit;
  end;

  next := pchar(Line);
  while next^ <> #0 do
  begin
    base := next;
    slen := 0;
    while (next^ <> #0) and (slen < MaxCol) do
    begin
      if not (next^ in LeadBytes) then
      begin
        Inc(next);
        Inc(slen);
      end
      else
      if slen < MaxCol - 1 then
      begin
        Inc(next);
        Inc(slen);
        if next^ in LeadBytes then
        begin
          Inc(next);
          Inc(slen);
        end;
      end
      else Break;
    end;
    SetString(stmp, base, slen);
    List.Add(stmp);
  end;
end;

procedure WrapStringsTo(Source, List: TStrings; MaxCol: integer);
var
  index: integer;
begin
  if MaxCol < 2 then MaxCol := 2;
  for index := 0 to Source.Count - 1 do
    WrapLineTo(Source[index], List, MaxCol);
end;

procedure Rectout(Lines: TStrings;
                  Canvas: TCanvas;
                  Rect: TRect;
                  HA: TEasyHorzAlign;
                  VA: TEasyVertAlign;
                  LineDistance: integer;
                  WordWrap: boolean);
var
  LD: integer;  // lines distances
  LW: integer;  // lines width & height
  LH: integer;
  RW: integer;   // rect widht & height
  RH: integer;
  FS: integer;  // original font size
  TH: integer;  // text height
  LS: TStrings; // lines
  fsize: integer;

  function TextInRect: boolean;
  var
    A, W: integer;
  begin
    LW := 0;
    for A := 0 to LS.Count - 1 do
    begin
      W := Canvas.TextWidth(LS[A]);
      if W > LW then LW := W;
      LS.Objects[A] := TObject(W);
    end;
    TH := Canvas.TextHeight('H');
    if LW > 0 then
      LH := TH * LS.Count + LD else
      LH := 0;
    Result := (LW <= RW) and (LH <= RH);
  end;

  function WrapInRect: boolean;
  begin
    if WordWrap and (LW > RW) then
    begin
      LS.Clear;
      WrapStringsTo(lines, LS, Max(2, RW div Canvas.TextWidth('W')));
      Result := TextInRect;
    end
    else Result := false;
  end;
  
  function CharDistance(const Text: string; Len: integer): integer;
  var
    A: integer;
  begin
    Result := 0;
    A := 1;
    while A <= Length(Text) do
    begin
      if Text[A] > #128 then Inc(A);
      Inc(A);
      Inc(Result);
    end;
    if Result > 1 then
      Result := (LW - Len) div (Result - 1) else
      Result := 0;
  end;
  procedure SplitDraw(const Text: string; D, X, Y: integer);
  var
    A: integer;
    S: string;
  begin
    A := 1;
    while A <= Length(Text) do
    begin
      if Text[A] > #128 then
      begin
        S := Copy(Text, A, 2);
        Inc(A, 2);
        end else begin
        S := Text[A];
        Inc(A);
      end;
      Canvas.TextOut(X, Y, S);
      Inc(X, Canvas.TextWidth(S) + D);
    end;
  end;
  procedure Draw;
  var
    A, W, X, Y: integer;
  begin
    with Rect do if VA = evaTop then
      Y := Top + 1 else
    if VA = evaCenter then
      Y := Top + (Bottom - Top - LH) div 2 else
      Y := Bottom - LH - 1;
    for A := 0 to LS.Count - 1 do
    begin
      if Y > Rect.Top then
        Rect.Top := Y;
      W := integer(LS.Objects[A]);
      with Rect do if HA = ehaLeft then
        X := Left + 1 else
      if HA = ehaCenter then
        X := Left + (Right - Left - W) div 2 else
        X := Right - W - 1;
      Canvas.TextRect(Rect, X, Y, LS[A]);
      Inc(Y, TH + LineDistance);
    end;
  end;
begin
  RW := Rect.Right - Rect.Left;
  RH := Rect.Bottom - Rect.Top;
  LW := 0;
  LH := 0;
  LD := (Lines.Count - 1) * LineDistance;
  FS := Canvas.Font.Size;
  try
    if WordWrap then
      LS := TStringList.Create else
      LS := lines;
    try
      if WordWrap then
        LS.Assign(lines);
      if not TextInRect and not WrapInRect then
        for fsize := FS - 1 downto 1 do
        begin
          if WordWrap then
            LS.Assign(lines);
           Canvas.Font.Size := fsize;
          if TextInRect or WrapInRect then Break;
         end;
      if LH > 0 then Draw;
    finally
      if WordWrap then LS.Free;
    end;
  finally
    Canvas.Font.Size := FS;
  end;
end;

{
procedure Rectout(Lines: TStrings; Canvas: TCanvas;
  Color: TColor; Rect: TRect;
  HA: TEasyHorzAlign; VA: TEasyVertAlign;
  Style: TEasyItemStyles; LineDistance: integer);
var
  LD: integer;  // lines distances
  LW: integer;  // lines width & height
  LH: integer;
  RW: integer;   // rect widht & height
  RH: integer;
  FS: integer;  // original font size
  TH: integer;  // text height
  function TextInRect: boolean;
  var
    A, W: integer;
  begin
    LW := 0;
    for A := 0 to Lines.Count - 1 do
    begin
      W := Canvas.TextWidth(Lines[A]);
      if W > LW then LW := W;
      Lines.Objects[A] := TObject(W);
    end;
    TH := Canvas.TextHeight('H');
    if LW > 0 then
      LH := TH * Lines.Count + LD else
      LH := 0;
    Result := (LW <= RW) and (LH <= RH);
  end;
  function FontResize: integer;
  var
    fs1, fs2: integer;
  begin
    Result := Canvas.Font.Size;
    if TextInRect or not (eisAutoFit in Style) then Exit;
    fs1 := 1;
    fs2 := Result;
    while fs1 < fs2 - 1 do
    begin
      Canvas.Font.Size := (fs1 + fs2) div 2;
      if TextInRect then
        fs1 := Canvas.Font.Size else
        fs2 := Canvas.Font.Size;
    end;
  end;
  function CharDistance(const Text: string; Len: integer): integer;
  var
    A: integer;
  begin
    Result := 0;
    A := 1;
    while A <= Length(Text) do
    begin
      if Text[A] > #128 then Inc(A);
      Inc(A);
      Inc(Result);
    end;
    if Result > 1 then
      Result := (LW - Len) div (Result - 1) else
      Result := 0;
  end;
  procedure SplitDraw(const Text: string; D, X, Y: integer);
  var
    A: integer;
    S: string;
  begin
    A := 1;
    while A <= Length(Text) do
    begin
      if Text[A] > #128 then
      begin
        S := Copy(Text, A, 2);
        Inc(A, 2);
        end else begin
        S := Text[A];
        Inc(A);
      end;
      Canvas.TextOut(X, Y, S);
      Inc(X, Canvas.TextWidth(S) + D);
    end;
  end;
  procedure Draw;
  var
    A, D, W, X, Y: integer;
  begin
    with Rect do if VA = evaTop then
      Y := Top + 1 else
    if VA = evaCenter then
      Y := Top + (Bottom - Top - LH) div 2 else
      Y := Bottom - LH - 1;
    if eisSpacing in Style then
    begin
      with Rect do if HA = ehaLeft then
        X := Left + 1 else
      if HA = ehaCenter then
        X := Left + (Right - Left - LW) div 2 else
        X := Right - LW - 1;
      for A := 0 to Lines.Count - 1 do
      begin
        if Y > Rect.Top then
          Rect.Top := Y;
        W := integer(Lines.Objects[A]);
        if W >= LW then
          Canvas.TextRect(Rect, X, Y, Lines[A]) else
        begin
          D := CharDistance(Lines[A], W);
          if D = 0 then
            Canvas.TextRect(Rect, X, Y, Lines[A]) else
          begin
            Canvas.FillRect(Rect);
            SplitDraw(Lines[A], D, X, Y);
          end;
        end;
        Inc(Y, TH + LineDistance);
      end;
    end
    else
    for A := 0 to Lines.Count - 1 do
    begin
      if Y > Rect.Top then
        Rect.Top := Y;
      W := integer(Lines.Objects[A]);
      with Rect do if HA = ehaLeft then
        X := Left + 1 else
      if HA = ehaCenter then
        X := Left + (Right - Left - W) div 2 else
        X := Right - W - 1;
      Canvas.TextRect(Rect, X, Y, Lines[A]);
      Inc(Y, TH + LineDistance);
    end;
  end;
begin
  RW := Rect.Right - Rect.Left;
  RH := Rect.Bottom - Rect.Top;
  LW := 0;
  LH := 0;
  LD := (Lines.Count - 1) * LineDistance;
  FS := FontResize;
  try
    if LH > 0 then Draw;
  finally
    Canvas.Font.Size := FS;
  end;
end;
}

function FitSize(Image, View: TPoint): TPoint;
var
  R1, R2: double;
begin
  Result.X := Image.X;
  Result.Y := Image.Y;
  if Image.X > View.X then
  begin
    R1 := Image.X / Image.Y;
    R2 := View.X / View.Y;
    if (Image.Y <= View.Y) or (R1 >= R2) then
    begin
      Result.X := View.X;
      Result.Y := (Result.X * Image.Y) div Image.X;
    end
    else
    begin
      Result.Y := View.Y;
      Result.X := (Result.Y * Image.X) div Image.Y;
    end;
  end
  else
  if Image.Y > View.Y then
  begin
    Result.Y := View.Y;
    Result.X := (Result.Y * Image.X) div Image.Y;
  end;
end;

procedure DrawBox(Canvas: TCanvas; R: TRect; Edges: TEasyEdges;
  PointWidth, PointHeight: integer);
var
  Z: integer;
  P: TPenStyle;
begin
  Z := Canvas.Pen.Width;
  P := Canvas.Pen.Style;

  if eeLeft in Edges then
  begin
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.Pen.Width := PointWidth;
    Canvas.LineTo(R.Left, R.Bottom - 1);
  end;

  if eeRight in Edges then
  begin
    Canvas.MoveTo(R.Right - PointWidth, R.Top);
    Canvas.Pen.Width := PointWidth;
    Canvas.LineTo(R.Right - PointWidth, R.Bottom - 1);
  end;

  if eeTop in Edges then
  begin
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.Pen.Width := PointHeight;
    Canvas.LineTo(R.Right - 1, R.Top);
  end;

  if eeBottom in Edges then
  begin
    Canvas.MoveTo(R.Left, R.Bottom - PointHeight);
    Canvas.Pen.Width := PointHeight;
    Canvas.LineTo(R.Right - 1, R.Bottom - PointHeight);
  end;
  Canvas.Pen.Width := Z;
  Canvas.Pen.Style := P;
end;

function stod(const S: string): extended;
begin
  if not TextToFloat(PChar(Trim(S)), Result, fvExtended) then
    Result := 0;
end;

function stoi(const S: string): int64;
begin
  Result := Trunc(stod(S));
end;

function stob(const S: string): boolean;
begin
  Result := not IsZero(stod(S));
end;

function dtos(const value: double): string;
begin
  if IsZero(value) then
    Result := '' else
    Result := FloatToStr(value);
end;

function EncodeColumnID(const Col: integer): string; // 1 ...
const
  K = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
begin
  if Col > 0 then
    Result := EncodeColumnID((Col - 1) div 26) + K[((Col - 1) mod 26) + 1] else
    Result := '';
end;

function DecodeColumnID(const ID: string): integer;
var
  X, base: integer;

  function ctox(ch: char): integer;
  begin
    if ch in ['a'..'z'] then
      Result := Ord(ch) - Ord('a') + 1 else
    if ch in ['A'..'Z'] then
      Result := Ord(ch) - Ord('A') + 1 else
      Result := 0;
  end;

begin
  Result := 0;
  base := 1;
  for X := Length(ID) downto 1 do
  begin
    Inc(Result, ctox(ID[X]) * base);
    base := base * 26;
  end;
end;

function EncodeRangeID(const Row, Col: integer): string;
begin
  Result := Format('%s%d', [EncodeColumnID(Col), Row]);
end;

function EncodeRangeID(const Row, Col, EndRow, EndCol: integer): string;
begin
  Result := EncodeRangeID(Row, Col);
  if (Row <> EndRow) or (Col <> EndCol) then
    Result := Result + ':' + EncodeRangeID(EndRow, EndCol);
end;

function DecodeRangeID(const ID: string; var P: TPoint): boolean;
var
  X: integer;
  S: string;
begin
  S := ExtractFirstRangeID(ID);    
  X := Pos(':', S);
  if X > 0 then
    S := Trim(Copy(S, 1, X - 1));
  if (S <> '') and (S[1] in AlphaChar) then
  begin
    X := 2;
    while (X <= Length(S)) and (S[X] in AlphaChar) do Inc(X);
    P.X := DecodeColumnID(Copy(S, 1, X - 1)) - 1;
    P.Y := stoi(Trim(Copy(S, X, MaxInt))) - 1;
    Result := (P.X >= 0) and (P.Y >= 0);
  end
  else Result := false;
end;

function DecodeRangeID(const ID: string; var R: TRect): boolean;
var
  X: integer;
  S: string;
begin
  S := ExtractFirstRangeID(ID);
  X := Pos(':', S);
  if X > 0 then
  begin
    Result := DecodeRangeID(Copy(S, 1, X - 1), R.Topleft);
    if Result then
      Result := DecodeRangeID(Copy(S, X + 1, MaxInt), R.Bottomright);
  end
  else
  begin
    Result := DecodeRangeID(S, R.Topleft);
    if Result then
      R.BottomRight := R.TopLeft;
  end;
end;

function ExtractFirstRangeID(const ID: string): string;
var
  X: integer;
begin
  X := Pos(',', ID);
  if X > 0 then
    Result := Trim(Copy(ID, 1, X - 1)) else
    Result := Trim(ID);
end;

function DecodeCommaText(const CommaText: string): string;
var
  list: TStrings;
  size: integer;
begin
  list := TStringList.Create;
  try
    list.CommaText := CommaText;
    Result := list.Text;
    size := Length(Result);
    if size > 0 then
    begin
      if Result[size] = #10 then Dec(size);
      if (size > 0) and (Result[size] = #13) then Dec(size);
      SetLength(Result, size);
    end;
  finally
    list.Free;
  end;
end;

function SelectIV(Flag: boolean; TrueValue, FalseValue: integer): integer;
begin
  if Flag then
    Result := TrueValue else
    Result := FalseValue;
end;

function SwapMinMax(var V1, V2: integer): integer;
begin
  if V1 > V2 then
  begin
    Result := V2;
    V2 := V1;
    V1 := Result;
  end
  else Result := V1;
end;

procedure CheckRow(ARow, RowRange: integer);
begin
  if (ARow < 0) or (ARow >= RowRange) then
    lse_error('invalid row index: %d', [ARow]);
end;

procedure CheckColumn(ACol, ColRange: integer);
begin
  if (ACol < 0) or (ACol >= ColRange) then
    lse_error('invalid column index: %d', [ACol]);
end;

function WideCase(Ch: char): WideChar;
var
  S: WideString;
begin
  S := string(Ch);
  if S <> '' then
    Result := S[1] else
    Result := #0;
end;

{ TEasyStream }

function TEasyStream.read_byte: byte;
begin
  read_easytag_type_check(egtInt8);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.read_easytag: TEasyTag;
var
  v: byte;
begin
  ReadBuffer(v, sizeof(v));
  while v > byte(High(TEasyTag)) do
  begin
    skip_value;
    ReadBuffer(v, sizeof(v));
  end;
  Result := TEasyTag(v);
end;

function TEasyStream.read_easytag_check(tag: TEasyTag): TEasyTag;
const
{  TT: array[TEasyTag] of string = (
    'egtInt8', 'egtInt16', 'egtInt32', 'egtInt64', 'egtString', 'egtStream'
  );}
  EF = '"%s" expected but "%s" found!';
begin
  Result := read_easytag;
  if Result <> tag then
    raise Exception.CreateFmt(EF, [IntToStr(integer(tag)),
      IntToStr(integer(Result))]);
end;

function TEasyStream.read_easytag_type: TEasyTagType;
var
  v: byte;
begin
  ReadBuffer(v, sizeof(v));
  if v > byte(High(TEasyTagType)) then
    raise Exception.CreateFmt('unknown easy tag type value %d!', [integer(v)]);
  Result := TEasyTagType(v);
end;

function TEasyStream.read_easytag_type_check(tagtype: TEasyTagType): TEasyTagType;
const
  TT: array[TEasyTagType] of string = (
    'egtInt8', 'egtInt16', 'egtInt32', 'egtInt64', 'egtString'
  );
  EF = '"%s" expected but "%s" found!';
begin
  Result := read_easytag_type;
  if Result <> TagType then
    raise Exception.CreateFmt(EF, [TT[TagType], TT[Result]]);
end;

function TEasyStream.read_hex(Count: integer): string;
var
  V: byte;
begin
  Count := Min(Max(0, Count), Size - Position);
  Result := '';
  while Count > 0 do
  begin
    Read(V, sizeof(byte));
    Result := Result + IntToHex(V, 2);
    Dec(Count);
  end;
end;

function TEasyStream.read_int64: int64;
begin
  read_easytag_type_check(egtInt64);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.read_longint: longint;
begin
  read_easytag_type_check(egtInt32);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.read_longword: longword;
begin
  read_easytag_type_check(egtInt32);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.read_shortint: shortint;
begin
  read_easytag_type_check(egtInt8);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.read_smallint: smallint;
begin
  read_easytag_type_check(egtInt16);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.read_string: string;
var
  L: longint;
begin
  read_easytag_type_check(egtString);
  ReadBuffer(L, sizeof(longint));
  if L > 0 then
  begin
    SetLength(Result, L);
    ReadBuffer(pointer(Result)^, L);
  end
  else Result := '';
end;

function TEasyStream.read_taged_byte(tag: TEasyTag): byte;
begin
  read_easytag_check(tag);
  Result := read_byte;
end;

function TEasyStream.read_taged_int64(tag: TEasyTag): int64;
begin
  read_easytag_check(tag);
  Result := read_int64;
end;

function TEasyStream.read_taged_longint(tag: TEasyTag): longint;
begin
  read_easytag_check(tag);
  Result := read_longint;
end;

function TEasyStream.read_taged_longword(tag: TEasyTag): longword;
begin
  read_easytag_check(tag);
  Result := read_longword;
end;

function TEasyStream.read_taged_shortint(tag: TEasyTag): shortint;
begin
  read_easytag_check(tag);
  Result := read_shortint;
end;

function TEasyStream.read_taged_smallint(tag: TEasyTag): smallint;
begin
  read_easytag_check(tag);
  Result := read_smallint;
end;

function TEasyStream.read_taged_string(tag: TEasyTag): string;
begin
  read_easytag_check(tag);
  Result := read_string;
end;

function TEasyStream.read_taged_word(tag: TEasyTag): word;
begin
  read_easytag_check(tag);
  Result := read_word;
end;

function TEasyStream.read_word: word;
begin
  read_easytag_type_check(egtInt16);
  ReadBuffer(Result, sizeof(Result));
end;

function TEasyStream.skip_value: TEasyTagType;
var
  L: longint;
begin
  Result := read_easytag_type;
  case Result of
    egtInt8  : Position := Position + sizeof(byte);
    egtInt16 : Position := Position + sizeof(word);
    egtInt32 : Position := Position + sizeof(longword);
    egtInt64 : Position := Position + sizeof(int64);
    egtString: begin
      ReadBuffer(L, sizeof(L));
      if L > 0 then Position := Position + L;
    end;
  end;
end;

procedure TEasyStream.write_byte(value: byte);
begin
  write_easytag_type(egtInt8);
  WriteBuffer(value, sizeof(value));
end;

procedure TEasyStream.write_easytag(value: TEasyTag);
var
  b: byte;
begin
  b := byte(value);
  WriteBuffer(b, sizeof(byte));
end;

procedure TEasyStream.write_easytag_type(value: TEasyTagType);
var
  b: byte;
begin
  b := byte(value);
  WriteBuffer(b, sizeof(byte));
end;

function TEasyStream.write_hex(const HS: string): integer;
var
  X, L: integer;
  C: char;
begin
  Result := 0;
  if __inCharSet(pchar(HS), HexChar) then
  begin
    L := Length(HS);
    X := 1;
    while X < L do
    begin
      C := char(__hexcv(HS[X]) * 16 + __hexcv(HS[X + 1]));
      Write(C, sizeof(char));
      Inc(X, 2);
    end;
    Result := L div 2;
  end;
end;

procedure TEasyStream.write_int64(value: int64);
begin
  write_easytag_type(egtInt64);
  WriteBuffer(value, sizeof(value));
end;

procedure TEasyStream.write_longint(value: longint);
begin
  write_easytag_type(egtInt32);
  WriteBuffer(value, sizeof(value));
end;

procedure TEasyStream.write_longword(value: longword);
begin
  write_easytag_type(egtInt32);
  WriteBuffer(value, sizeof(value));
end;

procedure TEasyStream.write_shortint(value: shortint);
begin
  write_easytag_type(egtInt8);
  WriteBuffer(value, sizeof(value));
end;

procedure TEasyStream.write_smallint(value: smallint);
begin
  write_easytag_type(egtInt16);
  WriteBuffer(value, sizeof(value));
end;

procedure TEasyStream.write_string(const value: string);
var
  L: longint;
begin
  write_easytag_type(egtString);
  L := Length(value);
  WriteBuffer(L, sizeof(L));
  if L > 0 then
    WriteBuffer(pointer(value)^, L);
end;

procedure TEasyStream.write_taged_byte(tag: TEasyTag; value: byte);
begin
  write_easytag(tag);
  write_byte(value);
end;

procedure TEasyStream.write_taged_int64(tag: TEasyTag; value: int64);
begin
  write_easytag(tag);
  write_int64(value);
end;

procedure TEasyStream.write_taged_longint(tag: TEasyTag; value: Integer);
begin
  write_easytag(tag);
  write_longint(value);
end;

procedure TEasyStream.write_taged_longword(tag: TEasyTag; value: longword);
begin
  write_easytag(tag);
  write_longword(value);
end;

procedure TEasyStream.write_taged_shortint(tag: TEasyTag; value: shortint);
begin
  write_easytag(tag);
  write_shortint(value);
end;

procedure TEasyStream.write_taged_smallint(tag: TEasyTag; value: smallint);
begin
  write_easytag(tag);
  write_smallint(value);
end;

procedure TEasyStream.write_taged_string(tag: TEasyTag; const value: string);
begin
  write_easytag(tag);
  write_string(value);
end;

procedure TEasyStream.write_taged_word(tag: TEasyTag; value: word);
begin
  write_easytag(tag);
  write_word(value);
end;

procedure TEasyStream.write_word(value: word);
begin
  write_easytag_type(egtInt16);
  WriteBuffer(value, sizeof(value));
end;

{ TEasyItem }

procedure TEasyItem.Changed;
begin
  FForm.BeginUpdate;
  try
    if Active then
      FForm.FView.AddChangedRange(ItemRange);
    FForm.Modified := true;
  finally
    FForm.EndUpdate;
  end;
end;

function TEasyItem.Contains(ACol, ARow: integer): boolean;
begin
  Result := (ARow >= FRow) and
            (ARow < FRow + FRowSpan) and
            (ACol >= FCol) and
            (ACol < FCol + FColSpan);
end;

constructor TEasyItem.Create(AForm: TEasyForm);
begin
  FForm := AForm;
  FForm.BeginUpdate;
  try
    IncRefcount;
    FForm.FItems.Add(Self);
    FRowSpan := 1;
    FColSpan := 1;
    FStyle := '';
    FForm.Modified := true;
  finally
    FForm.EndUpdate;
  end;
end;

procedure TEasyItem.Delete;
begin
  Leave;
  DecRefcount;
end;

destructor TEasyItem.Destroy;
begin
  Leave;
  inherited;
end;

var
  DT: TStrings;

procedure TEasyItem.Draw(Canvas: TCanvas; P: TPoint; Printing: boolean);
var
  D, W, H: integer;
  R: TRect;
  S: TEasyStyle;
begin
  S := GetStyle;

  // 1. clean display area
  R := MakeRect(P.x, P.y, P.x + Width, P.y + Height);
  if not Printing then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := S.FBackground;
    Canvas.FillRect(R);
  end;
  Canvas.Brush.Style := bsClear;

  // 2. draw the cell
  if FText <> '' then
  begin
    S.Write(Canvas.Font);
    DT.CommaText := FText;
//  if Printing and (S.FLineDistance > 0) then
//    D := stop_y(S.FLineDistance) else
      D := S.FLineDistance;
    Rectout(DT, Canvas, R, S.FHAlign, S.FVAlign, D, S.FWordWrap);
  end;

  // 3. draw edges
  if S.FEdges <> [] then
  begin
{   if Printing then
    begin
      W := Max(1, mtop_x(PrinterCap, 0.1)); // line width
      H := Max(1, mtop_y(PrinterCap, 0.1)); // line height
    end
    else
    begin}
      W := 1;
      H := 1;
//  end;
    R := ZoomRect(R, W, H);
    DrawBox(Canvas, R, S.FEdges, W, H);
  end;
end;

procedure TEasyItem.Ensure;
begin
  FForm.BeginUpdate;
  try
    FForm.SetRowCount(Max(FForm.RowCount, FRow + FRowSpan));
    FForm.SetColCount(Max(FForm.ColCount, FCol + FColSpan));
  finally
    FForm.EndUpdate;
  end;
end;

function TEasyItem.GetStyle: TEasyStyle;
begin
  Result := FForm.FStyles.Find(FStyle);
end;

function TEasyItem.IntersectRanges(Ranges: PEasyRanges): boolean;
var
  A: integer;
begin
  if Ranges <> nil then
    for A := 0 to Ranges^.count - 1 do
      if Intersects(Ranges^.range[A]) then
      begin
        Result := true;
        Exit;
      end;
  Result := false;
end;

function TEasyItem.Intersects(ARange: TRect): boolean;
begin
  Result := (ARange.Bottom >= FRow) and
            (ARange.Top < FRow + FRowSpan) and
            (ARange.Right >= FCol) and
            (ARange.Left < FCol + FColSpan);
end;

function TEasyItem.ItemRange: TRect;
begin
  Result.Left := FCol;
  Result.Top := FRow;
  Result.Right := FCol + FColSpan - 1;
  Result.Bottom := FRow + FRowSpan - 1;
end;

function TEasyItem.ItemRangeID: string;
begin
  Result := EncodeRangeID(FRow + 1, FCol + 1, FRow + FRowSpan, FCol + FColSpan);
end;

procedure TEasyItem.Resize(ACol, ARow, AColSpan, ARowSpan: integer);
var
  R: TRect;
  N: TEasyItem;
  A: integer;
begin
  if (ACol = FCol) and (AColSpan = FColSpan) and
     (ARow = FRow) and (ARowSpan = FRowSpan) then Exit;

  R := MakeRect(ACol, ARow, ACol + AColSpan - 1, ARow + ARowSpan - 1);
  if not IsValidRange(R) then Exit;

  for A := 0 to FForm.GetItemCount - 1 do
  begin
    N := FForm.GetItem(A);
    if (N <> Self) and N.Intersects(R) then Exit;
  end;

  FForm.BeginUpdate;
  try
    Changed; {<--current rect changed}
    FCol := ACol;
    FRow := ARow;
    FColSpan := AColSpan;
    FRowSpan := ARowSpan;
    Ensure;
    Changed; {<--new rect changed}
  finally
    FForm.EndUpdate;
  end;
end;

procedure TEasyItem.SetCol(Value: integer);
begin
  Resize(Value, FRow, FColSpan, FRowSpan);
end;

procedure TEasyItem.SetColSpan(Value: integer);
begin
  Resize(FCol, FRow, Value, FRowSpan);
end;

procedure TEasyItem.SetStyle(const Value: TEasyStyle);
var
  N: string;
begin
  if (Value <> nil) and (Value <> ss_normal) then
    N := Value.FName else
    N := '';
  if FStyle <> N then
  begin
    FStyle := N;
    Changed;
  end;
end;

procedure TEasyItem.SetRow(Value: integer);
begin
  Resize(FCol, Value, FColSpan, FRowSpan);
end;

procedure TEasyItem.SetRowSpan(Value: integer);
begin
  Resize(FCol, FRow, FColSpan, Value);
end;

procedure TEasyItem.SetText(const Value: WideString);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TEasyItem.SaveToStream(Stream: TEasyStream);
begin
  Stream.write_easytag(egItemBegin);
  Stream.write_taged_longint(egItemRow, FRow);
  Stream.write_taged_longint(egItemRowSpan, FRowSpan);
  Stream.write_taged_longint(egItemCol, FCol);
  Stream.write_taged_longint(egItemColSpan, FColSpan);
  Stream.write_taged_string(egItemText, FText);
  Stream.write_taged_string(egItemSname, FStyle);
  Stream.write_easytag(egItemEnd);
end;

procedure TEasyItem.LoadFromStream(Stream: TEasyStream);
begin
  Stream.read_easytag_check(egItemBegin);
  FRow := Stream.read_taged_longint(egItemRow);
  FRowSpan := Stream.read_taged_longint(egItemRowSpan);
  FCol := Stream.read_taged_longint(egItemCol);
  FColSpan := Stream.read_taged_longint(egItemColSpan);
  FText := Stream.read_taged_string(egItemText);
  FStyle := Stream.read_taged_string(egItemSname);
  Stream.read_easytag_check(egItemEnd);
end;

function TEasyItem.Active: boolean;
begin
  Result := (FForm <> nil) and FForm.Active;
end;

procedure TEasyItem.Assign(AItem: TEasyItem);
begin
  FForm.BeginUpdate;
  try
    Changed;
    FRow := AItem.FRow;
    FCol := AItem.FCol;
    FRowSpan := AItem.FRowSpan;
    FColSpan := AItem.FColSpan;
    FText := AItem.FText;
    FStyle := FForm.FStyles.Add(AItem.GetStyle).FName;
    Ensure;
    Changed;
  finally
    FForm.EndUpdate;
  end;
end;

procedure TEasyItem.Leave;
var
  F: TEasyForm;
begin
  if FForm <> nil then
  begin
    F := FForm;
    if Assigned(F.FOnDelete) then
      F.FOnDelete(Self);
    F.BeginUpdate;
    try
      lse_remove_last_item(F.FItems, Self);
      Changed;
      FForm := nil;
    finally
      F.EndUpdate;
    end;
  end;
end;

function TEasyItem.Left: integer;
begin
  Result := FForm.GetLefts(FCol);
end;

function TEasyItem.Top: integer;
begin
  Result := FForm.GetTops(FRow);
end;

procedure TEasyItem.Validate;
begin
  if (Self = nil) or (FForm = nil) then
    lse_error('invalid easy item');
end;

function TEasyItem.Width: integer;
var
  A: integer;
begin
  Result := FColSpan - 1;
  for A := 0 to FColSpan - 1 do
    Inc(Result, FForm.GetColWidth(FCol + A));
end;

function TEasyItem.Height: integer;
var
  A: integer;
begin
  Result := FRowSpan - 1;
  for A := 0 to FRowSpan - 1 do
    Inc(Result, FForm.GetRowHeight(FRow + A));
end;

function TEasyItem.Selected: boolean;
begin
  Result := Active and 
            FForm.FView.Targeted and
            Contains(FForm.FView.FHot.X, FForm.FView.FHot.Y);
end;

function TEasyItem.ItemID: string;
begin
  Result := EncodeRangeID(FRow + 1, FCol + 1);
end;

{ TEasyForm }

procedure TEasyForm.Assign(AForm: TEasyForm);
var
  A: integer;
begin
  if TryBeginUpdate(Self <> AForm) then
  try
    Clear;
    FWidths.Assign(AForm.FWidths);
    FHeights.Assign(AForm.FHeights);
    FFixedRowCount := AForm.FFixedRowCount;
    FFixedColCount := AForm.FFixedColCount;
    for A := 0 to AForm.GetItemCount - 1 do
      TEasyItem.Create(Self).Assign(AForm.GetItem(A));
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.Clear;
var
  A: integer;
begin
  BeginUpdate;
  try
    FScript.Clear;
    FStyles.ClearUserStyles;
    for A := GetItemCount - 1 downto 0 do
      GetItem(A).Delete;
    FItems.Clear;
    FWidths.Clear;
    FHeights.Clear;
    if not (efsDestroying in FState) then
    begin
      AddInt(FWidths, DefWidth);
      AddInt(FHeights, DefHeight);
      if Active then
      begin
        FView.Targeted := false;
        FView.EventSelect(false);
      end;
    end;
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

function TEasyForm.Clone: TEasyForm;
begin
  Result := TEasyForm.Create;
  Result.Assign(Self);
end;

function TEasyForm.ColFromX(X: integer): integer;
var
  N, W: integer;
begin
  N := 1;
  Result := 0;
  while Result < GetColCount do
  begin
    W := GetColWidth(Result);
    if (X >= N) and (X < N + W) then Exit;
    Inc(N, W + 1);
    Inc(Result);
  end;
  Result := -1;
end;

constructor TEasyForm.Create;
begin
  BeginUpdate;
  try
    FName := '';
    FItems := TList.Create;
    FWidths := TList.Create;
    FHeights := TList.Create;
    AddInt(FWidths, DefWidth);
    AddInt(FHeights, DefHeight);
    FScript := TEasyScript.Create(Self);
    FStyles := TEasyStyles.Create(Self);
    FState := [];
    AddEasyForm(Self);
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.Delete(Index: integer);
begin
  BeginUpdate;
  try
    GetItem(Index).Delete;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.DeleteColumn(ACol: integer);
var
  N: TEasyItem;
  R: TRect;
  A: integer;
begin
  CheckColumn(ACol, GetColCount);
  BeginUpdate;
  try
    R := MakeRect(ACol, 0, ACol, GetRowCount);
    for A := GetItemCount - 1 downto 0 do
    begin
      N := GetItem(A);
      if N.FCol > ACol then Dec(N.FCol) else
      if N.Intersects(R) then
        if N.FColSpan > 1 then
          Dec(N.FColSpan) else
          N.Delete;
    end;
    if Active and FView.Targeted then
      if (ACol <> FView.FHot.X) and (ACol <> FView.FEnd.X) then
      begin
        if ACol < FView.FHot.X then Dec(FView.FHot.X);
        if ACol < FView.FEnd.X then Dec(FView.FEnd.X);
        if ACol < FView.FTargetedRange.Left then Dec(FView.FTargetedRange.Left);
        if ACol <= FView.FTargetedRange.Right then Dec(FView.FTargetedRange.Right);
      end
      else FView.Targeted := false;
    FWidths.Delete(ACol);
    if GetColCount = 0 then
      AddInt(FWidths, DefWidth);
    if FFixedColCount >= GetColCount then
      SetFixedCols(GetColCount - 1);
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.DeleteRow(Index: integer);
var
  N: TEasyItem;
  R: TRect;
  A: integer;
begin
  CheckRow(Index, GetRowCount);
  BeginUpdate;
  try
    R := MakeRect(0, Index, GetColCount, Index);
    for A := GetItemCount - 1 downto 0 do
    begin
      N := GetItem(A);
      if N.FRow > Index then Dec(N.FRow) else
      if N.Intersects(R) then
        if N.FRowSpan > 1 then
          Dec(N.FRowSpan) else
          N.Delete;
    end;
    if Active and FView.Targeted then
      if (Index <> FView.FHot.Y) and (Index <> FView.FEnd.Y) then
      begin
        if Index < FView.FHot.Y then Dec(FView.FHot.Y);
        if Index < FView.FEnd.Y then Dec(FView.FEnd.Y);
        if Index < FView.FTargetedRange.Top then Dec(FView.FTargetedRange.Top);
        if Index <= FView.FTargetedRange.Bottom then Dec(FView.FTargetedRange.Bottom);
      end
      else FView.Targeted := false;
    FHeights.Delete(Index);
    if GetRowCount = 0 then
      AddInt(FHeights, DefHeight);
    if FFixedRowCount >= GetRowCount then
      SetFixedRows(GetRowCount - 1);
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

destructor TEasyForm.Destroy;
begin
  BeginUpdate;
  try
    FState := [efsDestroying];
    if Assigned(FOnDestroying) then
      FOnDestroying(Self);
    Clear;
    FreeAndNil(FWidths);
    FreeAndNil(FHeights);
    FreeAndNil(FItems);
    FreeAndNil(FScript);
    FStyles.FForm := nil;
    FreeAndNil(FStyles);
    RemoveEasyForm(Self);
  finally
    FState := [efsDestroying];
  end;
  inherited;
end;

function TEasyForm.FindByPos(ACol, ARow: integer): TEasyItem;
var
  A: integer;
begin
  for A := 0 to GetItemCount - 1 do
  begin
    Result := GetItem(A);
    if Result.Contains(ACol, ARow) then Exit;
  end;
  Result := nil;
end;

function TEasyForm.Force(ACol, ARow: integer): TEasyItem;
begin
  if ACol < 0 then lse_error('invalid column: %d', [ACol]);
  if ARow < 0 then lse_error('invalid row: %d', [ARow]);
  Result := FindByPos(ACol, ARow);
  if Result = nil then
    Result := NewBy(ACol, ARow);
end;

function TEasyForm.GetColCount: integer;
begin
  Result := FWidths.Count;
end;

function TEasyForm.GetColWidth(ACol: integer): integer;
begin
  CheckColumn(ACol, GetColCount);
  Result := integer(FWidths[ACol]);
end;

function TEasyForm.GetHeight: integer;
var
  A: integer;
begin
  if GetRowCount > 0 then
  begin
    Result := GetRowCount + 1;
    for A := 0 to GetRowCount - 1 do
      Inc(Result, GetRowHeight(A));
  end
  else Result := 0;
end;

function TEasyForm.GetItem(Index: integer): TEasyItem;
begin
  Result := TEasyItem(FItems[Index]);
end;

function TEasyForm.GetItemCount: integer;
begin
  Result := FItems.Count;
end;

function TEasyForm.GetItemsInRange(ARange: TRect): TList;
var
  A: integer;
  N: TEasyItem;
begin
  Result := TList.Create;
  for A := 0 to GetItemCount - 1 do
  begin
    N := GetItem(A);
    if N.Intersects(ARange) then
      Result.Add(N);
  end;
end;

function TEasyForm.GetLefts(ACol: integer): integer;
var
  A: integer;
begin
  CheckColumn(ACol, GetColCount);
  Result := ACol + 1;
  for A := 0 to ACol - 1 do
    Inc(Result, GetColWidth(A));
end;

function TEasyForm.GetModified: boolean;
begin
  Result := efsModified in FState;
end;

function TEasyForm.GetRowCount: integer;
begin
  Result := FHeights.Count;
end;

function TEasyForm.GetRowHeight(ARow: integer): integer;
begin
  CheckRow(ARow, GetRowCount);
  Result := integer(FHeights[ARow]);
end;

function TEasyForm.GetTops(ARow: integer): integer;
var
  A: integer;
begin
  CheckRow(ARow, GetRowCount);
  Result := ARow + 1;
  for A := 0 to ARow - 1 do
    Inc(Result, GetRowHeight(A));
end;

function TEasyForm.GetWidth: integer;
var
  A: integer;
begin
  if GetColCount > 0 then
  begin
    Result := GetColCount + 1;
    for A := 0 to GetColCount - 1 do
      Inc(Result, GetColWidth(A));
  end
  else Result := 0;
end;

procedure TEasyForm.InsertColumn(ACol: integer);
var
  N: TEasyItem;
  A: integer;
begin
  CheckColumn(ACol, GetColCount + 1);
  BeginUpdate;
  try
    if Active then
      FView.EventInsertingColumn(ACol);
    if ACol < GetColCount then
    begin
      for A := 0 to GetItemCount - 1 do
      begin
        N := GetItem(A);
        if N.FCol >= ACol then Inc(N.FCol) else
        if N.FCol + N.FColSpan > ACol then Inc(N.FColSpan);
      end;
      if Active and FView.Targeted then
      begin
        if ACol <= FView.FHot.X then Inc(FView.FHot.X);
        if ACol <= FView.FEnd.X then Inc(FView.FEnd.X);
        if ACol <= FView.FTargetedRange.Left then Inc(FView.FTargetedRange.Left);
        if ACol <= FView.FTargetedRange.Right then Inc(FView.FTargetedRange.Right);
      end;
    end;
    FWidths.Insert(ACol, pointer(DefWidth));
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.InsertRow(Index: integer);
var
  N: TEasyItem;
  A: integer;
begin
  CheckRow(Index, GetRowCount + 1);
  BeginUpdate;
  try
    if Active then
      FView.EventInsertingRow(Index);
    if Index < GetRowCount then
    begin
      for A := 0 to GetItemCount - 1 do
      begin
        N := GetItem(A);
        if N.FRow >= Index then Inc(N.FRow) else
        if N.FRow + N.FRowSpan > Index then Inc(N.FRowSpan);
      end;
      if Active and FView.Targeted then
      begin
        if Index <= FView.FHot.Y then Inc(FView.FHot.Y);
        if Index <= FView.FEnd.Y then Inc(FView.FEnd.Y);
        if Index <= FView.FTargetedRange.Top then Inc(FView.FTargetedRange.Top);
        if Index <= FView.FTargetedRange.Bottom then Inc(FView.FTargetedRange.Bottom);
      end;
    end;
    FHeights.Insert(Index, pointer(DefHeight));
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

function TEasyForm.RowFromY(Y: integer): integer;
var
  N, H: integer;
begin
  N := 1;
  Result := 0;
  while Result < GetRowCount do
  begin
    H := GetRowHeight(Result);
    if (Y >= N) and (Y < N + H) then Exit;
    Inc(N, H + 1);
    Inc(Result);
  end;
  Result := -1;
end;

procedure TEasyForm.SetColCount(Value: integer);
var
  A: integer;
begin
  if Value < 0 then Value := 0;
  if Value <> GetColCount then
  begin
    BeginUpdate;
    try
      if Value < GetColCount then
        for A := GetColCount - 1 downto Value do
          DeleteColumn(A)
      else
      if Value > GetColCount then
        for A := GetColCount to Value - 1 do
          AppendColumn;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TEasyForm.SetColWidth(ACol, Value: integer);
begin
  CheckColumn(ACol, GetColCount);
  if TryBeginUpdate(GetColWidth(ACol) <> Value) then
  try
    FWidths[ACol] := pointer(Value);
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.SetRowCount(Value: integer);
var
  A: integer;
begin
  if Value < 0 then Value := 0;
  if Value <> GetRowCount then
  begin
    BeginUpdate;
    try
      if Value < GetRowCount then
        for A := GetRowCount - 1 downto Value do
          DeleteRow(A)
      else
      if Value > GetRowCount then
        for A := GetRowCount to Value - 1 do
          AppendRow;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TEasyForm.SetRowHeight(ARow, Value: integer);
begin
  CheckRow(ARow, GetRowCount);
  if TryBeginUpdate(GetRowHeight(ARow) <> Value) then
  try
    FHeights[ARow] := pointer(Value);
    SetModified(true);
    ResizeView;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TEasyForm.ColDistance(Col1, Col2: integer): integer;
begin
  Result := 0;
  SwapMinMax(Col1, Col2);
  while Col1 < Col2 do
  begin
    Inc(Result, GetColWidth(Col1) + 1);
    Inc(Col1);
  end;
end;

function TEasyForm.RowDistance(Row1, Row2: integer): integer;
begin
  SwapMinMax(Row1, Row2);
  Result := 0;
  while Row1 < Row2 do
  begin
    Inc(Result, GetRowHeight(Row1) + 1);
    Inc(Row1);
  end;
end;

procedure TEasyForm.SetFixedCols(Value: integer);
begin
  CheckColumn(Value, GetColCount);
  if TryBeginUpdate(Value <> FFixedColCount) then
  try
    FFixedColCount := Value;
    SetModified(true);
    if Active and (FView.FMode <> emDesign) then
      ResizeView;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.SetFixedRows(Value: integer);
begin
  CheckRow(Value, GetRowCount);
  if TryBeginUpdate(Value <> FFixedRowCount) then
  try
    FFixedRowCount := Value;
    SetModified(true);
    if Active and (FView.FMode <> emDesign) then
      ResizeView;
  finally
    EndUpdate;
  end;
end;

function TEasyForm.FixedColWidth: integer;
begin
  if FFixedColCount > 0 then
    Result := GetLefts(FFixedColCount) else
    Result := 0;
end;

function TEasyForm.FixedRowHeight: integer;
begin
  if FFixedRowCount > 0 then
    Result := GetTops(FFixedRowCount) else
    Result := 0;
end;

procedure TEasyForm.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    SetModified(true);
  end;
end;

procedure TEasyForm.Draw(Canvas: TCanvas; P: TPoint);
var
  A: integer;
  N: TEasyItem;
begin
  for A := 0 to GetItemCount - 1 do
  begin
    N := GetItem(A);
    Inc(P.X, N.Left);
    Inc(P.Y, N.Top);
    N.Draw(Canvas, P, false);
  end;
end;

procedure TEasyForm.Print;
begin
{ if HasPrinter then
  begin
    Printer.BeginDoc;
    try}
      DrawOnPrinter;
     {Printer.EndDoc;
    except
      Printer.Abort;
      raise;
    end;
  end;}
end;

procedure TEasyForm.DrawOnPrinter;
{var
  F: TEasyForm;
  A, N: integer;
  HP, VP: integer;
  M: TRect;
  P, T: TPoint;}
begin
{ F := Clone;
  try
    // 1. calc pixel with and height
    HP := Max(1, mtop_x(PrinterCap, 0.1));
    VP := Max(1, mtop_y(PrinterCap, 0.1));

    // 2. calc printing area
    M.Left := mtop_x(PrinterCap, PageInfo.Left);
    M.Top := mtop_y(PrinterCap, PageInfo.Top);
    M.Right := PrinterCap^.pw - mtop_x(PrinterCap, PageInfo.Right);
    M.Bottom := PrinterCap^.ph - mtop_y(PrinterCap, PageInfo.Bottom);

    // 3. scale col widths and row heights
    for A := 0 to F.ColCount - 1 do
      F.ColWidths[A] := stop_x(F.ColWidths[A]);

    for A := 0 to F.RowCount - 1 do
      F.RowHeights[A] := stop_y(F.RowHeights[A]);

    // 4. calc starting point
    P := M.TopLeft;

    if PageInfo.HorzCenter then
    begin
      N := F.Width + F.ColCount * (HP - 1); // actual width
      Inc(P.X, (M.Right - M.Left - N) div 2);
    end;

    if PageInfo.VertCenter then
    begin
      N := F.Height + F.RowCount * (VP - 1); // actual height
      Inc(P.Y, (M.Bottom - M.Top - N) div 2);
    end;

    // 5. print to printer
    Printer.Canvas.Brush.Style := bsClear;
    for A := 0 to F.GetItemCount - 1 do
    begin
      T.X := P.X + F.GetItem(A).Left;
      T.Y := P.Y + F.GetItem(A).Top;
      F.GetItem(A).Draw(Printer.Canvas, T, true);
    end;
  finally
    F.Free;
  end;}
end;

procedure TEasyForm.SaveToStream(Stream: TEasyStream);
var
  A, V: integer;
begin
  Stream.write_taged_longint(egMagic, VerMagic);
  Stream.write_taged_string(egName, FName);

  Stream.write_taged_longint(egRowCount, GetRowCount);
  for A := 0 to GetRowCount - 1 do
  begin
    V := GetRowHeight(A);
    Stream.WriteBuffer(V, sizeof(V));
  end;
  Stream.write_taged_longint(egFixedRows, FFixedRowCount);

  Stream.write_taged_longint(egColCount, GetColCount);
  for A := 0 to GetColCount - 1 do
  begin
    V := GetColWidth(A);
    Stream.WriteBuffer(V, sizeof(V));
  end;
  Stream.write_taged_longint(egFixedCols, FFixedColCount);

  Stream.write_taged_longint(egItemCount, GetItemCount);
  for A := 0 to GetItemCount - 1 do
    GetItem(A).SaveToStream(Stream);

  Modified := false;
end;

procedure TEasyForm.LoadFromStream(Stream: TEasyStream);
const
  ERRFMT = 'Invalid magic value: %.8x';
var
  A, V: integer;
  N: integer;
begin
  if FView <> nil then
  begin
    FView.UndoListClear;
    FView.FUndoing := true;
  end;
  BeginUpdate;
  try
    Clear;
    N := Stream.read_taged_longint(egMagic);
    if N <> VerMagic then
      raise Exception.CreateFmt(ERRFMT, [N]);
    FName := Stream.read_taged_string(egName);
    
    SetRowCount(Stream.read_taged_longint(egRowCount));
    for A := 0 to GetRowCount - 1 do
    begin
      Stream.ReadBuffer(V, sizeof(V));
      SetRowHeight(A, V);
    end;
    FFixedRowCount := Stream.read_taged_longint(egFixedRows);

    SetColCount(Stream.read_taged_longint(egColCount));
    for A := 0 to ColCount - 1 do
    begin
      Stream.ReadBuffer(V, sizeof(V));
      SetColWidth(A, V);
    end;
    FFixedColCount := Stream.read_taged_longint(egFixedCols);

    for A := Stream.read_taged_longint(egItemCount) downto 1 do
      TEasyItem.Create(Self).LoadFromStream(Stream);

    Modified := false;
  finally
    EndUpdate;
    if FView <> nil then
    begin
      FView.UndoListClear;
      FView.FUndoing := false;
    end;
  end;
end;

function TEasyForm.NewBy(ACol, ARow: integer): TEasyItem;
begin
  BeginUpdate;
  try
    Result := TEasyItem.Create(Self);
    Result.FCol := ACol;
    Result.FRow := ARow;
    Result.Ensure;
    Result.Changed;
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.Sort;

  function Compare(Index1, Index2: integer): integer;
  var
    M1, M2: TEasyItem;
  begin
    M1 := GetItem(Index1);
    M2 := GetItem(Index2);
    Result := M1.FRow - M2.FRow;
    if Result = 0 then
      Result := M1.FCol - M2.FCol;
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(I, P) < 0 do Inc(I);
        while Compare(J, P) > 0 do Dec(J);
        if I <= J then
        begin
          FItems.Exchange(I, J);
          SetModified(true);
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if FItems.Count > 1 then
  begin
    BeginUpdate;
    try
      QuickSort(0, FItems.Count - 1);
    finally
      EndUpdate;
    end;
  end;
end;

function TEasyForm.SetupRangeRec(const Range: string; ER: PEasyRanges): boolean;
var
  comma: integer;
  temp: string;
begin
  ER^.count := 0;
  temp := Trim(Range) + ',';
  comma := Pos(',', temp);
  while comma > 0 do
  begin
    if DecodeRangeID(Copy(temp, 1, comma - 1), ER^.range[ER^.Count]) then
    begin
      ER^.range[ER^.count] := MakeRange(ER^.range[ER^.count], GetColCount, GetRowCount);
      Inc(ER^.count);
      if ER^.count > MaxEasyRanges then
        lse_error('too many ranges: %d', [ER^.count]);
    end;
    temp := Copy(temp, comma + 1, MaxInt);
    comma := Pos(',', temp);
  end;
  Result := (ER^.count > 0);
end;

function TEasyForm.GetRangedItems(const Range: string): TList;
var
  R: REasyRanges;
  M: TEasyItem;
  A: integer;
begin
  Result := TList.Create;
  if SetupRangeRec(Range, @R) then
    for A := 0 to GetItemCount - 1 do
    begin
      M := GetItem(A);
      if M.IntersectRanges(@R) then
        Result.Add(M);
    end;
end;

function TEasyForm.SumRanged(const Range: string;
  var sumv, avev, maxv, minv: double): integer;
var
  L: TList;
  M: TEasyItem;
  V: double;
  A: integer;
begin
  Result := 0;
  sumv := 0;
  avev := 0;
  maxv := 0;
  minv := 0;
  L := GetRangedItems(Range);
  try
    for A := 0 to L.Count - 1 do
    begin
      M := TEasyItem(L[A]);
      if TryStrToFloat(Trim(DecodeCommaText(M.FText)), V) then
      begin
        sumv := sumv + V;
        if Result = 0 then
        begin
          minv := V;
          maxv := V;
        end
        else if minv > V then minv := V
        else if maxv < V then maxv := V;
        Inc(Result);
      end;
    end;
    if Result > 0 then
      avev := sumv / Result;
  finally
    L.Free;
  end;
end;

function TEasyForm.GetRangeID(ARange: TRect): string;
begin
  ARange := MakeRange(ARange, GetColCount, GetRowCount);
  if IsValidRange(ARange) then
    Result := EncodeRangeID(ARange.Top + 1, ARange.Left + 1, ARange.Bottom + 1, ARange.Right + 1) else
    Result := '';
end;

function TEasyForm.TryBeginUpdate(Condition: boolean): boolean;
begin
  Result := Condition;
  if Result then BeginUpdate;
end;

function TEasyForm.CopyPart(Range: TRect): TEasyForm;
var
  X, Y: integer;
  L: TList;
  N, M: TEasyItem;
begin
  Result := TEasyForm.Create;
  Result.BeginUpdate;
  try
    Result.FWidths.Clear;
    for X := Range.Left to Range.Right do
      Result.FWidths.Add(FWidths[X]);

    Result.FHeights.Clear;
    for Y := Range.Top to Range.Bottom do
      Result.FHeights.Add(FHeights[Y]);

    L := GetItemsInRange(Range);
    try
      for X := 0 to L.Count - 1 do
      begin
        M := TEasyItem(L[X]);
        N := TEasyItem.Create(Result);
        N.FRow := M.FRow - Range.Top;
        N.FCol := M.FCol - Range.Left;
        N.FRowSpan := M.FRowSpan;
        N.FColSpan := M.FColSpan;
        N.FText := M.FText;
        N.SetStyle(Result.FStyles.Add(M.GetStyle));
      end;
    finally
      L.Free;
    end;
    
    Result.FState := [];
  finally
    Result.EndUpdate;
  end;
end;

procedure TEasyForm.AppendColumn;
begin
  InsertColumn(GetColCount);
end;

procedure TEasyForm.AppendRow;
begin
  InsertRow(GetRowCount);
end;

function ListCompIndex(Item1, Item2: Pointer): Integer;
begin
  Result := pchar(Item1) - pchar(Item2);
end;

function TEasyForm.HasCol(ACol: integer): boolean;
begin
  Result := (ACol >= 0) and (ACol < GetColCount);
end;

function TEasyForm.HasRow(ARow: integer): boolean;
begin
  Result := (ARow >= 0) and (ARow < GetRowCount);
end;

{ TEasyForm }

procedure TEasyForm.LoadFromFile(const FileName: string);
var
  M: TEasyStream;
begin
  M := TEasyStream.Create;
  try
    M.LoadFromFile(FileName);
    M.Position := 0;
    LoadFromStream(M);
  finally
    M.Free;
  end;
end;

procedure TEasyForm.SaveToFile(const FileName: string);
var
  M: TEasyStream;
begin
  M := TEasyStream.Create;
  try
    SaveToStream(M);
    M.SaveToFile(FileName);
  finally
    M.Free;
  end;
end;

procedure TEasyForm.SetModified(Value: boolean);
begin
  BeginUpdate;
  try
    if Value then
      FState := FState + [efsModified, efsChanged] else
      FState := FState - [efsModified, efsChanged];
  finally
    EndUpdate;
  end;
end;

procedure TEasyForm.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if efsResized in FState then
    begin
      FState := FState - [efsResized];
      if Active then
        FView.ResetView;
    end
    else
    if Active then
      FView.DrawChangedRange;
    if efsChanged in FState then
    begin
      FState := FState - [efsChanged];
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end;
end;

procedure TEasyForm.ResizeView;
begin
  BeginUpdate;
  try
    FState := FState + [efsResized];
  finally
    EndUpdate;
  end;
end;

function TEasyForm.Active: boolean;
begin
  Result := (FView <> nil) and not (csDestroying in FView.ComponentState);
end;

{ TEasyScript }

procedure TEasyScript.Clear;
var
  A: integer;
begin
  FForm.BeginUpdate;
  try
    for A := FList.Count - 1 downto 0 do
      Delete(A);
  finally
    FForm.EndUpdate;
  end;
end;

constructor TEasyScript.Create(AForm: TEasyForm);
begin
  FForm := AForm;
  FList := TStringList.Create;
  FList.Sorted := true;
end;

procedure TEasyScript.Delete(Index: integer);
begin
  FList.Objects[Index].Free;
  FList.Delete(Index);
  FForm.Modified := true;
end;

destructor TEasyScript.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TEasyScript.GetCount: integer;
begin
  Result := FList.Count;
end;

function TEasyScript.GetEntry(Index: integer): string;
begin
  Result := FList[Index];
end;

function TEasyScript.GetScript(Index: integer): string;
begin
  Result := TStrings(FList.Objects[Index]).Text;
end;

function TEasyScript.IndexOf(const Name: string): integer;
begin
  Result := FList.IndexOf(UpperCase(Name));
end;

function TEasyScript.Read(const Name, DefaultScript: string): string;
var
  Index: integer;
begin
  if __IsIDStr(pchar(Name)) then
  begin
    Index := FList.IndexOf(Name);
    if Index >= 0 then
      Result := TStrings(FList.Objects[Index]).Text else
      Result := DefaultScript;
  end
  else Result := DefaultScript;
end;

procedure TEasyScript.Remove(const Name: string);
var
  Index: integer;
begin
  Index := IndexOf(Name);
  if Index >= 0 then
    Delete(Index);
end;

procedure TEasyScript.SetEntry(Index: integer; const Value: string);
var
  S: string;
  X: integer;
  T: TObject;
begin
  S := UpperCase(Value);
  if __IsIDStr(pchar(S)) and (S <> FList[Index]) then
  begin
    X := FList.IndexOf(S);
    if X >= 0 then
    begin
      T := FList.Objects[Index];
      FList.Objects[Index] := FList.Objects[X];
      FList.Objects[X] := T;
      Delete(Index);
    end
    else
    begin
      FList.Sorted := false;
      FList[Index] := S;
      FList.Sorted := true;
      FForm.Modified := true;
    end;
  end;
end;

procedure TEasyScript.SetScript(Index: integer; const Value: string);
var
  code: string;
begin
  code := TrimRight(Value);
  if code <> '' then
  begin
    TStrings(FList.Objects[Index]).Text := code;
    FForm.Modified := true;
  end
  else Delete(Index);
end;

function TEasyScript.Write(const Name, Script: string): string;
var
  value: string;
  index: integer;
begin
  Result := '';
  if __IsIDStr(pchar(Name)) then
  begin
    index := FList.IndexOf(Name);
    value := TrimRight(Script);
    if value <> '' then
    begin
      if index < 0 then
        index := FList.AddObject(Name, TStringList.Create);
      TStrings(FList.Objects[index]).Text := value;
      FForm.Modified := true;
    end
    else if index >= 0 then Delete(Index);
  end;
end;

{ TEasyView }

constructor TEasyView.Create(AOwner: TComponent);
begin
  inherited;
  FForm := TEasyForm.Create;
  BeginUpdate;
  try
    // 1. setup internal form
//  FForm.SetColCount(1);
//  FForm.SetRowCount(1);
    FForm.SetModified(false);
    FForm.FView := Self;
    FForm.OnChange := {$IFDEF FPC}@{$ENDIF}EventChange;

    // 2. initialize
    FUndoStream := TEasyStream.Create;
    FUndoList := TList.Create;
    FUndoing := false;
    {$IFNDEF FPC}
    BevelInner := bvNone;
    BevelOuter := bvNone;
    {$ENDIF}
    TabStop := true;
    FLineColor := DefLineColor;
    FBackground := DefBackground;
    FHorzRuleHeight := 18;
    FVertRuleWidth := 32;
    FChangedRange.Right := -1;

    // 3. setup horz scroll bar
    FHorzScrollBar := TScrollBar.Create(Self);
    FHorzScrollBar.Parent := Self;
    FHorzScrollBar.Align := alBottom;
    FHorzScrollBar.TabStop := false;
    FHorzScrollBar.Visible := false;
    FHorzScrollBar.Max := FForm.ColCount - 1;
    FHorzScrollBar.Position := 0;
    FHorzScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}HorzScrollBarChange;

    // 4. setup vert scroll bar
    FVertScrollBar := TScrollBar.Create(Self);
    FVertScrollBar.Parent := Self;
    FVertScrollBar.Kind := sbVertical;
    FVertScrollBar.Align := alRight;
    FVertScrollBar.TabStop := false;
    FVertScrollBar.Visible := false;
    FVertScrollBar.Max := FForm.RowCount - 1;
    FVertScrollBar.Position := 0;
    FVertScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}VertScrollBarChange;

    // 5. setup work area
    FEditArea := TPanel.Create(Self);
    FEditArea.Parent := Self;
    FEditArea.Align := alClient;
    FEditArea.BevelInner := bvNone;
    FEditArea.BevelOuter := bvNone;
    FEditArea.TabStop := false;
    FEditArea.Color := FBackground;
    FEditArea.Visible := true;

    // 6. setup horz rule
    FHorzRulePanel := TPanel.Create(Self);
    FHorzRulePanel.Parent := FEditArea;
    FHorzRulePanel.Align := alTop;
    FHorzRulePanel.Height := FHorzRuleHeight;
    FHorzRulePanel.BevelInner := bvNone;
    FHorzRulePanel.BevelOuter := bvNone;
    FHorzRulePanel.BorderStyle := bsNone;
    FHorzRulePanel.Color := clBtnFace;
    FHorzRulePanel.TabStop := false;
    FHorzRulePanel.Visible := false;

    FCorner := TPaintBox.Create(Self);
    FCorner.Parent := FHorzRulePanel;
    FCorner.Align := alLeft;
    FCorner.Visible := true;
    FCorner.Width := FVertRuleWidth;
    FCorner.OnPaint := {$IFDEF FPC}@{$ENDIF}CornerPaint;
    FCorner.OnMouseDown := {$IFDEF FPC}@{$ENDIF}CornerLTMouseDown;

    FHorzRule := TPaintBox.Create(Self);
    FHorzRule.Parent := FHorzRulePanel;
    FHorzRule.Align := alClient;
    FHorzRule.Visible := true;
    FHorzRule.OnMouseDown := {$IFDEF FPC}@{$ENDIF}HorzRuleMouseDown;
    FHorzRule.OnMouseMove := {$IFDEF FPC}@{$ENDIF}HorzRuleMouseMove;
    FHorzRule.OnMouseUp := {$IFDEF FPC}@{$ENDIF}HorzRuleMouseUp;
    FHorzRule.OnPaint := {$IFDEF FPC}@{$ENDIF}HorzRulePaint;

    // 7. setup vert rule
    FVertRule := TPaintBox.Create(Self);
    FVertRule.Parent := FEditArea;
    FVertRule.Align := alLeft;
    FVertRule.Width := FVertRuleWidth;
    FVertRule.Visible := true;
    FVertRule.OnMouseDown := {$IFDEF FPC}@{$ENDIF}VertRuleMouseDown;
    FVertRule.OnMouseMove := {$IFDEF FPC}@{$ENDIF}VertRuleMouseMove;
    FVertRule.OnMouseUp := {$IFDEF FPC}@{$ENDIF}VertRuleMouseUp;
    FVertRule.OnPaint := {$IFDEF FPC}@{$ENDIF}VertRulePaint;

    // 8. setup editing base area

    FBody := TPaintBox.Create(Self);
    FBody.Parent := FEditArea;
    FBody.Align := alClient;
    FBody.Visible := true;
    FBody.OnMouseDown := {$IFDEF FPC}@{$ENDIF}BodyMouseDown;
    FBody.OnMouseMove := {$IFDEF FPC}@{$ENDIF}BodyMouseMove;
    FBody.OnMouseUp := {$IFDEF FPC}@{$ENDIF}BodyMouseUp;
    FBody.OnPaint := {$IFDEF FPC}@{$ENDIF}BodyPaint;

    // 9. other necessary
    FOption := [eoShowRule, eoShowGridLine];
    FKeyClear := true;
    FLeadChar := #0;
    FMode := emEnter;
    SetMode(emDesign);
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.CutToClipboard;
begin
  if Targeted then
  begin
    CopyToClipboard;
    EventChangingTarget;
    DeleteTarget(FMode = emEnter);
  end;
end;

procedure TEasyView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then SetFocus;
  inherited;
end;

{$IFDEF FPC}
procedure TEasyView.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: WideString;
begin
  S := UTF8Decode(UTF8Key);
  if S <> '' then
    ExecChar(S[1]);
  UTF8Key := '';
end;
{$ENDIF}

function TEasyView.UndoNew: PUndoRec;
var
  X: integer;
begin
  Result := lse_mem_alloc_zero(sizeof(TUndoRec));
  X := FUndoList.Add(Result);
  if X > 0 then
    FUndoStream.Position := UndoGet(X - 1)^.ur_epos else
    FUndoStream.Position := 0;
  Result^.ur_spos := FUndoStream.Position;
  Result^.ur_epos := Result^.ur_spos;
  Result^.ur_range := FTargetedRange;
  EventUndoStatus;
end;

{$IFDEF WINDOWS}
procedure TEasyView.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS;
end;
{$ENDIF}

function TEasyView.ColToX(Col: integer): integer;
var
  X: integer;
begin
  Result := 1;
  if StartCol = Col then Exit;
  if StartCol < Col then
    for X := StartCol to Col - 1 do
      Inc(Result, FForm.ColWidths[X] + 1)
  else
    for X := Col to StartCol - 1 do
      Dec(Result, FForm.ColWidths[X] + 1);
end;

procedure TEasyView.CopyToClipboard;
var
  F: TEasyForm;
  M: TEasyStream;
begin
  if Targeted then
  begin
    F := FForm.CopyPart(FTargetedRange);
    try
      M := TEasyStream.Create;
      try
        F.SaveToStream(M);
        M.Position := 0;
        Clipboard.AsText := M.read_hex(M.Size);
      finally
        M.Free;
      end;
    finally
      F.Free;
    end;
  end;
end;

procedure TEasyView.ExecDelete(Shift: TShiftState);
begin
  if Shift = [] then
    SetTargetText('', FMode = emEnter);
end;

procedure TEasyView.ExecDown(Shift: TShiftState);
var
  N: TEasyItem;
  P: TPoint;
begin
  if not Targeted then Exit;
  if ssShift in Shift then
  begin
    if (FMode = emDesign) and (FEnd.y < FForm.RowCount - 1) then
    begin
      P := FEnd;
      Inc(P.y);
      SelectRange(FHot, P);
    end;
  end
  else
  if FMode = emDesign then
  begin
    N := FForm.FindByPos(FHot.x, FHot.y);
    if Assigned(N) then
      P := Classes.Point(FHot.x, N.Row + N.RowSpan - 1) else
      P := FHot;
    Inc(P.y);
    if P.y < FForm.RowCount then
      SelectRange(P, P);
  end
  else SeekNext(false);
end;

procedure TEasyView.ExecLeft(Shift: TShiftState);
var
  N: TEasyItem;
  P: TPoint;
begin
  if not Targeted then Exit;
  if ssShift in Shift then
  begin
    if (FMode = emDesign) and (FEnd.x > 0) then
    begin
      P := FEnd;
      Dec(P.x);
      SelectRange(FHot, P);
    end;
  end
  else
  if FMode = emDesign then
  begin
    N := FForm.FindByPos(FHot.x, FHot.y);
    if Assigned(N) then
      P := Classes.Point(N.Col, FHot.y) else
      P := FHot;
    if P.x > 0 then
    begin
      Dec(P.x);
      SelectRange(P, P);
    end;
  end
  else SeekPrev(true);
end;

procedure TEasyView.BodyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: integer;
  P: TPoint;
begin
  if Button = mbLeft then
  begin
    if BodyHitOnCell(X, Y, Col, Row) then
    begin
      P := Classes.Point(Col, Row);
      if FMode = emDesign then
      begin
        SelectRange(P, P);
        FSizing := szSelect;
      end
      else SelectRange(P, P);
    end;
    SetFocus;
  end;
end;

procedure TEasyView.BodyMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Col, Row: integer;
begin
  if FSizing = szSelect then
    if BodyHitOnCell(X, Y, Col, Row) then
      if (Col <> FEnd.X) or (Row <> FEnd.Y) then
        SelectRange(FHot, Classes.Point(Col, Row));
end;

procedure TEasyView.BodyMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FSizing = szSelect then
  begin
    BodyMouseMove(nil, Shift, X, Y);
    FSizing := szNone;
  end;
end;

procedure TEasyView.BodyDrawGridLine;
var
  V: TRect;
  A, L, R: integer;
  P: TColor;
begin
  if ShowGridLine and (FMode = emDesign) then
  begin
    V := VisibleRange;
    P := FBody.Canvas.Pen.Color;
    FBody.Canvas.Pen.Color := FLineColor;
    try
      { h-lines }
      R := FForm.Width - FForm.Lefts[V.Left] + 1;
      L := 0;
      for A := V.Top to V.Bottom do
      begin
        FBody.Canvas.MoveTo(0, L);
        FBody.Canvas.LineTo(R, L);
        Inc(L, FForm.RowHeights[A] + 1);
      end;
      FBody.Canvas.MoveTo(0, L);
      FBody.Canvas.LineTo(R, L);
      { v-lines }
      R := FForm.Height - FForm.Tops[V.Top] + 1;
      L := 0;
      for A := V.Left to V.Right do
      begin
        FBody.Canvas.MoveTo(L, 0);
        FBody.Canvas.LineTo(L, R);
        Inc(L, FForm.ColWidths[A] + 1);
      end;
      FBody.Canvas.MoveTo(L, 0);
      FBody.Canvas.LineTo(L, R);
    finally
      FBody.Canvas.Pen.Color := P;
    end;
  end;
end;

function TEasyView.BodyHitOnCell(X, Y: integer; var Col, Row: integer): boolean;
var
  P, min_col, min_row: integer;
begin
  Result := false;

  Col := StartCol;
  if X > 0 then
  begin
    P := FForm.ColWidths[Col] + 1;
    while (X > P) and (Col < FForm.ColCount - 1) do
    begin
      Inc(Col);
      Inc(P, FForm.ColWidths[Col] + 1);
    end;
    if X >= P then Exit;
  end
  else
  if X < 0 then
  begin
    if FMode <> emDesign then
      min_col := FForm.FFixedColCount else
      min_col := 0;
    P := 0;
    while (X < P) and (Col > min_col) do
    begin
      Dec(Col);
      Dec(P, FForm.ColWidths[Col] + 1);
    end;
    if X <= P then Exit;
  end
  else Exit;

  Row := StartRow;
  if Y > 0 then
  begin
    P := FForm.RowHeights[Row] + 1;
    while (Y > P) and (Row < FForm.RowCount - 1) do
    begin
      Inc(Row);
      Inc(P, FForm.RowHeights[Row] + 1);
    end;
    if Y >= P then Exit;
  end
  else
  if Y < 0 then
  begin
    if FMode <> emDesign then
      min_row := FForm.FFixedRowCount else
      min_row := 0;
    P := 0;
    while (Y < P) and (Row > min_row) do
    begin
      Dec(Row);
      Dec(P, FForm.RowHeights[Row] + 1);
    end;
    if Y <= P then Exit;
  end
  else Exit;

  Result := true;
end;

procedure TEasyView.BodyPaint(Sender: TObject);
begin
  BodyDrawGridLine;
  DrawRanged(FBody, VisibleRange);
end;

procedure TEasyView.ExecRight(Shift: TShiftState);
var
  N: TEasyItem;
  P: TPoint;
begin
  if not Targeted then Exit;
  if ssShift in Shift then
  begin
    if (FMode = emDesign) and (FEnd.x < FForm.ColCount - 1) then
    begin
      P := FEnd;
      Inc(P.x);
      SelectRange(FHot, P);
    end;
  end
  else
  if FMode = emDesign then
  begin
    N := FForm.FindByPos(FHot.x, FHot.y);
    if Assigned(N) then
      P := Classes.Point(N.Col + N.ColSpan - 1, FHot.y) else
      P := FHot;
    Inc(P.x);
    if P.X < FForm.ColCount then
      SelectRange(P, P);
  end
  else SeekNext(true);
end;

procedure TEasyView.ExecUp(Shift: TShiftState);
var
  N: TEasyItem;
  P: TPoint;
begin
  if not Targeted then Exit;
  if ssShift in Shift then
  begin
    if (FMode = emDesign) and (FEnd.y > 0) then
    begin
      P := FEnd;
      Dec(P.y);
      SelectRange(FHot, P);
    end;
  end
  else
  if FMode = emDesign then
  begin
    N := FForm.FindByPos(FHot.x, FHot.y);
    if Assigned(N) then
      P := Classes.Point(FHot.x, N.Row) else
      P := FHot;
    if P.y > 0 then
    begin
      Dec(P.y);
      SelectRange(P, P);
    end;
  end
  else SeekPrev(false);
end;

function TEasyView.RowToY(Row: integer): integer;
var
  Y: integer;
begin
  Result := 1;
  if StartRow = Row then Exit;
  if StartRow < Row then
    for Y := StartRow to Row - 1 do
      Inc(Result, FForm.RowHeights[Y] + 1)
  else
    for Y := Row to StartRow - 1 do
      Dec(Result, FForm.RowHeights[Y] + 1);
end;

function TEasyView.SelectedRect: TRect;
begin
  if Targeted then
    Result := FTargetedRange else
    Result := MakeRect(-1, -1, -2, -2);
end;

procedure TEasyView.SelectRange(P, E: TPoint);
var
  N: TEasyItem;
  R: TRect;
begin
  if FMode <> emDesign then E := P;
  BeginUpdate;
  try
    if Targeted then
      AddChangedRange(FTargetedRange);
    if FMode <> emDesign then
    begin
      N := FForm.FindByPos(P.x, P.y);
      Targeted := Assigned(N) and (N.GetStyle.FItemType <> estTitle);
    end
    else
    begin
      R := MakeRect(0, 0, FForm.ColCount - 1, FForm.RowCount - 1);
      Targeted := PtInRange(R, P) and PtInRange(R, E);
    end;
    FHot := P;
    FEnd := E;
    FTargetedRange := CalcTargetRange(MakeRange(P, E));
    EventSelect(true);
    MakeCellVisible(E.x, E.y);
    if Targeted then
      AddChangedRange(FTargetedRange);
    FKeyClear := true;
    FLeadChar := #0;
  finally
    EndUpdate;
  end;
end;

function TEasyView.HorzRuleHitTest(X: integer; var Col: integer): TXYHitTest;
var
  P, W: integer;
begin
  Result := xyHead;

  P := 1;
  if X < P then Exit;

  Col := StartCol;

  if X = P then
  begin
    if Col = 0 then
      Result := xyNone else
    begin
      Result := xyLine;
      Dec(Col);
    end;
    Exit;
  end;

  Result := xyTail;

  while Col < FForm.ColCount do
  begin
    W := FForm.ColWidths[Col];
    Inc(P, W + 1);
    if X <= P then
    begin
      W := Min(3, W div 5);
      if X >= P - W then
        Result := xyLine else
        Result := xyItem;
      Exit;
    end;
    Inc(Col);
  end;
end;

function TEasyView.VertRuleHitTest(Y: integer; var Row: integer): TXYHitTest;
var
  P, H: integer;
begin
  Result := xyHead;

  P := 1;
  if Y < P then Exit;

  Row := StartRow;

  if Y = P then
  begin
    if Row = 0 then
      Result := xyNone else
    begin
      Result := xyLine;
      Dec(Row);
    end;
    Exit;
  end;

  Result := xyTail;

  while Row < FForm.RowCount do
  begin
    H := FForm.RowHeights[Row];
    Inc(P, H + 1);
    if Y <= P then
    begin
      H := Min(3, H div 5);
      if Y >= P - H then
        Result := xyLine else
        Result := xyItem;
      Exit;
    end;
    Inc(Row);
  end;
end;

procedure TEasyView.HorzRuleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if (FMode = emDesign) and (HorzRuleHitTest(X, Y) = xyLine) then
    begin
      FSizingPoint := Classes.Point(X, Y);  {Y is the column index}
      FSizing := szHorz;
    end;
    SetFocus;
  end;
end;

procedure TEasyView.HorzRuleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
const
  C: array[boolean] of TCursor = (crDefault, crHSplit);
begin
  if (FMode = emDesign) and (FSizing = szNone) then
    FHorzRule.Cursor := C[HorzRuleHitTest(X, Y) = xyLine];
end;

procedure TEasyView.HorzRuleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FMode = emDesign) and (FSizing = szHorz) then
  begin
    FHorzRule.Cursor := crDefault;
    FSizing := szNone;
    Y := FForm.ColWidths[FSizingPoint.Y];
    X := Max(0, X + Y - FSizingPoint.X);
    if X <> Y then
    begin
      EventChangingColumnWidth(FSizingPoint.Y);
      FForm.ColWidths[FSizingPoint.Y] := X;
    end;
  end;
end;

procedure TEasyView.HorzRulePaint(Sender: TObject);
begin
  if FMode = emDesign then
    HorzRuleDrawRule else
    HorzRuleDrawFixedRows;
end;

procedure TEasyView.VertRuleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if (FMode = emDesign) and (VertRuleHitTest(Y, X) = xyLine) then
    begin
      FSizingPoint := Classes.Point(Y, X);  {X is the sizing row index}
      FSizing := szVert;
    end;
    SetFocus;
  end;
end;

procedure TEasyView.VertRuleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  C: array[boolean] of TCursor = (crDefault, crVSplit);
begin
  if (FMode = emDesign) and (FSizing = szNone) then
    FVertRule.Cursor := C[(X > 0) and (X < FVertRuleWidth) and (VertRuleHitTest(Y, X) = xyLine)];
end;

procedure TEasyView.VertRuleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FMode = emDesign) and (FSizing = szVert) then
  begin
    FVertRule.Cursor := crDefault;
    FSizing := szNone;
    X := FForm.RowHeights[FSizingPoint.Y];
    Y := Max(0, X + Y - FSizingPoint.X);
    if X <> Y then
    begin
      EventChangingRowHeight(FSizingPoint.Y);
      FForm.RowHeights[FSizingPoint.Y] := Y;
    end;
  end;
end;

procedure TEasyView.VertRulePaint(Sender: TObject);
begin
  if FMode = emDesign then
    VertRuleDrawRule else
    VertRuleDrawFixedCols;
end;

function TEasyView.UndoDeleteLast: boolean;
var
  ux: integer;
  ur: PUndoRec;
begin
  ux := FUndoList.Count - 1;
  Result := (ux >= 0);
  if Result then
  begin
    ur := UndoGet(ux);
    FUndoList.Delete(ux);
    lse_mem_free(ur, sizeof(TUndoRec));
    if ux = 0 then
      FUndoStream.Clear;
    EventUndoStatus;
  end;
end;

procedure TEasyView.DeleteTarget(IgnoreReadonly: boolean);
var
  X, Y: integer;
  N: TEasyItem;
begin
  if TryBeginUpdate(Targeted) then
  try
    EventChangingTarget;
    for Y := FTargetedRange.Top to FTargetedRange.Bottom do
      for X := FTargetedRange.Left to FTargetedRange.Right do
      begin
        N := FForm.FindByPos(X, Y);
        if Assigned(N) then
          if not N.Style.IsReadonly or IgnoreReadonly or (FMode = emDesign) then
            N.Delete;
      end;
    EventSelect(false);
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.DeleteTargetColumns;
var
  X, N: integer;
begin
  if TryBeginUpdate(Targeted) then
  try
    Targeted := false;
    X := FTargetedRange.Left;
    for N := FTargetedRange.Right - X downto 0 do
      FForm.DeleteColumn(X);
    EventSelect(false);
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.DeleteTargetRows;
var
  Y, N: integer;
begin
  if TryBeginUpdate(Targeted) then
  try
    Targeted := false;
    Y := FTargetedRange.Top;
    for N := FTargetedRange.Bottom - Y downto 0 do
      FForm.DeleteRow(Y);
    EventSelect(false);
  finally
    EndUpdate;
  end;
end;

destructor TEasyView.Destroy;
begin
  UndoListClear;
  FreeAndNil(FUndoList);
  FreeAndNil(FUndoStream);
  FForm.FView := nil;
  FreeAndNil(FForm);
  inherited;
end;

procedure TEasyView.HorzScrollBarChange(Sender: TObject);
var
  P: integer;
begin
  P := FHorzScrollBar.Position;
  if P <> StartCol then
  begin
    FStartCol := P;
    FHorzRulePanel.Invalidate;
    FBody.Invalidate;
  end;
end;

procedure TEasyView.InsertColumnBeforeTarget;
begin
  if Targeted then
    FForm.InsertColumn(FTargetedRange.Left);
end;

procedure TEasyView.InsertRowBeforeTarget;
begin
  if Targeted then
    FForm.InsertRow(FTargetedRange.Top);
end;

procedure TEasyView.VertScrollBarChange(Sender: TObject);
var
  P: integer;
begin
  P := FVertScrollBar.Position;;
  if P <> StartRow then
  begin
    FStartRow := P;
    FVertRule.Invalidate;
    FBody.Invalidate;
  end;
end;

function TEasyView.VisibleColCount: integer;
var
  C, X, W: integer;
begin
  Result := 1;
  W := FBody.Width;
  C := FStartCol;
  X := FForm.ColWidths[C] + 2;
  for C := C + 1 to FForm.ColCount - 1 do
    if X < W then
    begin
      Inc(Result);
      Inc(X, FForm.ColWidths[C] + 1);
    end
    else Exit;
end;

function TEasyView.VisibleItems: TList;
begin
  Result := FForm.GetItemsInRange(VisibleRange);
end;

function TEasyView.VisibleRange: TRect;
begin
  Result.Left := StartCol;
  Result.Right := Result.Left + VisibleColCount - 1;
  Result.Top := StartRow;
  Result.Bottom := Result.Top + VisibleRowCount - 1;
end;

function TEasyView.VisibleRowCount: integer;
var
  R, Y, H: integer;
begin
  Result := 1;
  H := FBody.Height;
  R := FStartRow;
  Y := FForm.RowHeights[R] + 2;
  for R := R + 1 to FForm.RowCount - 1 do
    if Y < H then
    begin
      Inc(Result);
      Inc(Y, FForm.RowHeights[R] + 1);
    end
    else Exit;
end;

procedure TEasyView.SaveToFile(const fname: string);
begin
  FForm.SaveToFile(fname);
end;

function TEasyView.UndoSaveChange: PUndoRec;
begin
  if not FUndoing then
  begin
    Result := UndoGetLast;
    if Result <> nil then
      if Result^.ur_oper = undoChange then
        Exit;
    Result := UndoNew;
    Result^.ur_oper := undoChange;
    FForm.SaveToStream(FUndoStream);
    Result^.ur_epos := FUndoStream.Position;
  end
  else Result := nil;
end;

function TEasyView.UndoSaveRowHeight(Row: integer): PUndoRec;
begin
  if not FUndoing then
  begin
    Result := UndoGetLast;
    if Result <> nil then
      if Result^.ur_oper = undoChangeHeight then
        if Result^.ur_index = Row then
          Exit;
    Result := UndoNew;
    Result^.ur_oper := undoChangeHeight;
    Result^.ur_index := Row;
    Result^.ur_size := FForm.RowHeights[Row];
  end
  else Result := nil;
end;

procedure TEasyView.UnmergeTarget;
var
  X, Y: integer;
  N: TEasyItem;
begin
  if TryBeginUpdate(Targeted) then
  try
    EventChangingTarget;
    for Y := FTargetedRange.Top to FTargetedRange.Bottom do
      for X := FTargetedRange.Left to FTargetedRange.Right do
      begin
        N := FForm.FindByPos(X, Y);
        if Assigned(N) then
        begin
          N.ColSpan := 1;
          N.RowSpan := 1;
        end;
      end;
    EventSelect(false);
  finally
    EndUpdate;
  end;
end;

function TEasyView.UndoSaveChangeTarget: PUndoRec;
begin
  if not FUndoing then
  begin
    Result := UndoGetLast;
    if Result <> nil then
      if Result^.ur_oper = undoChangeTarget then
        if SameRect(Result^.ur_range, FTargetedRange) then
          Exit;
    Result := UndoNew;
    Result^.ur_oper := undoChangeTarget;
    SaveTargetToStream(FUndoStream);
    Result^.ur_epos := FUndoStream.Position;
  end
  else Result := nil;
end;

function TEasyView.UndoSaveColumnWidth(Col: integer): PUndoRec;
begin
  if not FUndoing then
  begin
    Result := UndoGetLast;
    if Result <> nil then
      if Result^.ur_oper = undoChangeWidth then
        if Result^.ur_index = Col then
          Exit;
    Result := UndoNew;
    Result^.ur_oper := undoChangeWidth;
    Result^.ur_index := Col;
    Result^.ur_size := FForm.ColWidths[Col];
  end
  else Result := nil;
end;

function TEasyView.UndoSaveInsertColumn(Index: integer): PUndoRec;
begin
  if not FUndoing then
  begin
    Result := UndoNew;
    Result^.ur_oper := undoInsertColumn;
    Result^.ur_index := Index;
  end
  else Result := nil;
end;

function TEasyView.UndoSaveInsertRow(Index: integer): PUndoRec;
begin
  if not FUndoing then
  begin
    Result := UndoNew;
    Result^.ur_oper := undoInsertRow;
    Result^.ur_index := Index;
  end
  else Result := nil;
end;

function TEasyView.UndoSaveResize: PUndoRec;
var
  X: integer;
begin
  if not FUndoing then
  begin
    Result := UndoGetLast;
    if Result <> nil then
      if Result^.ur_oper = undoResize then
        Exit;
    Result := UndoNew;
    Result^.ur_oper := undoResize;
    for X := 0 to FForm.ColCount - 1 do
      FUndoStream.write_longint(FForm.ColWidths[X]);
    for X := 0 to FForm.RowCount - 1 do
      FUndoStream.write_longint(FForm.RowHeights[X]);
    Result^.ur_epos := FUndoStream.Position;
  end
  else Result := nil;
end;

procedure TEasyView.LoadFromFile(const fname: string);
var
  M: TEasyMode;
begin
  BeginUpdate;
  try
    FTargeted := false;
    FStartCol := 0;
    FStartRow := 0;
    FForm.LoadFromFile(fname);
    M := FMode;
    if M = emEnter then
      FMode := emDesign else
      FMode := emEnter;
    SetMode(M);
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.ResetDesignView;
var
  view_w, view_h: integer;
  form_w, form_h: integer;
  rule_w, rule_h: integer;
begin
  FHorzRulePanel.Height := FHorzRuleHeight;
  FHorzRulePanel.Color := clBtnface;
  FHorzRulePanel.Visible := ShowRule;

  FVertRule.Width := FVertRuleWidth;
  FVertRule.Visible := ShowRule;
  FCorner.Width := FVertRuleWidth;

  rule_w := SelectIV(ShowRule, FVertRuleWidth,  0);
  form_w := FForm.Width;
  view_w := FEditArea.Width  - rule_w;

  rule_h := SelectIV(ShowRule, FHorzRuleHeight, 0);
  form_h := FForm.Height;
  view_h := FEditArea.Height - rule_h;

  FHorzScrollBar.Min := 0;
  FVertScrollBar.Min := 0;

  if (form_w <= view_w) and (form_h <= view_h) then
  begin
    FHorzScrollBar.Visible := false;
    FHorzScrollBar.Max := 0;
    FVertScrollBar.Visible := false;
    FVertScrollBar.Max := 0;
    Exit;
  end;
  
  if form_w > view_w then
  begin
    if FForm.ColCount > 1 then
      Dec(view_h, FHorzScrollBar.Height);
    if form_h > view_h then
      if FForm.RowCount > 1 then
        Dec(view_w, FVertScrollBar.Width);
  end
  else
  begin
    if FForm.RowCount > 1 then
      Dec(view_w, FVertScrollBar.Width);
    if form_w > view_w then
      if FForm.ColCount > 1 then
        Dec(view_h, FHorzScrollBar.Height);
  end;

  if (form_w > view_w) and (FForm.ColCount > 1) then
  begin
    FHorzScrollBar.Max := HorzScrollBarMax(view_w);
    FHorzScrollBar.Visible := true;
  end
  else
  begin
    FHorzScrollBar.Max := 0;
    FHorzScrollBar.Visible := false;
  end;

  if (form_h > view_h) and (FForm.RowCount > 1) then
  begin
    FVertScrollBar.Max := VertScrollBarMax(view_h);
    FVertScrollBar.Visible := true;
  end
  else
  begin
    FVertScrollBar.Max := 0;
    FVertScrollBar.Visible := false;
  end;
end;

procedure TEasyView.ResetEnterView;
var
  view_w, view_h: integer;
  form_w, form_h, f_cols, f_rows: integer;
  rule_w, rule_h: integer;
begin
  FHorzRulePanel.Height := FForm.FixedRowHeight;
  FHorzRulePanel.Color := FBackground;
  FHorzRulePanel.Visible := FForm.FFixedRowCount > 0;

  FVertRule.Width := FForm.FixedColWidth;
  FVertRule.Visible := (FForm.FFixedColCount > 0);
  FCorner.Width := FVertRule.Width;

  rule_w := FVertRule.Width;
  form_w := FForm.Width - rule_w;
  view_w := FEditArea.Width  - rule_w;
  f_cols := FForm.ColCount - FForm.FixedColCount; 

  rule_h := FHorzRulePanel.Height;
  form_h := FForm.Height - rule_h;
  view_h := FEditArea.Height - rule_h;
  f_rows := FForm.RowCount - FForm.FixedRowCount; 

  if (form_w <= view_w) and (form_h <= view_h) then
  begin
    FHorzScrollBar.Visible := false;
    FHorzScrollBar.Max := FForm.FixedColCount;
    FHorzScrollBar.Min := FForm.FixedColCount;
    FVertScrollBar.Visible := false;
    FVertScrollBar.Max := FForm.FixedRowCount;
    FVertScrollBar.Min := FForm.FixedRowCount;
    Exit;
  end;

  if form_w > view_w then
  begin
    if f_cols > 1 then
      Dec(view_h, FHorzScrollBar.Height);
    if form_h > view_h then
      if f_rows > 1 then
        Dec(view_w, FVertScrollBar.Width);
  end
  else
  begin
    if f_rows > 1 then
      Dec(view_w, FVertScrollBar.Width);
    if form_w > view_w then
      if f_cols > 1 then
        Dec(view_h, FHorzScrollBar.Height);
  end;

  if (form_w > view_w) and (f_cols > 1) then
  begin
    FHorzScrollBar.Max := HorzScrollBarMax(view_w);
    FHorzScrollBar.Visible := true;
  end
  else
  begin
    FHorzScrollBar.Max := FForm.FixedColCount;
    FHorzScrollBar.Visible := false;
  end;

  if (form_h > view_h) and (f_rows > 1) then
  begin
    FVertScrollBar.Max := VertScrollBarMax(view_h);
    FVertScrollBar.Visible := true;
  end
  else
  begin
    FVertScrollBar.Max := FForm.FixedRowCount;
    FVertScrollBar.Visible := false;
  end;

  FHorzScrollBar.Min := FForm.FixedColCount;
  FVertScrollBar.Min := FForm.FixedRowCount;
end;

procedure TEasyView.ResetView;
begin
  FChangedRange.Right := -1;
  if HandleAllocated and (Parent <> nil) and Parent.HandleAllocated then
  begin
    if FMode = emDesign then
      ResetDesignView else
      ResetEnterView;
    HorzScrollBarChange(nil);
    VertScrollBarChange(nil);
    FHorzRulePanel.Invalidate;
    FVertRule.Invalidate;
    FBody.Invalidate;
  end;
end;

procedure TEasyView.Clear;
begin
  BeginUpdate;
  try
    FForm.Clear;
    UndoListClear;
    if not (csDestroying in ComponentState) then
      ResetView;
  finally
    EndUpdate;
  end;
end;

function TEasyView.UndoGetLast: PUndoRec;
var
  X: integer;
begin
  X := UndoCount - 1;
  if X >= 0 then
    Result := UndoGet(X) else
    Result := nil;
end;

function TEasyView.UndoListClear: integer;
begin
  Result := 0;
  while UndoCount > 0 do
  begin
    UndoDeleteLast;
    Inc(Result);
  end;
end;

procedure TEasyView.PasteFromClipboard;
var
  A: integer;
  N, T: TEasyItem;
  G: TRect;
  M: TEasyStream;
  F: TEasyForm;
begin
  if Targeted and Clipboard.HasFormat(CF_TEXT) then
  begin
    M := TEasyStream.Create;
    try
      M.write_hex(Clipboard.AsText);
      try
        M.Position := 0;
        F := TEasyForm.Create;
        try
          F.LoadFromStream(M);
          G.Left := 0;
          G.Top :=  0;
          G.Right := F.ColCount;
          G.Bottom := F.RowCount;
          EventPasting;
          BeginUpdate;
          try
            FTargetedRange.Right := Max(FTargetedRange.Right, (G.Right - G.Left) + FTargetedRange.Left - 1);
            FTargetedRange.Bottom := Max(FTargetedRange.Bottom, (G.Bottom - G.Top) + FTargetedRange.Top - 1);
            if FForm.ColCount < FTargetedRange.Right + 1 then
              FForm.ColCount := FTargetedRange.Right + 1;
            if FForm.RowCount < FTargetedRange.Bottom + 1 then
              FForm.RowCount := FTargetedRange.Bottom + 1;
            DeleteTarget(false);
            for A := 0 to F.ItemCount - 1 do
            begin
              T := F.GetItem(A);
              N := TEasyItem.Create(FForm);
              N.FRow := T.FRow + FTargetedRange.Top;
              N.FCol := T.FCol + FTargetedRange.Left;
              N.FRowSpan := T.FRowSpan;
              N.FColSpan := T.FColSpan;
              N.FText := T.FText;
              N.SetStyle(FForm.FStyles.Add(T.GetStyle));
              N.Ensure;
            end;
            ResetView;
          finally
            EndUpdate;
          end;
        finally
          F.Free;
        end;
      except
        { do nothing }
      end;
    finally
      M.Free;
    end;
  end;
end;

procedure TEasyView.Preview;
begin
  if Assigned(FOnPreview) then
    FOnPreview(Self);
end;

function TEasyView.GetSelectedItem: TEasyItem;
begin
  if Targeted then
    Result := FForm.FindByPos(FHot.X, FHot.Y) else
    Result := nil;
end;

procedure TEasyView.SetGridLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    if FMode = emDesign then
      FBody.Invalidate;
  end;
end;

function TEasyView.GetOption(AOption: TEasyOption): boolean;
begin
  Result := AOption in FOption;
end;

procedure TEasyView.SetOption(AOption: TEasyOption; Value: boolean);
begin
  if Value <> (AOption in FOption) then
  begin
    if Value then
      FOption := FOption + [AOption] else
      FOption := FOption - [AOption];
    BeginUpdate;
    try
      case AOption of
        eoShowRule:
        if FMode = emDesign then
        begin
          FHorzRulePanel.Visible := Value;
          FVertRule.Visible := Value;
          FForm.ResizeView;
        end;
        eoShowGridLine:
        if FMode = emDesign then
          FBody.Invalidate;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TEasyView.SetMode(Value: TEasyMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    ResetView;
    if not FTargeted and (FMode <> emDesign) then
      SeekNext(true);
  end;
end;

function CmpPos(Item1, Item2: TEasyItem; Horz: boolean): integer;
begin
  if Horz then
  begin
    Result := Item1.Row - Item2.Row;
    if Result = 0 then
      Result := Item1.Col - Item2.Col;
    end
  else
  begin
    Result := Item1.Col - Item2.Col;
    if Result = 0 then
      Result := Item1.Row - Item2.Row;
  end;
end;

procedure TEasyView.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    if FMode <> emDesign then
      FHorzRulePanel.Color := FBackground;
    FEditArea.Color := FBackground;
  end;
end;

procedure TEasyView.EventSelect(RepaintRules: boolean);
begin
  if RepaintRules and (FMode = emDesign) and ShowRule then
    if not (csDestroying in ComponentState) then
    begin
      FHorzRule.Invalidate;
      FVertRule.Invalidate;
    end;
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TEasyView.EventUndoStatus;
begin
  if Assigned(FOnUndoStatus) then
    FOnUndoStatus(Self);
end;

procedure TEasyView.EventChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEasyView.AddChangedRange(ARange: TRect);
begin
  BeginUpdate;
  try
    ARange := MakeRange(ARange, FForm.GetColCount, FForm.GetRowCount);
    if IsValidRange(FChangedRange) then
      FChangedRange := MakeRange(FChangedRange, ARange) else
      FChangedRange := ARange;
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.BeginUpdate;
begin
  FForm.BeginUpdate;
end;

procedure TEasyView.EndUpdate;
begin
  FForm.EndUpdate;
end;

procedure TEasyView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP: ExecUp(Shift);
    VK_DOWN:   ExecDown(Shift);
    VK_LEFT:   ExecLeft(Shift);
    VK_RIGHT:  ExecRight(Shift);
    VK_DELETE: ExecDelete(Shift);
    VK_F10:    Preview;
  end;
end;

procedure TEasyView.ExecChar(Ch: WideChar);
var
  W: WideString;
  N: TEasyItem;
begin
  if not FTargeted or (Ch = #0) then Exit;
  BeginUpdate;
  try
    if Ch = #13 then ExecDown([]) else
    begin
      N := FForm.FindByPos(FHot.X, FHot.Y);
      if Assigned(N) and not FKeyClear then
        W := N.FText else
        W := '';
      if Ch = #8 then {backspace}
        SetLength(W, Length(W) - 1) else
        W := W + Ch;
      if W <> '' then
        if N = nil then
          N := FForm.Force(FHot.X, FHot.Y);
      if N <> nil then
        N.SetText(W);
      FKeyClear := false;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.KeyPress(var Key: Char);
var
  S: WideString;
  N: TEasyItem;
begin
  if FTargeted and (Key <> #0) then
    if not (Key in LeadBytes) then
    begin
      FLeadChar := #0;
      ExecChar(WideCase(Key));
    end
    else
    if FKeyClear then
    begin
      FKeyClear := false;
      FLeadChar := Key;
      N := FForm.FindByPos(FHot.X, FHot.Y);
      if N <> nil then
        N.SetText('');
    end
    else
    if FLeadChar <> #0 then
    begin
      S := string(FLeadChar + Key);
      FLeadChar := #0;
      if S <> '' then
        ExecChar(S[1]);
    end
    else FLeadChar := Key;
  Key := #0;
end;

procedure TEasyView.Resize;
begin
  inherited;
  ResetView;
end;

procedure TEasyView.SeekNext(Horz: boolean);
var
  N: TEasyItem;
  P: TPoint;
begin
  if Targeted then
  begin
    N := FForm.FindByPos(FHot.x, FHot.y);
    if Assigned(N) then
      P := Classes.Point(N.Col + N.ColSpan - 1, FHot.y) else
      P := FHot;
  end
  else P := Classes.Point(-1, -1);
  N := FindDataItem(P.x, P.y, true, Horz);
  if Assigned(N) then
    P := Classes.Point(N.Col, N.Row) else
    P := Classes.Point(-1, -1);
  SelectRange(P, P);
end;

procedure TEasyView.SeekPrev(Horz: boolean);
var
  N: TEasyItem;
  P: TPoint;
begin
  if Targeted then
  begin
    N := FForm.FindByPos(FHot.x, FHot.y);
    if Assigned(N) then
      P := Classes.Point(N.Col + N.ColSpan - 1, FHot.y) else
      P := FHot;
  end
  else P := Classes.Point(-1, -1);
  N := FindDataItem(P.x, P.y, false, Horz);
  if Assigned(N) then
    P := Classes.Point(N.Col, N.Row) else
    P := Classes.Point(-1, -1);
  SelectRange(P, P);
end;

function TEasyView.FindDataItem(Col, Row: integer; Next, Horz: boolean): TEasyItem;
var
  P, N: TEasyItem;
  A: integer;
  F: boolean;
  function Refpos(Item1, Item2: TEasyItem): integer;
  begin
    if Horz then
    begin
      Result := Item1.Row - Item2.Row;
      if Result = 0 then
        Result := Item1.Col - Item2.Col;
      end
    else
    begin
      Result := Item1.Col - Item2.Col;
      if Result = 0 then
        Result := Item1.Row - Item2.Row;
    end;
    // convert prevward to nextward
    if not Next and (Result <> 0) then
      Result := -Result;
  end;
begin
  P := FForm.FindByPos(Col, Row);
  N := nil;
  for A := 0 to FForm.ItemCount - 1 do
  begin
    Result := FForm.Items[A];
    
    F := (Result.GetStyle.FItemType <> estTitle);
    if F and (Mode <> emDesign) then
      F := (Result.Col >= FForm.FFixedColCount) and
           (Result.Row >= FForm.FFixedRowCount);
    if not F then Continue;
    
    if N = nil then
    begin
      if Result <> P then N := Result;
    end
    else
    if P = nil then
    begin
      if Refpos(Result, N) < 0 then N := Result;
    end
    else
    if Refpos(Result, P) > 0 then
    begin
      if (Refpos(N, P) < 0) or (Refpos(Result, N) < 0) then
      N := Result;
    end
    else
    if (Refpos(N, P) < 0) and (Refpos(Result, N) < 0) then
      N := Result;
  end;
  Result := N;
end;

procedure TEasyView.MakeCellVisible(Col, Row: integer);
var
  view_w, view_h: integer;
  item_w, item_h: integer;
  N: TEasyItem;
  R, C: integer;
begin
  if FForm.HasCol(Col) and FForm.HasRow(Row) then
  begin  
    view_h := FBody.Height;
    view_w := FBody.Width;
    N := FForm.FindByPos(Col, Row);
    if N = nil then
    begin
      item_h := FForm.RowHeights[Row];
      item_w := FForm.ColWidths[Col];
    end
    else
    begin
      Col := N.FCol;
      Row := N.FRow;
      item_h := N.Height;
      item_w := N.Width;
    end;
    
    if Row > StartRow then
    begin
      Inc(item_h, RowToY(Row));
      R := StartRow;
      while (R < Row) and (item_h > view_h) do
      begin
        Dec(item_h, FForm.RowHeights[R] + 1);
        Inc(R);
      end;
      FVertScrollBar.Position := R;
    end
    else
    if Row < StartRow then
      FVertScrollBar.Position := Max(Row, FVertScrollBar.Min);
      
    if Col > StartCol then
    begin
      Inc(item_w, ColToX(Col));
      C := StartCol;
      while (C < Col) and (item_w > view_w) do
      begin
        Dec(item_w, FForm.ColWidths[C] + 1);
        Inc(C);
      end;
      FHorzScrollBar.Position := C;
    end
    else
    if Col < StartCol then
      FHorzScrollBar.Position := Max(Col, FHorzScrollBar.Min);
  end;
end;

procedure TEasyView.MakeFocusedCellVisible;
begin
  if Targeted then
    MakeCellVisible(HotPoint.x, HotPoint.y);
end;

procedure TEasyView.MergeTarget;
var
  N, T: TEasyItem;
  X, Y: integer;
begin
  if TryBeginUpdate(Targeted) then
  try
    EventChangingTarget;
    N := FForm.Force(FHot.x, FHot.y);
    for Y := FTargetedRange.Top to FTargetedRange.Bottom do
      for X := FTargetedRange.Left to FTargetedRange.Right do
      begin
        T := FForm.FindByPos(X, Y);
        if Assigned(T) and (T <> N) then T.Delete;
      end;
    with FTargetedRange do
      N.Resize(Left, Top, Right - Left + 1, Bottom - Top + 1);
    EventSelect(false);
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.VertRuleDrawRule;
var
  A: integer;
  H: integer;   {<--height of text}
  R: integer;   {<--row height}
  W: integer;   {<--FVertRule width}
  Y: integer;   {<--start position}
  F: boolean;   {<--if is target}
  S: string;
begin
  if FMode = emDesign then with FVertRule do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
    W := Width;
    H := Canvas.TextHeight('H');
    Y := 0;
    Canvas.Pen.Color := clBtnShadow;
    FVertRule.Canvas.MoveTo(0, Y);
    FVertRule.Canvas.LineTo(W, Y);
    for A := StartRow to FForm.RowCount - 1 do
    begin
      Inc(Y);
      R := FForm.RowHeights[A];

      F := Targeted and (A >= FTargetedRange.Top) and (A <= FTargetedRange.Bottom);
      if F then
        Canvas.Brush.Color := DefRuleColor else
        Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(0, Y, W, Y + R));

      if R > H then
      begin
        Canvas.Brush.Style := bsClear;
        S := IntToStr(A + 1);
        Canvas.TextOut((W - Canvas.TextWidth(S)) div 2, Y + ((R - H) div 2), S);
        Canvas.Brush.Style := bsSolid;
      end;

      Canvas.Pen.Color := RuleBorder[F];
      Canvas.MoveTo(0, Y);
      Canvas.LineTo(0, Y + R);
      Canvas.MoveTo(0, Y);
      Canvas.LineTo(W, Y);

      Canvas.Pen.Color := RuleBorder[not F];
      Canvas.MoveTo(W - 1, Y);
      Canvas.LineTo(W - 1, Y + R);
      Inc(Y, R);
      Canvas.MoveTo(0, Y);
      Canvas.LineTo(W, Y);
    end;
  end;
end;

procedure TEasyView.VertRuleDrawFixedCols;
var
  R: TRect;
begin
  FVertRule.Canvas.FillRect(FVertRule.ClientRect);
  R.Left := 0;
  R.Right := StartCol - 1;
  R.Top := StartRow;
  R.Bottom := R.Top + VisibleRowCount - 1;
  if IsValidRange(R) then
    DrawRanged(FVertRule, R);
end;

procedure TEasyView.HorzRuleDrawFixedRows;
var
  R: TRect;
begin
  FHorzRule.Canvas.FillRect(FHorzRule.ClientRect);
  R.Top := 0;
  R.Bottom := StartRow - 1;
  R.Left := StartCol;
  R.Right := R.Left + VisibleColCount - 1;
  if IsValidRange(R) then
    DrawRanged(FHorzRule, R);
end;

procedure TEasyView.HorzRuleDrawRule;
var
  A: integer;
  H: integer;    {<--FHorzRule height}
  W: integer;    {<--text width}
  X: integer;    {<--start position}
  C: integer;    {<--col width}
  Y: integer;    {<--Y position where draw text}
  F: boolean;    {<--if in target}
  S: string;
begin
  if FMode = emDesign then with FHorzRule do
  begin
    Canvas.FillRect(ClientRect);
    H := Height;
    Y := (H - Canvas.TextHeight('H')) div 2;
    X := 0;
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(X, 0);
    Canvas.LineTo(X, H);
    for A := StartCol to FForm.ColCount - 1 do
    begin
      Inc(X);
      C := FForm.ColWidths[A];
      S := EncodeColumnID(A + 1);
      W := Canvas.TextWidth(S);

      F := Targeted and (A >= FTargetedRange.Left) and (A <= FTargetedRange.Right);
      if F then
        Canvas.Brush.Color := DefRuleColor else
        Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(X, 0, X + C, H));

      if C >= W then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(X + ((C - W) div 2), Y, S);
        Canvas.Brush.Style := bsSolid;
      end;

      Canvas.Pen.Color := RuleBorder[F];
      Canvas.MoveTo(X, 0);
      Canvas.LineTo(X, H);
      Canvas.MoveTo(X, 0);
      Canvas.LineTo(X + C, 0);

      Canvas.Pen.Color := RuleBorder[not F];
      Canvas.MoveTo(X, H - 1);
      Canvas.LineTo(X + C, H - 1);
      Inc(X, C);
      Canvas.MoveTo(X, 0);
      Canvas.LineTo(X, H);
    end;
  end;
end;

function TEasyView.HorzScrollBarMax(ViewWidth: integer): integer;
var
  start_col, w: integer;
begin
  Result := FForm.ColCount - 1;
  if FForm.ColCount > 1 then
  begin
    if FMode = emDesign then
      start_col := 0 else
      start_col := FForm.FixedColCount - 1;
    w := FForm.GetColWidth(Result) + 2;
    if (w < ViewWidth) and (Result > start_col) then
    begin
      repeat
        Dec(Result);
        Inc(w, FForm.GetColWidth(Result) + 1);
      until (w >= ViewWidth) or (Result = start_col);
      if w > ViewWidth then
        Inc(Result);
    end;
  end;
end;

function TEasyView.CalcTargetRange(ARect: TRect): TRect;
var
  A, B: integer;
  L: TList;
  N: TEasyItem;
begin
  L := TList.Create;
  try
    B := 0;
    for A := 0 to FForm.GetItemCount - 1 do
    begin
      N := FForm.GetItem(A);
      if N.Intersects(ARect) then
      begin
        ARect := MakeRange(ARect, N.ItemRange);
        Inc(B);
      end
      else L.Add(N);
    end;
    while B > 0 do
    begin
      B := 0;
      for A := L.Count - 1 downto 0 do
      begin
        N := TEasyItem(L[A]);
        if N.Intersects(ARect) then
        begin
          ARect := MakeRange(ARect, N.ItemRange);
          Inc(B);
          L.Delete(A);
        end;
      end;
    end;
    Result := ARect;
  finally
    L.Free;
  end;
end;

function TEasyView.VertScrollBarMax(ViewHeight: integer): integer;
var
  start_row, h: integer;
begin
  Result := FForm.RowCount - 1;
  if FForm.RowCount > 1 then
  begin
    if FMode = emDesign then
      start_row := 0 else
      start_row := FForm.FixedRowCount - 1;
    h := FForm.GetRowHeight(Result) + 2;
    if (h < ViewHeight) and (Result > start_row) then
    begin
      repeat
        Dec(Result);
        Inc(h, FForm.GetRowHeight(Result) + 1);
      until (h >= ViewHeight) or (Result = start_row);
      if h > ViewHeight then
        Inc(Result);
    end;
  end;
end;

procedure TEasyView.CornerPaint(Sender: TObject);
var
  R: TRect;
begin
  R := MakeRect(0, 0, FForm.FFixedColCount - 1, FForm.FFixedRowCount - 1);
  if IsValidRange(R) then
    DrawRanged(FCorner, R);
end;

procedure TEasyView.CornerLTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then SetFocus;
  inherited;
end;

procedure TEasyView.RefreshSelected;
begin
  if Targeted then
    DrawRanged(FBody, FTargetedRange);
end;

procedure TEasyView.DrawChangedRange;
var
  R: TRect;
begin
  if IsValidRange(FChangedRange) then
  begin
    R := MakeRange(FChangedRange, FForm.GetColCount, FForm.GetRowCount);
    if IsValidRange(R) then
      DrawRanged(FBody, R);
    FChangedRange.Right := -1;
  end;
end;

procedure TEasyView.DrawRanged(ABox: TPaintBox; Range: TRect);
var
  block: TRect;
  items: TList;
  row, col, I, X, Y, start_col, start_row: integer;
  
  procedure draw_cell;
  begin
    X := FForm.ColDistance(col, start_col) + 1;
    Y := FForm.RowDistance(row, start_row) + 1;
    Range := MakeRect(X, Y, X + FForm.ColWidths[col], Y + FForm.RowHeights[row]);
    ABox.Canvas.Brush.Style := bsSolid;
    ABox.Canvas.Brush.Color := FBackground;
    if Targeted and XyInRange(FTargetedRange, col, row) then
    begin
      ABox.Canvas.Pen.Color := DefHotColor;
      if (col = FHot.x) and (row = FHot.y) then
        ABox.Canvas.Rectangle(Range);
      Range := ZoomRect(Range, 1, 1);
      ABox.Canvas.Brush.Style := bsClear;
      ABox.Canvas.Rectangle(Range);
    end
    else
    begin
      ABox.Canvas.Pen.Color := FBackground;
      ABox.Canvas.Rectangle(Range);
    end;
  end;

  procedure draw_item(item: TEasyItem);
  begin
    col := item.Col;
    row := item.Row;
    X := FForm.ColDistance(col, start_col) + 1;
    if col < start_col then X := 2 - X;
    Y := FForm.RowDistance(row, start_row) + 1;
    if row < start_row then Y := 2 - Y;
    Range := MakeRect(X, Y, X + item.Width, Y + item.Height);
    ABox.Canvas.Brush.Style := bsSolid;
    item.Draw(ABox.Canvas, Range.TopLeft, false);
    if Targeted and XyInRange(FTargetedRange, col, row) then
    begin
      ABox.Canvas.Brush.Style := bsClear;
      ABox.Canvas.Pen.Color := DefHotColor;
      if item.Selected then
        ABox.Canvas.Rectangle(Range);
      Range := ZoomRect(Range, 1, 1);
      ABox.Canvas.Rectangle(Range);
    end;
  end;

begin
  if ABox = FBody then
  begin
    block := VisibleRange;
    start_col := StartCol;
    start_row := StartRow;
    BodyDrawGridLine;
  end
  else
  if ABox = FCorner then
  begin
    block := Range;
    start_col := 0;
    start_row := 0;
  end
  else
  if ABox = FHorzRule then
  begin
    block := Range;
    start_col := StartCol;
    start_row := 0;
  end
  else
  if ABox = FVertRule then
  begin
    block := Range;
    start_col := 0;
    start_row := StartRow;
  end
  else Exit;
  block.Left := Max(block.Left, Range.Left);
  block.Top := Max(block.Top, Range.Top);
  block.Right := Min(block.Right, Range.Right);
  block.Bottom := Min(block.Bottom, Range.Bottom);
  items := FForm.GetItemsInRange(block);
  try
    for col := block.Left to block.Right do
      for row := block.Top to block.Bottom do
        if FindEasyItem(items, col, row) = nil then
          draw_cell;
    for I := 0 to items.Count - 1 do
      draw_item(TEasyItem(items[I]));
  finally
    items.Free;
  end;
end;

function TEasyView.GetModified: boolean;
begin
  Result := FForm.GetModified;
end;

procedure TEasyView.SetModified(Value: boolean);
begin
  FForm.SetModified(Value);
end;

procedure TEasyView.Repaint;
begin
  FCorner.Invalidate;
  FHorzRule.Invalidate;
  FVertRule.Invalidate;
  FBody.Invalidate;
end;

procedure TEasyView.ListTarget(List: TList);
var
  X, Y: integer;
  N: TEasyItem;
begin
  List.Clear;
  if not Targeted then Exit;
  for Y := FTargetedRange.Top to FTargetedRange.Bottom do
    for X := FTargetedRange.Left to FTargetedRange.Right do
    begin
      N := FForm.FindByPos(X, Y);
      if (N <> nil) and (List.IndexOf(N) < 0) then List.Add(N);
    end;
end;

procedure TEasyView.SaveTargetToStream(Stream: TEasyStream);
var
  L: TList;
  X: integer;
begin
  L := TList.Create;
  try
    ListTarget(L);
    Stream.write_longint(FTargetedRange.Left);
    Stream.write_longint(FTargetedRange.Top);
    Stream.write_longint(FTargetedRange.Right);
    Stream.write_longint(FTargetedRange.Bottom);
    Stream.write_longint(L.Count);
    for X := 0 to L.Count - 1 do
      TEasyItem(L[X]).SaveToStream(Stream);
  finally
    L.Free;
  end;
end;

procedure TEasyView.LoadTargetFromStream(Stream: TEasyStream);
var
  X, Y: integer;
  N: TEasyItem;
  R: TRect;
begin
  R.Left := Stream.read_longint;
  R.Top := Stream.read_longint;
  R.Right := Stream.read_longint;
  R.Bottom := Stream.read_longint;
  for Y := R.Top to R.Bottom do
    for X := R.Left to R.Right do
    begin
      N := FForm.FindByPos(X, Y);
      if N <> nil then N.Delete;
    end;
  Y := Stream.read_longint;
  for X := 0 to Y - 1 do
    TEasyItem.Create(FForm).LoadFromStream(Stream);
end;

procedure TEasyView.EventChangingTarget;
begin
  UndoSaveChangeTarget;
  if Assigned(FOnChangingTarget) then
    FOnChangingTarget(Self);
end;

procedure TEasyView.EventPasting;
begin
  UndoSaveChange;
  if Assigned(FOnPasting) then
    FOnPasting(Self);
end;

procedure TEasyView.EventInsertingColumn(Index: integer);
begin
  UndoSaveInsertColumn(Index);
  if Assigned(FOnInsertingColumn) then
    FOnInsertingColumn(Self, Index);
end;

procedure TEasyView.EventInsertingRow(Index: integer);
begin
  UndoSaveInsertRow(Index);
  if Assigned(FOnInsertingRow) then
    FOnInsertingRow(Self, Index);
end;

procedure TEasyView.EventChangingColumnWidth(Index: integer);
begin
  UndoSaveColumnWidth(Index);
  if Assigned(FOnChangingColumnWidth) then
    FOnChangingColumnWidth(Self, Index);
end;

procedure TEasyView.EventChangingRowHeight(Index: integer);
begin
  UndoSaveRowHeight(Index);
  if Assigned(FOnChangingRowHeight) then
    FOnChangingRowHeight(Self, Index);
end;

procedure TEasyView.Sort;
begin
  FForm.Sort;
end;

function TEasyView.TryBeginUpdate(Condition: boolean): boolean;
begin
  Result := FForm.TryBeginUpdate(Condition);
end;

function TEasyView.Undo: boolean;
var
  ur: PUndoRec;
  ux: integer;

  procedure undo_resize;
  var
    X: integer;
  begin
    for X := 0 to FForm.ColCount - 1 do
      FForm.ColWidths[X] := FUndoStream.read_longint;
    for X := 0 to FForm.RowCount - 1 do
      FForm.RowHeights[X] := FUndoStream.read_longint;
  end;
  
begin
  ux := UndoCount - 1;
  Result := (ux >= 0);
  if Result then
  begin
    ur := UndoGet(ux);
    FUndoStream.Position := ur^.ur_spos;
    FUndoing := true;
    BeginUpdate;
    try
      case ur^.ur_oper of
        undoChangeTarget: LoadTargetFromStream(FUndoStream);
        undoChange      : Form.LoadFromStream(FUndoStream);
        undoResize      : undo_resize;
        undoInsertRow   : FForm.DeleteRow(ur^.ur_index);
        undoInsertColumn: FForm.DeleteColumn(ur^.ur_index);
        undoChangeHeight: FForm.RowHeights[ur^.ur_index] := ur^.ur_size;
        undoChangeWidth : FForm.ColWidths[ur^.ur_index] := ur^.ur_size;
      end;
      SelectRange(ur^.ur_range.TopLeft, ur^.ur_range.BottomRight);
      UndoDeleteLast;
    finally
      EndUpdate;
      FUndoing := false;
    end;
  end;
end;

procedure TEasyView.Print;
begin
  if Assigned(FOnPrint) then
    FOnPrint(Self) else
    FForm.Print;
end;

function TEasyView.GetTargetedRangeID: string;
begin
  if FTargeted then
    Result := FForm.GetRangeID(FTargetedRange) else
    Result := '';
end;

function TEasyView.UndoCount: integer;
begin
  if FundoList <> nil then
    Result := FUndoList.Count else
    Result := 0;
end;

function TEasyView.UndoGet(Index: integer): PUndoRec;
begin
  Result := PUndoRec(FUndoList[Index]);
end;

procedure TEasyView.SetTargetedRangeID(const Value: string);
var
  range: TRect;
begin
  if DecodeRangeID(Value, range) then
    SelectRange(range.TopLeft, range.BottomRight);
end;

procedure TEasyView.SetTargetStyle(AStyle: TEasyStyle);
var
  X, Y: integer;
begin
  if TryBeginUpdate(Targeted) then
  try
    EventChangingTarget;
    for Y := FTargetedRange.Top to FTargetedRange.Bottom do
      for X := FTargetedRange.Left to FTargetedRange.Right do
        FForm.Force(X, Y).Style := AStyle;
  finally
    EndUpdate;
  end;
end;

procedure TEasyView.SetTargetText(const NewText: string; IgnoreReadonly: boolean);
var
  X, Y: integer;
  N: TEasyItem;
begin
  if TryBeginUpdate(Targeted) then
  try
    EventChangingTarget;
    for Y := FTargetedRange.Top to FTargetedRange.Bottom do
      for X := FTargetedRange.Left to FTargetedRange.Right do
      begin
        N := FForm.Force(X, Y);
        if not N.Style.IsReadonly or IgnoreReadonly or (FMode = emDesign) then
          N.SetText(NewText);
      end;
  finally
    EndUpdate;
  end;
end;

function TEasyView.GetHotPoint: TPoint;
begin
  Result := FHot;
end;

function TEasyView.GetEndPoint: TPoint;
begin
  Result := FEnd;
end;

{ TEasyStyle }

procedure TEasyStyle.Assign(AStyle: TEasyStyle);
begin
  BeginUpdate;
  try
    if (AStyle <> nil) and (AStyle <> Self) then
    begin
      FontName := AStyle.FontName;
      FontSize := AStyle.FontSize;
      FontStyle := AStyle.FontStyle;
      TextColor := AStyle.TextColor;
      Background := AStyle.Background;
      HAlign := AStyle.HAlign;
      VAlign := AStyle.VAlign;
      LineDistance := AStyle.LineDistance;
      WordWrap := AStyle.WordWrap;
      ItemType := AStyle.ItemType;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TEasyStyle.Create(const Name: string; Builtin: boolean);
begin
  FName := Name;
  FBuiltin := Builtin;
  if FBuiltin then IncRefcount;
  FStyleList := TList.Create;
  FFontName := DefFontName;
  FFontSize := DefFontSize;
  FFontStyle := DefFontStyles;
  FTextColor := DefTextColor;
  FBackground := DefBackground;
  FHAlign := DefHAlign;
  FVAlign := DefVAlign;
  FLineDistance := DefLineDistance;
  FItemType := estNormal;
end;

destructor TEasyStyle.Destroy;
var
  index: integer;
  list: TEasyStyles;
begin
  for index := FStyleList.Count - 1 downto 0 do
  begin
    list := TEasyStyles(FStyleList[index]);
    FStyleList.Delete(index);
    list.FList.Remove(Self);
  end;
  FStyleList.Free;
  inherited;
end;

procedure TEasyStyle.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TEasyStyle.GetFontStyleText: string;
var
  list: TStrings;
begin
  list := TStringList.Create;
  try
    if fsBold in FFontStyle then list.Add('粗体');
    if fsItalic in FFontStyle then list.Add('斜体');
    if fsUnderline in FFontStyle then list.Add('下划线');
    if fsStrikeOut in FFontStyle then list.Add('删除线');
    Result := list.CommaText;
  finally
    list.Free;
  end;
end;

function TEasyStyle.GetStyle(const Index: TFontStyle): boolean;
begin
  Result := Index in FFontStyle;
end;

function TEasyStyle.IsNormal: boolean;
begin
  Result := (FItemType = estNormal);
end;

function TEasyStyle.IsReadonly: boolean;
begin
  Result := (FItemType <> estNormal);
end;

function TEasyStyle.IsTitle: boolean;
begin
  Result := (FItemType = estTitle);
end;

function TEasyStyle.Match(const AFontName: string;
                          AFontSize: integer;
                          AFontStyle: TFontStyles;
                          ATextColor, ABackColor: TColor;
                          AHAlign: TEasyHorzAlign;
                          AVAlign: TEasyVertAlign;
                          ALineDistance: integer;
                          AWordWrap: boolean;
                          AItemType: TEasyStyleType;
                          AEdges: TEasyEdges): boolean;
begin
  Result := AnsiSameText(AFontName, FFontName) and
            (AFontSize = FFontSize) and
            (AFontStyle = FFontStyle) and
            (ATextColor = FTextColor) and
            (ABackColor = FBackground) and
            (AHAlign = FHAlign) and
            (AVAlign = FVAlign) and
            (ALineDistance = FLineDistance) and
            (AWordWrap = FWordWrap) and
            (AItemType = FItemType) and
            (AEdges = FEdges);
end;

procedure TEasyStyle.Read(AFont: TFont);
begin
  FontName := AFont.Name;
  FontSize := AFont.Size;
  FontStyle := AFont.Style;
  TextColor := AFont.Color;
end;

procedure TEasyStyle.SetBackColor(const Value: TColor);
begin
  BeginUpdate;
  try
    if FBackground <> Value then
    begin
      FBackground := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetEdges(const Value: TEasyEdges);
begin
  BeginUpdate;
  try
    if FEdges <> Value then
    begin
      FEdges := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetFontColor(const Value: TColor);
begin
  BeginUpdate;
  try
    if FTextColor <> Value then
    begin
      FTextColor := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetFontName(const Value: string);
begin
  BeginUpdate;
  try
    if FFontName <> Value then
    begin
      FFontName := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetFontSize(const Value: integer);
begin
  BeginUpdate;
  try
    if FFontSize <> Value then
    begin
      FFontSize := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetFontStyle(const Value: TFontStyles);
begin
  BeginUpdate;
  try
    if FFontStyle <> Value then
    begin
      FFontStyle := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetHAlign(const Value: TEasyHorzAlign);
begin
  BeginUpdate;
  try
    if FHAlign <> Value then
    begin
      FHAlign := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetItemType(const Value: TEasyStyleType);
begin
  BeginUpdate;
  try
    if FItemType <> Value then
    begin
      FItemType := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetLineDistance(const Value: integer);
begin
  BeginUpdate;
  try
    if FLineDistance <> Value then
    begin
      FLineDistance := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetModified(const Value: boolean);
begin
  BeginUpdate;
  try
    FModified := Value;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetName(const Value: string);
begin
  BeginUpdate;
  try
    if not FBuiltin and (FName <> Value) then
    begin
      FName := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetStyle(const Index: TFontStyle; const Value: boolean);
begin
  if Value then
    SetFontStyle(FFontStyle + [Index]) else
    SetFontStyle(FFontStyle - [Index]);
end;

procedure TEasyStyle.SetVAlign(const Value: TEasyVertAlign);
begin
  BeginUpdate;
  try
    if FVAlign <> Value then
    begin
      FVAlign := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.SetWordWrap(const Value: boolean);
begin
  BeginUpdate;
  try
    if FWordWrap <> Value then
    begin
      FWordWrap := Value;
      FModified := true;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TEasyStyle.Write(AFont: TFont);
begin
  AFont.Name := FontName;
  AFont.Size := FontSize;
  AFont.Style := FontStyle;
  AFont.Color := TextColor;
end;

{ TEasyFontList }

function TEasyStyles.Add(AStyle: TEasyStyle): TEasyStyle;
begin
  Result := AStyle;
  if FList.IndexOf(AStyle) < 0 then
  begin
    if (AStyle = ss_normal) or (Find(AStyle.FName) = ss_normal) then
    begin
      FList.Add(AStyle);
      AStyle.IncRefcount;
      AStyle.FStyleList.Add(Self);
    end
    else Result := Force(AStyle.FFontName, AStyle.FFontSize, AStyle.FFontStyle,
                         AStyle.FTextColor, AStyle.FBackground, AStyle.HAlign,
                         AStyle.VAlign, AStyle.LineDistance, AStyle.FWordWrap,
                         AStyle.FItemType, AStyle.FEdges);
    Change(Self);
  end;
end;

procedure TEasyStyles.Change(Sender: TObject);
begin
  if FForm <> nil then
    FForm.Modified := true;
end;

procedure TEasyStyles.Clear;
var
  index: integer;
begin
  for index := GetCount - 1 downto 0 do
    Delete(index);
end;

procedure TEasyStyles.ClearUserStyles;
var
  index: integer;
begin
  for index := GetCount - 1 downto 0 do
    if not GetItem(index).Builtin then
      Delete(index);
end;

constructor TEasyStyles.Create(AForm: TEasyForm);
begin
  FForm := AForm;
  FList := TList.Create;
  Add(ss_normal);
end;

procedure TEasyStyles.Delete(index: integer);
var
  S: TEasyStyle;
  X: integer;
  M: TEasyItem;
begin
  S := GetItem(index);
  FList.Delete(index);
  S.FStyleList.Remove(Self);
  if FForm <> nil then
    for X := 0 to FForm.GetItemCount - 1 do
    begin
      M := FForm.GetItem(X);
      if M.FStyle = S.FName then
        M.SetStyle(nil);
    end;
  S.DecRefcount;
  Change(Self);
end;

destructor TEasyStyles.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TEasyStyles.Exists(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function TEasyStyles.Find(const Name: string): TEasyStyle;
var
  index: integer;
begin
  index := IndexOf(Name);
  if index >= 0 then
    Result := GetItem(index) else
    Result := ss_normal;
end;

function TEasyStyles.Force(const AFontName: string;
                           AFontSize: integer;
                           AFontStyle: TFontStyles;
                           ATextColor, ABackground: TColor;
                           AHAlign: TEasyHorzAlign;
                           AVAlign: TEasyVertAlign;
                           ALineDistance: integer;
                           AWordWrap: boolean;
                           AItemType: TEasyStyleType;
                           AEdges: TEasyEdges): TEasyStyle;
var
  index: integer;
begin
  for index := 0 to GetCount - 1 do
  begin
    Result := GetItem(index);
    if Result.Match(AFontName, AFontSize, AFontStyle, ATextColor, ABackground,
      AHAlign, AVAlign, ALineDistance, AWordWrap, AItemType, AEdges) then Exit;
  end;
  Result := TEasyStyle.Create(NewStyleName, false);
  Result.FontName := AFontName;
  Result.FontSize := AFontSize;
  Result.FontStyle := AFontStyle;
  Result.TextColor := ATextColor;
  Result.Background := ABackground;
  Result.HAlign := AHAlign;
  Result.VAlign := AVAlign;
  Result.LineDistance := ALineDistance;
  Result.WordWrap := AWordWrap;
  Result.ItemType := AItemType;
  Result.Edges := AEdges;
  Add(Result);
end;

function TEasyStyles.GetCount: integer;
begin
  Result := FList.Count;
end;

function TEasyStyles.GetItem(index: integer): TEasyStyle;
begin
  Result := TEasyStyle(FList[index]);
end;

function TEasyStyles.IndexOf(const Name: string): integer;
var
  index: integer;
begin
  for index := 0 to GetCount - 1 do
    if AnsiSameText(Name, GetItem(index).Name) then
    begin
      Result := index;
      Exit;
    end;
  Result := -1;
end;

procedure TEasyStyles.ListTo(const List: TStrings; Despite: TEasyStyle);
var
  index: integer;
  style: TEasyStyle;
begin
  for index := 0 to GetCount - 1 do
  begin
    style := GetItem(index);
    if style <> Despite then
      List.AddObject(style.Name, style);
  end;
end;

function TEasyStyles.NewStyleName: string;
var
  index: integer;
begin
  for index := 1 to 99999 do
  begin
    Result := Format('ns_%.5d', [index]);
    if not Exists(Result) then Exit;
  end;
  Result := '';
end;

procedure TEasyStyles.Remove(AStyle: TEasyStyle);
var
  index: integer;
begin
  index := FList.IndexOf(AStyle);
  if index >= 0 then
    Delete(index);
end;

procedure TEasyStyles.RemoveBy(const Name: string);
var
  index: integer;
begin
  index := IndexOf(Name);
  if index >= 0 then
    Delete(index);
end;

const
  efx_types: array[TEfxObject] of RLseClassRec = (
    (vtype       : LSV_OBJECT;
     name        : 'efxview';
     desc        : 'easy form editor';
     incRefcount : {$IFDEF FPC}@{$ENDIF}lse_IncRefCount;
     decRefcount : {$IFDEF FPC}@{$ENDIF}lse_DecRefCount;
     funcs       : (count:efxview_func_count; entry:@efxview_func_array);
    ),
    (vtype       : LSV_OBJECT;
     name        : 'efxbook';
     desc        : 'easy form book';
     incRefcount : {$IFDEF FPC}@{$ENDIF}lse_IncRefCount;
     decRefcount : {$IFDEF FPC}@{$ENDIF}lse_DecRefCount;
     funcs       : (count:efxbook_func_count; entry:@efxbook_func_array);
    ),
    (vtype       : LSV_OBJECT;
     name        : 'efxpage';
     desc        : 'easy form page';
     incRefcount : {$IFDEF FPC}@{$ENDIF}lse_IncRefCount;
     decRefcount : {$IFDEF FPC}@{$ENDIF}lse_DecRefCount;
     funcs       : (count:efxpage_func_count; entry:@efxpage_func_array);
    ),
    (vtype       : LSV_OBJECT;
     name        : 'efxcell';
     desc        : 'easy form page cell';
     incRefcount : {$IFDEF FPC}@{$ENDIF}lse_IncRefCount;
     decRefcount : {$IFDEF FPC}@{$ENDIF}lse_DecRefCount;
     funcs       : (count:efxcell_func_count; entry:@efxcell_func_array);
    )
  );

  efx_initr: RLseModuleRec = (
    iw_version   : '1.0';
    iw_desc      : 'efx module';
    iw_classes   : (count:Length(efx_types); entry:@efx_types);
    iw_libfuncs  : (count:efx_func_count; entry:@efx_func_array);
  );

var
  efx_books: TList;

function SetupEfxModule(const fname: string): boolean;
begin
  if efx_books = nil then
//  if lseu.lse_ensure_load_kernel then
      if lse_module_setup('efx', fname, @efx_initr) <> nil then
        efx_books := TList.Create;
  Result := Assigned(efx_books);
end;

function EfxClass(Index: TEfxObject): pointer;
begin
  Result := efx_types[Index].lysee_class;
end;

{ efx module }

procedure efx_BookCount(Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    invoker.returnInt(efx_books.Count);
  except
    invoker.returnError(efx_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efx_Book(Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    invoker.returnObject(EfxClass(efxBook), efx_books[invoker.paramInt(0)]);
  except
    invoker.returnError(efx_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efx_CellID(Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    invoker.returnStr(EncodeRangeID(invoker.paramInt(1), invoker.paramInt(0)));
  except
    invoker.returnError(efx_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efx_RangeID(Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    invoker.returnStr(EncodeRangeID(
      invoker.paramInt(1), invoker.paramInt(0),
      invoker.paramInt(3), invoker.paramInt(2)));
  except
    invoker.returnError(efx_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

{ efxview }

procedure efxview_Book(Param: pointer);cdecl;
var
  view: TEfxView;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(view) then
      invoker.returnObject(EfxClass(efxBook), view.Book);
  except
    invoker.returnError(efxview_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxview_PageCount(Param: pointer);cdecl;
var
  view: TEfxView;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(view) then
      invoker.returnInt(view.PageCount);
  except
    invoker.returnError(efxview_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxview_Page(Param: pointer);cdecl;
var
  view: TEfxView;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(view) then
      invoker.returnObject(EfxClass(efxPage),
        view.Page[invoker.paramInt(1)]);
  except
    invoker.returnError(efxview_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

{ efxbook }

procedure efxbook_PageCount(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      invoker.returnInt(book.PageCount);
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_Page(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      invoker.returnObject(EfxClass(efxPage),
        book.Page[invoker.paramInt(1)]);
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_LoadFromFile(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      book.LoadFromFile(invoker.paramStr(1));
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_SaveToFile(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      book.SaveToFile(invoker.paramStr(1));
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_Clear(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      book.Clear;
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_AddPage(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      invoker.returnObject(EfxClass(efxPage), book.AddPage);
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_DeletePage(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      book.Delete(invoker.paramInt(1));
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_View(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      invoker.returnObject(EfxClass(efxView), book.View);
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_GetModified(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      invoker.returnBool(book.Modified);
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxbook_SetModified(Param: pointer);cdecl;
var
  book: TEfxBook;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(book) then
      book.Modified := invoker.paramBool(1);
  except
    invoker.returnError(efxbook_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

{ efxpage }

procedure efxpage_Book(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnObject(EfxClass(efxBook), page.Book);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_View(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnObject(EfxClass(efxView), page.View);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetRowCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.RowCount);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetRowCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.RowCount := invoker.paramInt(1);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetName(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnStr(page.Name);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetName(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.Name := Trim(invoker.paramStr(1));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetColCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.ColCount);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetColCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.ColCount := invoker.paramInt(1);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetFixedRowCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.FixedRowCount);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetFixedRowCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.FixedRowCount := invoker.paramInt(1);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetFixedColCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.FixedColCount);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetFixedColCount(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.FixedColCount := invoker.paramInt(1);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Clear(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.Clear;
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Clone(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnObject(EfxClass(efxPage), page.Clone);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Assign(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.Assign(TEfxPage(invoker.paramObject(1)));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Sum(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnFloat(page.Sum(invoker.paramStr(1)));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Average(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnFloat(page.Average(invoker.paramStr(1)));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Max(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnFloat(page.MaxValue(invoker.paramStr(1)));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Min(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnFloat(page.MinValue(invoker.paramStr(1)));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_InsertRow(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.InsertRow(invoker.paramInt(1));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_InsertCol(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.InsertCol(invoker.paramInt(1));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_DeleteRow(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.DeleteRow(invoker.paramInt(1));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_DeleteCol(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.DeleteCol(invoker.paramInt(1));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_AddRow(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.AddRow;
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_AddCol(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.AddCol;
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetRowHeight(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.RowHeight[invoker.paramInt(1)]);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetRowHeight(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.RowHeight[invoker.paramInt(1)] := invoker.paramInt(2);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_GetColWidth(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.ColWidth[invoker.paramInt(1)]);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_SetColWidth(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.ColWidth[invoker.paramInt(1)] := invoker.paramInt(2);
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_ColToIndex(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      invoker.returnInt(page.ColumnIndex(invoker.paramStr(1)));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Cell(Param: pointer);cdecl;
var
  page: TEfxPage;
  cell: TEfxCell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  with invoker do
  try
    if GetThis(page) then
    begin
      cell := page.GetCellByPos(paramStr(1), paramBool(2));
      returnObject(EfxClass(efxCell), cell);
    end;
  except
    returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_ErrorAt(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.ErrorAt(invoker.paramStr(1), invoker.paramStr(2));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_Check(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.Check(invoker.paramStr(1), invoker.paramStr(2),
        invoker.paramStr(3), invoker.paramStr(4),
        invoker.paramStr(5), invoker.paramStr(6));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxpage_CheckHL(Param: pointer);cdecl;
var
  page: TEfxPage;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(page) then
      page.CheckHiLo(invoker.paramStr(1), invoker.paramStr(2),
        invoker.paramStr(3), invoker.paramStr(4), invoker.paramStr(5),
        invoker.paramInt(6), invoker.paramInt(7));
  except
    invoker.returnError(efxpage_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

{ efxcell }

procedure efxcell_View(Param: pointer);cdecl;
var
  cell: TEfxCell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnObject(EfxClass(efxView), cell.View);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_Book(Param: pointer);cdecl;
var
  cell: TEfxCell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnObject(EfxClass(efxBook), cell.Book);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_Page(Param: pointer);cdecl;
var
  cell: TEfxCell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnObject(EfxClass(efxPage), cell.Page);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_SetText(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      cell.Text := invoker.paramStr(1);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_GetText(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnStr(cell.Text);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_SetRow(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      cell.Row := invoker.paramInt(1);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_GetRow(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnInt(cell.Row);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_SetCol(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      cell.Col := invoker.paramInt(1);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_GetCol(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnInt(cell.Col);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_SetRowSpan(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      cell.RowSpan := invoker.paramInt(1);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_GetRowSpan(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnInt(cell.RowSpan);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_SetColSpan(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      cell.ColSpan := invoker.paramInt(1);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure efxcell_GetColSpan(Param: pointer);cdecl;
var
  cell: Tefxcell;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(param);
  try
    if invoker.GetThis(cell) then
      invoker.returnInt(cell.ColSpan);
  except
    invoker.returnError(efxcell_error, 0, lse_exception_str);
  end;
  invoker.Free;
end;

{ TEfxView }

constructor TEfxView.Create(ABook: TEfxBook);
begin
  FBook := ABook;
  FBook.FView := Self;
  FView := FBook.FForm.View;
end;

destructor TEfxView.Destroy;
begin
  if Assigned(FBook) then
  begin
    FBook.FView := nil;
    FBook := nil;
  end;
  inherited;
end;

function TEfxView.GetPage(Index: integer): TEfxPage;
begin
  Result := FBook.GetPage(Index);
end;

function TEfxView.GetPageCount: integer;
begin
  Result := 1;
end;

{ TEfxBook }

function TEfxBook.AddPage: TEfxPage;
begin
  Result := TEfxPage.Create(Self, FForm);
end;

procedure TEfxBook.Clear;
begin
  FForm.Clear;
end;

constructor TEfxBook.Create(AForm: TEasyForm);
begin
  FForm := AForm;
  FForm.OnDestroying := {$IFDEF FPC}@{$ENDIF}OnDestroying;
  FList := TList.Create;
  efx_books.Add(Self);
end;

procedure TEfxBook.Delete(Index: integer);
begin
  FForm.Delete(Index);
end;

destructor TEfxBook.Destroy;
begin
  OnDestroying(Self);
  inherited;
end;

procedure TEfxBook.DestroyingPage(Page: TEfxPage);
begin
  if Assigned(FOnDestroyingPage) then
    FOnDestroyingPage(Self, Page);
end;

procedure TEfxBook.ErrorAt(Page: TEfxPage; const Cell, Msg: string);
begin
  if Assigned(FOnErrorAt) then
    FOnErrorAt(Self, Page, Cell, Msg);
end;

function TEfxBook.GetCount: integer;
begin
  Result := 1;
end;

function TEfxBook.GetModified: boolean;
begin
  Result := FForm.Modified;
end;

function TEfxBook.GetPage(Index: integer): TEfxPage;
begin
  Result := MakeUp(FForm);
end;

function TEfxBook.GetView: TEfxView;
begin
  if (FView = nil) and (FForm.View <> nil) and
    not (csDestroying in FForm.View.ComponentState) then
      TEfxView.Create(Self);
  Result := FView;
end;

procedure TEfxBook.LoadFromFile(const fname: string);
begin
  if FForm.View <> nil then
    FForm.View.LoadFromFile(fname) else
    FForm.LoadFromFile(fname);
end;

function TEfxBook.MakeUp(AForm: TEasyForm): TEfxPage;
var
  index: integer;
begin
  if Assigned(AForm) then
  begin
    for index := 0 to FList.Count - 1 do
    begin
      Result := TEfxPage(FList[index]);
      if Result.FPage = AForm then Exit;
    end;
    Result := TEfxPage.Create(Self, AForm);
  end
  else Result := nil;
end;

procedure TEfxBook.OnDestroying(Sender: TObject);
var
  A: integer;
begin
  if Assigned(FForm) then
  begin
    FForm.OnDestroying := nil;
    FForm := nil;
  end;

  if Assigned(FView) then
  begin
    FView.FBook := nil;
    FView := nil;
  end;

  if Assigned(FList) then
  begin
    for A := FList.Count - 1 downto 0 do
      TEfxPage(FList[A]).OnDestroying(nil);
    FreeAndNil(FList);
  end;
  
  if Assigned(efx_books) then
    efx_books.Remove(Self);
end;

procedure TEfxBook.SaveToFile(const fname: string);
begin
  FForm.SaveToFile(fname);
end;

procedure TEfxBook.SetModified(Value: boolean);
begin
  FForm.Modified := Value;
end;

{ TEfxPage }

procedure TEfxPage.AddCol;
begin
  FPage.AppendColumn;
end;

procedure TEfxPage.AddRow;
begin
  FPage.AppendRow;
end;

function TEfxPage.Arrange(const Source, Patten, Spacer: string): string;
begin
  if Patten <> '' then
    Result := StringReplace(Source, Patten, Spacer, [rfReplaceAll, rfIgnoreCase]) else
    Result := Source;
end;

procedure TEfxPage.Assign(AForm: TEfxPage);
begin
  if AForm <> Self then
    if Assigned(AForm.FPage) then
      FPage.Assign(AForm.FPage) else
      FPage.Clear;
end;

function TEfxPage.Average(const Range: string): double;
var
  sumv, maxv, minv: double;
begin
  FPage.SumRanged(Range, sumv, Result, maxv, minv);
end;

procedure TEfxPage.Check(const Cell, Oper, Range, Msg, Patten, Spacer: string);
var
  test: set of 0..2;
  r1, r2: string;

  function relation(A, B: double): integer;
  begin
    A := A - B;
    if IsZero(A) then
      Result := 0 else if A > 0 then
      Result := 1  else
      Result := 2;
  end;

begin
  if Oper = '==' then test := [0]    else
  if Oper = '>'  then test := [1]    else
  if Oper = '>=' then test := [0, 1] else
  if Oper = '<'  then test := [2]    else
  if Oper = '<=' then test := [0, 2] else
  if Oper = '!=' then test := [1, 2] else
                      test := [];

  r1 := Arrange(Cell, Patten, Spacer);
  r2 := Arrange(Range, Patten, Spacer);
  
  if not (relation(Sum(r1), Sum(r2)) in test) then
    ErrorAt(r1, Msg);
end;

procedure TEfxPage.CheckHiLo(const Cell, Oper, Range, Msg, Patten: string;
  LoValue, HiValue: integer);
begin
  if LoValue > HiValue then
    CheckHiLo(Cell, Oper, Range, Msg, Patten, HiValue, LoValue) else
    repeat
      Check(Cell, Oper, Range, Msg, Patten, IntToStr(LoValue));
      Inc(LoValue);
    until LoValue > HiValue;
end;

procedure TEfxPage.Clear;
begin
  FPage.Clear;
end;

function TEfxPage.Clone: TEfxPage;
begin
  Result := TEfxPage.Create(FBook, FPage.Clone);
end;

function TEfxPage.ColumnIndex(const ID: string): integer;
begin
  if ID[1] in AlphaChar then
    Result := DecodeColumnID(ID) else
    Result := StrToInt(ID) + FixedColCount;
end;

constructor TEfxPage.Create(ABook: TEfxBook; AForm: TEasyForm);
begin
  FBook := ABook;
  FBook.FList.Add(Self);
  FPage := AForm;
  FPage.OnDestroying := {$IFDEF FPC}@{$ENDIF}OnDestroying;
  FPage.OnDelete := {$IFDEF FPC}@{$ENDIF}OnDelete;
  FList := TList.Create;
end;

procedure TEfxPage.DeleteCol(Col: integer);
begin
  FPage.DeleteColumn(Col - 1);
end;

procedure TEfxPage.DeleteRow(Row: integer);
begin
  FPage.DeleteRow(Row - 1);
end;

destructor TEfxPage.Destroy;
begin
  OnDestroying(Self);
  inherited;
end;

procedure TEfxPage.ErrorAt(const Cell, Msg: string);
begin
  FBook.ErrorAt(Self, Cell, Msg);
end;

function TEfxPage.GetCellByPos(const Pos: string; Force: boolean): TEfxCell;
var
  P: TPoint;
  N: TEasyItem;
begin
  if DecodeRangeID(Pos, P) then
  begin
    if Force then
      N := FPage.Force(P.X, P.Y) else
      N := FPage.FindByPos(P.X, P.Y);
    Result := MakeUp(N);
  end
  else Result := nil;
end;

function TEfxPage.GetColCount: integer;
begin
  Result := FPage.ColCount;
end;

function TEfxPage.GetColWidth(Index: Integer): integer;
begin
  Result := FPage.ColWidths[Index - 1];
end;

function TEfxPage.GetFixedColCount: integer;
begin
  Result := FPage.FixedColCount;
end;

function TEfxPage.GetFixedRowCount: integer;
begin
  Result := FPage.FixedRowCount;
end;

function TEfxPage.GetName: string;
begin
  Result := FPage.Name;
end;

function TEfxPage.GetPageIndex: integer;
begin
  Result := 0;
end;

function TEfxPage.GetRowCount: integer;
begin
  Result := FPage.RowCount;
end;

function TEfxPage.GetRowHeight(Index: Integer): integer;
begin
  Result := FPage.RowHeights[Index - 1];
end;

function TEfxPage.GetView: TEfxView;
begin
  Result := Book.View;
end;

procedure TEfxPage.InsertCol(Col: integer);
begin
  FPage.InsertColumn(Col - 1);
end;

procedure TEfxPage.InsertRow(Row: integer);
begin
  FPage.InsertRow(Row - 1);
end;

function TEfxPage.MakeUp(AItem: TEasyItem): TEfxCell;
var
  index: integer;
begin
  if Assigned(AItem) then
  begin
    for index := 0 to FList.Count - 1 do
    begin
      Result := TEfxCell(FList[index]);
      if Result.FItem = AItem then Exit;
    end;
    Result := TEfxCell.Create(Self, AItem);
  end
  else Result := nil;
end;

function TEfxPage.MaxValue(const Range: string): double;
var
  sumv, avev, minv: double;
begin
  FPage.SumRanged(Range, sumv, avev, Result, minv);
end;

function TEfxPage.MinValue(const Range: string): double;
var
  sumv, avev, maxv: double;
begin
  FPage.SumRanged(Range, sumv, avev, maxv, Result);
end;

procedure TEfxPage.OnDestroying(Sender: TObject);
var
  A: integer;
begin
  if Assigned(FBook) then
  begin
    FBook.DestroyingPage(Self);
    FBook.FList.Remove(Self);
    FBook := nil;
  end;

  if Assigned(FPage) then
  begin
    FPage.OnDestroying := nil;
    FPage.OnDelete := nil;
    FPage := nil;
  end;

  if Assigned(FList) then
  begin
    for A := 0 to FList.Count - 1 do
      with TEfxCell(FList[A]) do
      begin
        FItem := nil;
        FPage := nil;
      end;
    FreeAndNil(FList);
  end;
end;

procedure TEfxPage.OnDelete(AItem: TObject);
var
  index: integer;
  cell: TEfxCell;
begin
  for index := 0 to FList.Count - 1 do
  begin
    cell := TEfxCell(FList[index]);
    if cell.FItem = AItem then
    begin
      cell.FItem := nil;
      cell.FPage := nil;
      FList.Delete(index);
      Exit;
    end;
  end;
end;

procedure TEfxPage.SetColCount(const Value: integer);
begin
  FPage.ColCount := Value;
end;

procedure TEfxPage.SetColWidth(Index: Integer; const Value: integer);
begin
  FPage.ColWidths[Index - 1] := Value;
end;

procedure TEfxPage.SetFixedColCount(const Value: integer);
begin
  FPage.FixedColCount := Value;
end;

procedure TEfxPage.SetFixedRowCount(const Value: integer);
begin
  FPage.FixedRowCount := Value;
end;

procedure TEfxPage.SetName(const Value: string);
begin
  FPage.Name := Trim(Value);
end;

procedure TEfxPage.SetPageIndex(Value: integer);
begin

end;

procedure TEfxPage.SetRowCount(const Value: integer);
begin
  FPage.RowCount := Value;
end;

procedure TEfxPage.SetRowHeight(Index: Integer; const Value: integer);
begin
  FPage.RowHeights[Index - 1] := Value;
end;

function TEfxPage.Sum(const Range: string): double;
var
  avev, maxv, minv, sumv: double;
  index, pagex, count: integer;
  left, line, expr, rngs: string;
  oper, next: char;

  function no_comma(const S: string): string;
  var
    X1, X2: integer;
  begin
    X1 := 1;
    X2 := Length(S);
    while (X1 <= X2) and (S[X1] in [#1..' ', ',']) do Inc(X1);
    while (X2 >  X1) and (S[X2] in [#1..' ', ',']) do Dec(X2);
    Result := Copy(S, X1, X2 - X1 + 1);
  end;
  
  function find_opr(const S: string; var index: integer): char;
  var
    count: integer;
  begin
    count := 0;
    index := 1;
    while index < Length(S) do
    begin
      Result := S[index];
      case Result of
        '[': Inc(count);
        ']': Dec(count);
        '+': if count = 0 then Exit;
        '-': if count = 0 then Exit;
      end;
      Inc(index);
    end;
    Result := '+';
  end;

  function add_range(const R: string): string;
  begin
    if R <> '' then
      if rngs = '' then
        rngs := R else
        rngs := rngs + ',' + R;
    Result := rngs;
  end;
  
begin
  Result := 0;
  oper := '+';
  expr := no_comma(Range);
  while expr <> '' do
  begin
    next := find_opr(expr, index);
    line := no_comma(Copy(expr, 1, index - 1));
    expr := no_comma(Copy(expr, index + 1, MaxInt));
    index := Pos('[', line);
    rngs := '';
    while index > 0 do
    begin
      add_range(no_comma(Copy(line, 1, index - 1)));
      line := no_comma(Copy(line, index + 1, MaxInt));
      count := 1;
      index := 1;
      while index <= Length(line) do
      begin
        if line[index] = '[' then Inc(count) else
        if line[index] = ']' then
        begin
          Dec(count);
          if count = 0 then Break;
        end;
        Inc(index);
      end;
      if count = 0 then
      begin
        pagex := stoi(__extractNameValue(Copy(line, 1, index - 1), left, '!')) - 1;
        if (pagex >= 0) and (pagex < FBook.PageCount) then
        begin
          sumv := FBook.Page[pagex].Sum(no_comma(left));
          if oper = '+' then
            Result := Result + sumv else
            Result := Result - sumv;
        end;
        line := no_comma(Copy(line, index + 1, MaxInt));
        index := Pos('[', line);
      end
      else line := '';
    end;
    if add_range(line) <> '' then
    begin
      FPage.SumRanged(rngs, sumv, avev, maxv, minv);
      if oper = '+' then
        Result := Result + sumv else
        Result := Result - sumv;
    end;
    oper := next;
  end;
end;

{ TEfxCell }

constructor TEfxCell.Create(AForm: TEfxPage; AItem: TEasyItem);
begin
  FPage := AForm;
  FPage.FList.Add(Self);
  FItem := AItem;
end;

destructor TEfxCell.Destroy;
begin
  FItem := nil;
  if Assigned(FPage) then
  begin
    FPage.FList.Remove(Self);
    FPage := nil;
  end;
  inherited;
end;

function TEfxCell.GetBook: TEfxBook;
begin
  Result := Page.Book;
end;

function TEfxCell.GetCol: integer;
begin
  Result := FItem.Col;
end;

function TEfxCell.GetColSpan: integer;
begin
  Result := FItem.ColSpan;
end;

function TEfxCell.GetRow: integer;
begin
  Result := FItem.Row;
end;

function TEfxCell.GetRowSpan: integer;
begin
  Result := FItem.RowSpan;
end;

function TEfxCell.GetText: string;
begin
  Result := FItem.FText;
end;

function TEfxCell.GetView: TEfxView;
begin
  Result := Page.View;
end;

procedure TEfxCell.SetCol(Value: integer);
begin
  FItem.Col := Value;
end;

procedure TEfxCell.SetColSpan(Value: integer);
begin
  FItem.ColSpan := Value;
end;

procedure TEfxCell.SetRow(Value: integer);
begin
  FItem.Row := Value;
end;

procedure TEfxCell.SetRowSpan(Value: integer);
begin
  FItem.RowSpan := Value;
end;

procedure TEfxCell.SetText(const Text: string);
begin
  FItem.SetText(Text);
end;

initialization
begin
  EasyFormList := TList.Create;
  ss_normal := TEasyStyle.Create(DefStyleName, true);
  ss_normal.IncRefcount;
  DT := TStringList.Create;
end;

finalization
begin
  FreeAndNil(EasyFormList);
  FreeAndNil(DT);
  FreeAndNil(ss_normal);
end;

end.
