{==============================================================================}
{        UNIT: lse_dbu                                                         }
{ DESCRIPTION: database functions                                              }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2008-2010, Li Yun Jie                                          }
{ All rights reserved.                                                         }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ Redistributions of source code must retain the above copyright notice, this  }
{ list of conditions and the following disclaimer.                             }
{                                                                              }
{ Redistributions in binary form must reproduce the above copyright notice,    }
{ this list of conditions and the following disclaimer in the documentation    }
{ and/or other materials provided with the distribution.                       }
{                                                                              }
{ Neither the name of Li Yun Jie nor the names of its contributors may         }
{ be used to endorse or promote products derived from this software without    }
{ specific prior written permission.                                           }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    }
{ IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   }
{ ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  }
{ ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       }
{ DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   }
{ SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   }
{ CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           }
{ LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    }
{ OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  }
{ DAMAGE.                                                                      }
{==============================================================================}
{ The Initial Developer of the Original Code is Li Yun Jie (CHINA).            }
{ Portions created by Li Yun Jie are Copyright (C) 2008-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_dbu;

{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Db, lseu;

function __getft_name(field_type: TFieldType): string;
function __getft_lsv(field_type: TFieldType): integer;

{-----------------------------------------------------------------------
( F_NAME: __getfX
(
( F_DESC: get field type or fetch field value
(
( F_ARGS: field: TField - a field
(
( F_TYPE: IBDICS - something wanted
(
( EXCEPT:
(----------------------------------------------------------------------}
function __getft(field: TField): integer;
function __getfb(field: TField): boolean;
function __getfd(field: TField): double;
function __getfi(field: TField): integer;
function __getfm(field: TField): currency;
function __getfs(field: TField): string;

implementation

uses
  Math;

function stod(const S: string): extended;
begin
  Result := StrToFloatDef(S, 0);
end;

function __getft_name(field_type: TFieldType): string;
begin
  Result := '';
  case field_type of
    ftUnknown      : Result := 'unknown';
    ftString       : Result := 'string';
    ftSmallint     : Result := 'smallint';
    ftInteger      : Result := 'integer';
    ftWord         : Result := 'word';
    ftBoolean      : Result := 'boolean';
    ftFloat        : Result := 'float';
    ftCurrency     : Result := 'currency';
    ftBCD          : Result := 'bcd';
    ftDate         : Result := 'date';
    ftTime         : Result := 'time';
    ftDateTime     : Result := 'datetime';
    ftBytes        : Result := 'bytes';
    ftVarBytes     : Result := 'varbytes';
    ftAutoInc      : Result := 'autoinc';
    ftBlob         : Result := 'blob';
    ftMemo         : Result := 'memo';
    ftGraphic      : Result := 'graphic';
    ftFmtMemo      : Result := 'fmtmemo';
    ftParadoxOle   : Result := 'paradoxole';
    ftDBaseOle     : Result := 'dbaseole';
    ftTypedBinary  : Result := 'typedbinary';
    ftCursor       : Result := 'cursor';
    ftFixedChar    : Result := 'fixedchar';
    ftWideString   : Result := 'widestring';
    ftLargeint     : Result := 'largeint';
    ftADT          : Result := 'adt';
    ftArray        : Result := 'array';
    ftReference    : Result := 'reference';
    ftDataSet      : Result := 'dataset';
    ftOraBlob      : Result := 'orablob';
    ftOraClob      : Result := 'oraclob';
    ftVariant      : Result := 'variant';
    ftInterface    : Result := 'interface';
    ftIDispatch    : Result := 'idispatch';
    ftGuid         : Result := 'guid';
    ftTimeStamp    : Result := 'timestamp';
    ftFMTBcd       : Result := 'fmtbcd';
    ftFixedWideChar: Result := 'fixedwidechar';
    ftWideMemo     : Result := 'widememo';
    else
    case Ord(field_type) of
      40: Result := 'oratimestamp';  {ftOraTimeStamp}
      41: Result := 'orainterval';   {ftOraInterval}
    end;
  end;
end;

function __getft_lsv(field_type: TFieldType): integer;
begin
  Result := LSV_VOID;
  case field_type of
    ftUnknown      :;
    ftString       : Result := lSV_STRING;
    ftSmallint     : Result := LSV_INT;
    ftInteger      : Result := LSV_INT;
    ftWord         : Result := LSV_INT;
    ftBoolean      : Result := LSV_BOOL;
    ftFloat        : Result := LSV_FLOAT;
    ftCurrency     : Result := LSV_MONEY;
    ftBCD          : Result := LSV_MONEY;
    ftDate         : Result := LSV_FLOAT;
    ftTime         : Result := LSV_FLOAT;
    ftDateTime     : Result := LSV_FLOAT;
    ftBytes        :;
    ftVarBytes     :;
    ftAutoInc      : Result := LSV_INT;
    ftBlob         :;
    ftMemo         : Result := lSV_STRING;
    ftGraphic      :;
    ftFmtMemo      : Result := lSV_STRING;
    ftParadoxOle   :;
    ftDBaseOle     :;
    ftTypedBinary  :;
    ftCursor       :;
    ftFixedChar    : Result := lSV_STRING;
    ftWideString   : Result := lSV_STRING;
    ftLargeint     : Result := LSV_INT;
    ftADT          :;
    ftArray        :;
    ftReference    :;
    ftDataSet      :;
    ftOraBlob      :;
    ftOraClob      :;
    ftVariant      :;
    ftInterface    :;
    ftIDispatch    :;
    ftGuid         : Result := lSV_STRING;
    ftTimeStamp    :;
    ftFMTBcd       : Result := LSV_MONEY;
    ftFixedWideChar: Result := lSV_STRING;
    ftWideMemo     : Result := lSV_STRING;
//  else
//  case Ord(field_type) of
//    40: {ftOraTimeStamp}
//    41: {ftOraInterval}
//  end;
  end;
end;

function __getft(field: TField): integer;
begin
  Result := __getft_lsv(field.DataType);
end;

function __getfb(field: TField): boolean;
begin
  try
    if field.IsNull then Result := false else
    case __getft(field) of
      LSV_INT: Result := (field.AsInteger <> 0);
      LSV_FLOAT  : Result := not IsZero(field.AsFloat);
      LSV_MONEY  : Result := (field.AsCurrency <> 0);
      LSV_BOOL   : Result := field.AsBoolean;
      lSV_STRING : Result := (field.AsString <> '');
            else   Result := false;
    end;
  except
    Result := false;
  end;
end;

function __getfd(field: TField): double;
begin
  try
    if field.IsNull then Result := 0 else
    case __getft(field) of
      LSV_INT: Result := field.AsInteger;
      LSV_FLOAT  : Result := field.AsFloat;
      LSV_MONEY  : Result := field.AsCurrency;
      LSV_BOOL   : Result := Ord(field.AsBoolean);
      lSV_STRING : Result := stod(field.AsString);
            else   Result := 0;
    end;
  except
    Result := 0;
  end;
end;

function __getfi(field: TField): integer;
begin
  try
    if field.IsNull then Result := 0 else
    case __getft(field) of
      LSV_INT: Result := field.AsInteger;
      LSV_FLOAT  : Result := Trunc(field.AsFloat);
      LSV_MONEY  : Result := Trunc(field.AsCurrency);
      LSV_BOOL   : Result := Ord(field.AsBoolean);
      lSV_STRING : Result := Trunc(stod(field.AsString));
            else   Result := 0;
    end;
  except
    Result := 0;
  end;
end;

function __getfm(field: TField): currency;
begin
  try
    if field.IsNull then Result := 0 else
    case __getft(field) of
      LSV_INT: Result := field.AsInteger;
      LSV_FLOAT  : Result := field.AsFloat;
      LSV_MONEY  : Result := field.AsCurrency;
      LSV_BOOL   : Result := Ord(field.AsBoolean);
      lSV_STRING : Result := stod(field.AsString);
            else   Result := 0;
    end;
  except
    Result := 0;
  end;
end;

function __getfs(field: TField): string;
const
  BOOLEAN_STR: array[boolean] of string = ('0', '1');
begin
  try
    if field.IsNull then Result := '' else
    if field.DataType = ftBoolean then
      Result := BOOLEAN_STR[field.AsBoolean] else
      Result := field.AsString;
  except
    Result := '';
  end;
end;

end.
