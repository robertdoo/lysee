{==============================================================================}
{        UNIT: lysee_postgres_funcs                                            }
{ DESCRIPTION: postgres database verdor                                        }
{     CREATED: 2008/02/16                                                      }
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
unit lysee_postgres_funcs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lseu, lse_dbu, sqldb, pqconnection;

const
  DBV_NAME = 'postgres';
  DBV_DESC = 'PostgreSQL vendor (FPC) for lysee';
  
type
  TVendorConnection = TPQConnection;

{$i head.inc}

implementation

{$i body.inc}

procedure TVendorDB.SetConnStr(const ConnectionStr: string);
var
  T, U, P, S, M: string;
  L: integer;
begin
  FConnStr := Trim(ConnectionStr);
  L := Length(FConnStr);
  if (L > 6) and (FConnStr[1] = '[') and (FConnStr[L] = ']') then
  begin
    lse_decode_TUPSP(Copy(FConnStr, 2, L - 2), T, U, P, S, M);
    if T = '' then
      raise Exception.Create('Target postgres database not supplied');
    DatabaseName := T;
    if S = '' then
      HostName := 'localhost' else
      HostName := S;
    UserName := U;
    Password := P;
    Params.Text := M;
    LoginPrompt := false;
  end
  else raise Exception.CreateFmt('Invalid connection string: %s', [FConnStr]);
end;

end.
