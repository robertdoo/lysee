{==============================================================================}
{        FILE: head.inc                                                        }
{ DESCRIPTION: body include of database verdor (FPC)                           }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}

type
  TVendorDB = class(TVendorConnection)
  private
    FDB: PLseDB;
    FError: string;
    FRefcount: integer;
    FConnStr: string;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    procedure HandleError;
    function ExecSQL(const SQL: string): integer;
    procedure SetConnStr(const ConnectionStr: string);
  end;

  TVendorDS = class(TSQLQuery)
  private
    FDB: PLseDB;
    FDS: PLseDS;
    FError: string;
    FRefcount: integer;
    procedure HandleError;
  end;
  
function vdb_addref(DB: PLseDB): integer;cdecl;
function vdb_release(DB: PLseDB): integer;cdecl;

procedure register_dbv;
procedure register_module;
