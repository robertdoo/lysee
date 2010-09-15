{==============================================================================}
{     PROJECT: lysee_math                                                      }
{ DESCRIPTION: mathmetics functions                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2010/09/01                                                      }
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
{ Portions created by Li Yun Jie are Copyright (C) 2007-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_math;

{$mode objfpc}{$H+}

uses
  Classes,
  lseu in '../lseu.pad',
  lysee_math_funcs in 'lysee_math_funcs.pas';

{$IFDEF WINDOWS}{$R lysee_math.rc}{$ENDIF}

procedure InitExchange(rec: PLseModuleRec; proc: TLseQueryEntry);cdecl;
begin
  lse_prepare(proc);
  rec^.iw_production     := 'mathematic module for lysee';
  rec^.iw_version        := LSE_VERSION;
  rec^.iw_copyright      := 'Copyright (C) 2003-2010 libudi';
  rec^.iw_desc           := rec^.iw_production;;
  rec^.iw_homepage       := LSE_HOMEPAGE;
  rec^.iw_email          := LSE_EMAIL;
  rec^.iw_libfuncs.count := math_func_count;
  rec^.iw_libfuncs.entry :=@math_func_array;
end;

exports
  InitExchange;

begin
end.

