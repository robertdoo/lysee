{==============================================================================}
{     PROJECT: lysee_pad_fpc                                                   }
{ DESCRIPTION: lysee script file editing PAD (FPC)                             }
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
{ Portions created by Li Yun Jie are Copyright (C) 2003-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_pad_fpc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}cthreads,{$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lseu, lse_synedit, lse_msgbox, lse_pad_fpc, lse_about_fpc,
  lse_pad_open_fpc, lse_symbol, lse_spawn, lse_patten, lse_kernel, lse_funcs,
  lse_export, lse_cgi, lse_api, lse_syncobj;

{$IFDEF WINDOWS}
{$R lysee_pad_fpc.rc}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TLspadForm, LspadForm);
  Application.Run;
end.

