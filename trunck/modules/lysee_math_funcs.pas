{==============================================================================}
{     PROJECT: lysee_math_funcs                                                }
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
unit lysee_math_funcs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lseu;
  
procedure pp_math_abs(Param: pointer);cdecl;
procedure pp_math_ceil(Param: pointer);cdecl;
procedure pp_math_floor(Param: pointer);cdecl;
procedure pp_math_frac(Param: pointer);cdecl;
procedure pp_math_int(Param: pointer);cdecl;
procedure pp_math_roundto(Param: pointer);cdecl;
procedure pp_math_round(Param: pointer);cdecl;
procedure pp_math_trunc(Param: pointer);cdecl;
procedure pp_math_pi(Param: pointer);cdecl;
procedure pp_math_isZero(Param: pointer);cdecl;
procedure pp_math_ln(Param: pointer);cdecl;
procedure pp_math_lnxp1(Param: pointer);cdecl;
procedure pp_math_exp(Param: pointer);cdecl;
procedure pp_math_ldexp(Param: pointer);cdecl;
procedure pp_math_power(Param: pointer);cdecl;
procedure pp_math_sqrt(Param: pointer);cdecl;
procedure pp_math_sqr(Param: pointer);cdecl;
procedure pp_math_log10(Param: pointer);cdecl;
procedure pp_math_log2(Param: pointer);cdecl;
procedure pp_math_logn(Param: pointer);cdecl;
procedure pp_math_arcCos(Param: pointer);cdecl;
procedure pp_math_arcCosh(Param: pointer);cdecl;
procedure pp_math_arcSin(Param: pointer);cdecl;
procedure pp_math_arcSinh(Param: pointer);cdecl;
procedure pp_math_cos(Param: pointer);cdecl;
procedure pp_math_cosecant(Param: pointer);cdecl;
procedure pp_math_cosh(Param: pointer);cdecl;
procedure pp_math_cot(Param: pointer);cdecl;
procedure pp_math_csc(Param: pointer);cdecl;
procedure pp_math_sec(Param: pointer);cdecl;
procedure pp_math_sin(Param: pointer);cdecl;
procedure pp_math_sinh(Param: pointer);cdecl;
procedure pp_math_tan(Param: pointer);cdecl;
procedure pp_math_tanh(Param: pointer);cdecl;
procedure pp_math_arctan(Param: pointer);cdecl;


const
  KTE_MATH = 'mathError';

  math_func_count = 35;
  math_func_array: array[0..math_func_count - 1] of RLseFuncRec = (
    (fr_prot:'float pi()';
     fr_addr:@pp_math_pi;
     fr_desc:'3.1415926.....'
    ),
    (fr_prot:'bool zero(float value)';
     fr_addr:@pp_math_isZero;
     fr_desc:'test whether the argument is zero or differs from zero by at most epsilon'
    ),
    (fr_prot:'float abs(float value)';
     fr_addr:@pp_math_abs;
     fr_desc:'returns the absolute value of the argument'
    ),
    (fr_prot:'int ceil(float value)';
     fr_addr:@pp_math_ceil;
     fr_desc:'obtain the lowest integer greater than or equal to argument'
    ),
    (fr_prot:'int floor(float value)';
     fr_addr:@pp_math_floor;
     fr_desc:'obtain the highest integer less than or equal to argument'
    ),
    (fr_prot:'float frac(float value)';
     fr_addr:@pp_math_frac;
     fr_desc:'returns the fractional part of the argument'
    ),
    (fr_prot:'float intpart(float value)';
     fr_addr:@pp_math_int;
     fr_desc:'returns the integer part of argument'
    ),
    (fr_prot:'float roundto(float value, int digits)';
     fr_addr:@pp_math_roundto;
     fr_desc:'round argument to a specified power of ten'
    ),
    (fr_prot:'int round(float value)';
     fr_addr:@pp_math_round;
     fr_desc:'rounds a float value to an integer value'
    ),
    (fr_prot:'int trunc(float value)';
     fr_addr:@pp_math_trunc;
     fr_desc:'truncates a float value to an integer value'
    ),
    (fr_prot:'float ln(float value)';
     fr_addr:@pp_math_ln;
     fr_desc:'returns the natural logarithm (Ln(e) = 1) of the argument'
    ),
    (fr_prot:'float lnxp1(float value)';
     fr_addr:@pp_math_lnxp1;
     fr_desc:'returns the natural logarithm of (argument+1)'
    ),
    (fr_prot:'float sqrt(float value)';
     fr_addr:@pp_math_sqrt;
     fr_desc:'returns square root of argument'
    ),
    (fr_prot:'float sqr(float value)';
     fr_addr:@pp_math_sqr;
     fr_desc:'returns the square of the argument'
    ),
    (fr_prot:'float exp(float value)';
     fr_addr:@pp_math_exp;
     fr_desc:'returns the value of e raised to the power of argument, where e is the base of the natural logarithms'
    ),
    (fr_prot:'float log10(float value)';
     fr_addr:@pp_math_log10;
     fr_desc:'returns the log base 10 of argument'
    ),
    (fr_prot:'float log2(float value)';
     fr_addr:@pp_math_log2;
     fr_desc:'returns the log base 2 of argument'
    ),
    (fr_prot:'float logn(float value, float base)';
     fr_addr:@pp_math_logn;
     fr_desc:'returns the log base of argument'
    ),
    (fr_prot:'float ldexp(float X, int P)';
     fr_addr:@pp_math_ldexp;
     fr_desc:'returns X times (2 to the power of P)'
    ),
    (fr_prot:'float power(float base, float exponent)';
     fr_addr:@pp_math_power;
     fr_desc:'raises base to any power'
    ),
    (fr_prot:'float arcCos(float value)';
     fr_addr:@pp_math_arcCos;
     fr_desc:'returns the inverse cosine of argument'
    ),
    (fr_prot:'float arcCosh(float value)';
     fr_addr:@pp_math_arcCosh;
     fr_desc:'returns the inverse hyperbolic cosine of argument'
    ),
    (fr_prot:'float arcSin(float value)';
     fr_addr:@pp_math_arcSin;
     fr_desc:'returns the inverse sine of argument'
    ),
    (fr_prot:'float arcSinh(float value)';
     fr_addr:@pp_math_arcSinh;
     fr_desc:'returns the inverse hyperbolic sine of argument'
    ),
    (fr_prot:'float cos(float value)';
     fr_addr:@pp_math_cos;
     fr_desc:'returns the cosine of the angle argument'
    ),
    (fr_prot:'float cosh(float value)';
     fr_addr:@pp_math_cosh;
     fr_desc:'calculate the hyperbolic cosine of argument'
    ),
    (fr_prot:'float cot(float value)';
     fr_addr:@pp_math_cot;
     fr_desc:'obtain the cotangent of argument'
    ),
    (fr_prot:'float csc(float value)';
     fr_addr:@pp_math_csc;
     fr_desc:'calculate the cosecant of argument'
    ),
    (fr_prot:'float sec(float value)';
     fr_addr:@pp_math_sec;
     fr_desc:'obtain the secant of argument'
    ),
    (fr_prot:'float sin(float value)';
     fr_addr:@pp_math_sin;
     fr_desc:'returns the sine of the argument'
    ),
    (fr_prot:'float sinh(float value)';
     fr_addr:@pp_math_sinh;
     fr_desc:'calculates the hyperbolic sine of argument'
    ),
    (fr_prot:'float tan(float value)';
     fr_addr:@pp_math_tan;
     fr_desc:'returns the tangent of argument'
    ),
    (fr_prot:'float arcTan(float value)';
     fr_addr:@pp_math_arcTan;
     fr_desc:'returns the arctangent of argument'
    ),
    (fr_prot:'float tanh(float value)';
     fr_addr:@pp_math_tanh;
     fr_desc:'calculates the hyperbolic tangent of argument'
    ),
    (fr_prot:'float cosecant(float value)';
     fr_addr:@pp_math_cosecant;
     fr_desc:'calculate the cosecant of argument'
    )
  );

implementation

uses
  Math;

procedure pp_math_abs(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(abs(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_ceil(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnInt64(ceil(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_floor(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnInt64(floor(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_frac(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(frac(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_int(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(int(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_roundto(Param: pointer);cdecl;
var
  this: double;
  digits: TRoundToRange;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    digits := invoker.paramInt(1);
    invoker.returnFloat(Roundto(this, digits));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_round(Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnInt64(Round(invoker.paramFloat(0)));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_trunc(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnInt64(trunc(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_pi(Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnFloat(pi);
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_isZero(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnBool(math.IsZero(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_ln(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.ln(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_lnxp1(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.lnXP1(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_exp(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.exp(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_ldexp(Param: pointer);cdecl;
var
  this: double;
  digit: integer;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    digit := invoker.paramInt(1);
    invoker.returnFloat(math.Ldexp(this, digit));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_power(Param: pointer);cdecl;
var
  this, exponent: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    exponent := invoker.paramFloat(1);
    invoker.returnFloat(math.power(this, exponent));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_sqrt(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.sqrt(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_sqr(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.sqr(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_log10(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.Log10(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_log2(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.Log2(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_logn(Param: pointer);cdecl;
var
  this, base: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    base := invoker.paramFloat(1);
    invoker.returnFloat(math.LogN(base, this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_arcCos(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.ArcCos(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_arcCosh(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.ArcCosh(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_arcSin(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.ArcSin(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_arcSinh(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.ArcSinh(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_cos(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.cos(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_cosecant(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.Cosecant(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_cosh(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.Cosh(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_cot(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.cot(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_csc(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.csc(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_sec(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.sec(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_sin(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.sin(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_sinh(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.sinh(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_tan(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.tan(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_tanh(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(math.tanh(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure pp_math_arctan(Param: pointer);cdecl;
var
  this: double;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := invoker.paramFloat(0);
    invoker.returnFloat(system.arctan(this));
  except
    invoker.returnError(KTE_MATH, 0, lse_exception_str);
  end;
  invoker.Free;
end;

end.
