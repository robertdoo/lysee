unit lse_devcaps;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, {$IFDEF WINDOWS}Windows,{$ENDIF}
  Printers;

type
  TCapsRec = record
   dc: HDC;
    X: integer; {LOGPIXELSX     : Horz pixels per inch}
    Y: integer; {LOGPIXELSY     : Vert pixels per inch}
   hp: integer; {HORZRES        : Horz pixels}
   vp: integer; {VERTRES        : Vert pixeld}
   hm: integer; {HORZSIZE       : Horz mm width}
   vm: integer; {VERTSIZE       : Vert mm height}
   pw: integer; {PHYSICALWIDTH  : HORZRES + PHYSICALOFFSETX * 2}
   ph: integer; {PHYSICALHEIGHT : VERTRES + PHYSICALOFFSETY * 2}
   ox: integer; {PHYSICALOFFSETX:}
   oy: integer; {PHYSICALOFFSETY:}
  end;
  PCapsRec = ^TCapsRec;

{$IFDEF WINDOWS}
function set_capsrec(DC: HDC; CapsRec: PCapsRec): boolean;
{$ENDIF}
function pixels_width(CapsRec: PCapsRec): integer;
function pixels_height(CapsRec: PCapsRec): integer;
function mm_width(CapsRec: PCapsRec): double;
function mm_height(CapsRec: PCapsRec): double;
//
// 1. ptom --> pixels to mm
// 2. ptop --> pixels to pixels
// 3. mtom --> mm to mm
// 4. mtop --> mm to pixels
// 5. stop --> screen to printer by pixels
// 6. stom --> screen to printer by mm
//
function ptom_x(CapsRec: PCapsRec; Pixels: integer): double;
function ptom_y(CapsRec: PCapsRec; Pixels: integer): double;
function mtop_x(CapsRec: PCapsRec; mm: double): integer;
function mtop_y(CapsRec: PCapsRec; mm: double): integer;
function ptop_x(Src, Dst: PCapsRec; Pixels: integer): integer;
function ptop_y(Src, Dst: PCapsRec; Pixels: integer): integer;
function mtom_x(Src, Dst: PCapsRec; mm: double): double;
function mtom_y(Src, Dst: PCapsRec; mm: double): double;
function stop_x(Pixels: integer): integer;
function stop_y(Pixels: integer): integer;
function stom_x(mm: double): double;
function stom_y(mm: double): double;

function ScreenCap: PCapsRec;
function PrinterCap: PCapsRec;
function HasPrinter: boolean;

implementation

var
  devcaps_screen: TCapsRec;
  devcaps_screen_checked: boolean = false;

function ScreenCap: PCapsRec;
var
  DC: HDC;
begin
  if not devcaps_screen_checked then
  begin
    devcaps_screen_checked := true;
    DC := GetDC(0);
    set_capsrec(DC, @devcaps_screen);
    ReleaseDC(0, DC);
  end;
  Result := @devcaps_screen;
end;

var
  devcaps_printer: TCapsRec;
  devcaps_printer_checked: boolean = false;
  devcaps_has_printer: boolean;

function PrinterCap: PCapsRec;
begin
  if not devcaps_printer_checked then
  begin
    devcaps_printer_checked := true;
    devcaps_has_printer := (Printer.Printers.Count > 0);
    if devcaps_has_printer then
    begin
      {$IFDEF FPC}
      {$ELSE}
      set_capsrec(Printer.Handle, @devcaps_printer);
      {$ENDIF}
    end;
  end;
  if devcaps_has_printer then
    Result := @devcaps_printer else
    Result := ScreenCap;
end;

function HasPrinter: boolean;
begin
  if devcaps_printer_checked then
    Result := devcaps_has_printer else
    Result := (Printer.Printers.Count > 0); 
end;

{$IFDEF WINDOWS}
function set_capsrec(DC: HDC; CapsRec: PCapsRec): boolean;
begin
  CapsRec^.dc := DC;
  with CapsRec^ do
  begin
    X := GetDeviceCaps(DC, LOGPIXELSX);
    Y := GetDeviceCaps(DC, LOGPIXELSY);
   hp := GetDeviceCaps(DC, HORZRES);
   vp := GetDeviceCaps(DC, VERTRES);
   hm := GetDeviceCaps(DC, HORZSIZE);
   vm := GetDeviceCaps(DC, VERTSIZE);
   pw := GetDeviceCaps(DC, PHYSICALWIDTH);
   ph := GetDeviceCaps(DC, PHYSICALHEIGHT);
   ox := GetDeviceCaps(DC, PHYSICALOFFSETX);
   oy := GetDeviceCaps(DC, PHYSICALOFFSETY);
  end;
  Result := true;
end;
{$ENDIF}

function pixels_width(CapsRec: PCapsRec): integer;
begin
  Result := CapsRec^.hp + (CapsRec^.ox * 2);
end;

function pixels_height(CapsRec: PCapsRec): integer;
begin
  Result := CapsRec^.vp + (CapsRec^.oy * 2);
end;

function mm_width(CapsRec: PCapsRec): double;
begin
  Result := ptom_x(CapsRec, pixels_width(CapsRec));
end;

function mm_height(CapsRec: PCapsRec): double;
begin
  Result := ptom_y(CapsRec, pixels_height(CapsRec));
end;

function ptom_x(CapsRec: PCapsRec; Pixels: integer): double;
begin
  Result := (Pixels * CapsRec^.hm) / CapsRec^.hp;
end;

function ptom_y(CapsRec: PCapsRec; Pixels: integer): double;
begin
  Result := (Pixels * CapsRec^.vm) / CapsRec^.vp;
end;

function mtop_x(CapsRec: PCapsRec; mm: double): integer;
begin
  Result := Round((CapsRec^.hp * mm) / CapsRec^.hm);
end;

function mtop_y(CapsRec: PCapsRec; mm: double): integer;
begin
  Result := Round((CapsRec^.vp * mm) / CapsRec^.vm);
end;

function ptop_x(Src, Dst: PCapsRec; Pixels: integer): integer;
begin
  Result := mtop_x(Dst, ptom_x(Src, Pixels));
end;

function ptop_y(Src, Dst: PCapsRec; Pixels: integer): integer;
begin
  Result := mtop_y(Dst, ptom_y(Src, Pixels));
end;

function mtom_x(Src, Dst: PCapsRec; mm: double): double;
begin
  Result := ptom_x(Dst, mtop_x(Src, mm));
end;

function mtom_y(Src, Dst: PCapsRec; mm: double): double;
begin
  Result := ptom_y(Dst, mtop_y(Src, mm));
end;

function stop_x(Pixels: integer): integer;
begin
  Result := ptop_x(@devcaps_screen, @devcaps_printer, Pixels);
end;

function stop_y(Pixels: integer): integer;
begin
  Result := ptop_y(@devcaps_screen, @devcaps_printer, Pixels);
end;

function stom_x(mm: double): double;
begin
  Result := mtom_x(@devcaps_screen, @devcaps_printer, mm);
end;

function stom_y(mm: double): double;
begin
  Result := mtom_y(@devcaps_screen, @devcaps_printer, mm);
end;

end.
