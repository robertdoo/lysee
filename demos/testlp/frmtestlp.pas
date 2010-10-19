unit frmTestlp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, lse_kernel, SynEdit, lseu;

type

  THelloObject = class(TLseObject)
  private
    Text: string;
  end;

  { TTestlpForm }

  TTestlpForm = class(TForm)
    btnRun: TButton;
    hii: TLseModule;
    hii_alert: TLseFunc;
    hii_input: TLseFunc;
    hello: TLseClass;
    hello_hello: TLseMethod;
    hello_say: TLseMethod;
    hello_xxx: TLseMethod;
    Lysee: TLseEngine;
    Outputs: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Inputs: TSynEdit;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure helloGetObject(Sender: TObject; var Lobj: TLseObject);
    procedure hello_helloExecute(Lobj: TLseObject; Invoker: TLseInvoke);
    procedure hello_sayExecute(Lobj: TLseObject; Invoker: TLseInvoke);
    procedure hello_xxxExecute(Lobj: TLseObject; Invoker: TLseInvoke);
    procedure hii_alertExecute(Invoker: TLseInvoke);
    procedure hii_inputExecute(Invoker: TLseInvoke);
    procedure LyseeRead(Sender: TObject; const Buf: pchar; var Count: integer);
    procedure LyseeReadln(Sender: TObject; var S: string);
    procedure LyseeWrite(Sender: TObject; const Buf: pchar; var Count: integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TestlpForm: TTestlpForm;

implementation

uses
  Math;

procedure hii_clear(const Param: PLseParam);cdecl;
begin
  TestlpForm.Outputs.Lines.Clear;
  TestlpForm.Outputs.Lines.Add('cleared');
end;

{ TTestlpForm }

procedure TTestlpForm.LyseeRead(Sender: TObject; const Buf: pchar; var Count: integer);
var
  S: string;
  L: integer;
begin
  S := InputBox('Input', 'Input a string:', '');
  L := Max(0, Min(Count, Length(S)));
  if L > 0 then
    Move(pchar(S)^, Buf^, L);
  Count := L;
end;

procedure TTestlpForm.LyseeReadln(Sender: TObject; var S: string);
begin
  S := InputBox('Input', 'Input a string:', '');
end;

procedure TTestlpForm.LyseeWrite(Sender: TObject; const Buf: pchar; var Count: integer);
var
  S: string;
  L: TStrings;
  X: integer;
begin
  SetString(S, Buf, Count);
  L := TStringList.Create;
  try
    L.Text := S;
    for X := 0 to L.Count - 1 do
      Outputs.Lines.Add(L[X]);
  finally
    L.Free;
  end;
end;

procedure TTestlpForm.btnRunClick(Sender: TObject);
var
  S: string;
  L: integer;
begin
  Outputs.Lines.Clear;
  if Lysee.ExecuteCode(Inputs.Lines.Text) then
  begin
    S := Lysee.ResultText;
    L := Length(S);
    LyseeWrite(Self, pchar(S), L);
  end
  else Outputs.Lines.Text := Lysee.Error;
end;

procedure TTestlpForm.FormCreate(Sender: TObject);
begin
  if lse_startup then
  begin
    lse_setup_components([hii, Lysee]);
    hii.AddFunc('clear||', @hii_clear);
  end;
  Caption := ParamStr(0);
end;

procedure TTestlpForm.FormDestroy(Sender: TObject);
begin
  lse_cleanup;
end;

procedure TTestlpForm.helloGetObject(Sender: TObject; var Lobj: TLseObject);
begin
  Lobj := THelloObject.Create;
end;

procedure TTestlpForm.hello_helloExecute(Lobj: TLseObject; Invoker: TLseInvoke);
begin
  THelloObject(Lobj).Text := Invoker.paramStr(1);
end;

procedure TTestlpForm.hello_sayExecute(Lobj: TLseObject; Invoker: TLseInvoke);
begin
  ShowMessage(THelloObject(Lobj).Text);
end;

procedure TTestlpForm.hello_xxxExecute(Lobj: TLseObject; Invoker: TLseInvoke);
begin
  THelloObject(Lobj).Text := THelloObject(Lobj).Text + Invoker.paramStr(1);
  ShowMessage(THelloObject(Lobj).Text);
end;

procedure TTestlpForm.hii_alertExecute(Invoker: TLseInvoke);
begin
  ShowMessage(Invoker.ParamStr(0));
end;

procedure TTestlpForm.hii_inputExecute(Invoker: TLseInvoke);
var
  S: string;
begin
  S := InputBox('Input', Invoker.paramStr(0), '');
  Invoker.returnStr(S);
end;

initialization
  {$I frmtestlp.lrs}

end.

