{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lysee_laz; 

interface

uses
  lseu, lse_module_editor, lse_synedit, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('lseu', @lseu.Register); 
  RegisterUnit('lse_synedit', @lse_synedit.Register); 
end; 

initialization
  RegisterPackage('lysee_laz', @Register); 
end.
