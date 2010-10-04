{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lysee_laz; 

interface

uses
    lse_api, lse_cgi, lseu, lse_syncobj, lse_symbol, lse_components, 
  lse_export, lse_funcs, lse_kernel, lse_module_editor, lse_patten, lse_spawn, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('lse_components', @lse_components.Register); 
end; 

initialization
  RegisterPackage('lysee_laz', @Register); 
end.
