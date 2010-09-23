{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lysee_package; 

interface

uses
    lseu, lse_api, lse_cgi, lse_export, lse_funcs, lse_kernel, lse_patten, 
  lse_spawn, lse_symbol, lse_syncobj, lysee_laz, lysee_class_editor, 
  lysee_module_editor, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('lysee_laz', @lysee_laz.Register); 
end; 

initialization
  RegisterPackage('lysee_package', @Register); 
end.
