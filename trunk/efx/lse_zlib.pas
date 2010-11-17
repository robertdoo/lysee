unit lse_zlib;

interface

uses
  SysUtils, classes, windows;

type
  RLiFileRec = packed
  record
    f_name: array[0..MAX_PATH - 1] of char;  {<--file name}
    f_size: integer;                         {<--file size}
    c_size: integer;                         {<--file size after compression}
  end;

const
  FileRecSize = sizeof(RLiFileRec);

{ compress }

procedure Compress(Source: pointer; Count: integer; var Stream: TMemoryStream);
procedure CompressMemoryStream(Stream: TMemoryStream);
procedure CompressStream(Source, Desti: TStream; var Bytes: integer);
procedure CompressStreamToFile(Source: TStream; const Desti: string);
procedure CompressFile(const Source, Desti: string);
procedure CompressFiles(Files: TStrings; const Desti: string);

{ decompress }

procedure Decompress(Source: pointer; Count: integer; var Stream: TMemoryStream);
procedure DecompressStream(Source, Desti: TStream; var Bytes: integer);
procedure DecompressMemoryStream(Stream: TMemoryStream);
procedure DecompressFile(const Source, Desti: string);
procedure DecompressFiles(const Source, DestiDir: string; Files: TStrings);
procedure GetFileList(const Source: string; Files: TStrings);
function ExtractFile(const Source, FileName, DestiFile: string): boolean;

implementation

uses
  zlib, lseu;
  
{ TZLbMisc }

procedure Compress(Source: pointer; Count: integer; var Stream: TMemoryStream);
var
  S: TCompressionStream;
Begin
  Stream := TMemoryStream.Create;
  S := TCompressionStream.Create(clDefault, Stream);
  try
    S.Write(Source^, Count)
  finally
    S.Free;
  end;
  Stream.Position := 0;
end;

procedure CompressFile(const Source, Desti: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(Source);
    CompressMemoryStream(S);
    S.SaveToFile(Desti);
  finally
    S.Free;
  end;
end;

procedure CompressFiles(Files: TStrings; const Desti: string);
var
  F: TFileStream;      {<--desti file stream}
  M: TMemoryStream;    {<--source file buffer}
  A: integer;          {<--size of decompressed}
  R: RLiFileRec;
begin
  F := TFileStream.Create(Desti, fmCreate);
  try
    M := TMemoryStream.Create;
    try
      for A := 0 to Files.Count - 1 do
      begin
        M.LoadFromFile(Files[A]);
        FillChar(R, sizeof(R), 0);
        StrPCopy(R.f_name, Files[A]);
        R.f_size := M.Size;
        CompressMemoryStream(M);
        R.c_size := M.Size;
        F.Write(R, FileRecSize);
        F.Write(M.Memory^, M.Size);
      end;
    finally
      M.Free;
    end;
  finally
    F.Free;
  end;
end;

procedure CompressMemoryStream(Stream: TMemoryStream);
var
  M: TMemoryStream;
Begin
  Compress(Stream.Memory, Stream.Size, M);
  try
    Stream.LoadFromStream(M);
    Stream.Position := 0;
  finally
    M.Free;
  end;
end;

procedure CompressStream(Source, Desti: TStream;
  var Bytes: integer);
var
  S: TMemoryStream;
Begin
  S := TMemoryStream.Create;
  try
    S.LoadFromStream(Source);
    CompressMemoryStream(S);
    Bytes := Desti.Write(S.Memory^, S.Size);
  finally
    S.Free;
  end;
end;

procedure CompressStreamToFile(Source: TStream;
  const Desti: string);
var
  F: TFileStream;
  N: integer;
begin
  F := TFileStream.Create(Desti, fmCreate);
  try
    CompressStream(Source, F, N);
  finally
    F.Free;
  end;
end;

procedure Decompress(Source: pointer; Count: integer;
  var Stream: TMemoryStream);
begin
  Stream := TMemoryStream.Create;
  Stream.Write(Source^, Count);
  DecompressMemoryStream(Stream);
end;

procedure DecompressFile(const Source, Desti: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(Source);
    DecompressMemoryStream(S);
    S.SaveToFile(Desti);
  finally
    S.Free;
  end;
end;

procedure DecompressFiles(const Source, DestiDir: string;
  Files: TStrings);
var
  F: TFileStream;     {<--source file stream}
  S: TMemoryStream;   {<--source buffer}
  R: RLiFileRec;        {<--file information}
  E: string;          {<--result file}
  D: string;          {<--result directory}
begin
  F := TFileStream.Create(Source, fmShareDenyWrite);
  try
    S := TMemoryStream.Create;
    try
      D := lse_expand_fname(DestiDir);
      while F.Read(R, FileRecSize) = FileRecSize do
      begin
        S.SetSize(0);
        if R.c_size > 0 then
        begin
          S.CopyFrom(F, R.c_size);
          DecompressMemoryStream(S);
        end;
        E := D + ExtractFileName(R.f_name);
        S.SaveToFile(E);
        if Assigned(Files) then
          Files.Add(E);
      end;
    finally
      S.Free;
    end;
  finally
    F.Free;
  end;
end;

procedure DecompressMemoryStream(Stream: TMemoryStream);
var
  M: TMemoryStream;
  S: TDecompressionStream;
  B: array[0..8191] of byte;
  L: integer;
begin
  Stream.Position := 0;
  S := TDecompressionStream.Create(Stream);
  try
    M := TMemoryStream.Create;
    try
      L := S.Read(B[0], 8192);
      while L > 0 do
      begin
        M.Write(B[0], L);
        L := S.Read(B[0], 8192);
      end;
      Stream.LoadFromStream(M);
      Stream.Position := 0;
    finally
      M.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure DecompressStream(Source, Desti: TStream;
  var Bytes: integer);
var
  S: TMemoryStream;
Begin
  S := TMemoryStream.Create;
  try
    S.LoadFromStream(Source);
    DecompressMemoryStream(S);
    Desti.Write(S.Memory^, S.Size);
  finally
    S.Free;
  end;
end;

function ExtractFile(const Source, FileName,
  DestiFile: string): boolean;
var
  F: TFileStream;     {<--source file stream}
  S: TMemoryStream;   {<--source buffer}
  R: RLiFileRec;        {<--file information}
begin
  Result := false;
  F := TFileStream.Create(Source, fmShareDenyWrite);
  try
    while not Result and (F.Read(R, FileRecSize) = FileRecSize) do
    begin
      if CompareText(R.f_name, FileName) = 0 then
      begin
        S := TMemoryStream.Create;
        try
          S.SetSize(0);
          if R.c_size > 0 then
          begin
            S.CopyFrom(F, R.c_size);
            DecompressMemoryStream(S);
          end;
          S.SaveToFile(DestiFile);
          Result := true;
        finally
          S.Free;
        end;
      end
      else F.Seek(R.c_size, soFromCurrent);
    end;
  finally
    F.Free;
  end;
end;

procedure GetFileList(const Source: string; Files: TStrings);
var
  F: TFileStream;     {<--source file stream}
  R: RLiFileRec;        {<--file information}
begin
  F := TFileStream.Create(Source, fmShareDenyWrite);
  try
    while F.Read(R, FileRecSize) = FileRecSize do
    begin
      Files.Add(R.f_name);
      F.Seek(R.c_size, soFromCurrent);
    end;
  finally
    F.Free;
  end;
end;

end.
