unit dm_shaders;

interface

uses
   SysUtils, Classes, Generics.collections, OpenGL,
   JvStringHolder,
   SDL;

type
   TdmShaders = class(TDataModule)
      fragLibs: TJvMultiStringHolder;
      vertex: TJvMultiStringHolder;
      fragment: TJvMultiStringHolder;
      procedure DataModuleCreate(Sender: TObject);
      procedure DataModuleDestroy(Sender: TObject);
   private type
      TUniform = record
         prog: integer;
         name: string;
         constructor Create(aProg: integer; const aName: string);
      end;

   private
      FMap: TDictionary<string, integer>;
      FPrograms: TDictionary<TArray<integer>, integer>;
      FUniforms: TDictionary<TUniform, integer>;

      glCreateProgram: function: integer; stdcall;
      glCreateShader: function(shaderType: cardinal): integer; stdcall;
      glDeleteProgram: procedure(programObj: integer); stdcall;
      glDeleteShader: procedure(shaderObj: integer); stdcall;
      glShaderSource: procedure(shaderObj: integer; count: integer; _string: PPAnsiChar; lengths: PInteger); stdcall;
      glCompileShader: procedure(shaderObj: integer); stdcall;
      glGetShaderiv: procedure(shaderObj: integer; pname: cardinal; params: PInteger); stdcall;
      glGetShaderInfoLog: procedure(shaderObj: integer; maxLength: integer; var length: integer; infoLog: PAnsiChar); stdcall;
      glAttachShader: procedure(programObj, shaderObj: integer); stdcall;
      glLinkProgram: procedure(programObj: integer); stdcall;
      glGetProgramiv: procedure(programObj: integer; pname: cardinal; params: PInteger); stdcall;
      glGetProgramInfoLog: procedure(programObj: integer; maxLength: integer; var length: integer; infoLog: PAnsiChar); stdcall;
      glUseProgram: procedure(programObj: integer); stdcall;
      glGetUniformLocation: function(programObj: integer; const char: PAnsiChar): glint; stdcall;
      glUniform1f: procedure(location: GLint; v0: GLfloat); stdcall;
      glUniform1i: procedure(location: GLint; v0: GLint); stdcall;
      glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); stdcall;

      function GetShader(const name: string; container: TJvMultiStringHolder): integer;
      function GetShaderType(container: TJvMultiStringHolder): cardinal;
      function BuildProgram(units: TArray<integer>): integer;
      function GetUniformLocation(handle: integer; const name: string): integer;
   public
      function ShaderProgram(const vert, frag: string; const libs: string = ''): integer;
      procedure UseShaderProgram(value: integer);
      procedure SetUniformValue(handle: integer; const name: string; value: glInt); overload;
      procedure SetUniformValue(handle: integer; const name: string; value: glFloat); overload;
      procedure SetUniformValue(handle: integer; const name: string; const value: TGLArrayf3); overload;
   end;

implementation

{$R *.dfm}

const
   GL_FRAGMENT_SHADER = $8B30;
   GL_VERTEX_SHADER = $8B31;
   GL_COMPILE_STATUS = $8B81;
   GL_LINK_STATUS = $8B82;
   GL_FALSE = 0;
   GL_INFO_LOG_LENGTH = $8B84;

{ TdmShaders }

procedure TdmShaders.DataModuleCreate(Sender: TObject);
begin
   FMap := TDictionary<string, integer>.Create;
   FPrograms := TDictionary<TArray<integer>, integer>.Create;
   FUniforms := TDictionary<TUniform, integer>.Create;

   glCreateProgram := SDL_GL_GetProcAddress('glCreateProgram');
   glCreateShader := SDL_GL_GetProcAddress('glCreateShader');
   glDeleteProgram := SDL_GL_GetProcAddress('glDeleteProgram');
   glDeleteShader := SDL_GL_GetProcAddress('glDeleteShader');
   glShaderSource := SDL_GL_GetProcAddress('glShaderSource');
   glCompileShader := SDL_GL_GetProcAddress('glCompileShader');
   glGetShaderiv := SDL_GL_GetProcAddress('glGetShaderiv');
   glGetShaderInfoLog := SDL_GL_GetProcAddress('glGetShaderInfoLog');
   glAttachShader := SDL_GL_GetProcAddress('glAttachShader');
   glLinkProgram := SDL_GL_GetProcAddress('glLinkProgram');
   glGetProgramiv := SDL_GL_GetProcAddress('glGetProgramiv');
   glGetProgramInfoLog := SDL_GL_GetProcAddress('glGetProgramInfoLog');
   glUseProgram := SDL_GL_GetProcAddress('glUseProgram');
   glGetUniformLocation := SDL_GL_GetProcAddress('glGetUniformLocation');
   glUniform1f := SDL_GL_GetProcAddress('glUniform1f');
   glUniform1i := SDL_GL_GetProcAddress('glUniform1i');
   glUniform3fv := SDL_GL_GetProcAddress('glUniform3fv');
end;

procedure TdmShaders.DataModuleDestroy(Sender: TObject);
var
   handle: integer;
begin
   for handle in FPrograms.Values do
      glDeleteProgram(handle);
   for handle in FMap.Values do
      glDeleteShader(handle);
   FUniforms.Free;
   FPrograms.Free;
   FMap.Free;
end;

function TdmShaders.GetShader(const name: string;
  container: TJvMultiStringHolder): integer;
var
   shaderText, strShaderType: string;
   pShaderText: PAnsiChar;
   status, infoLogLength, dummy: integer;
   strInfoLog: AnsiString;
begin
   if not FMap.TryGetValue(name, result) then
   begin
      if name[1] = '#' then
         shaderText := name
      else shaderText := container.StringsByName[name].Text;
      result := glCreateShader(GetShaderType(container));
      pShaderText := PAnsiChar(AnsiString(shaderText));
      glShaderSource(result, 1, @pShaderText, nil);
      glCompileShader(result);
      glGetShaderiv(result, GL_COMPILE_STATUS, @status);
      if (status = GL_FALSE) then
      begin
         glGetShaderiv(result, GL_INFO_LOG_LENGTH, @infoLogLength);

         SetLength(strInfoLog , infoLogLength + 1);
         glGetShaderInfoLog(result, infoLogLength, dummy, @strInfoLog[1]);

         case GetShaderType(container) of
            GL_VERTEX_SHADER: strShaderType := 'vertex';
            GL_FRAGMENT_SHADER: strShaderType := 'fragment';
            else strShaderType := '';
         end;
         raise Exception.CreateFmt('Compile failure in %s shader: %s', [strShaderType, strInfoLog]);
       end;
       FMap.Add(name, result);
   end;
end;

function TdmShaders.GetShaderType(container: TJvMultiStringHolder): cardinal;
begin
   if container = vertex then
      result := GL_VERTEX_SHADER
   else if (container = fragment) or (container = fragLibs) then
      result := GL_FRAGMENT_SHADER
   else raise Exception.Create('Bad container');
end;

function TdmShaders.GetUniformLocation(handle: integer; const name: string): integer;
var
   uni: TUniform;
begin
   uni := TUniform.Create(handle, name);
   if not FUniforms.TryGetValue(uni, result) then
   begin
      result := glGetUniformLocation(handle, PAnsiChar(AnsiString(name)));
      if result = -1 then
         raise Exception.CreateFmt('No uniform "%s" found in program %d', [name, handle]);
      FUniforms.Add(uni, result);
   end;
end;

function TdmShaders.BuildProgram(units: TArray<integer>): integer;
var
   shader: integer;
   status, infoLogLength, dummy: integer;
   strInfoLog: AnsiString;
begin
   result := glCreateProgram;
   for shader in units do
      glAttachShader(result, shader);

   glLinkProgram(result);

   glGetProgramiv (result, GL_LINK_STATUS, @status);
   if (status = GL_FALSE) then
   begin
      glGetProgramiv(result, GL_INFO_LOG_LENGTH, @infoLogLength);
      SetLength(strInfoLog, infoLogLength + 1);
      glGetProgramInfoLog(result, infoLogLength, dummy, @strInfoLog[1]);
      raise Exception.CreateFmt('Shader link failure: %s', [strInfoLog]);
   end;
   FPrograms.Add(units, result);
end;

procedure TdmShaders.SetUniformValue(handle: integer; const name: string; value: glInt);
begin
   glUniform1i(GetUniformLocation(handle, name), value);
end;

procedure TdmShaders.SetUniformValue(handle: integer; const name: string; value: glFloat);
begin
   glUniform1f(GetUniformLocation(handle, name), value);
end;

procedure TdmShaders.SetUniformValue(handle: integer; const name: string;
  const value: TGLArrayf3);
begin
   glUniform3fv(GetUniformLocation(handle, name), 3, @value[0]);
end;

function TdmShaders.ShaderProgram(const vert, frag, libs: string): integer;
var
   vertMain, fragMain: cardinal;
   units: TArray<integer>;
   parser: TStringList;
   i: integer;
begin
   vertMain := GetShader(vert, vertex);
   fragMain := GetShader(frag, fragment);
   units := TArray<integer>.Create(vertmain, fragMain);
   if libs <> '' then
   begin
      parser := TStringList.Create;
      try
         parser.CommaText := libs;
         SetLength(units, 2 + parser.Count);
         for i := 0 to parser.Count - 1 do
            units[2 + i] := GetShader(parser[i], fragLibs);
      finally
         parser.Free;
      end;
   end;
   TArray.Sort<integer>(units);
   if not FPrograms.TryGetValue(units, result) then
      result := BuildProgram(units);
end;

procedure TdmShaders.UseShaderProgram(value: integer);
begin
   glUseProgram(value);
end;

{ TdmShaders.TUniform }

constructor TdmShaders.TUniform.Create(aProg: integer; const aName: string);
begin
   self.prog := aProg;
   self.name := aName;
end;

end.
