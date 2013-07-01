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
//      glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); stdcall;
//      glUniform4fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); stdcall;
      glUniform4f: procedure(location: GLint; v1, v2, v3, v4: GLFloat); stdcall;

      function GetShader(const name: string; container: TJvMultiStringHolder): integer;
      function GetShaderType(container: TJvMultiStringHolder): cardinal;
      function BuildProgram(units: TArray<integer>): integer;
      function GetUniformLocation(handle: integer; const name: string): integer;
   public
      function ShaderProgram(const vert, frag: string; const libs: string = ''): integer;
      procedure UseShaderProgram(value: integer);
      procedure SetUniformValue(handle: integer; const name: string; value: glInt); overload;
      procedure SetUniformValue(handle: integer; const name: string; value: glFloat); overload;
      procedure SetUniformValue(handle: integer; const name: string; const value: TGLArrayf4); overload;
   end;

procedure glCheckError;

implementation
uses
   Generics.Defaults;

{$R *.dfm}

const
   GL_FRAGMENT_SHADER = $8B30;
   GL_VERTEX_SHADER = $8B31;
   GL_COMPILE_STATUS = $8B81;
   GL_LINK_STATUS = $8B82;
   GL_FALSE = 0;
   GL_INFO_LOG_LENGTH = $8B84;

procedure glCheckError;
{$IFDEF DEBUG}
var
   err: glEnum;
begin
{   err := glGetError;
   if err <> GL_NO_ERROR then
      asm int 3 end;}
end;
{$ELSE}
begin
end;
{$ENDIF}

{ TdmShaders }

function UniformsEqual(const left, right: TdmShaders.TUniform): boolean;
begin
   result := (left.prog = right.prog) and (left.name = right.name);
end;

function UniformHash(const value: TdmShaders.Tuniform): integer;
begin
   result := BobJenkinsHash(value.name[1], length(value.name) * sizeof(char), 0) xor value.prog;
end;

procedure TdmShaders.DataModuleCreate(Sender: TObject);
begin
   FMap := TDictionary<string, integer>.Create;
   FPrograms := TDictionary<TArray<integer>, integer>.Create;
   FUniforms := TDictionary<TUniform, integer>.Create(TEqualityComparer<TUniform>.Construct(uniformsEqual, uniformHash));

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
//   glUniform3fv := SDL_GL_GetProcAddress('glUniform3fv');
//   glUniform4fv := SDL_GL_GetProcAddress('glUniform4fv');
   glUniform4f := SDL_GL_GetProcAddress('glUniform4f');
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
   glCheckError;
   uni := TUniform.Create(handle, name);
   if not FUniforms.TryGetValue(uni, result) then
   begin
      result := glGetUniformLocation(handle, PAnsiChar(AnsiString(name)));
      glCheckError;
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
   glCheckError;
   result := glCreateProgram;
   for shader in units do
   begin
      glAttachShader(result, shader);
      glCheckError;
   end;

   glLinkProgram(result);
   glCheckError;

   glGetProgramiv (result, GL_LINK_STATUS, @status);
   glCheckError;
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
   glCheckError;
end;

procedure TdmShaders.SetUniformValue(handle: integer; const name: string; value: glFloat);
begin
   glUniform1f(GetUniformLocation(handle, name), value);
   glCheckError;
end;

procedure TdmShaders.SetUniformValue(handle: integer; const name: string;
  const value: TGLArrayf4);
begin
   glUniform4f(GetUniformLocation(handle, name), value[0], value[1], value[2], value[3]);
//   glUniform4fv(GetUniformLocation(handle, name), 4, @value[0]);
   glCheckError;
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
   glCheckError;
   glUseProgram(value);
   glCheckError;
end;

{ TdmShaders.TUniform }

constructor TdmShaders.TUniform.Create(aProg: integer; const aName: string);
begin
   self.prog := aProg;
   self.name := aName;
end;

end.
