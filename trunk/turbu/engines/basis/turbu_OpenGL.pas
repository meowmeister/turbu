unit turbu_OpenGL;

interface
uses
   OpenGL,
   SDL;

const
   GL_VERTEX_ARRAY = $8074;
   GL_ARRAY_BUFFER = $8892;
   GL_DYNAMIC_DRAW = $88E8;
   GL_STREAM_DRAW = $88E0;
   GL_TEXTURE_RECTANGLE_ARB = $84F5;
   GL_TEXTURE_COORD_ARRAY = $8078;
   GL_CURRENT_PROGRAM = $8B8D;
   GL_TEXTURE0_ARB = $84C0;
   GL_TEXTURE1_ARB = $84C1;
   GL_MULTISAMPLE = $809D;

type
   TGLsizei = Integer;
   TGLenum = Cardinal;
   TGLuint = Cardinal;
   TGLint = Integer;
   PGLvoid = Pointer;
   GLHandle = Integer;
   GLenum = Cardinal;

var
   glGenBuffers: procedure(n: TGLsizei; buffers: PGLuint); stdcall;
   glBindBuffer: procedure(target: TGLenum; buffer: TGLuint); stdcall;
   glBufferData: procedure(target: TGLenum; size: TGLsizei; const data: PGLvoid; usage: TGLenum); stdcall;
   glEnableClientState: procedure(_array: TGLenum); stdcall;
   glDisableClientState: procedure(_array: TGLenum); stdcall;
   glVertexPointer: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); stdcall;
   glTexCoordPointer: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); stdcall;
   glDrawArrays: procedure(mode: TGLenum; first: TGLint; count: TGLsizei); stdcall;
   glBindTexture: procedure(target: TGLenum; texture: TGLuint); stdcall;
   glUseProgram: procedure(programObj: GLhandle); stdcall;
   glActiveTextureARB: procedure(texture: GLenum); stdcall;
   glMultiTexCoord2iARB: procedure(target: GLenum; s: GLint; t: GLint); stdcall;
   glBlendFuncSeparate: procedure(srcRGB, dstRGB, srcAlpha, dstAlpha: GLenum); stdcall;

   ImplementationRead: boolean;

procedure InitOpenGL;

implementation

procedure InitOpenGL;
begin
   glGenBuffers := SDL_GL_GetProcAddress('glGenBuffers');
   glBindBuffer := SDL_GL_GetProcAddress('glBindBuffer');
   glBufferData := SDL_GL_GetProcAddress('glBufferData');
   glEnableClientState := SDL_GL_GetProcAddress('glEnableClientState');
   glDisableClientState := SDL_GL_GetProcAddress('glDisableClientState');
   glVertexPointer := SDL_GL_GetProcAddress('glVertexPointer');
   glTexCoordPointer := SDL_GL_GetProcAddress('glTexCoordPointer');
   glDrawArrays := SDL_GL_GetProcAddress('glDrawArrays');
   glBindTexture := SDL_GL_GetProcAddress('glBindTexture');
   glUseProgram := SDL_GL_GetProcAddress('glUseProgram');
   glActiveTextureARB := SDL_GL_GetProcAddress('glActiveTextureARB');
   glMultiTexCoord2iARB := SDL_GL_GetProcAddress('glMultiTexCoord2iARB');
   glBlendFuncSeparate := SDL_GL_GetProcAddress('glBlendFuncSeparate');
end;

end.
