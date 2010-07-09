unit sdl_13;

interface
uses Windows, sysUtils;

{$I jedi-sdl.inc}

const
  SDLLibName = 'SDL.dll';

type
  PSdlDisplayMode = ^TSdlDisplayMode;
  TSdlDisplayMode = record
    format: Uint32;          // pixel format */
    w: integer;              // width */
    h: integer;              // height */
    refresh_rate: integer;   // refresh rate (or zero for unspecified) */
    driverdata: pointer;     // driver-specific data, initialize to 0 */
  end;
  SDL_DisplayMode = TSdlDisplayMode;
  {$EXTERNALSYM SDL_DisplayMode}

  TSdlWindowId = type UInt32;
  SDL_WindowID = TSdlWindowId;
  {$EXTERNALSYM SDL_WindowID}

  TSdlColor32 = type UInt32;

const
  SDL_WINDOWPOS_UNDEFINED = $7FFFFFF;
  SDL_WINDOWPOS_CENTERED = $7FFFFFE;

type

  EBadHandle = class(Exception);

{
 * SDL_WindowEventID
 *
 * Event subtype for window events

    SDL_WINDOWEVENT_NONE,               /**< Never used */
    SDL_WINDOWEVENT_SHOWN,              /**< Window has been shown */
    SDL_WINDOWEVENT_HIDDEN,             /**< Window has been hidden */
    SDL_WINDOWEVENT_EXPOSED,            /**< Window has been exposed and should be redrawn */
    SDL_WINDOWEVENT_MOVED,              /**< Window has been moved to data1,data2 */
    SDL_WINDOWEVENT_RESIZED,            /**< Window size changed to data1xdata2 */
    SDL_WINDOWEVENT_MINIMIZED,          /**< Window has been minimized */
    SDL_WINDOWEVENT_MAXIMIZED,          /**< Window has been maximized */
    SDL_WINDOWEVENT_RESTORED,           /**< Window has been restored to normal size and position */
    SDL_WINDOWEVENT_ENTER,              /**< The window has gained mouse focus */
    SDL_WINDOWEVENT_LEAVE,              /**< The window has lost mouse focus */
    SDL_WINDOWEVENT_FOCUS_GAINED,       /**< The window has gained keyboard focus */
    SDL_WINDOWEVENT_FOCUS_LOST,         /**< The window has lost keyboard focus */
    SDL_WINDOWEVENT_CLOSE               /**< The window manager requests that the window be closed */
}
  TSdlWindowEventID = (sdlweNone, sdlweShown, sdlweHidden, sdlweExposed,
                        sdlweMoved, sdlweResized, sdlweMinimized, sdlweMaximized,
                        sdlweRestored, sdlweEnter, sdlweLeave, sdlweFocusGained,
                        sdlweFocusLost, sdlweClose);
  SDL_WindowEventID = TSdlWindowEventID;
  {$EXTERNALSYM SDL_WindowEventID}

{The following enums are declared in SDL_Video.h, but never actually used as
enumerated types.  They're just sets of constants that get OR'd together to make
a bitfield.  Redefining them here since Delphi has a better way of making
bitfields. Final values ensure that the set will be 32 bits in size.}

(*{
 * SDL_WindowFlags
 *
 * The flags on a window

    SDL_WINDOW_FULLSCREEN = 0x00000001,         /**< fullscreen window, implies borderless */
    SDL_WINDOW_OPENGL = 0x00000002,             /**< window usable with OpenGL context */
    SDL_WINDOW_SHOWN = 0x00000004,              /**< window is visible */
    SDL_WINDOW_BORDERLESS = 0x00000008,         /**< no window decoration */
    SDL_WINDOW_RESIZABLE = 0x00000010,          /**< window can be resized */
    SDL_WINDOW_MINIMIZED = 0x00000020,          /**< minimized */
    SDL_WINDOW_MAXIMIZED = 0x00000040,          /**< maximized */
    SDL_WINDOW_INPUT_GRABBED = 0x00000100,      /**< window has grabbed input focus */
    SDL_WINDOW_INPUT_FOCUS = 0x00000200,        /**< window has input focus */
    SDL_WINDOW_MOUSE_FOCUS = 0x00000400,        /**< window has mouse focus */
    SDL_WINDOW_FOREIGN = 0x00000800             /**< window not created by SDL */
} SDL_WindowFlags;
    Added sdlwUnused to fill the 0x80 gap.*)
  TSdlWindowFlags = set of (sdlwFullscreen, sdlwOpenGl, sdlwShown,
                    sdlwBorderless, sdlwResizable, sdlwMinimized, sdlwMaximized,
                    sdlwUnused, sdlwInputGrabbed, dslwInputFocus, sdlwMouseFocus,
                    sdwlForeign, sdlwForce32 = 31);
  SDL_WindowFlags = TSdlWindowFlags;
  {$EXTERNALSYM SDL_WindowFlags}

(*{
 * SDL_RendererFlags
 *
 * Flags used when creating a rendering context

    SDL_RENDERER_SINGLEBUFFER = 0x00000001,     /**< Render directly to the window, if possible */
    SDL_RENDERER_PRESENTCOPY = 0x00000002,      /**< Present uses a copy from back buffer to the front buffer */
    SDL_RENDERER_PRESENTFLIP2 = 0x00000004,     /**< Present uses a flip, swapping back buffer and front buffer */
    SDL_RENDERER_PRESENTFLIP3 = 0x00000008,     /**< Present uses a flip, rotating between two back buffers and a front buffer */
    SDL_RENDERER_PRESENTDISCARD = 0x00000010,   /**< Present leaves the contents of the backbuffer undefined */
    SDL_RENDERER_PRESENTVSYNC = 0x00000020,     /**< Present is synchronized with the refresh rate */
    SDL_RENDERER_ACCELERATED = 0x00000040       /**< The renderer uses hardware acceleration */
} SDL_RendererFlags; *)
  TSdlRendererFlag = (sdlrSingleBuffer, sdlrPresentCopy, sdlrPresentFlip2,
                      sdlrPresentFlip3, sdlrPresentDiscard, sdlrPresentVsync,
                      sdlrAccelerated, sdlrForce32 = 31);
  TSdlRendererFlags = set of TSdlRendererFlag;

{$MINENUMSIZE 4}
(*{
    SDL_TEXTUREACCESS_STATIC,    /**< Changes rarely, not lockable */
    SDL_TEXTUREACCESS_STREAMING  /**< Changes frequently, lockable */
} SDL_TextureAccess; *)
  PSdlTextureAccess = ^TSdlTextureAccess;
  TSdlTextureAccess = (sdltaStatic, sdltaStreaming, sdltaRenderTarget);
  SDL_TextureAccess = TSdlTextureAccess;
  {$EXTERNALSYM SDL_TextureAccess}
{$MINENUMSIZE 1}

(*{
    SDL_TEXTUREMODULATE_NONE = 0x00000000,     /**< No modulation */
    SDL_TEXTUREMODULATE_COLOR = 0x00000001,    /**< srcC = srcC * color */
    SDL_TEXTUREMODULATE_ALPHA = 0x00000002     /**< srcA = srcA * alpha */
} SDL_TextureModulate; *)
  //"none" left out because a 0 value would break the set.
  //SDL_TEXTUREMODULATE_NONE = []
  TSdlTextureModulate = (sdltmColor, sdltmAlpha, sdltmForce32 = 31);
  TSdlTextureModulates = set of TSdlTextureModulate;

(*{
    SDL_BLENDMODE_NONE = 0x00000000,     /**< No blending */
    SDL_BLENDMODE_MASK = 0x00000001,     /**< dst = A ? src : dst (alpha is mask) */
    SDL_BLENDMODE_BLEND = 0x00000002,    /**< dst = (src * A) + (dst * (1-A)) */
    SDL_BLENDMODE_ADD = 0x00000004,      /**< dst = (src * A) + dst */
    SDL_BLENDMODE_MOD = 0x00000008       /**< dst = src * dst */
} SDL_BlendMode;*)
  //"none" left out because a 0 value would break the set.
  //SDL_BLENDMODE_NONE = []
   TSdlBlendMode = (sdlbMask, sdlbBlend, sdlbAdd, sdlbMod, sdlbForce32 = 31);
   TSdlBlendModes = set of TSdlBlendMode;

(*{
    SDL_TEXTURESCALEMODE_NONE = 0x00000000,     /**< No scaling, rectangles must match dimensions */
    SDL_TEXTURESCALEMODE_FAST = 0x00000001,     /**< Point sampling or equivalent algorithm */
    SDL_TEXTURESCALEMODE_SLOW = 0x00000002,     /**< Linear filtering or equivalent algorithm */
    SDL_TEXTURESCALEMODE_BEST = 0x00000004      /**< Bicubic filtering or equivalent algorithm */
} SDL_TextureScaleMode;*)
  //"none" left out because a 0 value would break the set.
  //SDL_TEXTURESCALEMODE_NONE = []
  TSdlTextureScaleMode = (sdltsFast, sdltsSlow, sdltsBest, sdltsForce32 = 31);
  TSdlTextureScaleModes = set of TSdlTextureScaleMode;

  PSDL_RendererInfo = ^TSDL_RendererInfo;
  TSDL_RendererInfo = record
    name: PAnsiChar;                    // The name of the renderer
    flags: TSdlRendererFlags;           // Supported SDL_RendererFlags
    mod_modes: TSdlTextureModulates;    // Supported channel modulations
    blend_modes: TSdlBlendModes;        // Supported blend modes
    scale_modes: TSdlTextureScaleModes; // Supported scale modes
    num_texture_formats: cardinal;      // The number of available texture formats
    texture_formats: array[0..19] of cardinal; // The available texture formats
    max_texture_width: integer;         // The maximimum texture width */
    max_texture_height: integer;        // The maximimum texture height */
    logical_size: boolean;
  end;

  SDL_RendererInfo = TSDL_RendererInfo;
  {$EXTERNALSYM SDL_RendererInfo}

  PPSdlRect = ^PSdlRect;
  PSdlRect = ^TSdlRect;
  TSdlRect = TRect;
  SDL_Rect = TSdlRect;
  {$EXTERNALSYM SDL_Rect}

  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    r: UInt8;
    g: UInt8;
    b: UInt8;
    unused: UInt8;
  end;
  SDL_Color = TSDL_Color;
  {$EXTERNALSYM SDL_Color}

  PSdlColorArray = ^TSdlColorArray;
  TSdlColorArray = array[0..255] of TSDL_Color;

  PSdlPalette = ^TSdlPalette;
  TSdlPaletteChangedFunc = function(userdata: pointer; palette: PSdlPalette): integer; cdecl;

  PSdlPaletteWatch = ^TSdlPaletteWatch;
  TSdlPaletteWatch = record
    callback: TSdlPaletteChangedFunc;
    userdata: pointer;
    next: PSdlPaletteWatch;
  end;

  TSdlPalette = record
    count: integer;
    colors: PSdlColorArray;
    refcount: integer;
    watch: PSdlPaletteWatch;
  end;

  PSdlPixelFormat = ^TSdlPixelFormat;
  TSdlPixelFormat = record
  private
    FPalette: PSdlPalette;
    FBpp: Uint8;
    FBypp: Uint8;
    FRloss, FGloss, FBloss, FAloss: UInt8;
    FRshift, FGshift, FBshift, FAshift: Uint8;
    FRmask, FGmask, FBmask, FAmask: Uint32;
  public
    property palette: PSdlPalette read FPalette;
    property BitsPerPixel: Uint8 read FBpp;
    property BytesPerPixel: Uint8 read FBypp;
    property RLoss: UInt8 read FRloss;
    property RGoss: UInt8 read FGloss;
    property RBoss: UInt8 read FBloss;
    property RAoss: UInt8 read FAloss;
    property RShift: UInt8 read FRShift;
    property GShift: UInt8 read FGShift;
    property BShift: UInt8 read FBShift;
    property AShift: UInt8 read FAShift;
    property RMask: UInt32 read FRMask;
    property GMask: UInt32 read FGMask;
    property BMask: UInt32 read FBMask;
    property AMask: UInt32 read FAMask;
  end;

  TSdlSurfaceFlag = (sdlsfPrealloc, sdlsfRleAccel, sdlsfColorKey = 17, sdlsfForce32 = 31);
  TSdlSurfaceFlags = set of TSdlSurfaceFlag;

  PSdlSurface = ^TSdlSurface;

  TSdlBlitFunc = function (src: PSdlSurface; srcRect: PSdlRect; dst: PSdlSurface; dstRect: PSdlRect): integer;

  TSdlBlitInfo = record
    src: PByte;
    src_w, src_h: integer;
    src_pitch: integer;
    src_skip: integer;
    dst: PByte;
    dst_w, dst_h: integer;
    dst_pitch: integer;
    dst_skip: integer;
    src_fmt: PSdlPixelFormat;
    dst_fmt: PSdlPixelFormat;
    table: PByte;
    flags: integer;
    colorkey: TSdlColor32;
    r, g, b, a: UInt8;
  end;

{$HINTS OFF} //don't warn me that I'm declaring symbols I don't use. They're
             //used internally by SDL and have to be declared here.
  PSdlBlitMap = ^TSdlBlitMap;
  TSdlBlitMap = record
  private
    dest: PSdlSurface;
    identity: integer;
    blit: TSdlBlitFunc;
    data: pointer;
    info: TSdlBlitInfo;
    formatVersion: cardinal;
  end;

  TSdlSurface = record
  private
    FFlags: TSdlSurfaceFlags;
    FFormat: PSdlPixelFormat;
    FWidth: integer;
    FHeight: integer;
    FPitch: integer;
    FPixels: pointer;
    FUserData: pointer;
    FLocked: integer;
    FLockData: pointer;
    FClipRect: TSdlRect;
    FBlitMap: PSdlBlitMap;
    FFormatVersion: cardinal;
    FRefcount: integer;
    procedure SetClipRect(const Value: TSdlRect);
    function GetMustLock: boolean;
    function GetColorKey: TSdlColor32;
    procedure SetColorKey(const color: TSdlColor32);
    function GetBlendMode: TSdlBlendModes;
    procedure SetBlendMode(const Value: TSdlBlendModes);
  public
    class function Create(width, height, depth: Integer; RMask: UInt32 = 0;
                   GMask: UInt32 = 0; BMask: UInt32 = 0; AMask: UInt32 = 0): PSdlSurface; static;
    class function Convert(source: PSdlSurface; format: PSdlPixelFormat): PSdlSurface; static;
    procedure Free;
    function Fill(area: PSdlRect; color: TSdlColor32): boolean;
    function ClearClipRect: boolean;
    function SetPalette(colors: PSdlColorArray; start, count: integer): boolean;
    procedure CopyPaletteFrom(const source: PSdlSurface);
    function LockSurface: boolean;
    procedure UnlockSurface;
    procedure AcquireReference;
    function BlitFrom(src: PSdlSurface; srcrect, dstrect: PSdlRect): integer; inline;

    property Flags: TSdlSurfaceFlags read FFlags;
    property Format: PSdlPixelFormat read FFormat;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Pitch: integer read FPitch;
    property Pixels: pointer read FPixels;
    property Tag: pointer read FUserData write FUserData;
    property Locked: integer read FLocked;
    property LockData: pointer read FLockData;
    property ClipRect: TSdlRect read FClipRect write SetClipRect;
    property ColorKey: TSdlColor32 read GetColorKey write SetColorKey;
    property BlendMode: TSdlBlendModes read GetBlendMode write SetBlendMode;
    property Refcount: integer read FRefcount;
    property MustLock: boolean read GetMustLock;
  end;
  SDL_Surface = TSdlSurface;
  {$EXTERNALSYM SDL_Surface}
{$HINTS ON}

  TSdlTextureID = type UInt32;
  SDL_TextureID = TSdlTextureID;
  {$EXTERNALSYM SDL_TextureID}

  TSdlTexture = record
  private
    FId: TSdlTextureID;
    function GetSize: TPoint;
    function GetColor(index: byte): TSDL_Color;
    function GetAlpha: byte;
    procedure SetAlpha(const Value: byte);
    function GetScaleMode: TSdlTextureScaleMode;
    procedure SetScaleMode(const Value: TSdlTextureScaleMode);
  public
    constructor Create(format: Uint32; access: TSdlTextureAccess; w, h: integer); overload;
    constructor Create(format: Uint32; surface: PSdlSurface); overload;
    procedure Free;

    property ID: TSdlTextureID read FId;
    property size: TPoint read GetSize;
    property color[index: byte]: TSDL_Color read GetColor;
    property alpha: byte read GetAlpha write SetAlpha;
    property scaleMode: TSdlTextureScaleMode read GetScaleMode write SetScaleMode;
  end;

  // SDL_error.h types
  TSDL_errorcode = (
    SDL_ENOMEM,
    SDL_EFREAD,
    SDL_EFWRITE,
    SDL_EFSEEK,
    SDL_LASTERROR);

  SDL_errorcode = TSDL_errorcode;
{$EXTERNALSYM SDL_errorcode}

{$I sdl_EventConsts.inc}

type
  TSdlScancode = 0..SDL_NUM_SCANCODES;
  TSdlKey = integer;

{*
 *  \brief The SDL keysym structure, used in key events.
 }
  TSdlKeysym = packed record
    scancode: TSdlScancode;
    sym: TSdlKey;
    mods: word;
    unicode: cardinal;
  end;

{*
 *  \brief Window state change event data (event.window.*)
 }
  TSdlWindowEvent = packed record
    Type_: cardinal;
    Window: cardinal;
    Event: TSdlWindowEventID;
    pad1: byte; pad2: word;
    Data1: integer;
    Data2: integer;
  end;

{*
 *  \brief Keyboard button event structure (event.key.*)
 *}
  TSdlKeyboardEvent = packed record
    Type_: cardinal;
    Window: cardinal;
    Which: byte;
    State: byte;
    pad: word;
    KeySym: TSdlKeysym;
  end;

{*
 *  \brief Keyboard text editing event structure (event.edit.*)
 }
  TSdlTextEditingEvent = packed record
  private
   const TEXT_SIZE = 32;
   type TEditBuffer = array[0..TEXT_SIZE - 1] of AnsiChar;
  public
    Type_: cardinal;
    Window: cardinal;
    Which: byte;
    Text: TEditBuffer;
    Start: integer;
    Length: integer;
  end;

  TSdlTextInputEvent = packed record
  private
   const TEXT_SIZE = 32;
   type TEditBuffer = array[0..TEXT_SIZE - 1] of AnsiChar;
  public
    type_: cardinal;
    window: cardinal;
    which: byte;
    pad1: byte; pad2: word;
    text: TEditBuffer;
  end;

{*
 *  \brief Mouse motion event structure (event.motion.*)
 }
  TSdlMouseMotionEvent = packed record
    type_: cardinal;
    window: cardinal;
    which: byte;
    state: byte;
    pad: word;
    x: integer;
    y: integer;
    z: integer;
    pressure: integer;
    pressure_min: integer;
    pressure_max: integer;
    rotation: integer;
    tilt_x: integer;
    tilt_y: integer;
    cursor: integer;
    xrel: integer;
    yrel: integer;
  end;

{*
 *  \brief Mouse button event structure (event.button.*)
 }
  TSdlMouseButtonEvent = packed record
    type_: cardinal;
    window: cardinal;
    which: byte;
    button: byte;
    state: byte;
    pad: byte;
    x: integer;
    y: integer;
  end;

{*
 *  \brief Mouse wheel event structure (event.wheel.*)
 }
  TSdlMouseWheelEvent = packed record
    type_: cardinal;
    window: cardinal;
    which: byte;
    pad1: byte;
    pad2: word;
    x: integer;
    y: integer;
  end;

{*
 * \brief Tablet pen proximity event
 }
  TSdlProximityEvent = packed record
    type_: cardinal;
    window: cardinal;
    which: byte;
    pad1: byte;
    pad2: word;
    cursor: integer;
    x: integer;
    y: integer;
  end;

{*
 *  \brief Joystick axis motion event structure (event.jaxis.*)
 }
  TSdlJoyAxisEvent = packed record
    type_: cardinal;
    which: byte;
    axis: byte;
    pad: word;
    value: integer;
  end;

{*
 *  \brief Joystick trackball motion event structure (event.jball.*)
 }
  TSdlJoyBallEvent = packed record
    type_: cardinal;
    which: byte;
    ball: byte;
    pad: word;
    xrel: integer;
    yrel: integer;
  end;

TSdlHatPosition = set of (sdlhUp, sdlhRight, sdlhDown, sdlhLeft);

{*
 *  \brief Joystick hat position change event structure (event.jhat.*)
 }
  TSdlJoyHatEvent = packed record
    type_: cardinal;
    which: byte;
    hat: byte;
    value: TSdlHatPosition;
    pad: byte;
  end;

{*
 *  \brief Joystick button event structure (event.jbutton.*)
 }
  TSdlJoyButtonEvent = packed record
    type_: cardinal;
    which: byte;
    buton: byte;
    stte: byte;
    pad: byte;
  end;

{*
 *  \brief The "quit requested" event
 }
  TSdlQuitEvent = packed record
    type_: cardinal;
  end;

{*
 *  \brief A user-defined event type (event.user.*)
 }
  TSdlUserEvent = packed record
    type_: cardinal;
    windowID: cardinal;
    code: integer;
    data1: pointer;
    data2: pointer;
  end;

{*
 *  \brief A video driver dependent system event (event.syswm.*)
 }
  TSdlSysWMEvent = packed record
     type_: cardinal;
     msg: pointer;
  end;

  TSdlEvent = record
    case UInt32 of
      SDL_FIRSTEVENT: (type_: cardinal);
      SDL_QUITEV: (quit: TSdlQuitEvent );
      SDL_WINDOWEVENT: (win: TSdlWindowEvent);
      SDL_SYSWMEVENT: (syswin: TSdlSysWMEvent);
      SDL_KEYDOWN, SDL_KEYUP: (key: TSdlKeyboardEvent);
      SDL_TEXTEDITING: (edit: TSdlTextEditingEvent);
      SDL_TEXTINPUT: (input: TSdlTextInputEvent);
      SDL_MOUSEMOTION: (motion: TSdlMouseMotionEvent);
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP: (button: TSDLMouseButtonEvent);
      SDL_MOUSEWHEEL: (wheel: TSdlMouseWheelEvent);
      SDL_PROXIMITYIN, SDL_PROXIMITYOUT: (prox: TSdlProximityEvent);
      SDL_JOYAXISMOTION: (jaxis: TSdlJoyAxisEvent );
      SDL_JOYBALLMOTION: (jball: TSdlJoyBallEvent );
      SDL_JOYHATMOTION: (jhat: TSdlJoyHatEvent );
      SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: (jbutton: TSdlJoyButtonEvent );
      SDL_USEREVENT : ( user : TSdlUserEvent );
  end;
  PSdlEvent = ^TSdlEvent;

  TSdlEventAction = (SdlAddEvent, SdlPeekEvent, SdlGetEvent);

  TSdlEventFilter = function (userdata: pointer; event: PSdlEvent): integer;

  TSdlEventArray = array of TSdlEvent;

function SDL_GetCurrentDisplayMode(var mode: TSdlDisplayMode): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetCurrentDisplayMode}

{**
 * SDL_CreateWindow
 *
 * Create a window with the specified position, dimensions, and flags.
 *
 * title: The title of the window, in UTF-8 encoding
 * x: The x position of the window, SDL_WINDOWPOS_CENTERED, or SDL_WINDOWPOS_UNDEFINED
 * y: The y position of the window, SDL_WINDOWPOS_CENTERED, or SDL_WINDOWPOS_UNDEFINED
 * w: The width of the window
 * h: The height of the window
 * flags: The flags for the window. Only the following flags will have any effect:
 * [sdlwFullscreen, sdlwOpenGl, sdlwShown, sdlwBorderless, sdlwResizable,
 * sdlwMinimized, sdlwMaximized, sdlwInputGrabbed]
 * Any others will be ignored.
 *
 * Returns the id of the window created, or zero if window creation failed.
 *}
function SDL_CreateWindow(title: PAnsiChar; x, y, w, h: integer; flags: TSdlWindowFlags): TSdlWindowID;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_CreateWindow'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_CreateWindow}

{**
 * SDL_CreateWindowFrom
 *
 * Create an SDL window struct from an existing native window.
 *
 * data: A pointer to driver-dependent window creation data
 *
 * Returns the id of the window created, or zero if window creation failed.
 *
 * Warning: This function is NOT SUPPORTED, use at your own risk!
 *}
{$IFDEF WIN32}
function SDL_CreateWindowFrom(data: HWND): TSdlWindowId;
{$ELSE}
function SDL_CreateWindowFrom(data: Pointer): TSdlWindowId;
{$ENDIF}
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_CreateWindowFrom'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_CreateWindowFrom}

procedure SDL_DestroyWindow(windowID: TSdlWindowID);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_DestroyWindow'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_DestroyWindow}

function SDL_GetWindowFlags(windowID: TSdlWindowID): TSdlWindowFlags; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetWindowFlags}

procedure SDL_ShowWindow(windowID: TSdlWindowID); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_ShowWindow}

(**
 * SDL_SetWindowSize
 *
 * Sets the size of the window's client area.
 *
 * NOTE: You can't change the size of a fullscreen window, it automatically
 * matches the size of the display mode.
 *)
procedure SDL_SetWindowSize(windowID: TSdlWindowID; w, h: integer); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_SetWindowSize}

(**
 * \SDL_GetWindowSize
 *
 * Gets the size of the window's client area.
 *)
procedure SDL_GetWindowSize(windowID: TSdlWindowID; var w, h: integer); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetWindowSize}

(**
 * SDL_SetWindowSize
 *
 * Sets the size of the window's client area.
 *
 * NOTE: You can't change the size of a fullscreen window, it automatically
 * matches the size of the display mode.
 *)
function SDL_SetWindowLogicalSize(windowID: TSdlWindowID; w, h: integer): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_SetWindowLogicalSize}

(**
 * \SDL_GetWindowSize
 *
 * Gets the size of the window's client area.
 *)
procedure SDL_GetWindowLogicalSize(windowID: TSdlWindowID; var w, h: integer); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetWindowLogicalSize}

function SDL_GetNumRenderDrivers(): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetNumRenderDrivers}

function SDL_GetRenderDriverInfo(index: integer; var info: TSDL_RendererInfo): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetRenderDriverInfo}

function SDL_GetRendererInfo(var info: TSDL_RendererInfo): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetRenderDriverInfo}

(**
 * SDL_CreateRenderer
 *
 * Creates and makes active a 2D rendering context for a window.
 *
 * windowID: The window used for rendering
 * index: The index of the rendering driver to initialize, or -1 to initialize
 *        the first one supporting the requested flags.
 * flags: SDL_RendererFlags
 *
 * Returns 0 on success, -1 if there was an error creating the renderer.
 *
 *)
function SDL_CreateRenderer(windowID: TSdlWindowID; index: integer; flags: TSdlRendererFlags): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_CreateRenderer}

function SDL_SelectRenderer(windowID: TSdlWindowID): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_SelectRenderer}

{*
 * SDL_SetRenderDrawColor
 *
 * Set the color used for drawing operations (Fill and Line).
 *
 * Returns 0 on success, or -1 if there is no rendering context current
 }
function SDL_SetRenderDrawColor(r, g, b, a: byte): integer; cdecl; overload; external SDLLibName;
{$EXTERNALSYM SDL_SetRenderDrawColor}

function SDL_SetRenderDrawColor(color: TSDL_Color): integer; overload;

(**
 *  SDL_SetRenderDrawBlendMode
 *
 *  Sets the blend mode used for drawing operations (Fill and Line).
 *
 *  blendMode: Mode to use for blending.
 *
 *  Returns 0 on success, or -1 if there is no rendering context current.
 *
 *  Note: If the blend mode is not supported, the closest supported mode is
 *        chosen.
 *)
function SDL_SetRenderDrawBlendMode(blendMode: TSdlBlendModes): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_SetRenderDrawBlendMode}

(**
 *  Gets the blend mode used for drawing operations.
 *
 *  blendMode: Used to return the current blend mode.
 *
 *  Returns 0 on success, or -1 if there is no rendering context current.
 *)
function SDL_GetRenderDrawBlendMode(var blendMode: TSdlBlendModes): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetRenderDrawBlendMode}

{*
 * SDL_GetRenderDrawColor
 *
 * Get the color used for drawing operations (Fill and Line).
 *
 * Returns 0 on success, or -1 if there is no rendering context current
 }
function SDL_GetRenderDrawColor(var r, g, b, a: byte): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetRenderDrawColor}

function SDL_RenderDrawRect(const rect: PSdlRect): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_RenderDrawRect}

function SDL_RenderDrawLine(x1, y1, x2, y2: integer): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_RenderDrawLine}

function SDL_RenderFillRect(const rect: PSdlRect): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_RenderFillRect}

{ Copy a portion of the texture to the current rendering target.
  textureID: The source texture.
  srcrect: A pointer to the source rectangle, or nil for the entire texture.
  dstrect: A pointer to the destination rectangle, or nil for the entire rendering target.
  returns 0 on success, or -1 if there is no rendering context current, or the driver
  doesn't support the requested operation.
}
function SDL_RenderCopy(textureID: TSdlTexture; const srcrect, dstrect: PSdlRect): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_RenderCopy}

procedure SDL_RenderPresent(); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_RenderPresent}

procedure SDL_DestroyRenderer(windowID: TSdlWindowID ); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_DestroyRenderer}

{ Maps an RGB triple to an opaque pixel value for a given pixel format }
function SDL_MapRGB(format: PSdlPixelFormat; r, g, b: UInt8): TSdlColor32;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_MapRGB'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_MapRGB}

{ Maps an RGBA quadruple to a pixel value for a given pixel format }
function SDL_MapRGBA(format: PSdlPixelFormat; r: UInt8; g: UInt8; b: UInt8; a: UInt8): TSdlColor32;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_MapRGBA'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_MapRGBA}

{ Maps a pixel value into the RGB components for a given pixel format }
procedure SDL_GetRGB(pixel: TSdlColor32; fmt: PSdlPixelFormat; var r, g, b: UInt8);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_GetRGB'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_GetRGB}

{ Maps a pixel value into the RGBA components for a given pixel format }
procedure SDL_GetRGBA(pixel: TSdlColor32; fmt: PSdlPixelFormat; var r, g, b: UInt8);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_GetRGBA'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_GetRGBA}

function SDL_CreateRGBSurface(flags: UInt32; width, height, depth: Integer; RMask, GMask, BMask, AMask: UInt32): PSdlSurface;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_CreateRGBSurface'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_CreateRGBSurface}

function SDL_ConvertSurface(src: PSdlSurface; fmt: PSdlPixelFormat; flags: Uint32): PSdlSurface;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_ConvertSurface'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_ConvertSurface}

procedure SDL_FreeSurface(surface: PSdlSurface);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_FreeSurface'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_FreeSurface}

function SDL_FillRect(dst: PSdlSurface; dstrect: PSdlRect; color: TSdlColor32) : Integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_FillRect'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_FillRect}

function SDL_SetColorKey(surface: PSdlSurface; flag: LongBool; key: TSdlColor32) : Integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_SetColorKey'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_SetColorKey}

function SDL_SetClipRect(surface: PSdlSurface; rect: PSdlRect): boolean; cdecl;
external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_SetClipRect'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_SetClipRect}

function SDL_LockSurface(surface: PSdlSurface): Integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_LockSurface'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_LockSurface}

procedure SDL_UnlockSurface(surface: PSdlSurface);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_UnlockSurface'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_UnlockSurface}

function SDL_BlitSurface(src: PSdlSurface; srcrect: PSdlRect; dst: PSdlSurface; dstrect: PSdlRect): Integer; inline;
{$EXTERNALSYM SDL_BlitSurface}

{  This is the public blit function, SDL_BlitSurface(), and it performs
   rectangle validation and clipping before passing it to SDL_LowerBlit() }
function SDL_UpperBlit(src: PSdlSurface; srcrect: PSdlRect; dst: PSdlSurface; dstrect: PSdlRect): Integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_UpperBlit'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_UpperBlit}

{ This is a semi-private blit function and it performs low-level surface
  blitting only. }
function SDL_LowerBlit(src: PSdlSurface; srcrect: PSdlRect; dst: PSdlSurface; dstrect: PSdlRect): Integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_LowerBlit'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_LowerBlit}

function SDL_SetSurfaceBlendMode(surface: PSdlSurface; blendMode: TSdlBlendModes): integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_SetSurfaceBlendMode'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_SetSurfaceBlendMode}

function SDL_GetSurfaceBlendMode(surface: PSdlSurface; var blendMode: TSdlBlendModes): integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_GetSurfaceBlendMode'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_GetSurfaceBlendMode}

function SDL_SetPaletteColors(palette: PSdlPalette;  colors: PSdlColorArray; firstcolor, ncolors: integer): integer;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_SetPaletteColors'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_SetPaletteColors}

function SDL_CreateTexture(format: Uint32; access: TSdlTextureAccess; w, h: integer): TSdlTextureID; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_CreateTexture}

function SDL_CreateTextureFromSurface(format: Uint32; surface: PSdlSurface): TSdlTextureID; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_CreateTextureFromSurface}

procedure SDL_DestroyTexture(textureID: TSDLTextureID); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_DestroyTexture}

function SDL_QueryTexture(textureID: TSdlTextureID; format: PCardinal;
                          access: PSdlTextureAccess; w, h: PInteger): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_QueryTexture}

function SDL_SetTargetTexture(texture: TSDLTexture): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_SetTargetTexture}

(**
 * SDL_SetTexturePalette
 *
 * Updates an indexed texture with a color palette
 *
 * texture: The texture to update
 * colors: The array of RGB color data
 * firstcolor: The first index to update
 * ncolors: The number of palette entries to fill with the color data
 * returns 0 on success, or -1 if the texture is not valid or not an indexed texture
 *)
function SDL_SetTexturePalette(textureID: TSdlTextureID; const colors: PSdlColorArray;
                               firstcolor, ncolors: integer): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_SetTexturePalette}

(**
 * SDL_GetTexturePalette
 *
 * Retrieves the color palette from an indexed texture
 *
 * texture: The texture to update
 * colors: The array of RGB color data
 * firstcolor: The first index to update
 * ncolors: The number of palette entries to fill with the color data
 * returns 0 on success, or -1 if the texture is not valid or not an indexed texture
 *)
function SDL_GetTexturePalette(textureID: TSdlTextureID; const colors: PSdlColorArray;
                               firstcolor, ncolors: integer): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetTexturePalette}

(**
 * SDL_SetTextureAlphaMod
 *
 * Sets an additional alpha value used in render copy operations
 *
 * texture: The texture to update
 * alpha: The source alpha value multiplied into copy operations.
 * returns 0 on success, or -1 if the texture is not valid or alpha modulation is not supported
 *)
function SDL_SetTextureAlphaMod(textureID: TSdlTextureID; alpha: byte): integer; cdecl;
external SDLLibName;
{$EXTERNALSYM SDL_SetTextureAlphaMod}

(**
 * SDL_GetTextureAlphaMod
 *
 * Gets the additional alpha value used in render copy operations
 *
 * texture: The texture to query
 * alpha: A pointer filled in with the source alpha value
 * returns 0 on success, or -1 if the texture is not valid
 *)
function SDL_GetTextureAlphaMod(textureID: TSdlTextureID; var alpha: byte): integer; cdecl;
external SDLLibName;
{$EXTERNALSYM SDL_GetTextureAlphaMod}

(**
 * SDL_SetTextureScaleMode
 *
 * Sets the scale mode used for texture copy operations.
 *
 * texture: The texture to update
 * scaleMode TSdlTextureScaleModes to use for texture scaling.
 * returns 0 on success, or -1 if the texture is not valid or alpha modulation is not supported
 *)
function SDL_SetTextureScaleMode(textureID: TSdlTextureID; scale: TSdlTextureScaleModes): integer; cdecl;
external SDLLibName;
{$EXTERNALSYM SDL_SetTextureScaleMode}

(**
 * SDL_GetTextureScaleMode
 *
 * Gets the scale mode used for texture copy operations.
 *
 * texture: The texture to query
 * scaleMode: Returns the current scale mode.
 * returns 0 on success, or -1 if the texture is not valid
 *)
function SDL_GetTextureScaleMode(textureID: TSdlTextureID; var scale: TSdlTextureScaleModes): integer; cdecl;
external SDLLibName;
{$EXTERNALSYM SDL_GetTextureScaleMode}

{*
 *  Pumps the event loop, gathering events from the input devices.
 *
 *  This function updates the event queue and internal input device state.
 *
 *  This should only be run in the thread that sets the video mode.
 }
procedure SDL_PumpEvents; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_PumpEvents}

{*
 *  Checks the event queue for messages and optionally returns them.
 *
 *  If action is SdlAddEvent, up to (numevents) events will be added to
 *  the back of the event queue.
 *
 *  If action is SdlPeekEvent, up to (numevents) events at the front
 *  of the event queue, in the range defined by (minType) and (maxType), will be
 *  returned and will not be removed from the queue.
 *
 *  If action is SdlGetEvent, up to (numevents) events at the front
 *  of the event queue, matching (mask), will be returned and will be
 *  removed from the queue.
 *
 *  Returns The number of events actually stored, or -1 if there was an error.
 *
 *  This function is thread-safe.
 }
function SDL_PeepEvents(events: PSdlEvent; numevents: integer; action: TSdlEventAction;
                        minType, maxType: cardinal): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_PeepEvents}

function SDL_GetEvents(minType: cardinal = 0; maxType: cardinal = SDL_LASTEVENT): TSdlEventArray;

{*
 *  Checks to see if certain event types are in the event queue.
 }
function SDL_HasEvent(type_: Cardinal): boolean; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_HasEvent}

function SDL_HasEvents(minType, maxType: Cardinal): boolean; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_HasEvents}

{*
 *  This procedure clears events from the event queue
 }
procedure SDL_FlushEvent(type_: Cardinal); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_FlushEvent}
procedure SDL_FlushEvents(minType, maxType: Cardinal); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_FlushEvents}

{*
 *  \brief Polls for currently pending events.
 *
 *  \return 1 if there are any pending events, or 0 if there are none available.
 *
 *  \param event If not NULL, the next event is removed from the queue and
 *               stored in that area.
 }
function SDL_PollEvent(event: PSdlEvent): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_PollEvent}

{*
 *  \brief Waits indefinitely for the next available event.
 *
 *  \return 1, or 0 if there was an error while waiting for events.
 *
 *  \param event If not NULL, the next event is removed from the queue and
 *               stored in that area.
 }
function SDL_WaitEvent(event: PSdlEvent): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_WaitEvent}

{*
 *  \brief Waits until the specified timeout (in milliseconds) for the next
 *         available event.
 *
 *  \return 1, or 0 if there was an error while waiting for events.
 *
 *  \param event If not NULL, the next event is removed from the queue and
 *               stored in that area.
 }
function SDL_WaitEventTimeout(event: PSdlEvent; timeout: integer): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_WaitEventTimeout}

{*
 *  \brief Add an event to the event queue.
 *
 *  \return 1 on success, 0 if the event was filtered, or -1 if the event queue
 *          was full or there was some other error.
 }
function SDL_PushEvent(const event: TSdlEvent): integer; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_PushEvent}

{*
 *  Sets up a filter to process all events before they change internal state and
 *  are posted to the internal event queue.
 *
 *  The filter is protypted as:
 *  \code
 *      int SDL_EventFilter(void *userdata, SDL_Event * event);
 *  \endcode
 *
 *  If the filter returns 1, then the event will be added to the internal queue.
 *  If it returns 0, then the event will be dropped from the queue, but the
 *  internal state will still be updated.  This allows selective filtering of
 *  dynamically arriving events.
 *
 *  \warning  Be very careful of what you do in the event filter function, as
 *            it may run in a different thread!
 *
 *  There is one caveat when dealing with the ::SDL_QUITEVENT event type.  The
 *  event filter is only called when the window manager desires to close the
 *  application window.  If the event filter returns 1, then the window will
 *  be closed, otherwise the window will remain open if possible.
 *
 *  If the quit event is generated by an interrupt signal, it will bypass the
 *  internal queue and be delivered to the application at the next event poll.
 }
procedure SDL_SetEventFilter(filter: TSdlEventFilter; userdata: pointer);
cdecl; external SDLLibName; {$EXTERNALSYM SDL_SetEventFilter}

{*
 *  Return the current event filter - can be used to "chain" filters.
 *  If there is no event filter set, this function returns SDL_FALSE.
 }
function SDL_GetEventFilter(out filter: TSdlEventFilter; out userdata: pointer): boolean;
cdecl; external SDLLibName; {$EXTERNALSYM SDL_GetEventFilter}

{*
 *  Run the filter function on the current event queue, removing any
 *  events for which the filter returns 0.
 }
procedure SDL_FilterEvents(filter: TSdlEventFilter; userdata: pointer); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_GetEventFilter}

{*
 *  This function allows you to set the state of processing certain events.
 *   - If (state) is set to SDL_IGNORE, that event will be automatically
 *     dropped from the event queue and will not event be filtered.
 *   - If (state) is set to SDL_ENABLE, that event will be processed
 *     normally.
 *   - If (state) is set to SDL_QUERY, SDL_EventState() will return the
 *     current processing state of the specified event.
 }
function SDL_EventState(type_: cardinal; state: integer): byte; cdecl; external SDLLibName;
{$EXTERNALSYM SDL_EventState}

function SDL_GetEventState(type_:cardinal): byte; inline;

{*
 *  This function allocates a set of user-defined events, and returns
 *  the beginning event number for that set of events.
 *
 *  If there aren't enough user-defined events left, this function
 *  returns (Uint32)-1
 }
function SDL_RegisterEvents(numevents: integer): cardinal;  cdecl; external SDLLibName;
{$EXTERNALSYM SDL_RegisterEvents}

(**
 * SDL_Free
 *
 * Frees memory allocated within SDL's private heap.
 *
 * mem: The pointer to free
 *)
procedure SDL_free(mem: pointer); cdecl; external SDLLibName;
{$EXTERNALSYM SDL_Free}

{------------------------------------------------------------------------------}
{ error-handling }
{------------------------------------------------------------------------------}
// Public functions
function SDL_GetError: PAnsiChar;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_GetError'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_GetError}
procedure SDL_SetError(fmt: PAnsiChar);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_SetError'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_SetError}
procedure SDL_ClearError;
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_ClearError'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_ClearError}

{$IFNDEF WINDOWS}
procedure SDL_Error(Code: TSDL_errorcode);
cdecl; external {$IFNDEF NDS}{$IFDEF __GPC__}name 'SDL_Error'{$ELSE} SDLLibName{$ENDIF __GPC__}{$ENDIF};
{$EXTERNALSYM SDL_Error}
{$ENDIF}

// Private error message function - used internally
procedure SDL_OutOfMemory;

function SDL_RendererIndex(name: AnsiString): integer;

implementation

{ TSdlSurface }

class function TSdlSurface.Convert(source: PSdlSurface; format: PSdlPixelFormat): PSdlSurface;
begin
  if not assigned(format) then
    format := source.FFormat;
  result := SDL_ConvertSurface(source, format, 0);
  if not assigned(result) then
    OutOfMemoryError;
end;

procedure TSdlSurface.CopyPaletteFrom(const source: PSdlSurface);
begin
   self.SetPalette(source.Format.palette.colors, 0, source.Format.palette.count);
end;

class function TSdlSurface.Create(width, height, depth: Integer; RMask, GMask,
  BMask, AMask: UInt32): PSdlSurface;
begin
  result := SDL_CreateRGBSurface(0, width, height, depth, rmask, gmask, bmask, amask);
  if not assigned(result) then
    OutOfMemoryError;
end;

function TSdlSurface.Fill(area: PSdlRect; color: TSdlColor32): boolean;
begin
   result := SDL_FillRect(@self, area, color) = 0;
end;

procedure TSdlSurface.Free;
begin
   SDL_FreeSurface(@self);
end;

function TSdlSurface.GetBlendMode: TSdlBlendModes;
begin
   SDL_GetSurfaceBlendMode(@self, result);
end;

function TSdlSurface.GetColorKey: TSdlColor32;
begin
  result := FBlitMap.info.colorkey;
end;

function TSdlSurface.GetMustLock: boolean;
begin
  result := sdlsfRleAccel in FFlags;
end;

function TSdlSurface.LockSurface: boolean;
begin
  result := SDL_LockSurface(@self) = 0;
end;

procedure TSdlSurface.UnlockSurface;
begin
  SDL_UnlockSurface(@self);
end;

procedure TSdlSurface.SetBlendMode(const Value: TSdlBlendModes);
begin
   SDL_SetSurfaceBlendMode(@self, value);
end;

procedure TSdlSurface.SetClipRect(const Value: TSdlRect);
begin
  SDL_SetClipRect(@self, @value);
end;

procedure TSdlSurface.SetColorKey(const color: TSdlColor32);
begin
  SDL_SetColorKey(@self, true, color);
  self.BlendMode := [sdlbBlend];
end;

function TSdlSurface.SetPalette(colors: PSdlColorArray; start,
  count: integer): boolean;
begin
  result := SDL_SetPaletteColors(FFormat.palette, colors, 0, count) = 0;
end;

procedure TSdlSurface.AcquireReference;
begin
  inc(FRefcount);
end;

function TSdlSurface.ClearClipRect: boolean;
begin
  result := SDL_SetClipRect(@self, nil);
end;

function TSdlSurface.BlitFrom(src: PSdlSurface; srcrect: PSdlRect; dstrect: PSdlRect): integer;
begin
   result := sdl_upperBlit(src, srcRect, @self, dstRect);
end;

{ TSdlTexture }

constructor TSdlTexture.Create(format: Uint32; access: TSdlTextureAccess; w, h: integer);
begin
  FId := SDL_CreateTexture(format, access, w, h);
  if FId = 0 then
    raise EBadHandle.Create(string(SDL_GetError));
end;

constructor TSdlTexture.Create(format: Uint32; surface: PSdlSurface);
begin
  FId := SDL_CreateTextureFromSurface(format, surface);
  if FId = 0 then
    raise EBadHandle.Create(string(SDL_GetError));
end;

procedure TSdlTexture.Free;
begin
  if FId <> 0 then
  begin
    SDL_DestroyTexture(FId);
    FId := 0;
  end;
end;

function TSdlTexture.GetColor(index: byte): TSDL_Color;
var
   colors: TSdlColorArray;
begin
  SDL_GetTexturePalette(FId, @colors, 0, 255);
  result := colors[index];
end;

function TSdlTexture.GetSize: TPoint;
begin
  if SDL_QueryTexture(FId, nil, nil, @result.X, @result.Y) <> 0 then
    raise EBadHandle.Create('SDL_QueryTexture failed due to invalid texture.');
end;

function TSdlTexture.GetAlpha: byte;
begin
   if SDL_GetTextureAlphaMod(FId, result) <> 0 then
      raise EBadHandle.Create('Alpha not supported for this texture.');
end;

procedure TSdlTexture.SetAlpha(const Value: byte);
begin
   if SDL_SetTextureAlphaMod(FId, Value) <> 0 then
      raise EBadHandle.Create('Alpha not supported for this texture.');
end;

procedure TSdlTexture.SetScaleMode(const Value: TSdlTextureScaleMode);
begin
   SDL_SetTextureScaleMode(FID, [Value]);
end;

{$WARN NO_RETVAL OFF}
function TSdlTexture.GetScaleMode: TSdlTextureScaleMode;
var
   modes: TSdlTextureScaleModes;
   mode: TSdlTextureScaleMode;
begin
   SDL_GetTextureScaleMode(FID, modes);
   assert(modes <> []);
   for mode in modes do
      Exit(mode);
end;
{$WARN NO_RETVAL ON}

procedure SDL_OutOfMemory;
begin
  {$IFNDEF WINDOWS}
  SDL_Error(SDL_ENOMEM);
  {$ENDIF}
end;

function SDL_BlitSurface(src: PSdlSurface; srcrect: PSdlRect; dst:
  PSdlSurface; dstrect: PSdlRect): Integer;
begin
  Result := SDL_UpperBlit(src, srcrect, dst, dstrect);
end;

function SDL_RendererIndex(name: AnsiString): integer;
var
  count: integer;
  i: integer;
  info: TSDL_RendererInfo;
begin
  count := SDL_GetNumRenderDrivers;
  for I := 0 to count - 1 do
  begin
    SDL_GetRenderDriverInfo(i, info);
    if info.name = name then
    begin
      result := i;
      Exit;
    end;
  end;
  result := -1;
end;

function SDL_SetRenderDrawColor(color: TSDL_Color): integer;
begin
   result := SDL_SetRenderDrawColor(color.r, color.g, color.b, color.unused);
end;

function SDL_GetEvents(minType: cardinal = 0; maxType: cardinal = SDL_LASTEVENT): TSdlEventArray;
begin
   SetLength(result, 128);
   SetLength(result, SDL_PeepEvents(@result[0], 128, SdlGetEvent, minType, maxType));
end;

function SDL_GetEventState(type_:cardinal): byte; inline;
begin
   result := SDL_EventState(type_, SDL_QUERY);
end;

initialization
sdl_getError; //do not smartlink this out!

end.
