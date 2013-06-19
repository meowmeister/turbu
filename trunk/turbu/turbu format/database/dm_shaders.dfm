object dmShaders: TdmShaders
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object fragLibs: TJvMultiStringHolder
    MultipleStrings = <
      item
        Name = 'shift'
        Strings.Strings = (
          'uniform int hShift;'
          'uniform float satMult;'
          'uniform float valMult;'
          'uniform vec4 rgbValues;'
          ''
          '#define RED 0'
          '#define GREEN 1'
          '#define BLUE 2'
          ''
          'const float epsilon = 1e-6;'
          'const vec3 Luma = vec3(0.299, 0.587, 0.114);'
          ''
          'vec3 RGBtoHSV(vec3 color)'
          '{'
          
            '    /* hue, saturation and value are all in the range [0,1> here' +
            ', as opposed to their'
          
            '       normal ranges of: hue: [0,360>, sat: [0, 100] and value: ' +
            '[0, 256> */'
          '    float hue, saturation, value, chroma, huePrime;'
          '    float minCol, maxCol;'
          '    int maxIndex;'
          ''
          #9'minCol = min(color.r, min(color.g, color.b));'
          #9'maxCol = max(color.r, max(color.g, color.b));'
          ''
          #9'if (maxCol == color.r){'
          #9#9'maxIndex = RED;}'
          #9'else if (maxCol == color.g){'
          #9#9'maxIndex = GREEN;}'
          #9'else maxIndex = BLUE;'
          ''
          '    chroma = maxCol - minCol;'
          ''
          '    /* Hue */'
          '    if( chroma < epsilon){'
          '        huePrime = 0.0;'
          '    }'
          '    else if(maxIndex == RED){'
          '        huePrime = ((color.g - color.b) / chroma);'
          '    }'
          '    else if(maxIndex == GREEN){'
          '        huePrime = ((color.b - color.r) / chroma) + 2.0;'
          '    }'
          '    else if(maxIndex == BLUE){'
          '        huePrime = ((color.r - color.g) / chroma) + 4.0;'
          '    }'
          '    '
          '    hue = huePrime / 6.0;'
          ''
          '    /* Saturation */'
          '    if(maxCol < epsilon)'
          '    {'
          '        saturation = 0.0;'
          '        hue = 1.0;'
          '    }'
          '    else'
          '        saturation = (maxCol - minCol) / maxCol;'
          ''
          '    /* Value */'
          '    value = maxCol;'
          ''
          '    return vec3(hue, saturation, value);'
          '}'
          ''
          'vec3 HSVtoRGB(vec3 color)'
          '{'
          '    float f,p,q,t, hueRound;'
          '    int hueIndex;'
          '    float hue, saturation, value;'
          '    vec3 result, luminance;'
          ''
          '    /* just for clarity */'
          '    hue = color.r;'
          '    saturation = color.g;'
          '    value = color.b;'
          ''
          '    hueRound = floor(hue * 6.0);'
          '    hueIndex = int(hueRound) % 6;'
          '    f = (hue * 6.0) - hueRound;'
          '    p = value * (1.0 - saturation);'
          '    q = value * (1.0 - f*saturation);'
          '    t = value * (1.0 - (1.0 - f)*saturation);'
          '    '
          '    switch(hueIndex)'
          '    {'
          '        case 0:'
          '            result = vec3(value,t,p);'
          '        break;'
          '        case 1:'
          '            result = vec3(q,value,p);'
          '        break;'
          '        case 2:'
          '            result = vec3(p,value,t);'
          '        break;'
          '        case 3:'
          '            result = vec3(p,q,value);'
          '        break;'
          '        case 4:'
          '            result = vec3(t,p,value);'
          '        break;'
          '        case 5:'
          '            result = vec3(value,p, q);'
          '        break;'
          '    }'
          '    return clamp(result, 0.0, 1.0);'
          '}'
          ''
          'vec3 slideHue(vec3 rgbColor)'
          '{'
          #9'vec3 hsvColor = RGBtoHSV(rgbColor);'
          #9'hsvColor.r = fract(hsvColor.r + (float(hShift) / 360.0) + 1.0);'
          #9'return HSVtoRGB(hsvColor);'
          '}'
          ''
          'vec3 HSVShift(vec3 rgbColor)'
          '{'
          '    vec3 grayVec;'
          '    float gray;'
          ''
          '    if (hShift != 0)'
          '        rgbColor = slideHue(rgbColor);'
          '    grayVec = rgbColor * Luma;'
          '    gray = (grayVec.r + grayVec.g + grayVec.b);'
          
            '    return clamp(mix(vec3(gray, gray, gray), rgbColor, satMult) ' +
            '* valMult, 0.0, 1.0);'
          '}'
          ''
          
            'void MixChannel(inout float channel, float modifier, float alpha' +
            ')'
          '{'
          #9'if (modifier < 1.0)'
          #9#9'channel *= modifier * alpha;'
          #9'else if (modifier > 1.0)'
          #9#9'channel += (modifier - 1.0) * alpha;'
          '}'
          ''
          'vec3 MixRGB(vec3 rgbColor)'
          '{'
          #9'MixChannel(rgbColor.r, rgbValues.r, rgbValues.a);'
          #9'MixChannel(rgbColor.g, rgbValues.g, rgbValues.a);'
          #9'MixChannel(rgbColor.b, rgbValues.b, rgbValues.a);'
          #9'return rgbColor;'
          '}'
          ''
          'vec3 ProcessShifts(vec3 rgbColor)'
          '{'
          #9'return(MixRGB(HSVShift(rgbColor)));'
          '}')
      end>
    Left = 136
    Top = 88
  end
  object vertex: TJvMultiStringHolder
    MultipleStrings = <
      item
        Name = 'default'
        Strings.Strings = (
          'varying vec4 v_color;'
          'varying vec2 texture_coordinate; '
          ' '
          'void main() '
          '{ '
          '  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; '
          
            '  // Passing The Texture Coordinate Of Texture Unit 0 To The Fra' +
            'gment Shader '
          '  texture_coordinate = vec2(gl_MultiTexCoord0); '
          '  v_color = gl_Color;'
          '  gl_FrontColor = gl_Color; '
          '}')
      end
      item
        Name = 'TextV'
        Strings.Strings = (
          'void main()'
          '{'
          '   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;'
          '   gl_TexCoord[0] = gl_MultiTexCoord0;'
          '   gl_TexCoord[1] = gl_MultiTexCoord1;'
          '   gl_FrontColor = gl_Color;'
          '}')
      end>
    Left = 24
    Top = 16
  end
  object fragment: TJvMultiStringHolder
    MultipleStrings = <
      item
        Name = 'defaultF'
        Strings.Strings = (
          'varying vec4 v_color;'
          'varying vec2 texture_coordinate; '
          'uniform sampler2DRect baseTexture; '
          ' '
          'void main() '
          '{ '
          
            '    gl_FragColor = texture2DRect(baseTexture, texture_coordinate' +
            ') * v_color; '
          '}')
      end
      item
        Name = 'tint'
        Strings.Strings = (
          'varying vec2 texture_coordinate; '
          'varying vec4 v_color;'
          'uniform sampler2DRect baseTexture;'
          'uniform vec4 rgbValues;'
          ''
          'vec3 ProcessShifts(vec3 rgbColor);'
          ''
          'void main(void)'
          '{'
          
            '    vec4 lColor =  texture2DRect(baseTexture, texture_coordinate' +
            ');'
          
            '    gl_FragColor = vec4(ProcessShifts(lColor.rgb), lColor.a * v_' +
            'color.a);'
          '}')
      end
      item
        Name = 'textF'
        Strings.Strings = (
          'uniform sampler2DRect texAlpha;'
          'uniform sampler2DRect texRGB;'
          'void main()'
          '{   '
          '   float alpha = texture2DRect(texAlpha, gl_TexCoord[0].st).a;'
          '   vec3 rgb = texture2DRect(texRGB, gl_TexCoord[1].st).rgb;'
          '   gl_FragColor = vec4(rgb, alpha);'
          '}')
      end
      item
        Name = 'textShadow'
        Strings.Strings = (
          'uniform sampler2DRect texAlpha;'
          'uniform float strength;'
          'float alpha;'
          'void main()'
          '{ '
          '   alpha = texture2DRect(texAlpha, gl_TexCoord[0].st).a;'
          '   if (alpha < 0.1)'
          '      discard;'
          '   gl_FragColor = vec4(0, 0, 0, alpha * strength);'
          '}'
          '')
      end
      item
        Name = 'textBlit'
        Strings.Strings = (
          'uniform sampler2D texAlpha;'
          'float alpha;'
          'void main()'
          '{ '
          '   alpha = texture2D(texAlpha, gl_TexCoord[0].st).a;'
          '   gl_FragColor = vec4(0, 0, 0, alpha);'
          '}'
          '')
      end
      item
        Name = 'flash'
        Strings.Strings = (
          'varying vec2 texture_coordinate; '
          'uniform sampler2DRect baseTexture;'
          'uniform vec4 flashColor;'
          ''
          'void main(void)'
          '{    '
          
            '    vec4 lColor =  texture2DRect(baseTexture, texture_coordinate' +
            ');'
          
            '    vec3 mixColor = mix(lColor.rgb, flashColor.rgb, flashColor.a' +
            ');'
          '    gl_FragColor = vec4(mixColor, lColor.a);'
          '}'
          '')
      end
      item
        Name = 'noAlpha'
        Strings.Strings = (
          'varying vec2 texture_coordinate;'
          'uniform sampler2DRect baseTexture;'
          ''
          'void main()'
          '{'
          
            '    gl_FragColor = vec4(texture2DRect(baseTexture, texture_coord' +
            'inate).rgb, gl_Color.a);'
          '}'
          '')
      end
      item
        Name = 'Ellipse'
        Strings.Strings = (
          'uniform vec2 tl;'
          'uniform vec2 br;'
          'float x;'
          'float y;'
          'float x0, y0, a, b;'
          'float dist;'
          ''
          'float sqr(float value)'
          '{'
          '   return value * value;'
          '}'
          ''
          'void main()'
          '{ '
          '   x = gl_TexCoord[0].s;'
          '   y = gl_TexCoord[0].t;'
          ''
          '   x0 =(tl.x + br.x) / 2.0;'
          '   y0 =(tl.y + br.y) / 2.0;'
          '   a = (tl.x - br.x) / 2.0;'
          '   b = (tl.y - br.y) / 2.0;'
          '   dist = sqr((x-x0)/a)+sqr((y-y0)/b);'
          '   if (abs(1.0 - dist) > 0.5)'
          '      discard;'
          ''
          '   gl_FragColor = vec4(1, 1, 1, 1);'
          '}')
      end
      item
        Name = 'flashtint'
        Strings.Strings = (
          'varying vec2 texture_coordinate; '
          'uniform sampler2DRect baseTexture;'
          'uniform vec4 flashColor;'
          'varying vec4 v_color;'
          'uniform vec4 rgbValues;'
          ''
          'vec3 ProcessShifts(vec3 rgbColor);'
          ''
          'void main(void)'
          '{    '
          
            '    vec4 lColor =  texture2DRect(baseTexture, texture_coordinate' +
            ');'
          
            '    vec3 mixColor = mix(lColor.rgb, flashColor.rgb, flashColor.a' +
            ');'
          
            '    gl_FragColor = vec4(ProcessShifts(mixColor), lColor.a * v_co' +
            'lor.a);'
          '}')
      end>
    Left = 136
    Top = 16
  end
end
