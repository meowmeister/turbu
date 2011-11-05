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
          'uniform vec3 rgbValues;'
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
          '    '#9'rgbColor = slideHue(rgbColor);'
          #9'grayVec = rgbColor * Luma;'
          #9'gray = (grayVec.r + grayVec.g + grayVec.b);'
          
            #9'return clamp(mix(vec3(gray, gray, gray), rgbColor, satMult) * v' +
            'alMult, 0.0, 1.0);'
          '}'
          ''
          'void MixChannel(inout float channel, float modifier)'
          '{'
          #9'if (modifier < 1.0)'
          #9#9'channel *= modifier;'
          #9'else if (modifier > 1.0)'
          #9#9'channel += modifier - 1.0;'
          '}'
          ''
          'vec3 MixRGB(vec3 rgbColor)'
          '{'
          #9'MixChannel(rgbColor.r, rgbValues.r);'
          #9'MixChannel(rgbColor.g, rgbValues.g);'
          #9'MixChannel(rgbColor.b, rgbValues.b);'
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
          
            'varying vec2 texture_coordinate; uniform sampler2D my_color_text' +
            'ure; '
          ''
          'void main()'
          '{'
          '  vec4 vertex = gl_Vertex;'
          ''
          '  gl_Position = gl_ModelViewProjectionMatrix * vertex;'
          
            '  // Passing The Texture Coordinate Of Texture Unit 0 To The Fra' +
            'gment Shader'
          '  texture_coordinate = vec2(gl_MultiTexCoord0);'
          ''
          
            '  gl_FrontColor = texture2D(my_color_texture, texture_coordinate' +
            ');'
          '}')
      end>
    Left = 24
    Top = 16
  end
  object fragment: TJvMultiStringHolder
    MultipleStrings = <
      item
        Name = 'default'
        Strings.Strings = (
          'void main()'
          '{'
          '    gl_FragColor = gl_color;'
          '}')
      end>
    Left = 136
    Top = 16
  end
end
