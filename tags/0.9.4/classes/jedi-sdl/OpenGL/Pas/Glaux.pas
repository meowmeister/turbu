unit Glaux;

interface

uses Windows,Opengl12;

type
	TAUX_RGBImageRec= record
		sizeX, sizeY: TGLint;
		data: pointer;
	end;
	PTAUX_RGBImageRec= ^TAUX_RGBImageRec;

function auxDIBImageLoadA(const dibfile: PChar): PTAUX_RGBImageRec; stdcall;
procedure auxWireSphere(value: TGLdouble);stdcall;
procedure auxSolidSphere(value: TGLdouble);stdcall;
procedure auxWireCube(value: TGLdouble);stdcall;
procedure auxSolidCube(value: TGLdouble);stdcall;
procedure auxWireBox(value,value1,value2: TGLdouble);stdcall;
procedure auxSolidBox(value,value1,value2: TGLdouble);stdcall;
procedure auxWireTorus(value,value1: TGLdouble);stdcall;
procedure auxSolidTorus(value,value1: TGLdouble);stdcall;
procedure auxWireCylinder(value,value1: TGLdouble);stdcall;
procedure auxSolidCylinder(value,value1: TGLdouble);stdcall;
procedure auxWireIcosahedron(value: TGLdouble);stdcall;
procedure auxSolidIcosahedron(value: TGLdouble);stdcall;
procedure auxWireOctahedron(value: TGLdouble);stdcall;
procedure auxSolidOctahedron(value: TGLdouble);stdcall;
procedure auxWireTetrahedron(value: TGLdouble);stdcall;
procedure auxSolidTetrahedron(value: TGLdouble);stdcall;
procedure auxWireDodecahedron(value: TGLdouble);stdcall;
procedure auxSolidDodecahedron(value: TGLdouble);stdcall;
procedure auxWireCone(value,value1: TGLdouble);stdcall;
procedure auxSolidCone(value,value1: TGLdouble);stdcall;
procedure auxWireTeapot(value: TGLdouble);stdcall;
procedure auxSolidTeapot(value: TGLdouble);stdcall;

const
	glaux1 = 'glaux.dll';

implementation

function auxDIBImageLoadA; external glaux1;
procedure auxWireSphere;external glaux1;
procedure auxSolidSphere;external glaux1;
procedure auxWireCube;external glaux1;
procedure auxSolidCube;external glaux1;
procedure auxWireBox;external glaux1;
procedure auxSolidBox;external glaux1;
procedure auxWireTorus;external glaux1;
procedure auxSolidTorus;external glaux1;
procedure auxWireCylinder;external glaux1;
procedure auxSolidCylinder;external glaux1;
procedure auxWireIcosahedron;external glaux1;
procedure auxSolidIcosahedron;external glaux1;
procedure auxWireOctahedron;external glaux1;
procedure auxSolidOctahedron;external glaux1;
procedure auxWireTetrahedron;external glaux1;
procedure auxSolidTetrahedron;external glaux1;
procedure auxWireDodecahedron;external glaux1;
procedure auxSolidDodecahedron;external glaux1;
procedure auxWireCone;external glaux1;
procedure auxSolidCone;external glaux1;
procedure auxWireTeapot;external glaux1;
procedure auxSolidTeapot;external glaux1;


end.
