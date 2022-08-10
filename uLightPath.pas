unit uLightPath;

{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
INTERFACE
uses SysUtils,Classes,uVect,uBMP,uModel,uXML,Math,getopts;


TYPE 
  LightPathRecord=RECORD
    LPMax:Integer;
    Ary:array[0..5] of VertexRecord;
    SR: SceneRec;
    procedure clear;
    procedure SetScene(sr_ :SceneRecord )
    procedure Add(cf_,p_,n_   : VecRecord;id_:integer);
    function GetPath(i:integer):VertexRecord;
    procedure SetLightPath;
  END;

  LightPathList=RECORD
    LPLMax:integer;
    ary:array[0..255] of LightPathRecord;
    procedure Add(LP : LigthPathRecord);
    procedure Clear;
    function GetLigthPath(i:integer):LigthPathRecord;
  END;




IMPLEMENTATION

procedure LightPathList.Add(LP :LightPathRecord );
BEGIN
  Inc(LMax);
  ary[LMax]:=LP;
END;

procedure LightPathList.Clear;
BEGIN
  LMax:=-1;
END;

function LightPathList.GetPath(i:integer):LightPathRecord;
BEGIN
  IF i>LMax THEN result:=NIL ELSE result:=SphereClass(sph[Ary[i]]);
END;

procedure LightPathList.clear;
BEGIN
  LPMax:=-1;
END;
procedure LightPathList.SetScene(sr_ :SceneRecord );
begin
   SR:=sr_;
end;

procedure LightPathList.Add(cf_,p_,n_: VecRecord;id_:integer);
BEGIN
  IF LPMax>=4 THEN BEGIN Writeln('Over Limit LPMax');halt;END;
  Inc(LPMax);
  WITH Ary[LPMax] DO BEGIN
    cf:=cf_;p:=p_;n:=n_;id:=id_;
  END;
END;
function LightPathList.GetPath(i:integer):VertexRecord;
BEGIN
  result:=Ary[i];
END;

procedure LightPathList.SetLightPath;
var
   v1  : VertextRecord;
   i,j : integer;
   
begin
   for i:=0 to SR.spl.count-1 do begin
      IF SphereClass(SR.spl[i]).isLight THEN BEGIN
         v1:=SR.GetLight(i);
         add(v1);
         for j:=1 to 4 do begin
         end;
      end;
   end;
end;

BEGIN
END.


