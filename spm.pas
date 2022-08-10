program smallpt;
{$MODE objfpc}{$H+}
{$INLINE ON}

uses SysUtils,Classes,uVect,uBMP,uModel,uFlux,uXML,Math,getopts;

const 
  eps=1e-4;
  INF=1e20;

VAR
   x,y,sx,sy,s                     : INTEGER;
   w,h,samps,height                  : INTEGER;
   temp                            : VecRecord;
   tempRay                           : RayRecord;
   Cam                              : CameraRecord;
   tColor,r: VecRecord;

BMPClass:BMPIOClass;
   T1,T2:TDateTime;
   HH,MM,SS,MS:WORD;
   vColor:rgbColor;
   ArgInt:integer;
   FN,ArgFN:string;
   c:char;
   Rt:TRenderClass;
   ModelID,AlgoID:integer;
   ScR:SceneRecord;
BEGIN
  FN:='temp.bmp';
  Rt:=TRenderClass.Create;
  InitScene;
  ModelID:=0;AlgoID:=1;
  ScR.spl:=CopyScene(ModelID);
  w:=320 ;h:=240;  samps := 16;
  c:=#0;
  repeat
    c:=getopt('m:o:a:s:w:x:');

    case c of
      'm': BEGIN
          ArgInt:=StrToInt(OptArg);
          ModelId:=ArgInt;
          FN:=ExtractFileName(paramStr(0));
          Delete(FN,Length(FN)-3,4);
          ScR.spl:=CopyScene(ArgInt);
          writeln('Model of Scene =',ArgInt,' ',ScName[ArgInt]);
      END;
      'a' : BEGIN
         ArgInt:=StrToInt(OptArg);
         AlgoID:=ArgInt;
         CASE ArgInt OF
           1 : BEGIN
             Writeln('Render=Orignal')
           END;
           2 : BEGIN
             Rt:=TNEERenderClass.Create;
             Writeln('Render=NEE');
           END;
           3 : BEGIN
             Rt:=TLoopRenderClass.Create;
             Writeln('Render=Non Loop');
           END;
           4 : BEGIN
             Rt:=TLightPathRenderClass.Create;
             writeln('Render=LightPath');
           END;
         END;
      END;
      'o'    : BEGIN
         ArgFN:=OptArg;
         IF ArgFN<>'' THEN FN:=ArgFN;
         writeln ('Output FileName =',FN);
      END;
      's'    : BEGIN
        ArgInt:=StrToInt(OptArg);
        samps:=ArgInt;
        writeln('samples =',ArgInt);
      END;
      'w'    : BEGIN
         ArgInt:=StrToInt(OptArg);
         w:=ArgInt;h:=w *3 div 4;
         writeln('w=',w,' ,h=',h);
      END;
      'x'    : BEGIN
         writeln('FN=',OptArg);
         ScR:=ReadXMLConf(OptArg);
         w:=Scr.Cam.w;h:=ScR.Cam.h;
         samps:=ScR.Cam.samples;
      END;
      '?',':' : BEGIN
         writeln(' -m [Model ID] Rendering Model');
         writeln(' -a [Render Algrithm] r1=Orignal  r2=Next Event  r3=No Loop r4=Light Path');
         writeln(' -o [finename] output filename');
         writeln(' -s [samps] sampling count');
         writeln(' -w [width] screen width pixel');
         writeln(' -x [filename] input Scene XML file');
         halt;
      END;
    end; { case }
  until c=endofoptions;
  height:=h;
  BMPClass:=BMPIOClass.Create(w,h);
  case AlgoID of
  1:FN:=FN+IntToStr(ModelId)+'org.bmp';
  2:FN:=FN+IntToStr(ModelId)+'NEE.bmp';  
  3:FN:=FN+IntToStr(ModelId)+'Loop.bmp';  
  4:FN:=FN+IntToStr(ModelId)+'LPath.bmp';
  end;
   
  Randomize;
  Rt.SceneRec:=ScR;
  Rt.SceneRec.Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

 
  T1:=Time;
  Writeln ('The time is : ',TimeToStr(Time));

  FOR y := 0 to h-1 DO BEGIN
    IF y mod 10 =0 then writeln('y=',y);
    FOR x := 0 TO w - 1 DO BEGIN
      r:=CreateVec(0, 0, 0);
      tColor:=ZeroVec;
      FOR sy := 0 TO 1 DO BEGIN
        FOR sx := 0 TO 1 DO BEGIN
          FOR s := 0 TO samps - 1 DO BEGIN
            temp:=Rt.Radiance(Rt.SceneRec.Cam.Ray(x,y,sx,sy), 0);
            temp:= temp/ samps;
            r:= r+temp;
          END;(*samps*)
          temp:= ClampVector(r)* 0.25;
          tColor:=tColor+ temp;
          r:=CreateVec(0, 0, 0);
        END;(*sx*)
      END;(*sy*)
      vColor:=ColToRGB(tColor);
      BMPClass.SetPixel(x,height-y,vColor);
    END;(* for x *)
  END;(*for y*)
  T2:=Time-T1;
  DecodeTime(T2,HH,MM,SS,MS);
  Writeln ('The time is : ',HH,'h:',MM,'min:',SS,'sec');
   
  BMPClass.WriteBMPFile(FN);
END.
