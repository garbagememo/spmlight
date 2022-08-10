program smallpt;
{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
uses SysUtils,Classes,uVect,uBMP,uModel,uScene,uFlux,Math,getopts;

const 
  DefaultOutFileName='out.bmp';
type
  FluxOptionRecord=record
    w,h,samps:integer;
    AlgolID,ModelID:integer;
    OutFN:string;
    procedure Setup(w_,h_,samp_,Algol,Model:integer;OFN:string);
    function OutFileName:string;
  end;
  procedure FluxOptionRecord.Setup(w_,h_,samp_,Algol,Model:integer;OFN:string);
  begin
    w:=w_;h:=h_;samps:=samp_;AlgolID:=Algol;ModelID:=Model;
    OutFN:=OFN;
  end;
  function FluxOptionRecord.OutFileName:string;
  var
    AlgolStr:string;
  begin
    if OutFN<>'' then begin
      result:=OutFN;
    end
    else begin
      AlgolStr:='Org';
      if AlgolID=1 then AlgolStr:='Org';
      if AlgolID=2 then AlgolStr:='NEE';
      if AlgolID=3 then AlgolStr:='Loop';
      if AlgolID=4 then AlgolStr:='LightPATH';
      result:='M'+IntToStr(ModelID)+AlgolStr+DefaultOutFileName;
    end;
  end;


VAR
   x,y,sx,sy,s                    : INTEGER;
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
   Rt:TFluxClass;
   ScR:SceneRecord;
  FluxOpt:FluxOptionRecord;
BEGIN
  FluxOpt.OutFN:='';//空白のとき名前を作る
  FluxOpt.AlgolID:=1;
  FluxOpt.ModelID:=0;
  FluxOpt.w:=320 ;FluxOpt.h:=240;  FluxOpt.samps := 16;


  c:=#0;
  repeat
    c:=getopt('m:o:a:s:w:');
    case c of
      'm': BEGIN
          ArgInt:=StrToInt(OptArg);
          FluxOpt.ModelId:=ArgInt;
      END;
      'a' : BEGIN
         ArgInt:=StrToInt(OptArg);
         FluxOpt.AlgolID:=ArgInt;
         CASE ArgInt OF
           1 : BEGIN
             Writeln('Render=Orignal')
           END;
           2 : BEGIN
             Writeln('Render=NEE');
           END;
           3 : BEGIN
             Writeln('Render=Non Loop');
           END;
           4 : BEGIN
             writeln('Render=LightPath');
           END;
           else FluxOpt.AlgolID:=1;
         END;
      END;
      'o'    : BEGIN
         ArgFN:=OptArg;
         IF ArgFN<>'' THEN FluxOpt.OutFN:=ArgFN;
         writeln ('Output FileName =',FN);
      END;
      's'    : BEGIN
        ArgInt:=StrToInt(OptArg);
        FluxOpt.samps:=ArgInt;
        writeln('samples =',ArgInt);
      END;
      'w'    : BEGIN
         ArgInt:=StrToInt(OptArg);
         FluxOpt.w:=ArgInt;FluxOpt.h:=FluxOpt.w *3 div 4;
         writeln('w=',FluxOpt.w,' ,h=',FluxOpt.h);
      END;
      '?',':' : BEGIN
         writeln(' -m [Model ID] Rendering Model');
         writeln(' -a [Render Algrithm] r1=Orignal  r2=Next Event  r3=No Loop r4=Light Path');
         writeln(' -o [finename] output filename');
         writeln(' -s [samps] sampling count');
         writeln(' -w [width] screen width pixel');
         halt;
      END;
    end; { case }
  until c=endofoptions;
//  height:=FluxOpt.h;
  BMPClass:=BMPIOClass.Create(FluxOpt.w,FluxOpt.h);
  case FluxOpt.AlgolID of
  1:RT:=TFluxClass.Create;
  2:RT:=TNEEFluxClass.Create;
  3:RT:=TLoopFluxClass.Create;
  4:RT:=TLightPathFluxClass.Create;
  end;
  
  SRList.InitSceneRecord(FluxOpt.w,FluxOpt.h);
  RT.Scene:=SRList.SRL[FluxOpt.ModelID];
//  Rt.Scene.mdl:=SRList.CopyScene(FluxOpt.ModelID);
  writeln('Model of Scene =',RT.Scene.SceneName);
  Randomize;

  if FluxOpt.AlgolID=4 then begin
    TLightPathFluxClass(Rt).LPList.SetScene(Rt.Scene)
  end;


 
  T1:=Time;
  Writeln ('The time is : ',TimeToStr(Time));

  FOR y := 0 to FluxOpt.h-1 DO BEGIN
    IF y mod 10 =0 then writeln('y=',y);
    FOR x := 0 TO FluxOpt.w - 1 DO BEGIN
      r:=CreateVec(0, 0, 0);
      tColor:=ZeroVec;
      FOR sy := 0 TO 1 DO BEGIN
        FOR sx := 0 TO 1 DO BEGIN
          FOR s := 0 TO FluxOpt.samps - 1 DO BEGIN
            temp:=Rt.Radiance(Rt.Scene.Cam.Ray(x,y,sx,sy), 0);
            temp:= temp/ FluxOpt.samps;
            r:= r+temp;
          END;(*samps*)
          temp:= ClampVector(r)* 0.25;
          tColor:=tColor+ temp;
          r:=CreateVec(0, 0, 0);
        END;(*sx*)
      END;(*sy*)
      vColor:=ColToRGB(tColor);
      BMPClass.SetPixel(x,FluxOpt.h-y,vColor);
    END;(* for x *)
  END;(*for y*)
  T2:=Time-T1;
  DecodeTime(T2,HH,MM,SS,MS);
  Writeln ('The time is : ',HH,'h:',MM,'min:',SS,'sec');
   
  BMPClass.WriteBMPFile(FluxOpt.OutFileName);
END.
