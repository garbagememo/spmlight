PROGRAM smallpt;
{$MODE objfpc}{$H+}
{$INLINE ON}

USES SysUtils,Classes,uVect,uBMP,uModel,uLightPath,uXML,Math,getopts;

CONST 
  eps=1e-4;
  INF=1e20;
TYPE 

  TRenderClass=CLASS
    SceneRec:SceneRecord;
    PROCEDURE SetScene(sr_:SceneRecord);virtual;
    FUNCTION Radiance(r : RayRecord;Depth:INTEGER ):VecRecord;Virtual;
  END;                
  TNEERenderClass=CLASS(TRenderClass)
    FUNCTION Radiance(r : RayRecord;Depth:INTEGER ):VecRecord;OverRide;
  END;                
  TLoopRenderClass=CLASS(TRenderClass)
    FUNCTION Radiance(r : RayRecord;Depth:INTEGER ):VecRecord;OverRide;
  END;                
  TLightPathRenderClass=CLASS(TRenderClass)
    LPList:LightPathList;
    PROCEDURE SetScene(sr_:SceneRecord);override;
    FUNCTION Radiance(r : RayRecord;Depth:INTEGER ):VecRecord;OverRide;
  END;
PROCEDURE TRenderClass.SetScene(sr_:SceneRecord);
BEGIN
  SceneRec:=sr_;
END;

FUNCTION TRenderClass.radiance( r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id:INTEGER;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
BEGIN
  id:=0;depth:=depth+1;
  IF SceneRec.intersect(r,t,id)=FALSE THEN BEGIN
    result:=ZeroVec;EXIT;
  END;
  obj:=SphereClass(SceneRec.spl[id]);
  x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
  IF VecDot(n,r.d)<0 THEN nl:=n ELSE nl:=n*-1;
  IF (f.x>f.y)AND(f.x>f.z) THEN
    p:=f.x
  ELSE IF f.y>f.z THEN 
    p:=f.y
  ELSE
    p:=f.z;
   IF (depth>5) THEN BEGIN
    IF random<p THEN 
      f:=f/p 
    ELSE BEGIN
      result:=obj.e;
      EXIT;
    END;
  END;
  CASE obj.refl OF
    DIFF:BEGIN
      r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
      w:=nl;
      IF abs(w.x)>0.1 THEN
        u:=VecNorm(CreateVec(0,1,0)/w) 
      ELSE BEGIN
        u:=VecNorm(CreateVec(1,0,0)/w );
      END;
      v:=w/u;
      d := VecNorm(u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2));
      result:=obj.e+VecMul(f,radiance(CreateRay(x,d),depth) );
    END;(*DIFF*)
    SPEC:BEGIN
      result:=obj.e+VecMul(f,(radiance(CreateRay(x,r.d-n*2*(n*r.d) ),depth)));
    END;(*SPEC*)
    REFR:BEGIN
      RefRay:=CreateRay(x,r.d-n*2*(n*r.d) );
      into:= (n*nl>0);
      nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl; 
      cos2t:=1-nnt*nnt*(1-ddn*ddn);
      IF cos2t<0 THEN BEGIN   // Total internal reflection
        result:=obj.e + VecMul(f,radiance(RefRay,depth));
        EXIT;
      END;
      IF into THEN q:=1 ELSE q:=-1;
      tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
      IF into THEN Q:=-ddn ELSE Q:=tdir*n;
      a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
      Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
      IF depth>2 THEN BEGIN
        IF random<p THEN // 反射
          result:=obj.e+VecMul(f,radiance(RefRay,depth)*RP)
        ELSE //屈折
          result:=obj.e+VecMul(f,radiance(CreateRay(x,tdir),depth)*TP);
      END
      ELSE BEGIN// 屈折と反射の両方を追跡
        result:=obj.e+VecMul(f,radiance(RefRay,depth)*Re+radiance(CreateRay(x,tdir),depth)*Tr);
      END;
    END;(*REFR*)
  END;(*CASE*)
END;


FUNCTION TNEERenderClass.Radiance( r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id,i,tid:INTEGER;
  obj,s:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc,d2,a2:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf:VecRecord;
  E:INTEGER;
BEGIN
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF SceneRec.intersect(r,t,id)=FALSE THEN BEGIN
       result:=cl;
       EXIT;
    END;
    obj:=SphereClass(SceneRec.spl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    IF n*r.d<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN p:=f.x ELSE IF f.y>f.z THEN p:=f.y ELSE p:=f.z;
    tw:=obj.e*E;
    cl:=cl+VecMul(cf,tw);

    IF (Depth > 5) OR (p = 0) THEN
       IF (random < p) THEN BEGIN
         f:= f / p;
       END
       ELSE BEGIN
         Result := cl;
         EXIT;
       END;

    cf:=VecMul(cf,f);
    CASE obj.refl OF
      DIFF:BEGIN
        r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
        w:=nl;
        IF abs(w.x)>0.01 THEN
          u:=VecNorm(CreateVec(0,1,0)/w)
        ELSE BEGIN
          u:=VecNorm(CreateVec(1,0,0)/w);
        END;
        v:=w/u;

        sincos(r1,ss,cc);
        u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
        d:=VecAdd3(u,v,w);
        d:=VecNorm(d);

        // Loop over any lights
        EL:=ZeroVec;
        tid:=id;
        FOR i:=0 TO SceneRec.spl.count-1 DO BEGIN
          s:=SphereClass(SceneRec.spl[i]);
          IF (i=tid) THEN BEGIN
            continue;
          END;
          IF (s.e.x<=0) AND  (s.e.y<=0) AND (s.e.z<=0)  THEN continue; // skip non-lights
          sw:=s.p-x;
          d2:=sw*sw;  tr:=s.rad2/d2;
          IF abs(sw.x)/sqrt(tr)>0.1 THEN 
            su:=VecNorm(CreateVec(0,1,0)/sw) 
          ELSE 
            su:=VecNorm(CreateVec(1,0,0)/sw);
          sv:=sw/su;
          IF tr>1 THEN BEGIN
            (*半球の内外=cos_aがマイナスとsin_aが＋、－で場合分け*)
            (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)
            eps1:=M_2PI*random;eps2:=random;eps2s:=sqrt(eps2);
            sincos(eps1,ss,cc);
            l:=VecNorm(u*(cc*eps2s)+v*(ss*eps2s)+w*sqrt(1-eps2));
            IF SceneRec.intersect(CreateRay(x,l),t,id) THEN BEGIN
              IF id=i THEN BEGIN
                tr:=l*nl;
                tw:=s.e*tr;
                EL:=EL+VecMul(f,tw);
              END;
            END;
          END
          ELSE BEGIN //半球外部の場合;
            cos_a_max := sqrt(1-tr );
            eps1 := random; eps2:=random;
            cos_a := 1-eps1+eps1*cos_a_max;
            sin_a := sqrt(1-cos_a*cos_a);
            IF (1-2*random)<0 THEN sin_a:=-sin_a; 
            phi := M_2PI*eps2;
            tw:=sw*(cos(phi)*sin_a);tw:=tw+sv*(sin(phi)*sin_a);tw:=tw+sw*cos_a;
            l:=VecNorm(tw);
            IF (SceneRec.intersect(CreateRay(x,l), t, id) ) THEN BEGIN 
              IF id=i THEN BEGIN  // shadow ray
                omega := 2*(1-cos_a_max);//omega:=s.rad2/d2;
                tr:=l*nl;
                IF tr<0 THEN tr:=0;
                tw:=s.e*tr*omega;tw:=VecMul(f,tw);;
                EL := EL + tw;  // 1/pi for brdf
              END;
            END;
          END;
        END;(*for*)
        tw:=obj.e*e+EL;
        cl:= cl+VecMul(cf,tw );
        E:=0;
        r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tw:=obj.e*e;
        cl:=cl+VecMul(cf,tw);
        E:=1;tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      END;(*SPEC*)
      REFR:BEGIN
        tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        IF cos2t<0 THEN BEGIN   // Total internal reflection
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
          continue;
        END;
        IF into THEN q:=1 ELSE q:=-1;
        tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
        IF into THEN Q:=-ddn ELSE Q:=tdir*n;
        a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
        Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
        IF random<p THEN BEGIN// 反射
          cf:=cf*RP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;



FUNCTION TLoopRenderClass.Radiance( r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id:INTEGER;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  tu,tv:VecRecord;
  cl,cf:VecRecord;
BEGIN
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF SceneRec.intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      EXIT;
    END;
    obj:=SphereClass(SceneRec.spl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    nrd:=n*r.d;
    IF nrd<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN
      p:=f.x
    ELSE IF f.y>f.z THEN
      p:=f.y
    ELSE
      p:=f.z;
    cl:=cl+VecMul(cf,obj.e);
    IF (Depth > 5) OR (p = 0) THEN BEGIN
       //p=0は要するに発光体に撃ちあたる場合＝発光体は色がぜろだから
      IF (random < p) THEN BEGIN
        f:= f / p;
      END
      ELSE BEGIN
        Result := cl;
        EXIT;
      END;
    END;
    cf:=VecMul(cf,f);
    CASE obj.refl OF
      DIFF:BEGIN
        r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
        w:=nl;
        IF abs(w.x)>0.01 THEN
          u:=VecNorm(CreateVec(0,1,0)/w)
        ELSE BEGIN
          u:=VecNorm(CreateVec(1,0,0)/w);
        END;
        v:=w/u;

        sincos(r1,ss,cc);
        u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
        tu:=(u+v)+w;
        d:=VecNorm(tu);
        r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tv:=n*2*nrd ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      END;(*SPEC*)
      REFR:BEGIN
        tv:=n*2*nrd ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        IF cos2t<0 THEN BEGIN   // Total internal reflection
          cl:=cl+VecMul(cf,obj.e);
          r:=RefRay;
          continue;
        END;
        IF into THEN q:=1 ELSE q:=-1;
        tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
        IF into THEN Q:=-ddn ELSE Q:=tdir*n;
        a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
        Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
        IF random<p THEN BEGIN// 反射
          cf:=cf*RP;
          cl:=cl+VecMul(cf,obj.e);
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e);
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;

PROCEDURE TLightPathRenderClass.SetScene(sr_:SceneRecord);
BEGIN
  LPList.clear;
  LPList.SetScene(sr_);
  inherited SetScene(sr_);
END;


FUNCTION TLightPathRenderClass.Radiance( r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id,i,j,tid:INTEGER;
  obj,s:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf:VecRecord;
  E:INTEGER;
  LPRec:LightPathRecord;
  tVert:VertexRecord;
  FUNCTION GetLightPathEvent:VecRecord;
  VAR
    i,j:INTEGER;
    tRay:RayRecord;
    ts:real;
  BEGIN
    result:=ZeroVec;tid:=id;
    FOR i:=0 TO LPList.LMax DO BEGIN
      LPRec:=LPList.Ary[i];
      FOR j:=0 TO LPRec.LPMax DO BEGIN
        tVert:=LPRec.Ary[j];
        IF tVert.id=tid THEN continue;//光源だったら飛ばすに変えるべき
        s:=SphereClass(SceneRec.spl[tVert.id]);
        sw:=VecNorm(tVert.p-x);
        tRay.d:=sw;tRay.o:=x;
        IF sw*nl<0 THEN continue;//裏側につきぬけないように
        IF SceneRec.intersect(tRay,t,id)=FALSE THEN continue;
        IF id<>tVert.id THEN CONTINUE;//影がある？
        tr:=VecSQR(s.p-x);//ここが怖いところ。
        tr:=tVert.rad2/tr;
        ts:=sw*tVert.n;
        IF ts<0 THEN ts:=-ts;//球の表裏で変わるので・・・・
        IF tr>1 THEN BEGIN
          result:=result+VecMul(f,tVert.cf*ts );
        END
        ELSE BEGIN
          cos_a_max := sqrt(1-tr );
          omega := 2*PI*(1-cos_a_max);
          result:=result + VecMul(f,(tVert.cf*ts*omega))*M_1_PI;// 1/pi for brdf
        END;
      END;
    END;
  END;
BEGIN
  LPList.Clear;
  LPList.GetLigthPath;//////LPL
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF SceneRec.intersect(r,t,id)=FALSE THEN BEGIN
       result:=cl;
       EXIT;
    END;
    obj:=SphereClass(SceneRec.spl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    IF n*r.d<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN p:=f.x ELSE IF f.y>f.z THEN p:=f.y ELSE p:=f.z;
    tw:=obj.e*E;
    cl:=cl+VecMul(cf,tw);

    IF (Depth > 5) OR (p = 0) THEN
       IF (random < p) THEN BEGIN
         f:= f / p;
       END
       ELSE BEGIN
         Result := cl;
         EXIT;
       END;

    cf:=VecMul(cf,f);
    CASE obj.refl OF
      DIFF:BEGIN
        r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
        w:=nl;
        IF abs(w.x)>0.01 THEN
          u:=VecNorm(CreateVec(0,1,0)/w)
        ELSE BEGIN
          u:=VecNorm(CreateVec(1,0,0)/w);
        END;
        v:=w/u;

        sincos(r1,ss,cc);
        u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
        d:=VecNorm((u+v)+w);

//        EL:=GetNextEvent;
        EL:=GetLightPathEvent;
//        EL:=GetFirstLight;
        tw:=obj.e*e+EL;
        cl:= cl+VecMul(cf,tw );
        E:=0;
        r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tw:=obj.e*e;
        cl:=cl+VecMul(cf,tw);
        E:=1;tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      END;(*SPEC*)
      REFR:BEGIN
        tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        IF cos2t<0 THEN BEGIN   // Total internal reflection
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
          continue;
        END;
        IF into THEN q:=1 ELSE q:=-1;
        tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
        IF into THEN Q:=-ddn ELSE Q:=tdir*n;
        a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
        Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
        IF random<p THEN BEGIN// 反射
          cf:=cf*RP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;


VAR
   x,y,sx,sy,s       : INTEGER;
   w,h,samps,height  : INTEGER;
   temp              : VecRecord;
   tColor,r: VecRecord;

BMPClass:BMPIOClass;
   T1,T2:TDateTime;
   HH,MM,SS,MS:WORD;
   vColor:rgbColor;
   ArgInt:INTEGER;
   FN,ArgFN:string;
   c:CHAR;
   Rt:TRenderClass;
   ModelID:INTEGER;
   ScR:SceneRecord;
   isCamSetup:boolean;
BEGIN
  isCamSetup:=FALSE;
  FN:='temp';
  w:=320 ;h:=240;  samps := 16;
  Rt:=TRenderClass.Create;
  SLR.InitScene(320,240);
  ModelID:=0;
  ScR:=SLR.CopyScene(ModelID,w,h);
  c:=#0;
  REPEAT
    c:=getopt('m:o:r:s:w:x:');
    CASE c OF
      'm': BEGIN
          ArgInt:=StrToInt(OptArg);
          ModelId:=ArgInt;
          FN:=ExtractFileName(paramStr(0));
          Delete(FN,Length(FN)-3,4);
          FN:=FN+IntToStr(ModelId);
          ScR:=SLR.CopyScene(ArgInt,w,h);
          WRITELN('Model of Scene =',ArgInt,' ',ScR.SceneName);
      END;
      'r' : BEGIN
         ArgInt:=StrToInt(OptArg);
         CASE ArgInt OF
           1 : BEGIN
             WRITELN('Render=Orignal')
           END;
           2 : BEGIN
             Rt:=TNEERenderClass.Create;
             WRITELN('Render=NEE');
           END;
           3 : BEGIN
             Rt:=TLoopRenderClass.Create;
             WRITELN('Render=Non Loop');
           END;
           4 : BEGIN
             Rt:=TLightPathRenderClass.Create;
             WRITELN('Render=LightPath');
           END;
         END;
      END;
      'o'    : BEGIN
         ArgFN:=OptArg;
         IF ArgFN<>'' THEN FN:=ArgFN;
         WRITELN ('Output FileName =',FN);
      END;
      's'    : BEGIN
        ArgInt:=StrToInt(OptArg);
        samps:=ArgInt;
        WRITELN('samples =',ArgInt);
      END;
      'w'    : BEGIN
         ArgInt:=StrToInt(OptArg);
         w:=ArgInt;h:=w *3 div 4;
         WRITELN('w=',w,' ,h=',h);
      END;
      'x'    : BEGIN
         WRITELN('FN=',OptArg);
         ScR:=ReadXMLConf(OptArg);
         w:=Scr.Cam.w;h:=ScR.Cam.h;
         isCamSetup:=TRUE;
         samps:=ScR.Cam.samples;
         FN:=OptArg;
      END;
      '?',':' : BEGIN
         WRITELN(' -m [Model ID] Rendering Model');
         WRITELN(' -r [Render Algrithm] r1=Orignal  r2=Next Event  r3=No Loop r4=Light Path');
         WRITELN(' -o [finename] output filename');
         WRITELN(' -s [samps] sampling count');
         WRITELN(' -w [width] screen width pixel');
         WRITELN(' -x [filename] input Scene XML file');
         HALT;
      END;
    END; { case }
  UNTIL c=endofoptions;
  height:=h;
  BMPClass:=BMPIOClass.Create(w,h);
 
  Randomize;
  Rt.SetScene(ScR);
  IF isCamSetup=FALSE THEN 
    Rt.SceneRec.Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

 
  T1:=Time;
  WRITELN ('The time is : ',TimeToStr(Time));

  FOR y := 0 TO h-1 DO BEGIN
    IF y mod 10 =0 THEN WRITELN('y=',y);
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
  WRITELN ('The time is : ',HH,'h:',MM,'min:',SS,'sec');
   
  BMPClass.WriteBMPFile(FN+'.bmp');
END.


(**EVAL LightPath*)
