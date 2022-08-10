{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

}
UNIT uFlux;

{$mode objfpc}{$H+}

INTERFACE

USES
  SysUtils, uVect,uModel,uScene,uLightPath,math;


TYPE
 
  TFluxClass = CLASS
    Scene:SceneRecord;
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;virtual;
  END;
  TNEEFluxClass = CLASS(TFluxClass)
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;OverRide;
  END;
  TLoopFluxClass= CLASS(TFluxClass)
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;OverRide;
  END;
  TLightPathFluxClass=CLASS(TFluxClass)
    LPList:LightPathList;
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;OverRide;
  END;


IMPLEMENTATION


FUNCTION TFluxClass.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
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
  IF Scene.intersect(r,t,id)=FALSE THEN BEGIN
    result:=ZeroVec;EXIT;
  END;
  obj:=SphereClass(Scene.mdl[id]);
  x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
  IF VecDot(n,r.d)<0 THEN nl:=n ELSE nl:=n*-1;
  IF (f.x>f.y)AND(f.x>f.z) THEN
    p:=f.x
  ELSE IF f.y>f.z THEN 
    p:=f.y
  ELSE
    p:=f.z;
  IF (Depth > 5) OR (p = 0) THEN BEGIN
      IF (random < p) THEN BEGIN
        f:= f / p;
        IF (p = 1) AND (f.x = 1) AND (f.y = 1) AND (f.z = 1) THEN BEGIN
          Result := obj.e;
          EXIT;
        END;
      END
      ELSE BEGIN
        Result := obj.e;
        EXIT;
      END;
  END;
  CASE obj.refl OF
    DIFF:BEGIN
      r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
      w:=nl;
      IF abs(w.x)>0.1 THEN
        u:=VecNorm(MidOneVec/w)
      ELSE BEGIN
        u:=VecNorm(TopOneVec/w);
      END;
      v:=w/u;
      d := VecNorm(u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2));
      result:=obj.e+VecMul(f,Radiance(CreateRay(x,d),depth) );
    END;(*DIFF*)
    SPEC:BEGIN
      result:=obj.e+VecMul(f,(Radiance(CreateRay(x,r.d-n*2*(n*r.d) ),depth)));
    END;(*SPEC*)
    REFR:BEGIN
      RefRay:=CreateRay(x,r.d-n*2*(n*r.d) );
      into:= (n*nl>0);
      nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl; 
      cos2t:=1-nnt*nnt*(1-ddn*ddn);
      IF cos2t<0 THEN BEGIN   // Total internal reflection
        result:=obj.e + VecMul(f,Radiance(RefRay,depth));
        EXIT;
      END;
      IF into THEN q:=1 ELSE q:=-1;
      tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
      IF into THEN Q:=-ddn ELSE Q:=tdir*n;
      a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
      Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
      IF depth>2 THEN BEGIN
        IF random<p THEN // 反射
          result:=obj.e+VecMul(f,Radiance(RefRay,depth)*RP)
        ELSE //屈折
          result:=obj.e+VecMul(f,Radiance(CreateRay(x,tdir),depth)*TP);
      END
      ELSE BEGIN// 屈折と反射の両方を追跡
        result:=obj.e+VecMul(f,Radiance(RefRay,depth)*Re+Radiance(CreateRay(x,tdir),depth)*Tr);
      END;
    END;(*REFR*)
  END;(*CASE*)
END;
FUNCTION Utils_kahanSum3(a, b, c : real) : real;
VAR
  sum,cc,y,t: real;
BEGIN
  sum := a;
  cc  := 0.0;

  y   := b - cc;
  t   := sum + y;
  cc  := (t - sum) - y;
  sum := t;

  y   := c - cc;
  t   := sum + y;
  cc  := (t - sum) - y;
  sum := t;

  Utils_kahanSum3 := sum;
END;

FUNCTION Vector_Add3(a, b, c : VecRecord):VecRecord;
BEGIN
  Result.x := Utils_kahanSum3(a.x, b.x, c.x);
  Result.y := Utils_kahanSum3(a.y, b.y, c.y);
  Result.z := Utils_kahanSum3(a.z, b.z, c.z);
END;


FUNCTION TNEEFluxClass.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id,i,tid:INTEGER;
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
BEGIN
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF Scene.intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      EXIT;
    END;
    obj:=SphereClass(Scene.mdl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    IF n*r.d<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN
      p:=f.x
    ELSE IF f.y>f.z THEN
      p:=f.y
    ELSE
      p:=f.z;
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
        r1  := M_2PI * random;
        r2  := random;
        r2s := sqrt(r2);
        w   := nl;

        IF (abs(w.x) > 0.1) THEN BEGIN
          m1 := 1/sqrt(w.z*w.z+w.x*w.x);
          u := CreateVec(w.z*m1, 0, -w.x*m1);
          v := CreateVec(w.y*u.z, w.z*u.x-w.x*u.z, -w.y*u.x); //4* vs 6*
        END
        ELSE BEGIN
          m1 := 1/sqrt(w.z*w.z+w.y*w.y);
          u := CreateVec(0, -w.z*m1, w.y*m1);
          v := CreateVec(w.y*u.z-w.z*u.y, -w.x*u.z, w.x*u.y); //4* vs 6*
        END;
        sincos(r1,ss,cc);

        u:= u*( cc * r2s); //4* cos
        v:= v*(ss * r2s); //4* sin
        w:= w*( sqrt(1 - r2));  //3* sqrt

        d:=Vector_Add3(u, v, w);d:=VecNorm(d);
        // Loop over any lights
        EL:=ZeroVec;
        tid:=id;
        FOR i:=0 TO Scene.mdl.count-1 DO BEGIN
          s:=SphereClass(Scene.mdl[i]);
          IF (i=tid) THEN BEGIN
            continue;
          END;
          IF (s.e.x<=0) AND  (s.e.y<=0) AND (s.e.z<=0)  THEN continue; // skip non-lights
          sw:=s.p-x;
          tr:=sw*sw;  tr:=s.rad2/tr;
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
            tu:=u*(cc*eps2s);tu:=tu+v*(ss*eps2s);tu:=tu+w*sqrt(1-eps2);
            l:=VecNorm(tu);
             IF Scene.intersect(CreateRay(x,l),t,id) THEN BEGIN
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
             l:=VecNorm(sw*(cos(phi)*sin_a)+sv*(sin(phi)*sin_a)+sw*cos_a);
            IF (Scene.intersect(CreateRay(x,l), t, id) ) THEN BEGIN 
              IF id=i THEN BEGIN  // shadow ray
                omega := 2*PI*(1-cos_a_max);
                tr:=l*nl;
                IF tr<0 THEN tr:=0;
                tw:=s.e*tr*omega;tw:=VecMul(f,tw);tw:=tw*M_1_PI;
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


FUNCTION TLoopFluxClass.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
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
  id:=0;cl:=ZeroVec;cf:=OneVec;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF Scene.intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      EXIT;
    END;
    obj:=SphereClass(Scene.mdl[id]);
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
        IF (p = 1) AND (f.x = 1) AND (f.y = 1) AND (f.z = 1) THEN BEGIN
            Result := cl;
          EXIT;
        END;
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

FUNCTION TLightPathFluxClass.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
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
        s:=SphereClass(Scene.mdl[tVert.id]);
        sw:=VecNorm(tVert.p-x);
        tRay.d:=sw;tRay.o:=x;
        IF sw*nl<0 THEN continue;//裏側につきぬけないように
        IF Scene.intersect(tRay,t,id)=FALSE THEN continue;
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
    IF Scene.intersect(r,t,id)=FALSE THEN BEGIN
       result:=cl;
       EXIT;
    END;
    obj:=SphereClass(Scene.mdl[id]);
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

BEGIN
END.


