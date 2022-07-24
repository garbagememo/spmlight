UNIT uModel;
{$MODE objfpc}
{$INLINE ON}
{$modeswitch advancedrecords}
INTERFACE
USES SysUtils,Classes,uVect,math;


CONST
  eps            = 1e-4;
  INF            = 1e20;
  M_1_PI         = 1/pi;
  M_2PI          = 2*pi;
  DefaultSamples = 16;
TYPE

  VertexRecord=RECORD
    cf:VecRecord;
    p,n:VecRecord;
    rad2:real;
    id:INTEGER;
  //omega,ts,tr,preTR,preRad2:real;//debug
  END;

  SphereClass=CLASS
    rad,rad2:real;       //radius, radius^2
    p,e,c:VecRecord;// position. emission,color
    refl:RefType;
    isLight:BOOLEAN;
    CONSTRUCTOR Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
    FUNCTION intersect(CONST r:RayRecord):real;
  END;

  CameraRecord=RECORD
    o,d,cx,cy : VecRecord;
    dist      : real;
    w,h       : INTEGER;
    ratio     : real;
    samples   : INTEGER;
    PROCEDURE Setup(o_,d_: VecRecord;w_,h_:INTEGER;ratio_,dist_:real);
    PROCEDURE SetSamples(sam :INTEGER);
    FUNCTION Ray(x,y,sx,sy : INTEGER):RayRecord;
  END;

  SceneRecord=RECORD
    SceneName : string;
    spl       : TList;
    cam       : CameraRecord;
    FUNCTION Intersect(CONST r:RayRecord;VAR t:real; VAR id:INTEGER):BOOLEAN;
    FUNCTION GenLight(LightID:INTEGER):VertexRecord;
  END;

  SceneListRecord=RECORD
    MaxIndex : INTEGER;
    SrA      : ARRAY[0..50] OF SceneRecord;
    PROCEDURE InitScene(w,h:INTEGER);
    FUNCTION CopyScene(id,w,h :INTEGER):SceneRecord;
  END;

  SnapRecord = RECORD
    SnapList: TList;
    SceneIndex	     : INTEGER;
    CurSceneRec     : SceneRecord;
    PROCEDURE MakeSnap;
    FUNCTION CopySnap(id:INTEGER):TList;
    FUNCTION CopyCamera(id,w,h:INTEGER):CameraRecord;
    FUNCTION GetNextScene(w,h:INTEGER):BOOLEAN;
  END;

VAR
  SR:SnapRecord;
  SLR:SceneListRecord;

IMPLEMENTATION
FUNCTION SceneRecord.Intersect(CONST r:RayRecord;VAR t:real; VAR id:INTEGER):BOOLEAN;
VAR
  d:real;
  i:INTEGER;
BEGIN
  t:=INF;
  FOR i:=0 TO spl.count-1 DO BEGIN
    d:=SphereClass(spl[i]).intersect(r);
    IF d<t THEN BEGIN
      t:=d;
      id:=i;
    END;
  END;
  result:=(t<inf);
END;

FUNCTION SceneRecord.GenLight(LightID:INTEGER):VertexRecord;
VAR
  s:SphereClass;
  p,t,st:real;
  n:VecRecord;
BEGIN
  s:=SphereClass(spl[LightID]);
  result.cf:=s.e;
  p:=2.0*PI*random;t:=2.0*arccos(sqrt(1.0-random));
  st:=sin(t);
  result.n:=VecNorm(CreateVec(cos(p)*st,cos(t),sin(p)*st) );
  result.p:=s.p+result.n*s.rad;
  result.rad2:=s.rad2;
  result.id:=LightID;
  IF (result.p-cam.o)*result.n<0 THEN result.n:=result.n*-1;
END;


PROCEDURE SceneListRecord.InitScene(w,h:INTEGER);
VAR
  i:INTEGER;
  ScList:TList;
  Cen,C,TC,SCC:VecRecord;
  R,T,D,Z:real;
BEGIN
   MaxIndex:=-1;
   Inc(MaxIndex);

   //-------------Debug Scene sc1-------------

   SrA[MaxIndex].SceneName:='0-Debug Scene';
   ScList:=TList.Create;
   ScList.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
   ScList.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
   ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
   ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
   ScList.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
   ScList.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
   ScList.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
   ScList.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
   ScList.add( SphereClass.Create(600,CreateVec(50,681.6-0.27,81.6), CreateVec(4,4,4),   ZeroVec,  DIFF) );//Ligth

   SrA[MaxIndex].spl:=ScList;
   SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);



   //----------cornel box sc1-----------
   Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='1-cornel Box';
  ScList:=TList.Create;
  ScList.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  ScList.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  ScList.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  ScList.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  ScList.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  ScList.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
  ScList.add( SphereClass.Create( 1.5,CreateVec(50,81.6-16.5,81.6), CreateVec(4,4,4)*100,   ZeroVec,  DIFF) );//Ligth

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //-----------sky sc2--------------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='2-Sky';
  ScList:=TList.Create;
  Cen:=CreateVec(50,40.8,-860);

  ScList.add(SphereClass.Create(1600, CreateVec(1,0,2)*3000, CreateVec(1,0.9,0.8)*1.2e1*1.56*2,ZeroVec, DIFF)); // sun
  ScList.add(SphereClass.Create(1560, CreateVec(1,0,2)*3500,CreateVec(1,0.5,0.05)*4.8e1*1.56*2, ZeroVec,  DIFF) ); // horizon sun2
  ScList.add(SphereClass.Create(10000,Cen+CreateVec(0,0,-200), CreateVec(0.00063842, 0.02001478, 0.28923243)*6e-2*8, CreateVec(0.7,0.7,1)*0.25,  DIFF)); // sky

  ScList.add(SphereClass.Create(100000, CreateVec(50, -100000, 0),ZeroVec,CreateVec(0.3,0.3,0.3),DIFF)); // grnd
  ScList.add(SphereClass.Create(110000, CreateVec(50, -110048.5, 0),CreateVec(0.9,0.5,0.05)*4,ZeroVec,DIFF));// horizon brightener
  ScList.add(SphereClass.Create(4e4, CreateVec(50, -4e4-30, -3000),ZeroVec,CreateVec(0.2,0.2,0.2),DIFF));// mountains

  ScList.add(SphereClass.Create(26.5,CreateVec(22,26.5,42),ZeroVec,CreateVec(1,1,1)*0.596, SPEC)); // white Mirr
  ScList.add(SphereClass.Create(13,CreateVec(75,13,82),ZeroVec,CreateVec(0.96,0.96,0.96)*0.96, REFR));// Glas
  ScList.add(SphereClass.Create(22,CreateVec(87,22,24),ZeroVec,CreateVec(0.6,0.6,0.6)*0.696, REFR));    // Glas2

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //------------nightsky sc3----
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='3-nightsky';
  ScList:=TList.Create;
  ScList.add(SphereClass.Create(2.5e3,CreateVec(0.82,0.92,0-2)*1e4,    CreateVec(1,1,1)*0.8e2,     ZeroVec, DIFF)); // moon
  ScList.add(SphereClass.Create(2.5e4,CreateVec(50, 0, 0),  CreateVec(0.114, 0.133, 0.212)*1e-2,  CreateVec(0.216,0.384,1)*0.003, DIFF)); // sky
  ScList.add(SphereClass.Create(5e0,  CreateVec(-0.2,0.16,-1)*1e4, CreateVec(1.00, 0.843, 0.698)*1e2,   ZeroVec, DIFF));  // star
  ScList.add(SphereClass.Create(5e0,  CreateVec(0,  0.18,-1)*1e4,  CreateVec(1.00, 0.851, 0.710)*1e2,  ZeroVec, DIFF));  // star
  ScList.add(SphereClass.Create(5e0,  CreateVec(0.3, 0.15,-1)*1e4, CreateVec(0.671, 0.780, 1.00)*1e2,   ZeroVec, DIFF));  // star
  ScList.add(SphereClass.Create(3.5e4,CreateVec(600,-3.5e4+1, 300), ZeroVec,   CreateVec(0.6,0.8,1)*0.01,  REFR));   //pool
  ScList.add(SphereClass.Create(5e4,  CreateVec(-500,-5e4+0, 0),    ZeroVec,   CreateVec(1,1,1)*0.35,  DIFF));    //hill
  ScList.add(SphereClass.Create(16.5, CreateVec(27,0,47),           ZeroVec,   CreateVec(1,1,1)*0.33, DIFF)); //hut
  ScList.add(SphereClass.Create(7,    CreateVec(27+8*sqrt(2),0,47+8*sqrt(2)),ZeroVec,  CreateVec(1,1,1)*0.33,  DIFF)); //door
  ScList.add(SphereClass.Create(500,  CreateVec(-1e3,-300,-3e3), ZeroVec,  CreateVec(1,1,1)*0.351,    DIFF));  //mnt
  ScList.add(SphereClass.Create(830,  CreateVec(0,   -500,-3e3), ZeroVec,  CreateVec(1,1,1)*0.354,    DIFF));  //mnt
  ScList.add(SphereClass.Create(490,  CreateVec(1e3, -300,-3e3), ZeroVec,  CreateVec(1,1,1)*0.352,    DIFF));  //mnt

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

//-----------island sc4-------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='4-island';

  ScList:=TList.Create;
  Cen:=CreateVec(50,-20,-860);

  ScList.add(SphereClass.Create(160, Cen+CreateVec(0, 600, -500),CreateVec(1,1,1)*2e2, ZeroVec,  DIFF)); // sun
  ScList.add(SphereClass.Create(800, Cen+CreateVec(0,-880,-9120),CreateVec(1,1,1)*2e1, ZeroVec,  DIFF)); // horizon
  ScList.add(SphereClass.Create(10000,Cen+CreateVec(0,0,-200), CreateVec(0.0627, 0.188, 0.569)*1e0, CreateVec(1,1,1)*0.4,  DIFF)); // sky
  ScList.add(SphereClass.Create(800, Cen+CreateVec(0,-720,-200),ZeroVec,  CreateVec(0.110, 0.898, 1.00)*0.996,  REFR)); // water
  ScList.add(SphereClass.Create(790, Cen+CreateVec(0,-720,-200),ZeroVec,  CreateVec(0.4,0.3,0.04)*0.6, DIFF)); // earth
  ScList.add(SphereClass.Create(325, Cen+CreateVec(0,-255,-50), ZeroVec,  CreateVec(0.4,0.3,0.04)*0.8, DIFF)); // island
  ScList.add(SphereClass.Create(275, Cen+CreateVec(0,-205,-33), ZeroVec,  CreateVec(0.02,0.3,0.02)*0.75,DIFF)); // grass

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //-------------Vista sc5------------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='5-Vista';

  ScList:=TList.Create;
  Cen:=CreateVec(50,-20,-860);

  ScList.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,-900),CreateVec(1,0.4,0.1)*5e-1, ZeroVec,  DIFF)); // sun
  ScList.add(SphereClass.Create(1e4,  Cen+ZeroVec, CreateVec(0.631, 0.753, 1.00)*3e-1, CreateVec(1,1,1)*0.5,  DIFF)); // sky

  ScList.add(SphereClass.Create(150,  Cen+CreateVec(-350,0, -100),ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(200,  Cen+CreateVec(-210,0,-100), ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(145,  Cen+CreateVec(-210,85,-100),ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF)); // snow
  ScList.add(SphereClass.Create(150,  Cen+CreateVec(-50,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(150,  Cen+CreateVec(100,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(125,  Cen+CreateVec(250,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(150,  Cen+CreateVec(375,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt

  ScList.add(SphereClass.Create(2500, Cen+CreateVec(0,-2400,-500),ZeroVec,  CreateVec(1,1,1)*0.1,  DIFF)); // mnt base

  ScList.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,200), ZeroVec,  CreateVec(0.2,0.2,1),    REFR)); // water
  ScList.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,1100),ZeroVec,  CreateVec(0,0.3,0),     DIFF)); // grass
  ScList.add(SphereClass.Create(8   , Cen+CreateVec(-75, -5, 850),ZeroVec,  CreateVec(0,0.3,0),     DIFF)); // bush
  ScList.add(SphereClass.Create(30,   Cen+CreateVec(0,   23, 825),ZeroVec,  CreateVec(1,1,1)*0.996, REFR)); // ball

  ScList.add(SphereClass.Create(30,  Cen+CreateVec(200,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(237,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds
  ScList.add(SphereClass.Create(28,  Cen+CreateVec(267,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds

  ScList.add(SphereClass.Create(40,  Cen+CreateVec(150,280,-1000),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(187,280,-1000),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  ScList.add(SphereClass.Create(40,  Cen+CreateVec(600,280,-1100),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(637,280,-1100),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  ScList.add(SphereClass.Create(37,  Cen+CreateVec(-800,280,-1400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF)); // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(0,280,-1600),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));    // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(537,280,-1800),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

//----------------Overlap  sc6-----------------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='6-Overlap';
ScList:=TList.Create;

D:=50;
R:=40;
  ScList.add(SphereClass.Create(150, CreateVec(50+75,28,62), CreateVec(1,1,1)*0e-3, CreateVec(1,0.9,0.8)*0.93, REFR));
  ScList.add(SphereClass.Create(28,  CreateVec(50+5,-28,62), CreateVec(1,1,1)*1e1, ZeroVec, DIFF));
  ScList.add(SphereClass.Create(300, CreateVec(50,28,62), CreateVec(1,1,1)*0e-3, CreateVec(1,1,1)*0.93, SPEC));

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //----------------wada  sc7-------------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='7-wada';
ScList:=TList.Create;

R:=60;
//double R=120;
T:=30*PI/180.;
D:=R/cos(T);
Z:=60;

  ScList.add(SphereClass.Create(1e5, CreateVec(50, 100, 0),      CreateVec(1,1,1)*3e0, ZeroVec, DIFF)); // sky
  ScList.add(SphereClass.Create(1e5, CreateVec(50, -1e5-D-R, 0), ZeroVec,     CreateVec(0.1,0.1,0.1),DIFF));           //grnd

  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec( cos(T),sin(T),0)*D, ZeroVec, CreateVec(1,0.3,0.3)*0.999, SPEC)); //red
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(-cos(T),sin(T),0)*D, ZeroVec, CreateVec(0.3,1,0.3)*0.999, SPEC)); //grn
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,-1,0)*D,         ZeroVec, CreateVec(0.3,0.3,1)*0.999, SPEC)); //blue
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,0,-1)*D,       ZeroVec, CreateVec(0.53,0.53,0.53)*0.999, SPEC)); //back
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,0,1)*D,      ZeroVec, CreateVec(1,1,1)*0.999, REFR)); //front

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //-----------------wada2 sc8----------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='8-wada2';
ScList:=TList.Create;

R:=120;     // radius
T:=30*PI/180.;
D:=R/cos(T);     //distance
Z:=62;
C:=CreateVec(0.275, 0.612, 0.949);

  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec( cos(T),sin(T),0)*D,    C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //red
  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(-cos(T),sin(T),0)*D,    C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //grn
  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(0,-1,0)*D,              C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //blue
  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(0,0,-1)*R*2*sqrt(2/3),C*0e-2,CreateVec(1,1,1)*0.996, SPEC)); //back
  ScList.add(SphereClass.Create(2*2*R*2*sqrt(2/3)-R*2*sqrt(2/3)/3, CreateVec(50,28,Z)+CreateVec(0,0,-R*2*sqrt(2/3)/3), CreateVec(1,1,1)*0,CreateVec(1,1,1)*0.5, SPEC)); //front

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //---------------forest sc9-----------
  Inc(MaxIndex);
  SrA[MaxIndex].SceneName:='9-forest';

ScList:=TList.Create;

tc:=CreateVec(0.0588, 0.361, 0.0941);
scc:=CreateVec(1,1,1)*0.7;
  ScList.add(SphereClass.Create(1e5, CreateVec(50, 1e5+130, 0),  CreateVec(1,1,1)*1.3,ZeroVec,DIFF)); //lite
  ScList.add(SphereClass.Create(1e2, CreateVec(50, -1e2+2, 47),  ZeroVec,CreateVec(1,1,1)*0.7,DIFF)); //grnd

  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, 300)+CreateVec(-sin(50*PI/180),0,cos(50*PI/180))*1e4, ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr L
  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, 300)+CreateVec(sin(50*PI/180),0,cos(50*PI/180))*1e4,  ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr R
  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, -50)+CreateVec(-sin(30*PI/180),0,-cos(30*PI/180))*1e4,ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr FL
  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, -50)+CreateVec(sin(30*PI/180),0,-cos(30*PI/180))*1e4, ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr


  ScList.add(SphereClass.Create(4, CreateVec(50,6*0.6,47),   ZeroVec,CreateVec(0.13,0.066,0.033), DIFF));//"tree"
  ScList.add(SphereClass.Create(16,CreateVec(50,6*2+16*0.6,47),   ZeroVec, tc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(11,CreateVec(50,6*2+16*0.6*2+11*0.6,47),   ZeroVec, tc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(7, CreateVec(50,6*2+16*0.6*2+11*0.6*2+7*0.6,47),   ZeroVec, tc,  DIFF));//"tree"

  ScList.add(SphereClass.Create(15.5,CreateVec(50,1.8+6*2+16*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(10.5,CreateVec(50,1.8+6*2+16*0.6*2+11*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(6.5, CreateVec(50,1.8+6*2+16*0.6*2+11*0.6*2+7*0.6,47),   ZeroVec, scc,  DIFF));//"tree"

  SrA[MaxIndex].spl:=ScList;
  SrA[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


END;

FUNCTION SceneListRecord.CopyScene(id,w,h:INTEGER):SceneRecord;
BEGIN
   result:=SrA[id];
   result.cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);
END;

PROCEDURE SnapRecord.MakeSnap;
VAR
  i,j,k   : INTEGER;
  r,f     : real;
  ef      : ARRAY[0..5] OF real;
  bc      : ARRAY[0..5] OF VecRecord;
   ScList : TList;
BEGIN
  SnapList:=TList.Create;
  bc[0]:=CreateVec(1.00,0.25,0.25);
  bc[1]:=CreateVec(0.5,0.25,0.5);
  bc[2]:=CreateVec(0.250,0.250,1.00);
  bc[3]:=CreateVec(0.250,0.75,0.275);
  bc[4]:=CreateVec(0.250,1.00,0.250);
  bc[5]:=CreateVec(0.5,0.5,0.20);
  FOR i:=0 TO 5 DO BEGIN
    FOR k:=0 TO 5 DO ef[k]:=0;
    ef[i]:=3;
    FOR j:=0 TO 20 DO BEGIN
      f:=1-abs(j*0.1-1);
      ScList:=TList.Create;
R:=50;
      ScList.add( SphereClass.Create( R,CreateVec(100,310+R,150), CreateVec(4,4,4)*2,   ZeroVec,  DIFF) );//Ligth
      ScList.add(SphereClass.Create(100000, CreateVec(50, -100000, 0),ZeroVec,CreateVec(0.4,0.6,0.99),DIFF)); // grnd

R:=14;
      ScList.Add(SphereClass.Create(R,CreateVec(    0,R, 40),bc[0]*ef[0]*f,bc[0],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec(-34.6,R, 20),bc[1]*ef[1]*f,bc[1],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec(-34.6,R,-20),bc[2]*ef[2]*f,bc[2],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec(    0,R,-40),bc[3]*ef[3]*f,bc[3],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec( 34.6,R,-20),bc[4]*ef[4]*f,bc[4],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec( 34.6,R, 20),bc[5]*ef[5]*f,bc[5],DIFF));

      SnapList.Add(ScList);
    END;
  END;
  SceneIndex:=0;
END;
FUNCTION SnapRecord.GetNextScene(w,h:INTEGER):BOOLEAN;
BEGIN
  IF Assigned(SnapList)=FALSE THEN BEGIN
    result:=FALSE;
    EXIT;
  END;
  IF SnapList.Count<=SceneIndex THEN BEGIN
    result:=FALSE;
    EXIT;
  END;
   CurSceneRec.spl:=CopySnap(SceneIndex);
   CurSceneRec.cam:=CopyCamera(SceneIndex,w,h);
   Inc(SceneIndex);
END;

FUNCTION SnapRecord.CopySnap(id:INTEGER):TList;
BEGIN
  result:=TList(SnapList[id]);
END;
FUNCTION SnapRecord.CopyCamera(id,w,h:INTEGER):CameraRecord;
VAR
  tCam:CameraRecord;
BEGIN
  tCam.Setup( CreateVec(0, 300, 0),VecNorm(CreateVec(0,-1,0) ),w,h,0.5135,140);
  result:=tCam;
END;

PROCEDURE CameraRecord.Setup(o_,d_:VecRecord;w_,h_:INTEGER;ratio_,dist_:real);
BEGIN
  ratio:=ratio_;dist:=dist_;w:=w_;h:=h_;
  o:=o_;d:=VecNorm(d_);
  cx:=CreateVec(ratio*w_/h_,0,0);
  cy:=VecNorm(cx/d_)*ratio;
   samples:=DefaultSamples;
END;

PROCEDURE CameraRecord.SetSamples(sam :INTEGER );
BEGIN
   samples:=sam;
END;

FUNCTION CameraRecord.Ray(x,y,sx,sy:INTEGER):RayRecord;
VAR
  r1,r2,dx,dy:real;
  td:VecRecord;
BEGIN
  r1:=2*random;
  IF r1<1 THEN dx:=sqrt(r1)-1 ELSE dx:=1-sqrt(2-r1);
  r2:=2*random;
  IF (r2 < 1) THEN dy := sqrt(r2)-1 ELSE dy := 1-sqrt(2-r2);
  td:= cy*(((sy + 0.5 + dy)/2 + (h-y-1))/h - 0.5)+cx*(((sx + 0.5 + dx)/2 + x)/w - 0.5)+d;
  td:=VecNorm(td);
  result.o:= td*dist+ o;
  result.d := td;
END;

CONSTRUCTOR SphereClass.Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
BEGIN
  rad:=rad_;p:=p_;e:=e_;c:=c_;refl:=refl_;rad2:=rad*rad;
  IF VecLen(e)>0 THEN isLight:=TRUE ELSE isLight:=FALSE;
END;
FUNCTION SphereClass.intersect(CONST r:RayRecord):real;
VAR
  op:VecRecord;
  t,b,det:real;
BEGIN
  op:=p-r.o;
  t:=eps;b:=op*r.d;det:=b*b-op*op+rad2;
  IF det<0 THEN
    result:=INF
  ELSE BEGIN
    det:=sqrt(det);
    t:=b-det;
    IF t>eps THEN
      result:=t
    ELSE BEGIN
      t:=b+det;
      IF t>eps THEN
        result:=t
      ELSE
        result:=INF;
    END;
  END;
END;


BEGIN
END.



