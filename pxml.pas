PROGRAM pXML;
{$mode objfpc}{$H+}
USES SysUtils,Classes,uXML,uModel,uVect,getopts;

VAR
  SR  : SnapRecord;
  ScR : SceneRecord;
  spl : TList;
  cam : CameraRecord;
  pSt : string;
  i:INTEGER;
  smax:INTEGER;
  c:char;
  StList:TStringList;
  fp:TEXT;
  sufSt:String;
BEGIN
  SLR.InitScene(320,240);//SR.InitSceneにするべき？
  smax:=SLR.MaxIndex;

  pSt:='xml';
  IF NOT DirectoryExists(pSt) THEN
    IF NOT CreateDir (pSt) THEN
      WRITELN ('Failed to create directory !')
    ELSE
      WRITELN ('Created "NewDir" directory');
  FOR i:=0 TO smax DO BEGIN
    ScR:=SLR.CopyScene(i,320,240);
    cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),640,480,0.5135,140);
    ScR.Cam:=cam;
    WriteXMLScene(ScR,pst+'/'+IntToStr(i)+'Model.xml');
  END;
  
  SR.MakeSnap;
  SR.GetNextScene(640,480);
  writeXMLScene(SR.CurSceneRec,pSt+'/'+'teste.xml');
  WRITELN(' Write XML');
  ScR:=ReadXMLConf(pSt+'/'+'teste.xml');
  WRITELN(' Read XML');
  writeXMLScene(ScR,'testf.xml'); 
  c:=#0;
  c:=getopt('a');
  IF c='a' THEN BEGIN

    writeln('Snap Scene List');
    IF Not DirectoryExists('AutoXML') THEN 
      IF Not CreateDir('AutoXML') THEN 
        writeln('Failed to CreateDirectory')
      ELSE
        writeln('Create Newdir Directory');
    SR.SceneIndex:=0;
    StList:=TStringLIst.Create;
writeln('SR Max=',SR.SnapList.Count);
    while SR.GetNextScene(320,240) DO BEGIN
      sufSt:=IntToStr(SR.SceneIndex)+'Auto.xml';
      WriteXMLScene(SR.CurSceneRec,'AutoXML'+'/'+SufSt);
      StList.Add('..\spm -r2 -x'+SufSt+' -o'+sufSt);
    END;
    assign(fp,'auto.bat');rewrite(fp);
    for i:=0 to StList.Count-1 do writeln(fp,StList[i]);
    close(fp);
  END;
END.
