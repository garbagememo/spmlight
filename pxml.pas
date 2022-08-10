program pXML;
{$mode objfpc}{$H+}
uses SysUtils,Classes,uXML,uModel,uVect;

VAR
  SR  : SnapRecord;
  ScR : SceneRecord;
  spl : TList;
  cam : CameraRecord;
  pSt : string;
  i:integer;
BEGIN
  InitScene;//SR.InitSceneにするべき？
  SR.MakeSnap;
  pSt:='xml';
  IF NOT DirectoryExists(pSt) THEN
    IF NOT CreateDir (pSt) THEN
      Writeln ('Failed to create directory !')
    ELSE
      Writeln ('Created "NewDir" directory');
  FOR i:=0 TO sc.count-1 DO BEGIN
    cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),640,480,0.5135,140);
    ScR.Cam:=cam;
    ScR.spl:=TList(sc[i]);
    WriteXMLScene(ScR,pst+'/'+IntToStr(i)+'Model.xml');
  END;
  Sr.GetNextScene(640,480);
  writeXMLScene(SR.CurSceneRec,pSt+'/'+'teste.xml');
  writeln(' Write XML');
  ScR:=ReadXMLConf(pSt+'/'+'teste.xml');
  writeln(' Read XML');
  writeXMLScene(ScR,'testf.xml'); 
END.
