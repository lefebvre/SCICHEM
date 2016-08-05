//*******************************************************************************
//$RCSfile$
//$Revision$
//$Date$
//*******************************************************************************

struct messageT
{
  int  iParm;
  int  jParm;
  char aString[128];
  char bString[128];
  char cString[128];
  char routine[80];
};
struct releaseT
{
  int   padding;
  int   type;
  int   status;
  float tRel;
  double xRel;
  double yRel;
  float zRel;
  float horzUnc;
  float vertUnc;
  float pa;
  int   relData[52];
  char  material[16];
  char  relName[32];
  char  relDisplay[48];
};
struct enviroT
{
  float z;
  float pressure;
  float potentialTemp; 
  float humidity;
  float windUComp;
  float windVComp;
  float windWComp;
};
struct environmentT
{
  float           sfcElevation;
  float           sfcPressure;
  float           mixingLayerHeight;
  struct enviroT  samples[250];
  int             nsamp;
};
struct updateRelT
{
  int                 mode;
  float               currentTime;
  float               nextUpdate;
  struct releaseT     release;
  struct environmentT environment;
};
struct computeEffT
{
  int incidentID;
  int effectID;
  int request;
  int addressIn;
  int addressOut;
};

#ifdef _WIN64

typedef long long callbackInt;

#else

typedef int callbackInt;

#endif
