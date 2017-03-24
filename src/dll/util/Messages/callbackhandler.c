//*******************************************************************************
//$RCSfile$
//$Revision$
//$Date$
//*******************************************************************************
#include "callbackhandler.h"

int  callbackMessage( callback, id, a, b, flag )
  int (*callback)();
  int *a, *b, *id;
  int flag;
  {
		switch (flag)
		{
			case 0:
				return (*callback)( *id, *a, *b );
			default:
				return (*callback)( *id, *a, b );
		}
  }

#ifdef Linux
int  callback_byreference_( callbackAddress, id, a, b )
  callbackInt *callbackAddress;
  int *a, *b, *id;
  {
	  return callbackMessage(*callbackAddress, id, a, b, 1 );
  }
int  callback_byvalue_( callbackAddress, id, a, b )
  callbackInt *callbackAddress;
  int *a, *b, *id;
  {
	  return callbackMessage(*callbackAddress, id, a, b, 0 );
  }
#else
//=======Duplicate function for different fortran compilers======
int  __cdecl CALLBACK_BYREFERENCE( callbackAddress, id, a, b )
  callbackInt *callbackAddress;
  int *a, *b, *id;
  {
	  return callbackMessage(*callbackAddress, id, a, b, 1 );
  }

int   __cdecl CALLBACK_BYVALUE( callbackAddress, id, a, b )
  callbackInt *callbackAddress;
  int *a, *b, *id;
  {
	  return callbackMessage(*callbackAddress, id, a, b, 0 );
  }
#endif

//===============================================================
