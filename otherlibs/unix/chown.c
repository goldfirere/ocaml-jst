/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_chown(path, uid, gid) /* ML */
     value path, uid, gid;
{
  int ret;
  ret = chown(String_val(path), Int_val(uid), Int_val(gid));
  if (ret == -1) uerror("chown", path);
  return Val_unit;
}
