#ifndef __hxrpp_server_h
#define __hxrpp_server_h

#include "utils/app_config.h"

CFG_OPTION(OPT_SET_BOOL,OPT_BOOL_ATOM,"help",0,false,"show help message")
CFG_OPTION(OPT_SET_INT,OPT_INT,"serve","SERVE_ON_PORT",I(0),"port to serve on")
CFG_OPTION(OPT_SET_BOOL,OPT_BOOL_ATOM,"send",0,false,"send packet trains to an another party")
CFG_OPTION(OPT_SET_BOOL,OPT_BOOL_ATOM,"receive",0,false,"receive packet trains from an another party")
CFG_OPTION(OPT_SET_STR,OPT_STR,"remote","REMOTE",0,"receive packet trains from an another party")

#define SERVE_ON_PORT "serve"
#define SEND_PACKETS "send"
#define RECEIVE "receive"
#define REMOTE "remote"

#endif
