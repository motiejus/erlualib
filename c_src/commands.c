#include <assert.h>
#include <erl_driver.h>
#include <ei.h>
#include <lua.h>
#include <lauxlib.h>
#include <string.h>

#include "lua_drv.h"
#include "commands.h"

static void reply_ok(lua_drv_t *driver_data);
static void reply_error(lua_drv_t *driver_data, const char*);
static char* decode_string(const char *buf, int *index);
static char* decode_binary(const char *buf, int *index, int *len);

void
erl_lua_call(lua_drv_t *driver_data, char *buf, int index)
{
  long args, results;
  
  ei_decode_long(buf, &index, &args);
  ei_decode_long(buf, &index, &results);
  
  lua_call(driver_data->L, args, results);
  
  reply_ok(driver_data);
}

void
erl_lua_concat(lua_drv_t *driver_data, char *buf, int index)
{
  long n;
  
  ei_decode_long(buf, &index, &n);
  
  lua_concat(driver_data->L, n);
  
  reply_ok(driver_data);
}

void
erl_lua_createtable(lua_drv_t *driver_data, char *buf, int index)
{
  long narr, nrec;

  ei_decode_long(buf, &index, &narr);
  ei_decode_long(buf, &index, &nrec);

  lua_createtable(driver_data->L, narr, nrec);

  reply_ok(driver_data);
}

void
erl_lua_objlen(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  size_t size;
  
  ei_decode_long(buf, &index, &i);
  size = lua_objlen(driver_data->L, i);

  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) size,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_newtable(lua_drv_t *driver_data, char *buf, int index)
{
  lua_newtable(driver_data->L);
  reply_ok(driver_data);
}

void
erl_lua_next(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  int ret;

  ei_decode_long(buf, &index, &i);

  ret = lua_next(driver_data->L, i);

  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) ret,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_getfield(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_string(buf, &index);
  
  lua_getfield(driver_data->L, i, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_getglobal(lua_drv_t *driver_data, char *buf, int index)
{
  char *name;

  name = decode_string(buf, &index);
  
  lua_getglobal(driver_data->L, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_gettable(lua_drv_t *driver_data, char *buf, int index)
{
    long i;

    ei_decode_long(buf, &index, &i);
    lua_gettable(driver_data->L, i);

    reply_ok(driver_data);
}

void
erl_lua_gettop(lua_drv_t *driver_data, char *buf, int index)
{
  int size;
  
  size = lua_gettop(driver_data->L);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) size,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_pushboolean(lua_drv_t *driver_data, char *buf, int index)
{
  int b;
  
  ei_decode_boolean(buf, &index, &b);
  
  lua_pushboolean(driver_data->L, b);
  
  reply_ok(driver_data);
}

void
erl_lua_pushinteger(lua_drv_t *driver_data, char *buf, int index)
{
  long long num;
  
  ei_decode_longlong(buf, &index, &num);
  
  lua_pushinteger(driver_data->L, num);
  
  reply_ok(driver_data);
}

void
erl_lua_pushlstring(lua_drv_t *driver_data, char *buf, int index)
{
  char *str;
  int len;

  str = decode_binary(buf, &index, &len);

  lua_pushlstring(driver_data->L, str, len);

  reply_ok(driver_data);
  free(str);
}

void
erl_lua_pushnil(lua_drv_t *driver_data, char *buf, int index)
{
  lua_pushnil(driver_data->L);
  reply_ok(driver_data);
}

void
erl_lua_pushnumber(lua_drv_t *driver_data, char *buf, int index)
{
  double dnum;
  long long lnum;
  int type, len;
  
  ei_get_type(buf, &index, &type, &len);
  
  switch (type) {
  case ERL_FLOAT_EXT:
    ei_decode_double(buf, &index, &dnum);
    lua_pushnumber(driver_data->L, dnum);
    break;
  default:
    ei_decode_longlong(buf, &index, &lnum);
    lua_pushnumber(driver_data->L, lnum);
    break;
  }
  
  reply_ok(driver_data);
}

void
erl_lua_remove(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  
  ei_decode_long(buf, &index, &i);
  
  lua_remove(driver_data->L, i);
  
  reply_ok(driver_data);
}

void
erl_lua_setfield(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_string(buf, &index);
  
  lua_setfield(driver_data->L, i, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_setglobal(lua_drv_t *driver_data, char *buf, int index)
{
  char *name;
  
  name = decode_string(buf, &index);
  
  lua_setglobal(driver_data->L, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_toboolean(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  int res;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_toboolean(driver_data->L, i);

  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_ATOM, driver_mk_atom(res ? "true" : "false"),
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tointeger(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  long long res;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_tointeger(driver_data->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) res,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tolstring(lua_drv_t *driver_data, char *buf, int index)
{
  size_t len;
  long i;
  const char *str;
  
  ei_decode_long(buf, &index, &i);
  
  str = lua_tolstring(driver_data->L, i, &len);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) str, len,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tonumber(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  double res;
  int encode_i = 0;
  int size;
  char *eibuf;
    
  ei_decode_long(buf, &index, &i);
  
  res = lua_tonumber(driver_data->L, i);
  
  ei_encode_version(NULL, &encode_i);
  if ((long long) res == res) {
    ei_encode_longlong(NULL, &encode_i, (long long) res);
    size = encode_i;
    encode_i = 0;
    eibuf = malloc(sizeof(char) * (size + 1));
    
    ei_encode_version(eibuf, &encode_i);
    ei_encode_longlong(eibuf, &encode_i, res);
  } else {
    ei_encode_double(NULL, &encode_i, res);
    size = encode_i;
    encode_i = 0;
    eibuf = malloc(sizeof(char) * (size + 1));

    ei_encode_version(eibuf, &encode_i);
    ei_encode_double(eibuf, &encode_i, res);
  }
    
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) eibuf, size,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
  free(eibuf);
}

void
erl_lua_settable(lua_drv_t *driver_data, char *buf, int index)
{
    long i;
    ei_decode_long(buf, &index, &i);

    lua_settable(driver_data->L, i);

    reply_ok(driver_data);
}

void
erl_lua_type(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  int lua_t;
  
  ei_decode_long(buf, &index, &i);
  
  lua_t = lua_type(driver_data->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) lua_t,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


void
erl_lual_dostring(lua_drv_t *driver_data, char *buf, int index)
{
  char *code;
  int len;

  code = decode_binary(buf, &index, &len);
  code[len] = '\0';
  
  if (!luaL_dostring(driver_data->L, code))
    reply_ok(driver_data);
  else
    reply_error(driver_data, lua_tostring(driver_data->L, -1));
  free(code);
}

void
erl_luam_multicall(lua_drv_t *driver_data, char *buf, int index)
{
  long args, level, ret_results;
  ei_decode_long(buf, &index, &args);

  /* level := function's index - 1 */
  level = lua_gettop(driver_data->L) - args - 1;

  lua_call(driver_data->L, args, LUA_MULTRET);

  ret_results = lua_gettop(driver_data->L) - level;
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) ret_results,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


void
erl_lua_no_command(lua_drv_t *driver_data)
{  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_ERROR,
        ERL_DRV_STRING, (ErlDrvTermData) "No Command Found", 16,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


static void
reply_ok(lua_drv_t *driver_data)
{
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, ATOM_OK};
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

static void
reply_error(lua_drv_t *driver_data, const char *err)
{
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_ERROR,
        ERL_DRV_STRING, (ErlDrvTermData) err, strlen(err),
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


static char*
decode_string(const char *buf, int *index)
{
  int type, length;
  char *str;
  
  ei_get_type(buf, index, &type, &length);
  str = malloc(sizeof(char) * (length + 1));
  ei_decode_string(buf, index, str);
  return str;
}

static char*
decode_binary(const char *buf, int *index, int *len)
{
  int type;
  char *str;
  long length; /* from ei_decode_binary */
  
  ei_get_type(buf, index, &type, len);

  str = malloc(sizeof(char) * (*len + 1));
  ei_decode_binary(buf, index, str, &length);
  assert((int)length == *len);
  return str;
}
