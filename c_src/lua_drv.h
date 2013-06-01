typedef struct _lua_drv_t {
  ErlDrvPort port;
  ErlDrvTermData drvport;
  lua_State *L;
} lua_drv_t;


#ifdef __cplusplus
extern "C" {
#endif

  /* Fix a silly Lua warning */

  void luaL_openlibs (lua_State *L);

  /* Commands */

  void erl_lua_call(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_concat(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_createtable(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_getfield (lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_getglobal(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_gettable(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_gettop(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_newtable(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_next(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_objlen(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushboolean(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushinteger(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushlstring(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushnil(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushnumber(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_rawequal(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_remove(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_setfield(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_setglobal(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_toboolean(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_tointeger(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_tolstring(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_tonumber(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_settable(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_type(lua_drv_t *driver_data, char *buf, int index);

  void erl_lual_dostring (lua_drv_t *driver_data, char *buf, int index);

  void erl_luam_multipcall (lua_drv_t *driver_data, char *buf, int index);
  void erl_luam_maybe_atom (lua_drv_t *driver_data, char *buf, int index);

  void erl_lua_no_command (lua_drv_t *driver_data);

#ifdef __cplusplus
}
#endif				/* __cplusplus */
