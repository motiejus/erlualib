#include <string.h>

#include "lauxlib.h"
#include "liberlang.h"

/* Take the String on stack and convert it to atom
 *
 * Works by having a user data in which we store the string, and its metatable
 * is erlang.mt_atom
 */
static int e_atom(lua_State *L) {
    const char *str;
    size_t len;

    char *userdata;

    luaL_checkstring(L, 1);
    str = lua_tolstring(L, 1, &len);

    userdata = (char*)lua_newuserdata(L, len+1);
    memcpy(userdata, str, len+1);
    luaL_getmetatable(L, "erlang.t_atom");
    lua_setmetatable(L, -2);
    return 1;
}

static const struct luaL_Reg liberlang[] = {
    {"atom", e_atom},
    {NULL, NULL}
};

int luaopen_erlang(lua_State *L) {
    luaL_newmetatable(L, "erlang.t_atom");

#if LUA_VERSION_NUM > 501
    lua_newtable(L);
    luaL_setfuncs (L, liberlang, 0);
    lua_setglobal(L, "erlang");
#else
    luaL_register(L, "erlang", liberlang);
#endif
    lua_pop(L, 2);
    return 1;
}
