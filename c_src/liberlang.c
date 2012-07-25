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

    userdata = (char*)lua_newuserdata(L, len);
    memcpy(userdata, str, len);
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
    luaL_register(L, "erlang", liberlang);
    lua_pop(L, 2);
    return 1;
}
