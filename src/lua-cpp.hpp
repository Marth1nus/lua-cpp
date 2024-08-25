#ifndef LUA_CPP_HPP
#define LUA_CPP_HPP

#include <lua.hpp>

#pragma region Std includes
#include <concepts>
#include <string_view>
#include <string>
#include <tuple>
#include <array>
#include <optional>
#include <stdexcept>
#include <string>
#include <typeindex>
#include <memory>
#pragma endregion

#pragma region Types
namespace lua::types
{
  enum class type_id : int
  {
    NONE /*          */ = LUA_TNONE,
    NIL /*           */ = LUA_TNIL,
    BOOLEAN /*       */ = LUA_TBOOLEAN,
    LIGHTUSERDATA /* */ = LUA_TLIGHTUSERDATA,
    NUMBER /*        */ = LUA_TNUMBER,
    STRING /*        */ = LUA_TSTRING,
    TABLE /*         */ = LUA_TTABLE,
    FUNCTION /*      */ = LUA_TFUNCTION,
    USERDATA /*      */ = LUA_TUSERDATA,
    THREAD /*        */ = LUA_TTHREAD,
    NUMTYPES /*      */ = LUA_NUMTYPES,
  };
  using enum type_id;

  using state = lua_State;

#define UNIQUE_EMPTY_STRUCT /* clang-format off */ struct {} /* clang-format on */
  using nil /*           */ = UNIQUE_EMPTY_STRUCT;
  using boolean /*       */ = bool;
  using lightuserdata /* */ = void *;
  using integer /*       */ = lua_Integer;
  using number /*        */ = lua_Number;
  using lstring /*       */ = std::string_view;
  using string /*        */ = char const *;
  using table /*         */ = UNIQUE_EMPTY_STRUCT;
  using function /*      */ = lua_CFunction;
  using thread /*        */ = UNIQUE_EMPTY_STRUCT;
#undef UNIQUE_EMPTY_STRUCT

  auto inline static constexpr type_ids = /* clang-format off */
                std::array{NIL, TABLE, THREAD,  NUMBER, NUMBER,  STRING, STRING, FUNCTION, LIGHTUSERDATA, BOOLEAN, USERDATA}; /* clang-format on */
  using types = std::tuple<nil, table, thread, integer, number, lstring, string, function, lightuserdata, boolean>;
  static_assert(type_ids.size() == 1 + std::tuple_size_v<types> and USERDATA == type_ids.at(std::tuple_size_v<types>));

  auto inline static constexpr type_index_userdata = std::tuple_size_v<types>;
  auto inline static constexpr type_index_invalid = ~size_t{0};

  template <size_t I>
    requires(I <= type_index_userdata)
  using types_at = decltype([]
                            { if constexpr (I < type_index_userdata) return std::tuple_element_t<I, types>{}; }());

  template <typename T>
  auto inline static constexpr type_index = []() -> size_t
  {
    using type = std::decay_t<T>;
    if constexpr (not std::movable<type> and not std::copyable<type>)
      return type_index_invalid;
    else if constexpr (std::integral<type> and not std::same_as<type, integer>)
      return type_index<integer>;
    else if constexpr (std::floating_point<type> and not std::same_as<type, number>)
      return type_index<number>;
    else
    {
      size_t index = type_index_userdata;
      [&]<size_t... I>(std::index_sequence<I...>)
      {
        return ((std::same_as /*  */<type, types_at<I>> and ((index = I), true)) or ...) or // exact match
               ((std::convertible_to<type, types_at<I>> and ((index = I), true)) or ...);   // preferred conversion order
      }(std::make_index_sequence<std::tuple_size_v<types>>());
      return index;
    }
  }();

  template <typename T>
  concept userdata = type_index<T> == type_index_userdata;
  template <typename T>
  concept internal = type_index<T> < type_index_userdata;

  template <typename T>
  using internal_type = decltype([]
                                 { if constexpr (internal<T>) return types_at<type_index<T>>{}; }());

  template <typename T>
  type_id inline static constexpr type_id_of = type_ids.at(type_index<T>);

  auto inline constexpr implicit_cast(internal auto &&val) noexcept -> internal_type<decltype(val)> { return val; }
  auto inline constexpr explicit_cast(internal auto &&val) noexcept -> internal_type<decltype(val)> { return static_cast<internal_type<decltype(val)>>(val); }
  auto inline constexpr internal_cast(internal auto &&val) noexcept -> internal_type<decltype(val)> { return explicit_cast(val); }

  template <typename T>
  concept internal_strict = internal<T> and std::convertible_to<internal_type<T>, T>;
  template <typename T>
  concept workable = internal<T> or userdata<T>;
}
namespace lua // using types::
{
  using enum types::type_id;
  using types::type_id,
      types::type_id_of,
      //
      types::state,
      //
      types::nil,
      types::boolean,
      types::lightuserdata,
      types::integer,
      types::number,
      types::lstring,
      types::string,
      types::table,
      types::function,
      types::thread,
      //
      types::internal,
      types::internal_type,
      types::internal_cast,
      types::internal_strict;
}
#pragma endregion

#pragma region Exceptions
namespace lua::exceptions
{
  struct argument_type_error : std::runtime_error
  {
    using std::runtime_error::runtime_error;
  };

  struct failed_assert : std::runtime_error
  {
    using std::runtime_error::runtime_error;
  };
}
#pragma endregion

#pragma region Utils
namespace lua::utils
{
  /// @brief Helper that catches c++ exceptions safely obeying scope exit rules.
  /// @note Use to make lua function type-and-raii-safe.
  /// @note `fn` will have its destructor called in case of error.
  /// @note DO NOT ASSUME that values outside of `fn` will have their destructors called.
  struct exceptions_to_errors
  {
    state *L;
    auto inline constexpr operator<<(std::invocable auto fn) const -> decltype(fn())
    {
      auto error_message = std::string{""};
      try
      {
        return fn();
      }
      catch (std::exception const &e)
      {
        error_message = e.what();
      }
      catch (...)
      {
        error_message = "Non-Standard-Exception";
      }
      lua_pushfstring(L, "C++ exception: %s", error_message.c_str());
      // std::destroy_at(this); // `this` ia a trivial object and does not need destruction
      std::destroy_at(&error_message);
      std::destroy_at(&fn);
      lua_error(L); // will call longjmp
      [[unreachable]] throw;
    }
  };

  auto inline static assert(std::convertible_to<bool> auto &&value, lstring message = "failed-assert") -> decltype(value)
  {
    if (not value)
      throw exceptions::failed_assert{std::string{message}};
    return std::forward<decltype(value)>(value);
  }
}
#pragma endregion

#pragma region Imports
namespace lua::imports::lua
{
  [[nodiscard]] auto inline static type /*       */ (state *L, int idx /*      */ /*            */) noexcept -> auto { return static_cast<type_id>(lua_type(L, idx)); }
  [[nodiscard]] auto inline static type_of /*    */ (state *L, int idx /*      */ /*            */) noexcept -> auto { return type(L, idx); }
  [[nodiscard]] auto inline static type_name /*  */ (state *L, type_id type /* */ /*            */) noexcept -> auto { return lstring{lua_typename(L, static_cast<int>(type))}; }
  [[nodiscard]] auto inline static type_name /*  */ (state *L, int idx /*      */ /*            */) noexcept -> auto { return type_name(L, type(L, idx)); }

  auto inline static constexpr *gettop /*        */ = lua_gettop;
  [[nodiscard("getmetatable is not guaranteed to push a value. Check return value to see if the a metatable was pushed")]]
  auto inline static /*      */ getmetatable /*  */ (state *L, int objindex /* */ /*            */) noexcept -> bool /* clang-format off */ { return lua_getmetatable(L, objindex); } /* clang-format on */
  auto inline static /*      */ geti /*          */ (state *L, int idx /*      */, integer n /* */) noexcept -> type_id { return static_cast<type_id>(lua_geti(L, idx, n)); }
  auto inline static /*      */ getfield /*      */ (state *L, int idx /*      */, string k /*  */) noexcept -> type_id { return static_cast<type_id>(lua_getfield(L, idx, k)); }
  auto inline static /*      */ gettable /*      */ (state *L, int idx /*      */ /*            */) noexcept -> type_id { return static_cast<type_id>(lua_gettable(L, idx)); }
  auto inline static /*      */ getglobal /*     */ (state *L, string name /*  */ /*            */) noexcept -> type_id { return static_cast<type_id>(lua_getglobal(L, name)); }
  auto inline static /*      */ getiuservalue /* */ (state *L, int idx /*      */, int n /*     */) noexcept -> type_id { return static_cast<type_id>(lua_getiuservalue(L, idx, n)); }

  auto inline static constexpr *settop /*        */ = lua_settop;
  auto inline static /*      */ setmetatable /*  */ (state *L, int objindex /* */ /*            */) noexcept -> void { return (void)lua_setmetatable(L, objindex); }
  auto inline static constexpr *seti /*          */ = lua_seti;
  auto inline static constexpr *setfield /*      */ = lua_setfield;
  auto inline static constexpr *settable /*      */ = lua_settable;
  auto inline static constexpr *setglobal /*     */ = lua_setglobal;
  auto inline static /*      */ setiuservalue /* */ (state *L, int idx /*      */, int n /*     */) noexcept -> bool { return lua_setiuservalue(L, idx, n) not_eq 0; }

  auto inline static constexpr *absindex /*      */ = lua_absindex;
  auto inline static constexpr upvalueindex /*   */ (int i /*                  */ /*            */) noexcept -> auto { return lua_upvalueindex(i); }

  auto inline static /*      */ rawequal /*      */ (state *L, int idx1 /*     */, int idx2 /*  */) noexcept -> bool { return lua_rawequal(L, idx1, idx2) not_eq 0; }

  auto inline static constexpr *pushvalue /*     */ = lua_pushvalue;
  auto inline static /*      */ pop /*           */ (state *L, int n = 1 /*    */ /*            */) noexcept -> auto { return lua_pop(L, n); }

  auto inline static pushnil /*           */ (state *L /*                       */ /*                  */) noexcept -> auto { return lua_pushnil(L); }
  auto inline static pushtable /*         */ (state *L, int narr = 0 /*         */, int nrec = 0 /*    */) noexcept -> auto { return lua_createtable(L, narr, nrec); }
  auto inline static pushthread /*        */ (state *L /*                       */ /*                  */) noexcept -> auto { return lua_pushthread(L); }
  auto inline static pushboolean /*       */ (state *L, boolean /*          */ val /*                  */) noexcept -> auto { return lua_pushboolean(L, val); }
  auto inline static pushlightuserdata /* */ (state *L, lightuserdata /*    */ val /*                  */) noexcept -> auto { return lua_pushlightuserdata(L, val); }
  auto inline static pushinteger /*       */ (state *L, integer /*          */ val /*                  */) noexcept -> auto { return lua_pushinteger(L, val); }
  auto inline static pushnumber /*        */ (state *L, number /*           */ val /*                  */) noexcept -> auto { return lua_pushnumber(L, val); }
  auto inline static pushlstring /*       */ (state *L, lstring /*          */ val /*                  */) noexcept -> auto { return lstring{lua_pushlstring(L, val.data(), val.size()), val.size()}; }
  auto inline static pushstring /*        */ (state *L, string /*           */ val /*                  */) noexcept -> auto { return pushlstring(L, val); }
  auto inline static pushfunction /*      */ (state *L, function /*         */ val, int n = 0 /*       */) noexcept -> auto { return lua_pushcclosure(L, val, n); }
  auto inline static pushuserdata /*      */ (state *L, auto && /*          */ val, int nuvalue = 1 /* */) noexcept -> std::decay_t<decltype(val)> * { return new (lua_newuserdatauv(L, sizeof(val), nuvalue)) std::decay_t<decltype(val)>{std::forward<decltype(val)>(val)}; }

  [[nodiscard]] auto inline static isnil /*           */ (state *L, int idx) noexcept -> bool { return type(L, idx) == NIL; }
  [[nodiscard]] auto inline static istable /*         */ (state *L, int idx) noexcept -> bool { return type(L, idx) == TABLE; }
  [[nodiscard]] auto inline static isthread /*        */ (state *L, int idx) noexcept -> bool { return type(L, idx) == THREAD; }
  [[nodiscard]] auto inline static isboolean /*       */ (state *L, int idx) noexcept -> bool { return type(L, idx) == BOOLEAN; }
  [[nodiscard]] auto inline static islightuserdata /* */ (state *L, int idx) noexcept -> bool { return type(L, idx) == LIGHTUSERDATA; }
  [[nodiscard]] auto inline static isinteger /*       */ (state *L, int idx) noexcept -> bool { return lua_isinteger(L, idx) == 1; }
  [[nodiscard]] auto inline static isnumber /*        */ (state *L, int idx) noexcept -> bool { return lua_isnumber(L, idx) == 1; }
  [[nodiscard]] auto inline static islstring /*       */ (state *L, int idx) noexcept -> bool { return lua_isstring(L, idx) == 1; }
  [[nodiscard]] auto inline static isstring /*        */ (state *L, int idx) noexcept -> bool { return lua_isstring(L, idx) == 1; }
  [[nodiscard]] auto inline static isfunction /*      */ (state *L, int idx) noexcept -> bool { return lua_iscfunction(L, idx) == 1; }
  [[nodiscard]] auto inline static isuserdata /*      */ (state *L, int idx) noexcept -> bool { return lua_isuserdata(L, idx) == 1; }

  [[nodiscard]] auto inline static tonil /*           */ (state *L, int idx) noexcept -> auto { return isnil(L, idx) ? std::optional{nil{}} : std::nullopt; }
  [[nodiscard]] auto inline static toboolean /*       */ (state *L, int idx) noexcept -> auto { return std::optional{lua_toboolean(L, idx) == 1}; }
  [[nodiscard]] auto inline static tolightuserdata /* */ (state *L, int idx) noexcept -> auto { return islightuserdata(L, idx) ? std::optional{lua_touserdata(L, idx)} : std::nullopt; }
  [[nodiscard]] auto inline static tointeger /*       */ (state *L, int idx) noexcept -> auto
  {
    auto isnum = 0;
    auto num = lua_tointegerx(L, idx, &isnum);
    return isnum ? std::optional{num} : std::nullopt;
  }
  [[nodiscard]] auto inline static tonumber /*        */ (state *L, int idx) noexcept -> auto
  {
    auto isnum = 0;
    auto num = lua_tonumberx(L, idx, &isnum);
    return isnum ? std::optional{num} : std::nullopt;
  }
  [[nodiscard]] auto inline static tolstring /*       */ (state *L, int idx) noexcept -> auto
  {
    auto len = size_t{};
    auto str = lua_tolstring(L, idx, &len);
    return str ? std::optional{lstring{str, len}} : std::nullopt;
  }
  [[nodiscard]] auto inline static tostring /*        */ (state *L, int idx) noexcept -> auto { return tolstring(L, idx); }
  [[nodiscard]] auto inline static totable /*         */ (state *L, int idx) noexcept -> auto { return istable(L, idx) ? std::optional{table{}} : std::nullopt; }
  [[nodiscard]] auto inline static tofunction /*      */ (state *L, int idx) noexcept -> auto { return isfunction(L, idx) ? std::optional{lua_tocfunction(L, idx)} : std::nullopt; }
  [[nodiscard]] auto inline static tothread /*        */ (state *L, int idx) noexcept -> auto { return isthread(L, idx) ? std::optional{thread{}} : std::nullopt; }
  [[nodiscard]] auto inline static touserdata /*      */ (state *L, int idx) noexcept -> auto { return isuserdata(L, idx) ? std::optional{lua_touserdata(L, idx)} : std::nullopt; }
}
namespace lua::imports::luaL
{
  auto inline static constexpr *checknumber /*  */ = luaL_checknumber;
  auto inline static constexpr *checkinteger /* */ = luaL_checkinteger;
  auto inline static /*      */ checklstring /* */ (state *L, int idx) noexcept -> lstring
  {
    auto len = size_t{0};
    auto str = luaL_checklstring(L, idx, &len);
    return lstring{str, len};
  }
  auto inline static constexpr *checkstring /*  */ = checklstring;
  auto inline static constexpr *checkudata /*   */ = luaL_checkudata;
  auto inline static constexpr *checkany /*     */ = luaL_checkany;
  auto inline static constexpr *checkoption /*  */ = luaL_checkoption;
  auto inline static constexpr *checkstack /*   */ = luaL_checkstack;
  auto inline static constexpr *checktype /*    */ = luaL_checktype;

  auto inline static constexpr *newmetatable /* */ = luaL_newmetatable;
  auto inline static /*      */ getmetatable /* */ (state *L, string name) noexcept -> type_id { return (type_id)luaL_getmetatable(L, name); }
}
namespace lua // using lua::imports::lua
{
  using namespace imports::lua;
}
namespace luaL // using lua::imports::luaL
{
  using namespace lua::imports::luaL;
}
#pragma endregion

#pragma region Userdata
namespace lua::userdata
{
  using types::userdata;

  template <userdata T>
  string inline static constexpr type_name = typeid(T).name();

  template <userdata T>
  auto static push_metatable(state *L) noexcept -> int;

  template <userdata T>
  auto static push_shared(state *L, std::shared_ptr<T> const &value_ptr, int nuvalue = 1) -> std::shared_ptr<T>;

  template <userdata T>
    requires(not std::is_pointer_v<T>)
  auto static push(state *L, T &&value, int nuvalue = 1) -> T *;

  template <userdata T>
  [[nodiscard]] auto static is(state *L, int idx, [[maybe_unused]] T const & = *(T *)0) noexcept -> bool;

  template <userdata T>
  [[nodiscard]] auto static to(state *L, int idx, [[maybe_unused]] T const & = *(T *)0) noexcept -> std::optional<T *>;
}
#pragma endregion

#pragma region Overloads
namespace lua::overloads
{
  using types::userdata;

  /// @brief Push a value onto the lua stack
  /// @param L
  /// @param val
  /// @return allocation pointer if it makes sense to do so
  auto inline push(state *L, nil /*           */ val /*                       */) noexcept -> auto { return pushnil /*           */ (L); }
  auto inline push(state *L, thread /*        */ val /*                       */) noexcept -> auto { return pushthread /*        */ (L); }
  auto inline push(state *L, boolean /*       */ val /*                       */) noexcept -> auto { return pushboolean /*       */ (L, val); }
  auto inline push(state *L, lightuserdata /* */ val /*                       */) noexcept -> auto { return pushlightuserdata /* */ (L, val); }
  auto inline push(state *L, integer /*       */ val /*                       */) noexcept -> auto { return pushinteger /*       */ (L, val); }
  auto inline push(state *L, number /*        */ val /*                       */) noexcept -> auto { return pushnumber /*        */ (L, val); }
  auto inline push(state *L, lstring /*       */ val /*                       */) noexcept -> auto { return pushlstring /*       */ (L, val); }
  auto inline push(state *L, string /*        */ val /*                       */) noexcept -> auto { return pushlstring /*       */ (L, val); }
  auto inline push(state *L, table /*         */ val, int narr = 0, int nrec = 0) noexcept -> auto { return pushtable /*    */ (L, narr, nrec); }
  auto inline push(state *L, function /*      */ val, int n = 0 /*            */) noexcept -> auto { return pushfunction /* */ (L, val, n); }
  auto inline push(state *L, internal auto /* */ &&val /*                     */) noexcept -> auto { return push(L, types::implicit_cast(std::forward<decltype(val)>(val))); } // WARN type casting warnings appear in `types::implicit_cast`
  auto inline push(state *L, userdata auto /* */ &&val, int nuvalues = 1 /*   */) noexcept -> auto { return userdata::push(L, std::forward<decltype(val)>(val), nuvalues); }

  /// @brief Test the type of an argument
  /// @param L
  /// @param idx
  /// @param arg3 unused overload differentiator
  /// @return `true` if the value at `idx` matches the type else `false`
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] nil /*               */) noexcept -> auto { return isnil /*           */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] table /*             */) noexcept -> auto { return istable /*         */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] thread /*            */) noexcept -> auto { return isthread /*        */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] boolean /*           */) noexcept -> auto { return isboolean /*       */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] lightuserdata /*     */) noexcept -> auto { return islightuserdata /* */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] integer /*           */) noexcept -> auto { return isinteger /*       */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] number /*            */) noexcept -> auto { return isnumber /*        */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] lstring /*           */) noexcept -> auto { return islstring /*       */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] string /*            */) noexcept -> auto { return isstring /*        */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] function /*          */) noexcept -> auto { return isfunction /*      */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] internal auto &&v /* */) noexcept -> auto { return is(L, idx, internal_type<decltype(v)>{}); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] userdata auto &&v /* */) noexcept -> auto { return userdata::is(L, idx, std::forward<decltype(v)>(v)); }
  template <internal T>
  [[nodiscard]] auto inline static is(state *L, int idx) noexcept -> auto { return is(L, idx, internal_type<T>{}); }
  template <userdata T>
  [[nodiscard]] auto inline static is(state *L, int idx) noexcept -> auto { return userdata::is<T>(L, idx); }

  /// @brief Convert and get an argument from the lua stack
  /// @param L
  /// @param idx
  /// @param arg3 unused overload differentiator
  /// @return The converted value at `idx` in an std::optional. `std::nullopt` if the value at `idx` was not convertible.
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] nil /*               */) noexcept -> auto { return tonil /*           */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] boolean /*           */) noexcept -> auto { return toboolean /*       */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] lightuserdata /*     */) noexcept -> auto { return tolightuserdata /* */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] integer /*           */) noexcept -> auto { return tointeger /*       */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] number /*            */) noexcept -> auto { return tonumber /*        */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] lstring /*           */) noexcept -> auto { return tolstring /*       */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] string /*            */) noexcept -> auto { return tostring /*        */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] table /*             */) noexcept -> auto { return totable /*         */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] function /*          */) noexcept -> auto { return tofunction /*      */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] thread /*            */) noexcept -> auto { return tothread /*        */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] internal auto &&v /* */) noexcept -> auto { return to(L, idx, internal_type<decltype(v)>{}); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] userdata auto &&v /* */) noexcept -> auto { return userdata::to(L, idx, std::forward<decltype(v)>(v)); };
  template <internal T>
  [[nodiscard]] auto inline to(state *L, int idx) noexcept -> auto { return to(L, idx, internal_type<T>{}); };
  template <userdata T>
  [[nodiscard]] auto inline to(state *L, int idx) noexcept -> auto { return userdata::to<T>(L, idx); };

  /// @brief Convert and get an argument with prettier exceptions
  /// @param L
  /// @param idx
  /// @param arg3 unused overload differentiator
  /// @return `*to(L, idx, v)`
  /// @throws `argument_type_error` ("Tried to access argument %d, absindex:%d, of type:%s, as type:%s")
  /// @note Use this in combination with `utils::exceptions_to_errors` instead of luaL::catch functions.
  template <std::destructible T>
  [[nodiscard]] auto inline as(state *L, int idx, [[maybe_unused]] T const & = *(T *)0) -> std::remove_reference_t<decltype(*to<T>(L, idx))>
  {
    if (auto opt_res = to<T>(L, idx))
      return *opt_res;
    type_id idx_type /**/ = type /*     */ (L, idx /**/), /* | */ T_type /**/ = type_id_of<T>;
    lstring idx_type_name = type_name /**/ (L, idx_type), /* | */ T_type_name = type_name(L, T_type);
    if constexpr (auto userdata_detailed_names = true)
    {
      if (idx_type == USERDATA and getmetatable(L, idx))
      {
        getfield(L, -1, "typename");
        idx_type_name = to<lstring>(L, -1).value_or(idx_type_name);
        pop(L, 2);
      }
      if constexpr (userdata::userdata<T>)
        T_type_name = userdata::type_name<T>;
    }
    char msg[512];
    [[maybe_unused]] auto const msg_size = std::snprintf(
        msg, std::size(msg), "Tried to access argument %d, absindex:%d, of type:%s, as type:%s",
        idx, absindex(L, idx), idx_type_name.data(), T_type_name.data());
    throw exceptions::argument_type_error{msg};
  };
}
namespace lua::overloads::helpers
{
  using overloads::push;

  template <typename RT, internal... ARGS>
  auto inline push(state *L, RT (*val)(ARGS...)) noexcept -> decltype(push(L, function{}, 1))
    requires(not std::convertible_to<decltype(val), function>)
  {
    function fn = [](state *L) -> int
    { return utils::exceptions_to_errors{L} << [&]() -> int
      { return [&]<size_t... I>(std::index_sequence<I...>) -> int
        {
          auto const fn = static_cast<RT (*)(ARGS...)>(as<lightuserdata>(L, upvalueindex(1)));
          if (not fn)
            throw std::runtime_error{"captured function-pointer was nullptr"};
          if (gettop(L) not_eq sizeof...(ARGS))
            throw std::runtime_error{"Too few or many arguments"};
          if constexpr (internal<RT>)
            return /* */ push(L, internal_cast(fn(as<ARGS>(L, 1 + I)...))), 1;
          else
            return /*                */ ((void)fn(as<ARGS>(L, 1 + I)...)), 0;
        }(std::make_index_sequence<sizeof...(ARGS)>()); }; };
    return push(L, lightuserdata{val}), push(L, fn, 1);
  }
}
namespace lua::overloads // using helpers::
{
  using helpers::push;
}
namespace lua // using overloads::
{
  using overloads::push,
      overloads::is,
      overloads::to,
      overloads::as;
}
#pragma endregion

#pragma region Userdata Impl
namespace lua::userdata::impl
{
  struct userdata_container_base
  {
  public:
    inline userdata_container_base() noexcept = default;
    inline userdata_container_base(userdata_container_base &&) noexcept = default;
    inline userdata_container_base(userdata_container_base const &) noexcept = default;
    inline userdata_container_base &operator=(userdata_container_base &&) noexcept = default;
    inline userdata_container_base &operator=(userdata_container_base const &) noexcept = default;

  public: /* Virtual functions */ /* clang-format off */
    /*                 */ public:    virtual ~userdata_container_base() {}
    /*                 */ protected: virtual auto location() noexcept -> void * = 0;
    /* clang-format on */ public:
    virtual auto type() const noexcept -> std::type_info const & = 0;

  public:
    template <userdata T>
    auto get_if() noexcept -> T * { return type() == typeid(T) ? static_cast<T *>(location()) : nullptr; }
  };

  auto inline static to_userdata_container(state *L, int idx) noexcept -> std::optional<userdata_container_base *>
  {
    auto udata = (userdata_container_base *)touserdata(L, idx).value_or(nullptr);
    return udata ? std::optional{udata} : std::nullopt;
  }

  template <userdata T>
  struct userdata_container final : userdata_container_base
  {
    T value;

  public:
    inline userdata_container(auto &&...args) noexcept
      requires(std::constructible_from<T, decltype(args)...>)
        : value{std::forward<decltype(args)>(args)...}
    {
    }
    inline userdata_container(userdata_container &&) noexcept = default;
    inline userdata_container(userdata_container const &) noexcept = default;
    inline userdata_container &operator=(userdata_container &&) noexcept = default;
    inline userdata_container &operator=(userdata_container const &) noexcept = default;

  public:
    virtual ~userdata_container() override {}
    virtual auto location() noexcept -> void * final override { return &value; }
    virtual auto type() const noexcept -> std::type_info const & final override { return typeid(T); }
  };

  template <userdata T>
  struct userdata_container<std::shared_ptr<T>> final : userdata_container_base
  {
    std::shared_ptr<T> value;

  public:
    inline userdata_container(std::shared_ptr<T> const &value = {}) noexcept : value{value} {}
    inline userdata_container(userdata_container &&) noexcept = default;
    inline userdata_container(userdata_container const &) noexcept = default;
    inline userdata_container &operator=(userdata_container &&) noexcept = default;
    inline userdata_container &operator=(userdata_container const &) noexcept = default;

  public:
    virtual ~userdata_container() override {}
    virtual auto location() noexcept -> void * final override { return value.get(); }
    virtual auto type() const noexcept -> std::type_info const & final override { return typeid(T); }
  };

  template <userdata T>
  userdata_container(T) -> userdata_container<T>;
}
namespace lua::userdata // impl
{
  using overloads::as;

  template <userdata T>
  auto static push_metatable(state *L) noexcept -> int
  {
    if (not luaL::newmetatable(L, type_name<T>))
      return 1;
    using lua::push;

    function static constexpr gc = [](state *L) -> int
    {
      return utils::exceptions_to_errors{L} << [L]() -> int
      {
        utils::assert(is<T>(L, 1));                          // no conversions
        std::destroy_at(*impl::to_userdata_container(L, 1)); // virtual dispatch to destructor
        return 1;
      };
    };
    push(L, gc), setfield(L, -2, "__gc");
    push(L, type_name<T>), setfield(L, -2, "typename");

    return 1;
  }

  template <userdata T>
  auto static push_shared(state *L, std::shared_ptr<T> const &value_ptr, int nuvalue) -> std::shared_ptr<T>
  {
    auto udata = pushuserdata(L, impl::userdata_container{value_ptr}, nuvalue);
    push_metatable<T>(L), setmetatable(L, -2);
    utils::assert(udata->get_if<T>(), "This is a bug! "
                                      "Somehow the userdata container "
                                      "does not contain the correct type "
                                      "or incorrectly reports its type");
    return udata->value;
  }

  template <userdata T>
    requires(not std::is_pointer_v<T>)
  auto static push(state *L, T &&value, int nuvalue) -> T *
  {
    auto udata = pushuserdata(L, impl::userdata_container{std::forward<decltype(value)>(value)}, nuvalue);
    push_metatable<T>(L), setmetatable(L, -2);
    utils::assert(udata->get_if<T>(), "This is a bug! "
                                      "Somehow the userdata container "
                                      "does not contain the correct type "
                                      "or incorrectly reports its type");
    return &udata->value;
  }

  template <userdata T>
  [[nodiscard]] auto static is(state *L, int idx, [[maybe_unused]] T const &) noexcept -> bool
  {
    // clang-format off
    return USERDATA not_eq type(L, idx) /*                       */ ? (/*      */ false) : // idx is not userdata
    TABLE not_eq type_id(getmetatable(L, idx)) /*                */ ? (pop(L, 1), false) : // idx has not metatable
    TABLE not_eq type_id(luaL::getmetatable(L, type_name<T>)) /* */ ? (pop(L, 2), false) : // T has no metatable
    true not_eq rawequal(L, -2, -1) /*                           */ ? (pop(L, 2), false) : // idx and T do not have the same metatable
    /*                                                             */ (pop(L, 2), true ) ; // idx constains an instance of T
    // clang-format on
    // LOW-TODO Consider user userdata_container_base to verify type further
  }

  template <userdata T>
  [[nodiscard]] auto static to(state *L, int idx, [[maybe_unused]] T const &) noexcept -> std::optional<T *>
  {
    auto udata = impl::to_userdata_container(L, idx);
    if (auto res = udata->get_if<T>())
      return std::optional{res};
    // TODO Conversions and casting
    return std::nullopt;
  }
}
namespace lua // using userdata::
{
  using userdata::push_shared,
      userdata::push,
      userdata::is,
      userdata::to,
      userdata::as;
}
#pragma endregion

#endif // LUA_CPP_HPP