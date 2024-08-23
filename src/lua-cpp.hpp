#ifndef LUA_CPP_HPP
#define LUA_CPP_HPP

#include <lua.hpp>

#include <concepts>
#include <string_view>
#include <string>
#include <tuple>
#include <array>
#include <optional>
#include <stdexcept>
#include <string>
#include <typeindex>

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
  concept internal = type_index<T> < type_index_userdata;
  template <typename T>
  concept userdata = type_index<T> == type_index_userdata;

  template <typename T>
  using type = decltype([]
                        { if constexpr (internal<T>) return types_at<type_index<T>>{}; }());

  template <typename T>
  auto inline static constexpr type_id_of = type_ids.at(type_index<T>);

  auto inline constexpr implicit_cast(internal auto &&val) noexcept -> type<decltype(val)> { return val; }
  auto inline constexpr explicit_cast(internal auto &&val) noexcept -> type<decltype(val)> { return static_cast<type<decltype(val)>>(val); }
  auto inline constexpr internal_cast(internal auto &&val) noexcept -> type<decltype(val)> { return explicit_cast(val); }

  template <typename T>
  concept strict_internal = internal<T> and std::convertible_to<type<T>, T>;
}
namespace lua
{
  using namespace types;
}

#pragma endregion
#pragma region Utils
namespace lua::utils
{
  struct exceptions_to_errors
  {
    state *L;
    auto inline constexpr operator<<(std::invocable auto &&fn) -> decltype(fn())
    {
      try
      {
        return fn();
      } // will call appropriate destructors
      catch (std::exception const &e)
      {
        luaL_error(L, "C++ exception: %s", e.what()); // will call longjmp
        throw;
      }
    }
  };

}

#pragma endregion
#pragma region Imports
namespace lua::imports
{
  [[nodiscard]] auto inline static type(state *L, int idx) noexcept -> auto { return static_cast<type_id>(lua_type(L, idx)); }
  [[nodiscard]] auto inline static type_of(state *L, int idx) noexcept -> auto { return type(L, idx); }
  [[nodiscard]] auto inline static type_name(state *L, type_id type) noexcept -> auto { return lstring{lua_typename(L, static_cast<int>(type))}; }
  [[nodiscard]] auto inline static type_name(state *L, int idx /**/) noexcept -> auto { return type_name(L, type(L, idx)); }

  auto inline static constexpr *gettop = lua_gettop;
  auto inline static constexpr *settop = lua_settop;
  auto inline static constexpr *setmetatable = lua_setmetatable;
  auto inline static constexpr *getmetatable = lua_getmetatable;
  auto inline static constexpr *geti = lua_geti;
  auto inline static constexpr *seti = lua_seti;
  auto inline static constexpr *getfield = lua_getfield;
  auto inline static constexpr *setfield = lua_setfield;
  auto inline static constexpr *gettable = lua_gettable;
  auto inline static constexpr *settable = lua_settable;
  auto inline static constexpr *getglobal = lua_getglobal;
  auto inline static constexpr *setglobal = lua_setglobal;
  auto inline static constexpr *getiuservalue = lua_getiuservalue;
  auto inline static constexpr *setiuservalue = lua_setiuservalue;
  auto inline static constexpr *absindex = lua_absindex;
  auto inline static pop(state *L, int n = 1) noexcept -> auto { return lua_pop(L, n); }
  auto inline static constexpr upvalueindex(int i) noexcept -> auto { return lua_upvalueindex(i); }

  auto inline static constexpr *pushvalue = lua_pushvalue;
  auto inline static pushnil /*           */ (state *L /*                                  */) noexcept -> auto { return lua_pushnil(L); }
  auto inline static pushtable /*         */ (state *L, int narr = 0, int nrec = 0 /*      */) noexcept -> auto { return lua_createtable(L, narr, nrec); }
  auto inline static pushthread /*        */ (state *L /*                                  */) noexcept -> auto { return lua_pushthread(L); }
  auto inline static pushboolean /*       */ (state *L, boolean /*          */ val /*      */) noexcept -> auto { return lua_pushboolean(L, val); }
  auto inline static pushlightuserdata /* */ (state *L, lightuserdata /*    */ val /*      */) noexcept -> auto { return lua_pushlightuserdata(L, val); }
  auto inline static pushinteger /*       */ (state *L, integer /*          */ val /*      */) noexcept -> auto { return lua_pushinteger(L, val); }
  auto inline static pushnumber /*        */ (state *L, number /*           */ val /*      */) noexcept -> auto { return lua_pushnumber(L, val); }
  auto inline static pushlstring /*       */ (state *L, lstring /*          */ val /*      */) noexcept -> auto { return lstring{lua_pushlstring(L, val.data(), val.size()), val.size()}; }
  auto inline static pushstring /*        */ (state *L, string /*           */ val /*      */) noexcept -> auto { return pushlstring(L, val); }
  auto inline static pushfunction /*      */ (state *L, function /*         */ val, int n = 0) noexcept -> auto { return lua_pushcclosure(L, val, n); }

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
  [[nodiscard]] auto inline static tofunction /*      */ (state *L, int idx) noexcept -> auto { return lua_tocfunction(L, idx); }
  [[nodiscard]] auto inline static tothread /*        */ (state *L, int idx) noexcept -> auto { return isthread(L, idx) ? std::optional{thread{}} : std::nullopt; }

}
namespace lua
{
  using namespace imports;
}

#pragma endregion
#pragma region Overloads
namespace lua::overloads
{
  /// @brief Push a value onto the lua stack
  /// @param L
  /// @param val
  /// @return allocation details if it makes sense to do so
  auto inline push(state *L, nil /*           */ val) noexcept -> auto { return pushnil /*           */ (L); }
  auto inline push(state *L, thread /*        */ val) noexcept -> auto { return pushthread /*        */ (L); }
  auto inline push(state *L, boolean /*       */ val) noexcept -> auto { return pushboolean /*       */ (L, val); }
  auto inline push(state *L, lightuserdata /* */ val) noexcept -> auto { return pushlightuserdata /* */ (L, val); }
  auto inline push(state *L, integer /*       */ val) noexcept -> auto { return pushinteger /*       */ (L, val); }
  auto inline push(state *L, number /*        */ val) noexcept -> auto { return pushnumber /*        */ (L, val); }
  auto inline push(state *L, lstring /*       */ val) noexcept -> auto { return pushlstring /*       */ (L, val); }
  auto inline push(state *L, string /*        */ val) noexcept -> auto { return pushlstring /*       */ (L, val); }
  auto inline push(state *L, table /*         */ val, int narr = 0, int nrec = 0) noexcept -> auto { return pushtable /*    */ (L, narr, nrec); }
  auto inline push(state *L, function /*      */ val, int n = 0 /*            */) noexcept -> auto { return pushfunction /* */ (L, val, n); }
  auto inline push(state *L, internal auto /* */ &&val) noexcept -> auto { return push(L, implicit_cast(std::forward<decltype(val)>(val))); }

  /// @brief Test the type of an argument
  /// @param L
  /// @param idx
  /// @param _ overload_differentiator
  /// @return `true` if the value at `idx` matches the type else `false`
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] nil /*           */) noexcept -> bool { return isnil /*           */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] table /*         */) noexcept -> bool { return istable /*         */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] thread /*        */) noexcept -> bool { return isthread /*        */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] boolean /*       */) noexcept -> bool { return isboolean /*       */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] lightuserdata /* */) noexcept -> bool { return islightuserdata /* */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] integer /*       */) noexcept -> bool { return isinteger /*       */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] number /*        */) noexcept -> bool { return isnumber /*        */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] lstring /*       */) noexcept -> bool { return islstring /*       */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] string /*        */) noexcept -> bool { return isstring /*        */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] function /*      */) noexcept -> bool { return isfunction /*      */ (L, idx); }
  [[nodiscard]] auto inline static is(state *L, int idx, [[maybe_unused]] internal auto /* */ &&v) noexcept -> bool { return is(L, idx, types::type<decltype(v)>{}); }
  template <internal T>
  [[nodiscard]] auto inline static is(state *L, int idx) noexcept -> bool { return is(L, idx, types::type<T>{}); }

  /// @brief Convert and get an argument from the lua stack
  /// @param L
  /// @param idx
  /// @param arg3 overload_differentiator
  /// @return The converted value at `idx` in an std::optional. `std::nullopt` if the value at `idx` was not convertible.
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] nil /*           */) noexcept -> std::optional<nil /*           */> { return tonil /*           */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] boolean /*       */) noexcept -> std::optional<boolean /*       */> { return toboolean /*       */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] lightuserdata /* */) noexcept -> std::optional<lightuserdata /* */> { return tolightuserdata /* */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] integer /*       */) noexcept -> std::optional<integer /*       */> { return tointeger /*       */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] number /*        */) noexcept -> std::optional<number /*        */> { return tonumber /*        */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] lstring /*       */) noexcept -> std::optional<lstring /*       */> { return tolstring /*       */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] string /*        */) noexcept -> std::optional<lstring /*       */> { return tostring /*        */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] table /*         */) noexcept -> std::optional<table /*         */> { return totable /*         */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] function /*      */) noexcept -> std::optional<function /*      */> { return tofunction /*      */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] thread /*        */) noexcept -> std::optional<thread /*        */> { return tothread /*        */ (L, idx); };
  [[nodiscard]] auto inline to(state *L, int idx, [[maybe_unused]] internal auto /* */ &&v) noexcept -> auto { return to(L, idx, types::type<decltype(v)>{}); };
  template <internal T>
  [[nodiscard]] auto inline to(state *L, int idx) noexcept -> auto { return to(L, idx, types::type<T>{}); };

  /// @brief Convert and get an argument with prettier exceptions
  /// @param L
  /// @param idx
  /// @param _ overload_differentiator
  /// @return `*to(L, idx, v)`
  /// @throws `std::runtime_error` ("Tried to access argument %d, absindex:%d, of type:%s, as type %s")
  template <internal T>
  [[nodiscard]] auto inline as(state *L, int idx) -> decltype(*to<T>(L, idx))
  {
    try
    {
      return *to<T>(L, idx);
    }
    catch (std::bad_optional_access const &)
    {
      auto msg = std::string{pushfstring(L, "Tried to access argument %d, absindex:%d, of type:%s, as type %s",
                                         idx, absindex(L, idx) type_name(L, idx).data(), type_name(L, type_id_of<T>).data())};
      pop(L), throw std::runtime_error{std::move(msg)};
    }
  };
  template <internal T>
  [[nodiscard]] auto inline as(state *L, int idx, [[maybe_unused]] T &&) -> decltype(as<T>(L, idx)) { return as<T>(L, idx); }

  template <typename RT, internal... ARGS>
  auto inline push(state *L, RT (*val)(ARGS...)) noexcept -> decltype(push(L, function{}, 1))
  {
    function fn = [](state *L) -> int
    { return utils::exceptions_to_errors{L} << [&]
      { return [&]<size_t... I>(std::index_sequence<I...>)
        {
          auto const fn = static_cast<RT (*)(ARGS...)>(as<lightuserdata>(L, upvalueindex(1)));
          if (not fn)
            throw std::runtime_error{"captured function-pointer was nullptr"};
          if constexpr (internal<RT>)
            return /* */ push(L, fn(as<ARGS>(L, 1 + I)...)), 1;
          else
            return /*  */ ((void)fn(as<ARGS>(L, 1 + I)...)), 0;
        }(std::make_index_sequence<sizeof...(ARGS)>()); }; };
    return push(L, lightuserdata{val}), push(L, fn, 1);
  }

}
namespace lua
{
  using namespace lua::overloads;
}

#pragma endregion
#pragma region Userdata
namespace lua::userdata
{
  
}
namespace lua
{
  using namespace userdata;
}

#pragma endregion

#endif // LUA_CPP_HPP