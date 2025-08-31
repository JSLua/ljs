declare module 'ffi' {

type CNamespace = { [func: string]: any };
type CType = string | ((...init) => any);

/**
 *  This is the default C library namespace — note the uppercase 'C'. It
 *  binds to the default set of symbols or libraries on the target system.
 *  These are more or less the same as a C compiler would offer by default,
 *  without specifying extra link libraries.
 *
 *  On POSIX systems, this binds to symbols in the default or global
 *  namespace. This includes all exported symbols from the executable and
 *  any libraries loaded into the global namespace. This includes at least
 *  libc, libm, libdl (on Linux), libgcc (if compiled with GCC), as well as
 *  any exported symbols from the Lua/C API provided by LuaJIT itself.
 */
export const C : CNamespace;

/**
 *  Adds multiple C declarations for types or external symbols (named
 *  variables or functions).
 *
 *  The contents of the string (the part in green above) must be a sequence
 *  of C declarations, separated by semicolons. The trailing semicolon for
 *  a single declaration may be omitted.
 *
 *  @param def C declarations. Must be a Lua string.
 */
export function cdef(def: string) : void;

/**
 *  Creates a cdata object for the given ct. The cdata object is
 *  initialized according to the rules for initializers, using the optional
 *  init arguments. Excess initializers cause an error.
 *  @param cType
 *  @param init Initializers.
 */
function _new(cType: CType, ...init) : any;
export { _new as new };

/**
 *  This loads the dynamic library given by name and returns a new C
 *  library namespace which binds to its symbols.
 *
 *  @param name If name is a path, the library is loaded from this path.
 *  Otherwise name is canonicalized in a system-dependent way and searched
 *  in the default search path for dynamic libraries.
 */
export function load(name: string) : CNamespace;

/**
 *  Creates a ctype object for the given ct. This is especially useful
 *  to parse a cdecl only once and then use the resulting ctype object as
 *  a constructor.
 */
function _typeof(ct: CType) : (...init) => any;
export { _typeof as typeof };

/**
 *   Creates a scalar cdata object for the given ct. The cdata object is
 *   initialized with init using the "cast" variant of the C type
 *   conversion rules.
 *
 *   This functions is mainly useful to override the pointer compatibility
 *   checks or to convert pointers to addresses or vice versa.
 */
export function cast(cType: CType, init: any) : any;

/**
 *  Creates a ctype object for the given ct and associates it with
 *  a metatable. Only struct/union types, complex numbers and vectors are
 *  allowed. Other types may be wrapped in a struct, if needed.
 *
 *  The association with a metatable is permanent and cannot be changed
 *  afterwards. Neither the contents of the metatable nor the contents of
 *  an __index table (if any) may be modified afterwards. The associated
 *  metatable automatically applies to all uses of this type, no matter
 *  how the objects are created or where they originate from. Note that
 *  predefined operations on types have precedence (e.g. declared field
 *  names cannot be overridden).
 */
export function metatype(ct: CType, metatable: Metatable) : (...init) => any;

/**
 *  Associates a finalizer with a pointer or aggregate cdata object. The
 *  cdata object is returned unchanged.
 *  @param cdata
 *  @param finalizer The finalizer. When the last reference to the cdata
 *  object is gone, it is called with the cdata object as an argument.
 */
export function gc(cdata: any, finalizer: () => void) : any;

/**
 *  Returns the size of cObj in bytes. Returns nil if the size is not known
 *  (e.g. for "void" or function types).
 */
export function sizeof(cObj: any) : number;

/**
 *  Returns the minimum required alignment for cObj in bytes.
 */
export function alignof(cObj: any) : number;

/**
 *  Returns true if obj has the C type given by ct, or false otherwise.
 *  C type qualifiers (const etc.) are ignored. Pointers are checked with
 *  the standard pointer compatibility rules, but without any special
 *  treatment for void *. If ct specifies a struct/union, then a pointer
 *  to this type is accepted, too. Otherwise the types must match exactly.
 */
export function istype(ct: CType, obj: any) : boolean;

/**
 *  Returns the error number set by the last C function call which
 *  indicated an error condition. If the optional newerr argument is
 *  present, the error number is set to the new value and the previous
 *  value is returned.
 *
 *  This function offers a portable and OS-independent way to get and set
 *  the error number. Note that only some C functions set the error number.
 *  And it's only significant if the function actually indicated an error
 *  condition (e.g. with a return value of -1 or NULL). Otherwise, it may
 *  or may not contain any previously set value.
 */
export function errno() : number;

/**
 *  Creates an interned Lua string from the data pointed to by ptr.
 *  @param ptr The data.
 *  @param length If present, ptr is converted to a "void *" and len gives
 *  the length of the data. The data may contain embedded zeros and need
 *  not be byte-oriented (though this may cause endianess issues).
 */
export function string(ptr: any, length?: number) : string;

/**
 *  Copies the data pointed to by src to dst. dst is converted to
 *  a "void *" and src is converted to a "const void *".
 *  @param dst Data pointer.
 *  @param src Source Lua string or pointer.
 *  @param len The number of bytes to copy. If src is a Lua string, len
 *  must not exceed `src.length + 1`.
 */
export function copy(dst: any, src: any, len?: number) : void;

/**
 *  Fills the data pointed to by dst with len constant bytes, given by c.
 *  @param dst Data pointer.
 *  @param len Byte count.
 *  @param c Constant byte. If omitted, the data is zero-filled.
 */
export function fill(dst: any, len: number, c?: number) : void;

/**
 *  Contains the target OS name: "Windows", "Linux", "OSX", "BSD", "POSIX"
 *  or "Other".
 */
export const os : string;

/**
 *  Contains the target architecture name: "x86", "x64", "arm", "arm64",
 *  "arm64be", "ppc", "mips", "mipsel", "mips64", "mips64el", "mips64r6",
 *  "mips64r6el".
 */
export const arch : string;

}