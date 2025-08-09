/// <reference path="./io.d.ts" />
/// <reference path="./os.d.ts" />
/// <reference path="./ffi.d.ts" />
/// <reference path="./string.buffer.d.ts" />

declare type Metatable = {
    __index?: { [method: string]: any } | ((key: string) => any),
    __add?: (other: any) => any,
    __sub?: (other: any) => any,
    __mul?: (other: any) => any,
    __div?: (other: any) => any,
    __pow?: (other: any) => any,
    __unm?: () => any,
    __eq?: (other: any) => boolean,
    __lt?: (other: any) => boolean,
    __le?: (other: any) => boolean,
    __gc?: () => void,
    __mode?: 'v' | 'k' | 'kv',
    __tostring?: () => string
};

interface String {

    /**
     *  Returns a formatted version of its variable number of arguments
     *  following the description given in the string. The description
     *  follows the same rules as the printf family of standard C functions.
     */
    format(...args: any[]) : string;

}

declare var module : { exports: any, id: string };

/** Same as module.exports */
declare var exports : any;

/** Load the module by id. A CommonJS-like loader will be used for JS. */
declare function require(id: string) : any;

/**
 *  Returns the metatable for object. If object does not have a metatable,
 *  returns nil. If object is a class, return the metatable for its instances.
 */
declare function getmetatable(object: any) : Metatable | null;

/**
 *  Sets the metatable for the given table. If metatable is nil, removes
 *  the metatable of the given table.
 *  @returns The value in `object`.
 */
declare function setmetatable(object: any, metatable: Metatable) : any;

/**
 *  A global variable that holds the global environment (that is, _G._G = _G).
 */
declare var _G : { [field: string]: any };

declare var jsrt : {

    /**
     *  Load a JS code file as FunctionBody.
     *  @param filename JS code file
     */
    loadFile(filename?: string) : Function;

    /**
     *  Load a JS code string as FunctionBody.
     *  @param str JS code
     *  @param name Name in stacktrace
     */
    loadString(str: string, name?: string) : Function;

    /** Flush pending Promise microtasks. */
    flushTask();

};

/*  Object process contains command line arguments and index-based access to
    environment variables. Available if launched via jsrt CLI. */
declare var process : null | {
    argv: string[],
    env: { [name: string]: string }
};
