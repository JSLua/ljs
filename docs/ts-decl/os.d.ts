declare module 'os' {

/**
 *  Returns an approximation of the amount in seconds of CPU time used by
 *  the program.
 */
export function clock() : number;

/**
 *  Returns a string or a table containing date and time, formatted according
 *  to the given string format.
 *  @param format If format starts with '!', then the date is formatted in
 *  Coordinated Universal Time. After this optional character, if format is
 *  the string "*t", then date returns a table. Otherwise, date returns
 *  the date as a string, formatted according to the same rules as
 *  the C function strftime.
 *  @param time If present, this is the time to be formatted. Otherwise, date
 *  formats the current time.
 */
export function date(format?: string, time?: number) : string | {
    year: number, month: number, day: number,
    hour: number, min: number,   sec: number,
    wday: number, yday: number,  isdst: boolean
};

/**
 *  Returns the number of seconds from time t1 to time t2. In POSIX, Windows,
 *  and some other systems, this value is exactly t2-t1.
 */
export function difftime(t2: number, t1: number) : number;

/**
 *  This function is equivalent to the C function system. It passes command
 *  to be executed by an operating system shell. It returns a status code,
 *  which is system-dependent. If command is absent, then it returns nonzero
 *  if a shell is available and zero otherwise.
 */
export function execute(command?: string) : number;

/**
 *  Calls the C function exit, with an optional code, to terminate the host
 *  program. The default value for code is the success code.
 */
export function exit(code?: number) : void;

/**
 *  Returns the value of the process environment variable varname, or null
 *  if the variable is not defined.
 */
export function getenv(varname: string) : string | null;

/**
 *  Deletes the file or directory with the given name. Directories must be
 *  empty to be removed.
 */
export function remove(filename: string) : void;

/**
 *  Renames file or directory named oldname to newname.
 */
export function rename(oldname: string, newname: string) : void;

/**
 *  Sets the current locale of the program.
 *  @param locale a string specifying a locale. If empty, the current locale is
 *  set to an implementation-defined native locale. If locale is the string "C",
 *  the current locale is set to the standard C locale.
 *  @param category an optional string describing which category to change:
 *  "all", "collate", "ctype", "monetary", "numeric", or "time"; the default
 *  category is "all".
 *  @returns The name of the new locale, or null if the request cannot be
 *  honored. When called with null as locale, the name of the current locale
 *  for the given category.
 */
export function setlocale(locale: string | null, category?: string) : void;

/**
 *  Returns the current time when called without arguments, or a time
 *  representing the date and time specified by the given table.
 *  @returns a number, whose meaning depends on your system. In POSIX, Windows,
 *  and some other systems, this number counts the number of seconds since some
 *  given start time (the "epoch").
 */
export function time(table?: {
    year: number, month: number, day: number,
    hour?: number, min?: number, sec?: number
}) : number;

/**
 *  Returns a string with a file name that can be used for a temporary file.
 *  The file must be explicitly opened before its use and explicitly removed
 *  when no longer needed.
 *
 *  On some systems (POSIX), this function also creates a file with that name,
 *  to avoid security risks. You still have to open the file to use it and to
 *  remove it (even if you do not use it).
 */
export function tmpname() : string;

}