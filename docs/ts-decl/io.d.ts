declare interface FileHandle {

    /**
     *  Closes file. Note that files are automatically closed when their
     *  handles are garbage collected, but that takes an unpredictable amount
     *  of time to happen.
     */
    close() : void;

    /**
     *  Saves any written data to file.
     */
    flush() : void;

    /**
     *  Reads the file file, according to the given formats, which specify
     *  what to read. For each format, the function returns a string (or
     *  a number) with the characters read, or nil if it cannot read data with
     *  the specified format. When called without formats, it uses a default
     *  format that reads the entire next line (see below).
     *
     *  @param format  The available formats are
     *  * "*n": reads a number; this is the only format that returns a number
     *  instead of a string.
     *  * "*a": reads the whole file, starting at the current position. On end
     *  of file, it returns the empty string.
     *  * "*l": reads the next line (skipping the end of line), returning nil
     *  on end of file. This is the default format.
     *  * number: reads a string with up to this number of characters,
     *  returning nil on end of file. If number is zero, it reads nothing and
     *  returns an empty string, or nil on end of file.
     */
    read(format: string | number) : string | null;

    /**
     *  Sets and gets the file position, measured from the beginning of
     *  the file, to the position given by offset plus a base specified by
     *  the string whence.
     *  @param whence
     *  * "set": base is position 0 (beginning of the file);
     *  * "cur": base is current position;
     *  * "end": base is end of file;
     *  @param offset
     *  @return In case of success, the final file position, measured in bytes
     *  from the beginning of the file. If this function fails, null.
     */
    seek(whence?: string, offset?: number) : number | null;

    /**
     *  Sets the buffering mode for an output file.
     *  @param mode
     *  * "no": no buffering; the result of any output operation appears
     *  immediately.
     *  * "full": full buffering; output operation is performed only when
     *  the buffer is full (or when you explicitly flush the file).
     *  * "line": line buffering; output is buffered until a newline is output
     *  or there is any input from some special files (such as a terminal).
     *  @param size The size of the buffer, in bytes.
     */
    setvbuf(mode: string, size?: number) : void;

    /**
     *  Writes the value of each of its arguments to the file.
     */
    write(...value: (string | number)[]) : void;

}

declare module 'io' {

export const stdin : FileHandle;
export const stdout : FileHandle;
export const stderr : FileHandle;

/**
 *  Opens a file, in the mode specified in the string mode.
 *  @param filename
 *  @param mode
 *  * "r": read mode (the default);
 *  * "w": write mode;
 *  * "a": append mode;
 *  * "r+": update mode, all previous data is preserved;
 *  * "w+": update mode, all previous data is erased;
 *  * "a+": append update mode, previous data is preserved, writing is only
 *  allowed at the end of file.
 *  @return A new file handle, or, in case of errors, null.
 */
export function open(filename: string, mode?: string) : FileHandle;

/**
 *  Starts program prog in a separated process and returns a file handle that
 *  you can use to read data from this program (if mode is "r", the default)
 *  or to write data to this program (if mode is "w").
 */
export function popen(prog: string, mode?: string) : FileHandle;

/**
 *  Returns a handle for a temporary file. This file is opened in update mode
 *  and it is automatically removed when the program ends.
 */
export function tmpfile() : FileHandle;

}