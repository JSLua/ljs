declare interface StringBuffer {

    /**
     *  Reset (empty) the buffer. The allocated buffer space is not freed and
     *  may be reused.
     */
    reset() : void;

    /**
     *  The buffer space of the buffer object is freed. The object itself
     *  remains intact, empty and may be reused.
     */
    free() : void;

    /**
     *  Appends a string str, a number num or any object obj with a __tostring
     *  metamethod to the buffer. Multiple arguments are appended in the given
     *  order.
     */
    put(...str) : StringBuffer;

    /**
     *  Appends the formatted arguments to the buffer. The format string
     *  supports the same options as string.format().
     */
    putf(format : string, ...arg) : StringBuffer;

    /**
     *  Appends the given len number of bytes from the memory pointed to by
     *  the FFI cdata object to the buffer. The object needs to be convertible
     *  to a (constant) pointer.
     */
    putcdata(cdata, len : number) : StringBuffer;

    /**
     *  Reserve at least size bytes of write space in the buffer. It returns
     *  an uint8_t * FFI cdata pointer ptr that points to this space.
     */
    reserve(size : number) : any;

    /**
     *  Append the used bytes of the previously reserved write space to
     *  the buffer data.
     */
    commit(used) : StringBuffer;

    /**
     *  Skips (consumes) len bytes from the buffer up to the current length
     *  of the buffer data.
     */
    skip(len) : StringBuffer;

    /**
     *  Consumes the buffer data and returns one or more strings. If called
     *  without arguments, the whole buffer data is consumed. If called with
     *  a number, up to len bytes are consumed. A nil argument consumes the
     *  remaining buffer space (this only makes sense as the last argument).
     */
    get(len? : number | null) : string;

    /**
     *  Returns an uint8_t * FFI cdata pointer ptr that points to the buffer
     *  data.
     */
    ref() : any;

    /**
     *  The current length of the buffer data in bytes.
     */
    get length() : number;

}

declare module 'string.buffer' {

    /**
     *  Creates a new buffer object.
     *  @param size The minimum initial buffer size. This is strictly
     *  an optimization when the required buffer size is known beforehand.
     *  The buffer space will grow as needed, in any case.
     */
    function _new(size? : number) : StringBuffer;
    export { _new as new };

    }