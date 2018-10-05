[<AutoOpen>]
module Helpers

open System.IO

//type ConsoleWrapper (wrapped:TextWriter) =
//    inherit TextWriter()
//    override __.Encoding with get() = wrapped.Encoding
//    override __.Close() = wrapped.Close()
//    override __.Dispose() = wrapped.Dispose()
//    override
type ConsoleWrapper(wrapped:TextWriter,fOnWrite) =
    inherit TextWriter()
    override __.FormatProvider with get() = wrapped.FormatProvider
    override __.Encoding with get() = wrapped.Encoding
    override __.NewLine with get() = wrapped.NewLine and set v = wrapped.NewLine <-v
    override __.Close() = wrapped.Close ()
    override __.Flush() = wrapped.Flush ()
    override __.Write(value:char) =fOnWrite(); wrapped.Write (value)
    override __.Write(buffer:char[]) =fOnWrite(); wrapped.Write (buffer)
    override __.Write(buffer:char[],index:int32,count:int32) =fOnWrite(); wrapped.Write (buffer,index,count)
    override __.Write(value:bool) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:int32) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:uint32) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:int64) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:uint64) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:single) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:double) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:decimal) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:string) =fOnWrite(); wrapped.Write (value)
    override __.Write(value:obj) =fOnWrite(); wrapped.Write (value)
    override __.Write(format:string,arg0:obj) =fOnWrite(); wrapped.Write (format,arg0)
    override __.Write(format:string,arg0:obj,arg1:obj) =fOnWrite(); wrapped.Write (format,arg0,arg1)
    override __.Write(format:string,arg0:obj,arg1:obj,arg2:obj) =fOnWrite(); wrapped.Write (format,arg0,arg1,arg2)
    override __.Write(format:string,arg:obj[]) =fOnWrite(); wrapped.Write (format,arg)
    override __.WriteLine() =fOnWrite(); wrapped.WriteLine ()
    override __.WriteLine(value:char) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(buffer:char[]) =fOnWrite(); wrapped.WriteLine (buffer)
    override __.WriteLine(buffer:char[],index:int32,count:int32) =fOnWrite(); wrapped.WriteLine (buffer,index,count)
    override __.WriteLine(value:bool) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:int32) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:uint32) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:int64) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:uint64) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:single) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:double) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:decimal) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:string) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(value:obj) =fOnWrite(); wrapped.WriteLine (value)
    override __.WriteLine(format:string,arg0:obj) =fOnWrite(); wrapped.WriteLine (format,arg0)
    override __.WriteLine(format:string,arg0:obj,arg1:obj) =fOnWrite(); wrapped.WriteLine (format,arg0,arg1)
    override __.WriteLine(format:string,arg0:obj,arg1:obj,arg2:obj) =fOnWrite(); wrapped.WriteLine (format,arg0,arg1,arg2)
    override __.WriteLine(format:string,arg:obj[]) =fOnWrite(); wrapped.WriteLine (format,arg)
    override __.WriteAsync(value:char) =fOnWrite(); wrapped.WriteAsync (value)
    override __.WriteAsync(value:string) =fOnWrite(); wrapped.WriteAsync (value)
    member __.WriteAsync(buffer:char[]) =fOnWrite(); wrapped.WriteAsync (buffer)
    override __.WriteAsync(buffer:char[],index:int32,count:int32) =fOnWrite(); wrapped.WriteAsync (buffer,index,count)
    override __.WriteLineAsync(value:char) =fOnWrite(); wrapped.WriteLineAsync (value)
    override __.WriteLineAsync(value:string) =fOnWrite(); wrapped.WriteLineAsync (value)
    member __.WriteLineAsync(buffer:char[]) =fOnWrite(); wrapped.WriteLineAsync (buffer)
    override __.WriteLineAsync(buffer:char[],index:int32,count:int32) =fOnWrite(); wrapped.WriteLineAsync (buffer,index,count)
    override __.WriteLineAsync() =fOnWrite(); wrapped.WriteLineAsync ()
    override __.FlushAsync() = wrapped.FlushAsync ()
    override __.InitializeLifetimeService() = wrapped.InitializeLifetimeService ()
    override __.CreateObjRef(requestedType) = wrapped.CreateObjRef requestedType






