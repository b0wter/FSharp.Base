namespace b0wter.FSharp

module Streams =

    /// <summary>
    /// Reads a stream to the end synchrounously.
    /// </summary>
    let readStream (encoding: System.Text.Encoding) (stream: System.IO.Stream) =
        use reader = new System.IO.StreamReader(stream, encoding)
        reader.ReadToEnd ()

    /// <summary>
    /// Reads a stream to the end asynchrounously.
    /// </summary>
    let readStreamAsync (encoding: System.Text.Encoding) (stream: System.IO.Stream) =
        use reader = new System.IO.StreamReader(stream, encoding)
        reader.ReadToEndAsync () |> Async.AwaitTask