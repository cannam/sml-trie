
(* Read words from a text file and count occurrences using a trie
   map. At the end, list the words found, and for each word, also
   print any other words that started with it along with their
   occurrence counts. *)

functor ReadSummariseFn (T : STRING_TRIE_MAP) = struct

    fun loadFromToken (token, trie) =
        case String.translate
                 (fn c => if Char.isAlpha c
                          then str c
                          else "")
                 token of
            "" => trie
          | word => 
            case T.find (trie, word) of
                NONE => T.insert (trie, word, 1)
              | SOME n => T.insert (trie, word, n+1)
            
    fun loadFromLine (line, trie) =
        foldl loadFromToken trie (String.tokens Char.isSpace line)

    fun loadFromStream (stream, trie) =
      case TextIO.inputLine stream of
          NONE => trie
        | SOME line => loadFromStream (stream, loadFromLine (line, trie))

    fun summarise trie =
        let fun wc (k, v) = k ^ " (" ^ Int.toString v ^ ")"
        in
            app (fn (k, v) =>
                    let val matching = tl (T.prefixMatch (trie, k))
                    in
                        print (wc (k, v) ^
                               (case matching of
                                    [] => ""
                                  | m => ": " ^
                                         (String.concatWith ", " (map wc m)))
                               ^ "\n")
                    end)
                (T.enumerate trie)
        end
                
    fun process infile =
        let val instream = TextIO.openIn infile
            val trie = loadFromStream (instream, T.empty)
        in
            summarise trie;
            TextIO.closeIn instream
        end
end

structure ReadSummarise = ReadSummariseFn(StringATrieMap)
                                                  
fun usage () =
    let open TextIO
    in
        output (stdErr,
                "\nUsage:\n" ^
                "    readtest infile\n" ^
                "\n");
        raise Fail "Incorrect arguments specified"
    end
                 
fun handleArgs args =
    case args of
        [infile] => ReadSummarise.process infile
      | _ => usage ()
           
fun main () =
    handleArgs (CommandLine.arguments ())
    handle Fail msg =>
           (TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n");
            OS.Process.exit OS.Process.failure)


