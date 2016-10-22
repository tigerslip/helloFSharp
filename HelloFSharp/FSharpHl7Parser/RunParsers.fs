module RunParsers

open FParsec

let internal runParser p hl7 = match run p hl7 with
                                    | Success(result, _, _) -> result
                                    | Failure(errorMsg, _, _) -> raise(System.Exception(errorMsg))